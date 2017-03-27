/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

module type S = {
  type a;
  type t;

  let compare: Comparator.t t;
  let reduceRight:
    while_::('acc => a => bool)? => ('acc => a => 'acc) => 'acc => t => 'acc;
  let first: t => option a;
  let firstOrRaise: t => a;
  let last: t => option a;
  let lastOrRaise: t => a;
  let toIteratorRight: t => Iterator.t a;
  let toSequenceRight: t => Sequence.t a;
  let contains: a => t => bool;
  let toSet: t => ImmSet.t a;
  let reduce:
    while_::('acc => a => bool)? => ('acc => a => 'acc) => 'acc => t => 'acc;
  let toIterator: t => Iterator.t a;
  let equals: Equality.t t;
  let count: t => int;
  let empty: t;
  let isEmpty: t => bool;
  let isNotEmpty: t => bool;
  let toSequence: t => Sequence.t a;
  let removeAll: t => t;
  let add: a => t => t;
  let addAll: Iterator.t a => t => t;
  let from: Iterator.t a => t;
  let intersect: t => t => t;
  let remove: a => t => t;
  let subtract: t => t => t;
  let union: t => t => t;
  let removeFirstOrRaise: t => t;
  let removeLastOrRaise: t => t;
  let module Reducer: Reducer.S with type a:= a and type t:= t;
};

let module Make = fun (Comparable: Comparable.S) => {
  type a = Comparable.t;

  type t = {
    count: int,
    tree: AVLTreeSet.t a,
  };

  let comparator = Comparable.compare;

  let add (x: a) ({ count, tree } as sortedSet: t): t => {
    let newTree = tree |> AVLTreeSet.add comparator x;
    if (newTree === tree) sortedSet else { count: count + 1, tree: newTree }
  };

  let addAll (iter: Iterator.t a) (sortedSet: t): t => iter
    |> Iterator.reduce (fun acc next => acc |> add next) sortedSet;

  let contains (x: a) ({ tree }: t): bool =>
    AVLTreeSet.contains comparator x tree;

  let count ({ count }: t): int => count;

  let empty: t = { count: 0, tree: AVLTreeSet.Empty };

  let isEmpty ({ count }: t): bool => count === 0;

  let isNotEmpty ({ count }: t): bool => count !== 0;

  let from (iter: Iterator.t a): t =>
    empty |> addAll iter;

  let reduce
      while_::(predicate: 'acc => a => bool)=Functions.alwaysTrue2
      (f: 'acc => a => 'acc)
      (acc: 'acc)
      ({ tree }: t): 'acc =>
    if (predicate === Functions.alwaysTrue2) (AVLTreeSet.reduce f acc tree)
    else (AVLTreeSet.reduceWhile predicate f acc tree);

  let reduceRight
      while_::(predicate: 'acc => a => bool)=Functions.alwaysTrue2
      (f: 'acc => a => 'acc)
      (acc: 'acc)
      ({ tree }: t): 'acc =>
    if (predicate === Functions.alwaysTrue2) (AVLTreeSet.reduceRight f acc tree)
    else (AVLTreeSet.reduceRightWhile predicate f acc tree);

  let remove (x: a) ({ count, tree } as sortedSet: t): t => {
    let newTree = AVLTreeSet.remove comparator x tree;
    if (newTree === tree) sortedSet else { count: count - 1, tree: newTree }
  };

  let removeAll (_: t): t =>
    empty;

  let removeFirstOrRaise ({ count, tree }: t): t => {
    let newTree = AVLTreeSet.removeFirstOrRaise tree;
    { count: count - 1, tree: newTree }
  };

  let removeLastOrRaise ({ count, tree }: t): t => {
    let newTree = AVLTreeSet.removeLastOrRaise tree;
    { count: count - 1, tree: newTree }
  };

  let toSequence ({ tree }: t): (Sequence.t a) =>
    tree |> AVLTreeSet.toSequence;

  let toSequenceRight ({ tree }: t): (Sequence.t a) =>
    tree |> AVLTreeSet.toSequenceRight;

  let rec compareWith
      (valueCompare: Comparator.t 'a)
      (this: Sequence.t 'a)
      (that: Sequence.t 'a): Ordering.t => switch (this (), that ()) {
    | (Sequence.Next thisValue thisNext, Sequence.Next thatValue thatNext) =>
        let cmp = valueCompare thisValue thatValue;

        if (cmp === Ordering.equal) (compareWith valueCompare thisNext thatNext)
        else cmp
    | (Sequence.Completed, Sequence.Completed) => Ordering.equal
    | (Sequence.Next _ _, Sequence.Completed) => Ordering.greaterThan
    | (Sequence.Completed, Sequence.Next _ _) => Ordering.lessThan
  };

  let compare
      (this: t)
      (that: t): Ordering.t =>
    compareWith comparator (toSequence this) (toSequence that);

  let rec equalsWith (equality: Equality.t 'a) (this: Sequence.t 'a) (that: Sequence.t 'a): bool =>
    (that === this) ||
    switch (that (), this ()) {
      | (Sequence.Next thisValue thisNext, Sequence.Next thatValue thatNext) =>
          if (equality thisValue thatValue) (equalsWith equality thisNext thatNext)
          else false
      | (Sequence.Completed, Sequence.Completed) => true
      | _ => false
    };

  let equals
      (this: t)
      (that: t): bool =>
    (this === that) || (
      equalsWith
        (Comparator.toEquality comparator)
        (toSequence this)
        (toSequence that)
    );

  let first ({ tree }: t): (option a) =>
    AVLTreeSet.first tree;

  let firstOrRaise ({ tree }: t): a =>
    AVLTreeSet.firstOrRaise tree;

  let last ({ tree }: t): (option a) =>
    AVLTreeSet.last tree;

  let lastOrRaise ({ tree }: t): a =>
    AVLTreeSet.lastOrRaise tree;

  let toIterator (set: t): (Iterator.t a) =>
    if (isEmpty set) (Iterator.empty ())
    else { reduce: fun predicate f acc => reduce while_::predicate f acc set };

  let toIteratorRight (set: t): (Iterator.t a) =>
    if (isEmpty set) (Iterator.empty ())
    else { reduce: fun predicate f acc => reduceRight while_::predicate f acc set };

  let toSet (set: t): (ImmSet.t a) => {
    contains: fun a => contains a set,
    count: count set,
    iterator: fun () => toIterator set,
    sequence: fun () => toSequence set,
  };

  let intersect (this: t) (that: t): t =>
    /* FIXME: Improve this implementation */
    ImmSet.intersect (toSet this) (toSet that) |> from;

  let subtract (this: t) (that: t): t =>
    /* FIXME: Improve this implementation */
    ImmSet.subtract (toSet this) (toSet that) |> from;

  let union (this: t) (that: t): t =>
    /* FIXME: Improve this implementation */
    ImmSet.union (toSet this) (toSet that) |> from;

  let module Reducer = Reducer.Make {
    type nonrec a = a;
    type nonrec t = t;

    let reduce = reduce;
  };
};
