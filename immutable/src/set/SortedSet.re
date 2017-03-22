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
  let first: t => option a;
  let firstOrRaise: t => a;
  let reduceRight:
    while_::('acc => a => bool)? => ('acc => a => 'acc) => 'acc => t => 'acc;
  let last: t => option a;
  let lastOrRaise: t => a;
  let toIteratorRight: t => Iterator.t a;
  let toSequenceRight: t => Sequence.t a;
  let toKeyedIteratorRight: t => KeyedIterator.t a a;
  let equals: Equality.t t;
  let contains: a => t => bool;
  let toKeyedIterator: t => KeyedIterator.t a a;
  let toMap: t => ImmMap.t a a;
  let toSet: t => ImmSet.t a;
  let reduce:
    while_::('acc => a => bool)? => ('acc => a => 'acc) => 'acc => t => 'acc;
  let toIterator: t => Iterator.t a;
  let count: t => int;
  let isEmpty: t => bool;
  let isNotEmpty: t => bool;
  let toSequence: t => Sequence.t a;
  let removeAll: t => t;
  let add: a => t => t;
  let addAll: Iterator.t a => t => t;
  let intersect: t => t => t;
  let remove: a => t => t;
  let subtract: t => t => t;
  let union: t => t => t;
  let removeFirstOrRaise: t => t;
  let removeLastOrRaise: t => t;
  let empty: t;
  let from: Iterator.t a => t;
  let module Reducer: Reducer.S with type a := a and type t := t;
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

  let isEmpty ({ count }: t): bool => count == 0;

  let isNotEmpty ({ count }: t): bool => count != 0;

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
    if (isEmpty set) Iterator.empty
    else { reduce: fun predicate f acc => reduce while_::predicate f acc set };

  let toIteratorRight (set: t): (Iterator.t a) =>
    if (isEmpty set) Iterator.empty
    else { reduce: fun predicate f acc => reduceRight while_::predicate f acc set };

  let toKeyedIterator (set: t): (KeyedIterator.t a a)  =>
    if (isEmpty set) KeyedIterator.empty
    else {
      reduce: fun predicate f acc => set |> reduce
        while_::(fun acc next => predicate acc next next)
        (fun acc next => f acc next next)
        acc
    };

  let toKeyedIteratorRight (set: t): (KeyedIterator.t a a) =>
    if (isEmpty set) KeyedIterator.empty
    else {
      reduce: fun predicate f acc => set |> reduceRight
        while_::(fun acc next => predicate acc next next)
        (fun acc next => f acc next next)
        acc
    };

  let toSet (set: t): (ImmSet.t a) => {
    contains: fun a => contains a set,
    count: count set,
    iterator: toIterator set,
    sequence: toSequence set,
  };

  let toMap (set: t): (ImmMap.t a a) => {
    containsKey: fun k => set |> contains k,
    count: count set,
    get: fun k =>
      if (set |> contains k) (Some k)
      else None,
    getOrRaise: fun k =>
      if (set |> contains k) k
      else failwith "not found",
    keyedIterator: toKeyedIterator set,
    sequence: toSequence set |> Sequence.map (fun k => (k, k)),
  };

  let findRight (f: a => bool) (set: t): (option a) =>
    set |> toIteratorRight |> Iterator.Reducer.find f;

  let findRightOrRaise (f: a => bool) (set: t): a =>
    set |> toIteratorRight |> Iterator.Reducer.findOrRaise f;

  let intersect (this: t) (that: t): t =>
    /* FIXME: Improve this implementation to be O(log N) */
    ImmSet.intersect (toSet this) (toSet that) |> from;

  let subtract (this: t) (that: t): t =>
    /* FIXME: Improve this implementation to be O(log N) */
    ImmSet.subtract (toSet this) (toSet that) |> from;

  let union (this: t) (that: t): t =>
    /* FIXME: Improve this implementation to be O(log N) */
    ImmSet.union (toSet this) (toSet that) |> from;

  let module Reducer = Reducer.Make {
    type nonrec a = a;
    type nonrec t = t;

    let reduce = reduce;
  };
};
