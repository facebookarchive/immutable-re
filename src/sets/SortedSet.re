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
  let toSequentialCollection: t => SequentialCollection.t a;
  let last: t => option a;
  let lastOrRaise: t => a;
  let reduceReversed:
    while_::('acc => a => bool)? => ('acc => a => 'acc) => 'acc => t => 'acc;
  let toCollectionReversed: t => Collection.t a;
  let toIterableReversed: t => Iterable.t a;
  let toNavigableCollection: t => NavigableCollection.t a;
  let toNavigableCollectionReversed: t => NavigableCollection.t a;
  let toSequenceReversed: t => Sequence.t a;
  let toSequentialCollectionReversed: t => SequentialCollection.t a;
  let toNavigableSet: t => NavigableSet.t a;
  let toNavigableSetReversed: t => NavigableSet.t a;
  let toSetReversed: t => ImmSet.t a;
  let equals: Equality.t t;
  let contains: a => t => bool;
  let toSet: t => ImmSet.t a;
  let every: (a => bool) => t => bool;
  let find: (a => bool) => t => (option a);
  let findOrRaise: (a => bool) => t => a;
  let forEach: while_::(a => bool)? => (a => unit) => t => unit;
  let none: (a => bool) => t => bool;
  let reduce: while_::('acc => a => bool)? => ('acc => a => 'acc) => 'acc => t => 'acc;
  let some: (a => bool) => t => bool;
  let toIterable: t => (Iterable.t a);
  let count: t => int;
  let isEmpty: t => bool;
  let isNotEmpty: t => bool;
  let toCollection: t => Collection.t a;
  let toSequence: t => Sequence.t a;
  let removeAll: t => t;
  let add: a => t => t;
  let addAll: Iterable.t a => t => t;
  let intersect: t => t => t;
  let remove: a => t => t;
  let subtract: t => t => t;
  let union: t => t => t;
  let removeFirstOrRaise: t => t;
  let removeLastOrRaise: t => t;
  let empty: unit => t;
  let from: Iterable.t a => t;
};

let module Make = fun (Comparable: Comparable.S) => {
  type a = Comparable.t;

  type t = {
    count: int,
    tree: AVLTreeSet.t a,
  };

  let comparator = Comparable.compare;

  include (NavigableSet.Make {
    type nonrec a = a;
    type nonrec t = t;

    let contains (x: a) ({ tree }: t): bool =>
      AVLTreeSet.contains comparator x tree;

    let count ({ count }: t): int => count;

    let first ({ tree }: t): (option a) =>
      AVLTreeSet.first tree;

    let firstOrRaise ({ tree }: t): a =>
      AVLTreeSet.firstOrRaise tree;

    let last ({ tree }: t): (option a) =>
      AVLTreeSet.last tree;

    let lastOrRaise ({ tree }: t): a =>
      AVLTreeSet.lastOrRaise tree;

    let reduce
        while_::(predicate: 'acc => a => bool)
        (f: 'acc => a => 'acc)
        (acc: 'acc)
        ({ tree }: t): 'acc =>
      if (predicate === Functions.alwaysTrue2) (AVLTreeSet.reduce f acc tree)
      else (AVLTreeSet.reduceWhile predicate f acc tree);

    let reduceReversed
        while_::(predicate: 'acc => a => bool)
        (f: 'acc => a => 'acc)
        (acc: 'acc)
        ({ tree }: t): 'acc =>
      if (predicate === Functions.alwaysTrue2) (AVLTreeSet.reduceReversed f acc tree)
      else (AVLTreeSet.reduceReversedWhile predicate f acc tree);

    let toSequence ({ tree }: t): (Sequence.t a) =>
      tree |> AVLTreeSet.toSequence;

    let toSequenceReversed ({ tree }: t): (Sequence.t a) =>
      tree |> AVLTreeSet.toSequenceReversed;
  }: NavigableSet.S with type t:= t and type a:= a);

  let add (x: a) ({ count, tree } as sortedSet: t): t => {
    let newTree = tree |> AVLTreeSet.add comparator x;
    if (newTree === tree) sortedSet else { count: count + 1, tree: newTree }
  };

  let addAll (iter: Iterable.t a) (sortedSet: t): t => iter
    |> Iterable.reduce (fun acc next => acc |> add next) sortedSet;

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

  let emptyInstance: t = { count: 0, tree: AVLTreeSet.Empty };

  let empty (): t => emptyInstance;

  let rec equalsWith (equality: Equality.t 'a) (this: Sequence.t 'a) (that: Sequence.t 'a): bool =>
    (that === this) ||
    switch (that (), this ()) {
      | (Sequence.Next thisValue thisNext, Sequence.Next thatValue thatNext) =>
          if (equality thisValue thatValue) (equalsWith equality thisNext thatNext)
          else false
      | (Sequence.Completed, Sequence.Completed) => true
      | _ => false
    };

  let equality = Comparator.toEquality comparator;
  let equals (this: t) (that: t): bool => equalsWith
    equality
    (toSequence this)
    (toSequence that);

  let from (iter: Iterable.t a): t =>
    emptyInstance |> addAll iter;

  let remove (x: a) ({ count, tree } as sortedSet: t): t => {
    let newTree = AVLTreeSet.remove comparator x tree;
    if (newTree === tree) sortedSet else { count: count - 1, tree: newTree }
  };

  let removeAll (_: t): t =>
    emptyInstance;

  let removeFirstOrRaise ({ count, tree }: t): t => {
    let newTree = AVLTreeSet.removeFirstOrRaise tree;
    { count: count - 1, tree: newTree }
  };

  let removeLastOrRaise ({ count, tree }: t): t => {
    let newTree = AVLTreeSet.removeLastOrRaise tree;
    { count: count - 1, tree: newTree }
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
};
