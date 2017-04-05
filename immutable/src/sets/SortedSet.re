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
  let reduceReversed: while_::('acc => a => bool)? => ('acc => a => 'acc) => 'acc => t => 'acc;
  let toIterableReversed: t => Iterable.t a;
  let toNavigableCollection: t => NavigableCollection.t a;
  let toSequenceReversed: t => Sequence.t a;
  let toNavigableSet: t => NavigableSet.t a;
  let contains: a => t => bool;
  let toSet: t => ImmSet.t a;
  let reduce: while_::('acc => a => bool)? => ('acc => a => 'acc) => 'acc => t => 'acc;
  let toIterable: t => Iterable.t a;
  let equals: Equality.t t;
  let count: t => int;
  let empty: t;
  let isEmpty: t => bool;
  let isNotEmpty: t => bool;
  let toCollection: t => Collection.t a;
  let toSequence: t => Sequence.t a;
  let removeAll: t => t;
  let add: a => t => t;
  let addAll: Iterable.t a => t => t;
  let from: Iterable.t a => t;
  let intersect: t => t => t;
  let remove: a => t => t;
  let subtract: t => t => t;
  let union: t => t => t;
  let removeFirstOrRaise: t => t;
  let removeLastOrRaise: t => t;
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

  let addAll (iter: Iterable.t a) (sortedSet: t): t => iter
    |> Iterable.reduce (fun acc next => acc |> add next) sortedSet;

  let contains (x: a) ({ tree }: t): bool =>
    AVLTreeSet.contains comparator x tree;

  let count ({ count }: t): int => count;

  let empty: t = { count: 0, tree: AVLTreeSet.Empty };

  let isEmpty ({ count }: t): bool => count === 0;

  let isNotEmpty ({ count }: t): bool => count !== 0;

  let from (iter: Iterable.t a): t =>
    empty |> addAll iter;

  let reduceImpl
      while_::(predicate: 'acc => a => bool)
      (f: 'acc => a => 'acc)
      (acc: 'acc)
      ({ tree }: t): 'acc =>
    if (predicate === Functions.alwaysTrue2) (AVLTreeSet.reduce f acc tree)
    else (AVLTreeSet.reduceWhile predicate f acc tree);

  let reduce
      while_::(predicate: 'acc => a => bool)=Functions.alwaysTrue2
      (f: 'acc => a => 'acc)
      (acc: 'acc)
      (set: t): 'acc =>
    reduceImpl while_::predicate f acc set;

  let reduceReversedImpl
      while_::(predicate: 'acc => a => bool)
      (f: 'acc => a => 'acc)
      (acc: 'acc)
      ({ tree }: t): 'acc =>
    if (predicate === Functions.alwaysTrue2) (AVLTreeSet.reduceReversed f acc tree)
    else (AVLTreeSet.reduceReversedWhile predicate f acc tree);

  let reduceReversed
      while_::(predicate: 'acc => a => bool)=Functions.alwaysTrue2
      (f: 'acc => a => 'acc)
      (acc: 'acc)
      (set: t): 'acc =>
    reduceReversedImpl while_::predicate f acc set;

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

  let toSequenceReversed ({ tree }: t): (Sequence.t a) =>
    tree |> AVLTreeSet.toSequenceReversed;

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

  let iterator: Iterable.Iterator.t a t = { reduce: reduceImpl };

  let toIterable (set: t): (Iterable.t a) =>
    if (isEmpty set) (Iterable.empty ())
    else Iterable.Iterable set iterator;

  let iteratorReversed: Iterable.Iterator.t a t = { reduce: reduceReversedImpl };

  let toIterableReversed (set: t): (Iterable.t a) =>
    if (isEmpty set) (Iterable.empty ())
    else Iterable.Iterable set iteratorReversed;

  let collectionOps: Collection.Ops.t a t = {
    count,
    toIterable,
    toSequence,
  };

  let toCollection (set: t): (Collection.t a) =>
    if (isEmpty set) (Collection.empty ())
    else Collection.Collection set collectionOps;

  let seqCollectionOps: SequentialCollection.Ops.t a t = {
    count,
    first,
    firstOrRaise,
    toCollection,
    toIterable,
    toSequence,
  };

  let toSequentialCollection (set: t): (SequentialCollection.t a) =>
    if (isEmpty set) (SequentialCollection.empty ())
    else SequentialCollection.SequentialCollection set seqCollectionOps;

  let navCollectionOps: NavigableCollection.Ops.t a t = {
    count,
    first,
    firstOrRaise,
    last,
    lastOrRaise,
    toCollection,
    toSequentialCollection,
    toIterable,
    toIterableReversed,
    toSequence,
    toSequenceReversed,
  };

  let toNavigableCollection (set: t): (NavigableCollection.t a) =>
    if (isEmpty set) (NavigableCollection.empty ())
    else NavigableCollection.NavigableCollection set navCollectionOps;

  let setOps: ImmSet.Ops.t a t = {
    contains,
    count,
    toCollection,
    toIterable,
    toSequence,
  };

  let toSet (set: t): (ImmSet.t a) =>
    if (isEmpty set) (ImmSet.empty ())
    else ImmSet.Set set setOps;

  let navigableSetOps: NavigableSet.Ops.t a t = {
    contains,
    count,
    first,
    firstOrRaise,
    last,
    lastOrRaise,
    toCollection,
    toIterable,
    toIterableReversed,
    toNavigableCollection,
    toSequence,
    toSequenceReversed,
    toSequentialCollection,
    toSet,
  };

  let toNavigableSet (set: t): (NavigableSet.t a) =>
    if (isEmpty set) (NavigableSet.empty ())
    else NavigableSet.NavigableSet set navigableSetOps;

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
