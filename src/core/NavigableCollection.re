/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'a =
  | Empty
  | Instance
      'collection
      (SequentialCollection.s 'collection 'a)
      (SequentialCollection.s 'collection 'a): t 'a;

let count (collection: t 'a): int => switch collection {
  | Empty => 0
  | Instance collection { count } _ => count collection
};

let empty (): (t 'a) => Empty;

let first (collection: t 'a): (option 'a) => switch collection {
  | Empty => None
  | Instance collection { first } _ => first collection
};

let firstOrRaise (collection: t 'a): 'a => switch collection {
  | Empty => failwith "empty"
  | Instance collection { firstOrRaise } _ => firstOrRaise collection
};

let last (collection: t 'a): (option 'a) => switch collection {
  | Empty => None
  | Instance collection _ { first } => first collection
};

let lastOrRaise (collection: t 'a): 'a => switch collection {
  | Empty => failwith "empty"
  | Instance collection _ { firstOrRaise } => firstOrRaise collection
};

let isEmpty (collection: t 'a): bool =>
  (count collection) === 0;

let isNotEmpty (collection: t 'a): bool =>
  (count collection) !== 0;

let reduce
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (collection: t 'a): 'acc => switch collection {
  | Empty => acc
  | Instance collection { reduce } _ =>
      collection |> reduce while_::predicate f acc;
};

let reduceReversed
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (collection: t 'a): 'acc => switch collection {
  | Empty => acc
  | Instance collection _ { reduce } =>
      collection |> reduce while_::predicate f acc;
};

let toCollection (collection: t 'a): (Collection.t 'a) => switch collection {
  | Empty => Collection.empty ()
  | Instance collection { toCollection } _ => toCollection collection
};

let toCollectionReversed (collection: t 'a): (Collection.t 'a) => switch collection {
  | Empty => Collection.empty ()
  | Instance collection  _ { toCollection } => toCollection collection
};

let toIterable (collection: t 'a): (Iterable.t 'a) => switch collection {
  | Empty => Iterable.empty ()
  | Instance collection { toIterable } _ => toIterable collection
};

let toIterableReversed (collection: t 'a): (Iterable.t 'a) => switch collection {
  | Empty => Iterable.empty ()
  | Instance collection _ { toIterable } => toIterable collection
};

let toNavigableCollection (collection: t 'a): (t 'a) =>
  collection;

let toNavigableCollectionReversed (collection: t 'a): (t 'a) => switch collection {
  | Empty => Empty
  | Instance collection impl implReversed =>
      Instance collection implReversed impl
};

let toSequence (collection: t 'a): (Sequence.t 'a) => switch collection {
  | Empty => Sequence.empty ()
  | Instance collection { toSequence } _ => toSequence collection
};

let toSequenceReversed (collection: t 'a): (Sequence.t 'a) => switch collection {
  | Empty => Sequence.empty ()
  | Instance collection _ { toSequence } => toSequence collection
};

let toSequentialCollection (collection: t 'a): (SequentialCollection.t 'a) => switch collection {
  | Empty => SequentialCollection.empty ()
  | Instance collection sequentialCollectionBase _ =>
      SequentialCollection.Instance collection sequentialCollectionBase
};

let toSequentialCollectionReversed (collection: t 'a): (SequentialCollection.t 'a) => switch collection {
  | Empty => SequentialCollection.empty ()
  | Instance collection _ sequentialCollectionReversedBase =>
      SequentialCollection.Instance collection sequentialCollectionReversedBase
};

type navigableCollection 'a = t 'a;

module type S = {
  type a;
  type t;

  include SequentialCollection.S with type a := a and type t := t;

  let last: t => (option a);
  let lastOrRaise: t => a;
  let reduceReversed: while_::('acc => a => bool)? => ('acc => a => 'acc) => 'acc => t => 'acc;
  let toCollectionReversed: t => (Collection.t a);
  let toIterableReversed: t => (Iterable.t a);
  let toNavigableCollection: t => (navigableCollection a);
  let toNavigableCollectionReversed: t => (navigableCollection a);
  let toSequenceReversed: t => (Sequence.t a);
  let toSequentialCollectionReversed: t => (SequentialCollection.t a);
};

module type S1 = {
  type t 'a;

  include SequentialCollection.S1 with type t 'a := t 'a;

  let last: (t 'a) => (option 'a);
  let lastOrRaise: (t 'a) => 'a;
  let reduceReversed: while_::('acc => 'a => bool)? => ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let toCollectionReversed: t 'a => (Collection.t 'a);
  let toIterableReversed: t 'a => (Iterable.t 'a);
  let toNavigableCollection: (t 'a) => (navigableCollection 'a);
  let toNavigableCollectionReversed: (t 'a) => (navigableCollection 'a);
  let toSequenceReversed: (t 'a) => (Sequence.t 'a);
  let toSequentialCollectionReversed: t 'a => (SequentialCollection.t 'a);
};

let module Make = fun (Base: {
  type a;
  type t;

  let count: t => int;
  let first: t => (option a);
  let firstOrRaise: t => a;
  let last: t => (option a);
  let lastOrRaise: t => a;
  let reduce: while_::('acc => a => bool) => ('acc => a => 'acc) => 'acc => t => 'acc;
  let reduceReversed: while_::('acc => a => bool) => ('acc => a => 'acc) => 'acc => t => 'acc;
  let toSequence: t => Sequence.t a;
  let toSequenceReversed: t => Sequence.t a;
}) => ({
  include Base;

  include (SequentialCollection.Make Base: SequentialCollection.S with type t := t and type a := a);

  let module ReversedSequentialCollection = SequentialCollection.Make {
    type nonrec t = t;
    type nonrec a = a;

    let count = count;
    let first = last;
    let firstOrRaise = lastOrRaise;
    let reduce = Base.reduceReversed;
    let toSequence = toSequenceReversed;
  };

  let reduceReversed = ReversedSequentialCollection.reduce;

  let toIterableReversed = ReversedSequentialCollection.toIterable;

  let toSequenceReversed = ReversedSequentialCollection.toSequence;

  let toCollectionReversed = ReversedSequentialCollection.toCollection;

  let toSequentialCollectionReversed = ReversedSequentialCollection.toSequentialCollection;

  let navigableCollectionBase: SequentialCollection.s t a = {
    count,
    first,
    firstOrRaise,
    reduce: Base.reduce,
    toCollection,
    toIterable,
    toSequence,
  };

  let navigableCollectionReversedBase: SequentialCollection.s t a = {
    count,
    first: last,
    firstOrRaise: lastOrRaise,
    reduce: Base.reduceReversed,
    toCollection: toCollectionReversed,
    toIterable: toIterableReversed,
    toSequence: toSequenceReversed,
  };

  let toNavigableCollection (collection: t): (navigableCollection a) =>
    if (isEmpty collection) (empty ())
    else Instance collection navigableCollectionBase navigableCollectionReversedBase;

  let toNavigableCollectionReversed (collection: t): (navigableCollection a) =>
    if (isEmpty collection) (empty ())
    else Instance collection navigableCollectionReversedBase navigableCollectionBase;

}: S with type t := Base.t and type a := Base.a);

let module Make1 = fun (Base: {
  type t 'a;

  let count: t 'a => int;
  let first: t 'a => (option 'a);
  let firstOrRaise: t 'a => 'a;
  let last: t 'a => (option 'a);
  let lastOrRaise: t 'a => 'a;
  let reduce: while_::('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => t 'a => 'acc;
  let reduceReversed: while_::('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => t 'a => 'acc;
  let toSequence: t 'a => Sequence.t 'a;
  let toSequenceReversed: t 'a => Sequence.t 'a;
}) => ({
  include Base;

  include (SequentialCollection.Make1 Base: SequentialCollection.S1 with type t 'a := t 'a);

  let module ReversedSequentialCollection = SequentialCollection.Make1 {
    type nonrec t 'a = t 'a;

    let count = count;
    let first = last;
    let firstOrRaise = lastOrRaise;
    let reduce = Base.reduceReversed;
    let toSequence = toSequenceReversed;
  };

  let reduceReversed = ReversedSequentialCollection.reduce;

  let toIterableReversed = ReversedSequentialCollection.toIterable;

  let toSequenceReversed = ReversedSequentialCollection.toSequence;

  let toCollectionReversed = ReversedSequentialCollection.toCollection;

  let toSequentialCollectionReversed = ReversedSequentialCollection.toSequentialCollection;

  let navigableCollectionBase: SequentialCollection.s (t 'a) 'a = {
    count,
    first,
    firstOrRaise,
    reduce: Base.reduce,
    toCollection,
    toIterable,
    toSequence,
  };

  let navigableCollectionReversedBase: SequentialCollection.s (t 'a) 'a = {
    count,
    first: last,
    firstOrRaise: lastOrRaise,
    reduce: Base.reduceReversed,
    toCollection: toCollectionReversed,
    toIterable: toIterableReversed,
    toSequence: toSequenceReversed,
  };

  let toNavigableCollection (collection: t 'a): (navigableCollection 'a) =>
    if (isEmpty collection) (empty ())
    else Instance collection navigableCollectionBase navigableCollectionReversedBase;

  let toNavigableCollectionReversed (collection: t 'a): (navigableCollection 'a) =>
    if (isEmpty collection) (empty ())
    else Instance collection navigableCollectionReversedBase navigableCollectionBase;

}: S1 with type t 'a := Base.t 'a);
