/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type s 'collection 'a = {
  count: 'collection => int,
  first: 'collection => (option 'a),
  firstOrRaise: 'collection => 'a,
  last: 'collection => (option 'a),
  lastOrRaise: 'collection => 'a,
  reduce: 'acc . while_::('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => 'collection => 'acc,
  reduceReversed: 'acc . while_::('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => 'collection => 'acc,
  toCollection: 'collection => Collection.t 'a,
  toIterable: 'collection => Iterable.t 'a,
  toIterableReversed: 'collection => Iterable.t 'a,
  toSequence: 'collection => Sequence.t 'a,
  toSequenceReversed: 'collection => Sequence.t 'a,
  toSequentialCollection: 'collection => SequentialCollection.t 'a,
};

type t 'a =
  | Empty
  | Instance 'collection (s 'collection 'a): t 'a;

let count (collection: t 'a): int => switch collection {
  | Empty => 0
  | Instance collection { count } => count collection
};

let empty (): (t 'a) => Empty;

let first (collection: t 'a): (option 'a) => switch collection {
  | Empty => None
  | Instance collection { first } => first collection
};

let firstOrRaise (collection: t 'a): 'a => switch collection {
  | Empty => failwith "empty"
  | Instance collection { firstOrRaise } => firstOrRaise collection
};

let last (collection: t 'a): (option 'a) => switch collection {
  | Empty => None
  | Instance collection { last } => last collection
};

let lastOrRaise (collection: t 'a): 'a => switch collection {
  | Empty => failwith "empty"
  | Instance collection { lastOrRaise } => lastOrRaise collection
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
  | Instance collection { reduce } =>
      collection |> reduce while_::predicate f acc;
};

let reduceReversed
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (collection: t 'a): 'acc => switch collection {
  | Empty => acc
  | Instance collection { reduceReversed } =>
      collection |> reduceReversed while_::predicate f acc;
};

let toCollection (collection: t 'a): (Collection.t 'a) => switch collection {
  | Empty => Collection.empty ()
  | Instance collection { toCollection } => toCollection collection
};

let toIterable (collection: t 'a): (Iterable.t 'a) => switch collection {
  | Empty => Iterable.empty ()
  | Instance collection { toIterable } => toIterable collection
};

let toIterableReversed (collection: t 'a): (Iterable.t 'a) => switch collection {
  | Empty => Iterable.empty ()
  | Instance collection { toIterableReversed } => toIterableReversed collection
};

let toSequence (collection: t 'a): (Sequence.t 'a) => switch collection {
  | Empty => Sequence.empty ()
  | Instance collection { toSequence } => toSequence collection
};

let toSequenceReversed (collection: t 'a): (Sequence.t 'a) => switch collection {
  | Empty => Sequence.empty ()
  | Instance collection { toSequenceReversed } => toSequenceReversed collection
};

let toSequentialCollection (collection: t 'a): (SequentialCollection.t 'a) => switch collection {
  | Empty => SequentialCollection.empty ()
  | Instance collection { toSequentialCollection } => toSequentialCollection collection
};

let toNavigableCollection (collection: t 'a): (t 'a) =>
  collection;

type navigableCollection 'a = t 'a;

module type S = {
  type a;
  type t;

  include SequentialCollection.S with type a := a and type t := t;

  let last: t => (option a);
  let lastOrRaise: t => a;
  let reduceReversed: while_::('acc => a => bool)? => ('acc => a => 'acc) => 'acc => t => 'acc;
  let toIterableReversed: t => (Iterable.t a);
  let toNavigableCollection: t => (navigableCollection a);
  let toSequenceReversed: t => (Sequence.t a);
};

module type S1 = {
  type t 'a;

  include SequentialCollection.S1 with type t 'a := t 'a;

  let last: (t 'a) => (option 'a);
  let lastOrRaise: (t 'a) => 'a;
  let reduceReversed: while_::('acc => 'a => bool)? => ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let toIterableReversed: t 'a => (Iterable.t 'a);
  let toNavigableCollection: (t 'a) => (navigableCollection 'a);
  let toSequenceReversed: (t 'a) => (Sequence.t 'a);
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

  let iterableReversedBase: Iterable.s t a = { reduce: reduceReversed };

  let reduceReversed
      while_::(predicate: 'acc => a => bool)=Functions.alwaysTrue2
      (f: 'acc => a => 'acc)
      (acc: 'acc)
      (collection: t): 'acc =>
    reduceReversed while_::predicate f acc collection;

  let toIterableReversed (collection: t): (Iterable.t a) =>
    if (isEmpty collection) (Iterable.empty ())
    else Iterable.create iterableReversedBase collection;

  let navigableCollectionBase: s t a = {
    count,
    first,
    firstOrRaise,
    last,
    lastOrRaise,
    reduce: Base.reduce,
    reduceReversed: Base.reduceReversed,
    toCollection,
    toIterable,
    toIterableReversed,
    toSequence,
    toSequenceReversed,
    toSequentialCollection,
  };

  let toNavigableCollection (collection: t): (navigableCollection a) =>
    if (isEmpty collection) (empty ())
    else Instance collection navigableCollectionBase;

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

  let iterableReversedBase: Iterable.s (t 'a) 'a = { reduce: reduceReversed };

  let reduceReversed
      while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
      (f: 'acc => 'a => 'acc)
      (acc: 'acc)
      (collection: t 'a): 'acc =>
    reduceReversed while_::predicate f acc collection;

  let toIterableReversed (collection: t 'a): (Iterable.t 'a) =>
    if (isEmpty collection) (Iterable.empty ())
    else Iterable.create iterableReversedBase collection;

  let toSequenceReversed (collection: t 'a): (Sequence.t 'a) =>
    if (isEmpty collection) (Sequence.empty ())
    else (Base.toSequenceReversed collection);

  let navigableCollectionBase: s (t 'a) 'a = {
    count,
    first,
    firstOrRaise,
    last,
    lastOrRaise,
    reduce: Base.reduce,
    reduceReversed: Base.reduceReversed,
    toCollection,
    toIterable,
    toIterableReversed,
    toSequence,
    toSequenceReversed,
    toSequentialCollection,
  };

  let toNavigableCollection (collection: t 'a): (navigableCollection 'a) =>
    if (isEmpty collection) (empty ())
    else Instance collection navigableCollectionBase;

}: S1 with type t 'a := Base.t 'a);
