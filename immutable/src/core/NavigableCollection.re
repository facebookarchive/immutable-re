/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Functions.Operators;

let module Ops = {
  type t 'a 'collection = {
    count: 'collection => int,
    first: 'collection => (option 'a),
    firstOrRaise: 'collection => 'a,
    last: 'collection => (option 'a),
    lastOrRaise: 'collection => 'a,
    toCollection: 'collection => Collection.t 'a,
    toSequentialCollection: 'collection => SequentialCollection.t 'a,
    toIterable: 'collection => Iterable.t 'a,
    toIterableReversed: 'collection => Iterable.t 'a,
    toSequence: 'collection => Sequence.t 'a,
    toSequenceReversed: 'collection => Sequence.t 'a,
  };
};

type t 'a =
  | Empty
  | NavigableCollection 'collection (Ops.t 'a 'collection): t 'a;

let count (collection: t 'a): int => switch collection {
  | Empty => 0
  | NavigableCollection collection { count } => count collection
};

let empty (): (t 'a) => Empty;

let first (collection: t 'a): (option 'a) => switch collection {
  | Empty => None
  | NavigableCollection collection { first } => first collection
};

let firstOrRaise (collection: t 'a): 'a => switch collection {
  | Empty => failwith "empty"
  | NavigableCollection collection { firstOrRaise } => firstOrRaise collection
};

let last (collection: t 'a): (option 'a) => switch collection {
  | Empty => None
  | NavigableCollection collection { last } => last collection
};

let lastOrRaise (collection: t 'a): 'a => switch collection {
  | Empty => failwith "empty"
  | NavigableCollection collection { lastOrRaise } => lastOrRaise collection
};

let isEmpty (collection: t 'a): bool =>
  (count collection) === 0;

let isNotEmpty (collection: t 'a): bool =>
  (count collection) !== 0;

let toCollection (collection: t 'a): (Collection.t 'a) => switch collection {
  | Empty => Collection.empty ()
  | NavigableCollection collection { toCollection } => toCollection collection
};

let toSequentialCollection (collection: t 'a): (SequentialCollection.t 'a) => switch collection {
  | Empty => SequentialCollection.empty ()
  | NavigableCollection collection { toSequentialCollection } => toSequentialCollection collection
};

let toIterable (collection: t 'a): (Iterable.t 'a) => switch collection {
  | Empty => Iterable.empty ()
  | NavigableCollection collection { toIterable } => toIterable collection
};

let toIterableReversed (collection: t 'a): (Iterable.t 'a) => switch collection {
  | Empty => Iterable.empty ()
  | NavigableCollection collection { toIterableReversed } => toIterableReversed collection
};

let toSequence (collection: t 'a): (Sequence.t 'a) => switch collection {
  | Empty => Sequence.empty ()
  | NavigableCollection collection { toSequence } => toSequence collection
};

let toSequenceReversed (collection: t 'a): (Sequence.t 'a) => switch collection {
  | Empty => Sequence.empty ()
  | NavigableCollection collection { toSequenceReversed } => toSequenceReversed collection
};

let reduce
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (collection: t 'a): 'acc =>
  collection |> toIterable |> Iterable.reduce while_::predicate f acc;

let reduceReversed
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (collection: t 'a): 'acc =>
  collection |> toIterableReversed |> Iterable.reduce while_::predicate f acc;

let toNavigableCollection (collection: t 'a): (t 'a) =>
  collection;
