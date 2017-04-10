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
    toIterable: 'collection => Iterable.t 'a,
    toSequence: 'collection => Sequence.t 'a,
  };
};

type t 'a =
  | Empty
  | Collection 'collection (Ops.t 'a 'collection): t 'a;

let count (collection: t 'a): int => switch collection {
  | Empty => 0
  | Collection collection { count } => count collection
};

let empty (): (t 'a) => Empty;

let isEmpty (collection: t 'a): bool =>
  (count collection) === 0;

let isNotEmpty (collection: t 'a): bool =>
  (count collection) !== 0;

let toIterable (collection: t 'a): (Iterable.t 'a) => switch collection {
  | Empty => Iterable.empty ()
  | Collection collection { toIterable } => toIterable collection
};

let toSequence (collection: t 'a): (Sequence.t 'a) => switch collection {
  | Empty => Sequence.empty ()
  | Collection collection { toSequence } => toSequence collection
};

let reduce
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (collection: t 'a): 'acc =>
  collection |> toIterable |> Iterable.reduce while_::predicate f acc;

let toCollection (collection: t 'a): (t 'a) =>
  collection;
