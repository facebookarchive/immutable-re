/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Functions;
open Functions.Operators;
open Option.Operators;

let module Ops = {
  type t 'k 'v 'map = {
    containsKey: 'k => 'map => bool,
    count: 'map => int,
    get: 'k => 'map => (option 'v),
    getOrRaise: 'k => 'map => 'v,
    keys: 'map => Iterable.t 'k,
    keySet: 'map => ImmSet.t 'k,
    toIterable: 'map => Iterable.t ('k, 'v),
    toKeyedCollection: 'map => (KeyedCollection.t 'k 'v),
    toKeyedIterable: 'map => (KeyedIterable.t 'k 'v),
    toSequence: 'map => (Sequence.t ('k, 'v)),
    values: 'map => Iterable.t 'v,
  };
};

type t 'k 'v =
  | Empty
  | Map 'map (Ops.t 'k 'v 'map): t 'k 'v;

let containsKey (key: 'k) (map: t 'k 'v): bool => switch map {
  | Empty => false
  | Map map { containsKey } => containsKey key map
};

let count (map: t 'k 'v): int => switch map {
  | Empty => 0
  | Map map { count } => count map
};

let empty (): (t 'k 'v) => Empty;

let get (key: 'k) (map: t 'k 'v): (option 'v) => switch map {
  | Empty => None
  | Map map { get } => get key map
};

let getOrRaise (key: 'k) (map: t 'k 'v): 'v => switch map {
  | Empty => failwith "not found"
  | Map map { getOrRaise } => getOrRaise key map
};

let keys (map: t 'k 'v): Iterable.t 'k => switch map {
  | Empty => Iterable.empty ()
  | Map map { keys } => keys map
};

let keySet (map: t 'k 'v): ImmSet.t 'k => switch map {
  | Empty => ImmSet.empty ()
  | Map map { keySet } => keySet map
};

let isEmpty (map: t 'k 'v): bool =>
  (count map) === 0;

let isNotEmpty (map: t 'k 'v): bool =>
  (count map) !== 0;

let toKeyedCollection (map: t 'k 'v): (KeyedCollection.t 'k 'v) => switch map {
  | Empty => KeyedCollection.empty ()
  | Map map { toKeyedCollection } => toKeyedCollection map
};

let toKeyedIterable (map: t 'k 'v): (KeyedIterable.t 'k 'v) => switch map {
  | Empty => KeyedIterable.empty ()
  | Map map { toKeyedIterable } => toKeyedIterable map
};

let toIterable (map: t 'k 'v): (Iterable.t ('k, 'v)) =>
  map |> toKeyedIterable |> KeyedIterable.toIterable;

let toMap (map: t 'k 'v): (t 'k 'v) => map;

let toSequence (map: t 'k 'v): (Sequence.t ('k, 'v)) => switch map {
  | Empty => Sequence.empty ()
  | Map map { toSequence } => toSequence map
};

let values (map: t 'k 'v): Iterable.t 'v => switch map {
  | Empty => Iterable.empty ()
  | Map map { values } => values map
};

let reduce
    while_::(predicate: 'acc => 'k => 'v => bool)=Functions.alwaysTrue3
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    (map: t 'k 'v): 'acc =>
  map |> toKeyedIterable |> KeyedIterable.reduce while_::predicate f acc;
