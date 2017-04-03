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
    first: 'map => (option ('k, 'v)),
    firstOrRaise: 'map => ('k, 'v),
    firstKey: 'map => (option 'k),
    firstKeyOrRaise: 'map => 'k,
    firstValue: 'map => (option 'v),
    firstValueOrRaise: 'map => 'v,
    get: 'k => 'map => (option 'v),
    getOrRaise: 'k => 'map => 'v,
    keys: 'map => Iterable.t 'k,
    keysReversed: 'map => Iterable.t 'k,
    keySet: 'map => ImmSet.t 'k,
    navigableKeySet: 'map => NavigableSet.t 'k,
    last: 'map => (option ('k, 'v)),
    lastOrRaise: 'map => ('k, 'v),
    lastKey: 'map => (option 'k),
    lastKeyOrRaise: 'map => 'k,
    lastValue: 'map => (option 'v),
    lastValueOrRaise: 'map => 'v,
    toIterable: 'map => Iterable.t ('k, 'v),
    toIterableReversed: 'map => Iterable.t ('k, 'v),
    toKeyedCollection: 'map => (KeyedCollection.t 'k 'v),
    toKeyedIterable: 'map => (KeyedIterable.t 'k 'v),
    toKeyedIterableReversed: 'map => KeyedIterable.t 'k 'v,
    toMap: 'map => (ImmMap.t 'k 'v),
    toNavigableKeyedCollection: 'map => (NavigableKeyedCollection.t 'k 'v),
    toSequence: 'map => (Sequence.t ('k, 'v)),
    toSequenceReversed: 'map => (Sequence.t ('k, 'v)),
    values: 'map => Iterable.t 'v,
    valuesReversed: 'map => Iterable.t 'v,
  };
};

type t 'k 'v =
  | Empty
  | NavigableMap 'map (Ops.t 'k 'v 'map): t 'k 'v;

let containsKey (key: 'k) (map: t 'k 'v): bool => switch map {
  | Empty => false
  | NavigableMap map { containsKey } => containsKey key map
};

let count (map: t 'k 'v): int => switch map {
  | Empty => 0
  | NavigableMap map { count } => count map
};

let empty (): (t 'k 'v) => Empty;

let first (map: t 'k 'v): option ('k, 'v) => switch map {
  | Empty => None
  | NavigableMap map { first } => first map
};

let firstOrRaise (map: t 'k 'v): ('k, 'v) => switch map {
  | Empty => failwith "empty"
  | NavigableMap map { firstOrRaise } => firstOrRaise map
};

let firstKey (map: t 'k 'v): option 'k => switch map {
  | Empty => None
  | NavigableMap map { firstKey } => firstKey map
};

let firstKeyOrRaise (map: t 'k 'v): 'k => switch map {
  | Empty => failwith "empty"
  | NavigableMap map { firstKeyOrRaise } => firstKeyOrRaise map
};

let firstValue (map: t 'k 'v): option 'v => switch map {
  | Empty => None
  | NavigableMap map { firstValue } => firstValue map
};

let firstValueOrRaise (map: t 'k 'v): 'v => switch map {
  | Empty => failwith "empty"
  | NavigableMap map { firstValueOrRaise } => firstValueOrRaise map
};

let get (key: 'k) (map: t 'k 'v): (option 'v) => switch map {
  | Empty => None
  | NavigableMap map { get } => get key map
};

let getOrRaise (key: 'k) (map: t 'k 'v): 'v => switch map {
  | Empty => failwith "not found"
  | NavigableMap map { getOrRaise } => getOrRaise key map
};

let isEmpty (map: t 'k 'v): bool =>
  (count map) === 0;

let isNotEmpty (map: t 'k 'v): bool =>
  (count map) !== 0;

let keys (map: t 'k 'v): (Iterable.t 'k) => switch map {
  | Empty => Iterable.empty ()
  | NavigableMap map { keys } => keys map
};

let keysReversed (map: t 'k 'v): (Iterable.t 'k) => switch map {
  | Empty => Iterable.empty ()
  | NavigableMap map { keysReversed } => keysReversed map
};

let keySet (map: t 'k 'v): (ImmSet.t 'k) => switch map {
  | Empty => ImmSet.empty ()
  | NavigableMap map { keySet } => keySet map
};

let last (map: t 'k 'v): option ('k, 'v) => switch map {
  | Empty => None
  | NavigableMap map { last } => last map
};

let lastOrRaise (map: t 'k 'v): ('k, 'v) => switch map {
  | Empty => failwith "empty"
  | NavigableMap map { lastOrRaise } => lastOrRaise map
};

let lastKey (keyed: t 'k 'v): option 'k => switch keyed {
  | Empty => None
  | NavigableMap keyed { lastKey } => lastKey keyed
};

let lastKeyOrRaise (keyed: t 'k 'v): 'k => switch keyed {
  | Empty => failwith "empty"
  | NavigableMap keyed { lastKeyOrRaise } => lastKeyOrRaise keyed
};

let lastValue (keyed: t 'k 'v): option 'v => switch keyed {
  | Empty => None
  | NavigableMap keyed { lastValue } => lastValue keyed
};

let lastValueOrRaise (keyed: t 'k 'v): 'v => switch keyed {
  | Empty => failwith "empty"
  | NavigableMap keyed { lastValueOrRaise } => lastValueOrRaise keyed
};

let navigableKeySet (map: t 'k 'v): NavigableSet.t 'k => switch map {
  | Empty => NavigableSet.empty ()
  | NavigableMap map { navigableKeySet } => navigableKeySet map
};

let toIterable (map: t 'k 'v): (Iterable.t ('k, 'v)) =>switch map {
  | Empty => Iterable.empty ()
  | NavigableMap map { toIterable } => toIterable map
};

let toIterableReversed (map: t 'k 'v): (Iterable.t ('k, 'v)) =>switch map {
  | Empty => Iterable.empty ()
  | NavigableMap map { toIterableReversed } => toIterableReversed map
};

let toKeyedCollection (map: t 'k 'v): (KeyedCollection.t 'k 'v) => switch map {
  | Empty => KeyedCollection.empty ()
  | NavigableMap map { toKeyedCollection } => toKeyedCollection map
};

let toKeyedIterable (map: t 'k 'v): (KeyedIterable.t 'k 'v) => switch map {
  | Empty => KeyedIterable.empty ()
  | NavigableMap map { toKeyedIterable } => toKeyedIterable map
};

let toKeyedIterableReversed (map: t 'k 'v): (KeyedIterable.t 'k 'v) => switch map {
  | Empty => KeyedIterable.empty ()
  | NavigableMap map { toKeyedIterableReversed } => toKeyedIterableReversed map
};

let toMap (map: t 'k 'v): (ImmMap.t 'k 'v) => switch map {
  | Empty => ImmMap.empty ()
  | NavigableMap map { toMap } => toMap map
};

let toNavigableKeyedCollection (map: t 'k 'v): (NavigableKeyedCollection.t 'k 'v) => switch map {
  | Empty => NavigableKeyedCollection.empty ()
  | NavigableMap map { toNavigableKeyedCollection } => toNavigableKeyedCollection map
};

let toNavigableMap (map: t 'k 'v): (t 'k 'v) => map;

let toSequence (map: t 'k 'v): (Sequence.t ('k, 'v)) => switch map {
  | Empty => Sequence.empty ()
  | NavigableMap map { toSequence } => toSequence map
};

let toSequenceReversed (map: t 'k 'v): (Sequence.t ('k, 'v)) => switch map {
  | Empty => Sequence.empty ()
  | NavigableMap map { toSequenceReversed } => toSequenceReversed map
};

let values (map: t 'k 'v): (Iterable.t 'v) => switch map {
  | Empty => Iterable.empty ()
  | NavigableMap map { values } => values map
};

let valuesReversed (map: t 'k 'v): (Iterable.t 'v) => switch map {
  | Empty => Iterable.empty ()
  | NavigableMap map { valuesReversed } => valuesReversed map
};

let reduce
    while_::(predicate: 'acc => 'k => 'v => bool)=Functions.alwaysTrue3
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    (map: t 'k 'v): 'acc =>
  map |> toKeyedIterable |> KeyedIterable.reduce while_::predicate f acc;

let reduceReversed
    while_::(predicate: 'acc => 'k => 'v => bool)=Functions.alwaysTrue3
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    (map: t 'k 'v): 'acc =>
  map |> toKeyedIterableReversed |> KeyedIterable.reduce while_::predicate f acc;
