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
    get: 'k => 'map => (option 'v),
    getOrRaise: 'k => 'map => 'v,
    keys: 'map => ImmSet.t 'k,
    last: 'map => (option ('k, 'v)),
    lastOrRaise: 'map => ('k, 'v),
    toIterable: 'map => Iterable.t ('k, 'v),
    toIterableRight: 'map => Iterable.t ('k, 'v),
    toKeyedCollection: 'map => (KeyedCollection.t 'k 'v),
    toKeyedIterable: 'map => (KeyedIterable.t 'k 'v),
    toKeyedIterableRight: 'map => KeyedIterable.t 'k 'v,
    toMap: 'map => (ImmMap.t 'k 'v),
    toNavigableKeyedCollection: 'map => (NavigableKeyedCollection.t 'k 'v),
    toSequence: 'map => (Sequence.t ('k, 'v)),
    toSequenceRight: 'map => (Sequence.t ('k, 'v)),
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

let last (map: t 'k 'v): option ('k, 'v) => switch map {
  | Empty => None
  | NavigableMap map { last } => last map
};

let lastOrRaise (map: t 'k 'v): ('k, 'v) => switch map {
  | Empty => failwith "empty"
  | NavigableMap map { lastOrRaise } => lastOrRaise map
};

let toIterable (map: t 'k 'v): (Iterable.t ('k, 'v)) =>switch map {
  | Empty => Iterable.empty ()
  | NavigableMap map { toIterable } => toIterable map
};

let toIterableRight (map: t 'k 'v): (Iterable.t ('k, 'v)) =>switch map {
  | Empty => Iterable.empty ()
  | NavigableMap map { toIterableRight } => toIterableRight map
};

let toKeyedCollection (map: t 'k 'v): (KeyedCollection.t 'k 'v) => switch map {
  | Empty => KeyedCollection.empty ()
  | NavigableMap map { toKeyedCollection } => toKeyedCollection map
};

let toKeyedIterable (map: t 'k 'v): (KeyedIterable.t 'k 'v) => switch map {
  | Empty => KeyedIterable.empty ()
  | NavigableMap map { toKeyedIterable } => toKeyedIterable map
};

let toKeyedIterableRight (map: t 'k 'v): (KeyedIterable.t 'k 'v) => switch map {
  | Empty => KeyedIterable.empty ()
  | NavigableMap map { toKeyedIterableRight } => toKeyedIterableRight map
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

let toSequenceRight (map: t 'k 'v): (Sequence.t ('k, 'v)) => switch map {
  | Empty => Sequence.empty ()
  | NavigableMap map { toSequenceRight } => toSequenceRight map
};

let keyCollectionOps (): Collection.Ops.t 'k (t 'k 'v) => {
  count,
  toIterable: toKeyedIterable >> KeyedIterable.keys,
  toSequence: toSequence >> Sequence.map (fun (k, _) => k),
};

let keySetOps (): ImmSet.Ops.t 'k (t 'k 'v) => {
  contains: containsKey,
  count,
  toCollection: fun map =>
    if (isEmpty map) (Collection.empty ())
    else Collection.Collection map (keyCollectionOps ()),
  toIterable: toKeyedIterable >> KeyedIterable.keys,
  toSequence: toSequence >> Sequence.map (fun (k, _) => k),
};

let keys (map: t 'k 'v): (ImmSet.t 'k) =>
  if (isEmpty map) (ImmSet.empty ())
  else ImmSet.Set map (keySetOps ());

let reduce
    while_::(predicate: 'acc => 'k => 'v => bool)=Functions.alwaysTrue3
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    (map: t 'k 'v): 'acc =>
  map |> toKeyedIterable |> KeyedIterable.reduce while_::predicate f acc;

let reduceRight
    while_::(predicate: 'acc => 'k => 'v => bool)=Functions.alwaysTrue3
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    (map: t 'k 'v): 'acc =>
  map |> toKeyedIterableRight |> KeyedIterable.reduce while_::predicate f acc;

/*
let map (mapValues: 'k => 'a => 'b) (map: t 'k 'a): (t 'k 'b) => switch map {
  | Empty => Empty
  | NavigableMap map ops => NavigableMap map {
      containsKey: ops.containsKey,
      count: ops.count,
      get: fun k map => ops.get k map >>| mapValues k,
      getOrRaise: fun k map => {
        let v = map |> ops.getOrRaise k;
        mapValues k v;
      },
      toKeyedCollection: ops.toKeyedCollection >> KeyedCollection.map mapValues,
      toKeyedIterable: ops.toKeyedIterable >> KeyedIterable.mapValues mapValues,
      toSequence: ops.toSequence >> Sequence.map (fun (k, v) => (k, mapValues k v)),
    }
};*/

let module KeyedReducer = KeyedIterable.KeyedReducer.Make2 {
  type nonrec t 'k 'v = t 'k 'v;

  let reduce = reduce;
  let toIterable = toIterable;
  let toKeyedIterable = toKeyedIterable;
};
