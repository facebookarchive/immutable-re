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
    toKeyedIterable: 'map => (KeyedIterable.t 'k 'v),
    toSequence: 'map => (Sequence.t ('k, 'v)),
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

let isEmpty (map: t 'k 'v): bool =>
  (count map) === 0;

let isNotEmpty (map: t 'k 'v): bool =>
  (count map) !== 0;

let toKeyedIterable (map: t 'k 'v): (KeyedIterable.t 'k 'v) => switch map {
  | Empty => KeyedIterable.empty ()
  | Map map { toKeyedIterable } => toKeyedIterable map
};

let toIterable (map: t 'k 'v): (Iterable.t ('k, 'v)) =>
  map |> toKeyedIterable |> KeyedIterable.toIterable;

let toSequence (map: t 'k 'v): (Sequence.t ('k, 'v)) => switch map {
  | Empty => Sequence.empty ()
  | Map map { toSequence } => toSequence map
};

let keySetOps (): ImmSet.Ops.t 'k (t 'k 'v) => {
  contains: containsKey,
  count,
  toIterable: toKeyedIterable >> KeyedIterable.keys,
  toSequence: toSequence >> Sequence.map (fun (k, _) => k),
};

let keys (map: t 'k 'v): (ImmSet.t 'k) =>
  ImmSet.Set map (keySetOps ());

let reduce
    while_::(predicate: 'acc => 'k => 'v => bool)=Functions.alwaysTrue3
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    (map: t 'k 'v): 'acc =>
  map |> toKeyedIterable |> KeyedIterable.reduce while_::predicate f acc;

let map (mapValues: 'k => 'a => 'b) (map: t 'k 'a): (t 'k 'b) => switch map {
  | Empty => Empty
  | Map map ops => Map map {
      containsKey: ops.containsKey,
      count: ops.count,
      get: fun k map => ops.get k map >>| mapValues k,
      getOrRaise: fun k map => {
        let v = map |> ops.getOrRaise k;
        mapValues k v;
      },
      toKeyedIterable: ops.toKeyedIterable >> KeyedIterable.mapValues mapValues,
      toSequence: ops.toSequence >> Sequence.map (fun (k, v) => (k, mapValues k v)),
    }
};

let toMap (map: t 'k 'v): (t 'k 'v) => map;

let module KeyedReducer = KeyedIterable.KeyedReducer.Make2 {
  type nonrec t 'k 'v = t 'k 'v;

  let reduce = reduce;
  let toIterable = toIterable;
  let toKeyedIterable = toKeyedIterable;
};
