/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Option.Operators;

type t 'k 'v = {
  containsWith: (Equality.t 'v) => 'k => 'v => bool,
  containsKey: 'k => bool,
  count: int,
  get: 'k => (option 'v),
  getOrRaise: 'k => 'v,
  keyedIterator: (KeyedIterator.t 'k 'v),
  sequence: (Sequence.t ('k, 'v)),
};

let containsWith
    (valueEquality: Equality.t 'v)
    (key: 'k)
    (value: 'v)
    ({ containsWith }: t 'k 'v): bool =>
  containsWith valueEquality key value;

let contains
    (key: 'k)
    (value: 'v)
    ({ containsWith }: t 'k 'v): bool =>
  containsWith Equality.structural key value;

let containsKey (key: 'k) ({ containsKey }: t 'k 'v): bool =>
  containsKey key;

let count ({ count }: t 'k 'v) => count;

let empty: (t 'k 'v) = {
  containsWith: fun _ _ _ => false,
  containsKey: fun _ => false,
  count: 0,
  get: fun _ => None,
  getOrRaise: fun _ => failwith "empty",
  keyedIterator: KeyedIterator.empty,
  sequence: Sequence.empty,
};

let equalsWith (valueEquality: Equality.t 'v) (that: t 'k 'v) (this: t 'k 'v): bool =>
  if (this === that) true
  else if (this.count != that.count) false
  else this.keyedIterator |> KeyedIterator.every (
    fun key thisValue => that |> containsWith valueEquality key thisValue
  );

let equals (that: t 'k 'v) (this: t 'k 'v): bool =>
  equalsWith Equality.structural that this;

let get (key: 'k) ({ get }: t 'k 'v): (option 'v) =>
  get key;

let getOrRaise (key: 'k) ({ getOrRaise }: t 'k 'v): 'v =>
  getOrRaise key;

let hashWith (keyHash: Hash.t 'k) (valueHash: Hash.t 'v) ({ keyedIterator }: t 'k 'v): int =>
  keyedIterator |> KeyedIterator.reduce
    (MapEntry.hashReducer keyHash valueHash)
    Hash.initialValue;

let hash (map: t 'k 'v): int =>
  hashWith Hash.structural Hash.structural map;

let isEmpty ({ count }: t 'k 'v): bool =>
  count == 0;

let isNotEmpty ({ count }: t 'k 'v): bool =>
  count != 0;

let keys (map: t 'k 'v): (ImmSet.t 'k) => {
  contains: fun k => map |> containsKey k,
  count: map.count,
  iterator: map.keyedIterator |> KeyedIterator.keys,
  sequence: map.sequence |> Sequence.map (fun (k, _) => k),
};

let ofSet (set: ImmSet.t 'a): (t 'a 'a) => {
  containsWith: fun equals k v =>
    if (set |> ImmSet.contains k) (equals k v)
    else false,
  containsKey: fun k => set |> ImmSet.contains k,
  count: ImmSet.count set,
  get: fun k =>
    if (set |> ImmSet.contains k) (Some k)
    else None,
  getOrRaise: fun k =>
    if (set |> ImmSet.contains k) k
    else failwith "not found",
  keyedIterator: {
    reduceWhile: fun predicate f acc =>
      set |> ImmSet.toIterator |> Iterator.reduceWhile
        (fun acc next => predicate acc next next)
        (fun acc next => f acc next next)
        acc
  },
  sequence: ImmSet.toSequence set |> Sequence.map (fun k => (k, k)),
};

let reduce (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) ({ keyedIterator }: t 'k 'v): 'acc =>
  keyedIterator |> KeyedIterator.reduce f acc;

let reduceWhile
    (predicate: 'acc => 'k => 'v => bool)
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    ({ keyedIterator }: t 'k 'v): 'acc =>
  keyedIterator |> KeyedIterator.reduceWhile predicate f acc;

let toIterator ({ keyedIterator }: t 'k 'v): (Iterator.t ('k, 'v)) =>
  keyedIterator |> KeyedIterator.toIterator;

let toKeyedIterator ({ keyedIterator }: t 'k 'v): (KeyedIterator.t 'k 'v) =>
  keyedIterator;

let map (m: 'k => 'a => 'b) (map: t 'k 'a): (t 'k 'b) => {
  containsWith: fun equals k b => map.get k >>| (fun a => m k a |> equals b) |? false,
  containsKey: map.containsKey,
  count: map.count,
  get: fun k => map.get k >>| m k,
  getOrRaise: fun k => {
    let v = map.getOrRaise k;
    m k v;
  },
  keyedIterator: map.keyedIterator |> KeyedIterator.map m,
  sequence: map.sequence |> Sequence.map (fun (k, v) => (k, m k v)),
};

let toMap (map: t 'k 'v): (t 'k 'v) => map;

let toSetWith
    (equals: Equality.t 'v)
    (map: t 'k 'v): (ImmSet.t ('k, 'v)) => {
  contains: fun (k, v) => map.get k >>| equals v |? false,
  count: map.count,
  iterator: map |> toIterator,
  sequence: map.sequence,
};

let toSet (map: t 'k 'v): (ImmSet.t ('k, 'v)) =>
  toSetWith Equality.structural map;

let toSequence ({ sequence }: t 'k 'v): (Sequence.t ('k, 'v)) => sequence;

let values ({ keyedIterator }: t 'k 'v): (Iterator.t 'v) =>
  keyedIterator |> KeyedIterator.values;
