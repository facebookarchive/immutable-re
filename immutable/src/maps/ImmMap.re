/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Functions.Operators;
open Option.Operators;

type t 'k 'v = {
  containsKey: 'k => bool,
  count: int,
  get: 'k => (option 'v),
  getOrRaise: 'k => 'v,
  keyedIterator: unit => (KeyedIterator.t 'k 'v),
  sequence: unit => (Sequence.t ('k, 'v)),
};

let containsKey (key: 'k) ({ containsKey }: t 'k 'v): bool =>
  containsKey key;

let count ({ count }: t 'k 'v) => count;

let empty (): (t 'k 'v) => {
  containsKey: fun _ => false,
  count: 0,
  get: fun _ => None,
  getOrRaise: fun _ => failwith "empty",
  keyedIterator: KeyedIterator.empty,
  sequence: Sequence.empty,
};

let get (key: 'k) ({ get }: t 'k 'v): (option 'v) =>
  get key;

let getOrRaise (key: 'k) ({ getOrRaise }: t 'k 'v): 'v =>
  getOrRaise key;

let isEmpty ({ count }: t 'k 'v): bool =>
  count === 0;

let isNotEmpty ({ count }: t 'k 'v): bool =>
  count !== 0;

let keys (map: t 'k 'v): (ImmSet.t 'k) => {
  contains: fun k => map |> containsKey k,
  count: map.count,
  iterator: map.keyedIterator >> KeyedIterator.keys,
  sequence: map.sequence >> Sequence.map (fun (k, _) => k),
};

let reduce
    while_::(predicate: 'acc => 'k => 'v => bool)=Functions.alwaysTrue3
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    ({ keyedIterator }: t 'k 'v): 'acc =>
  keyedIterator () |> KeyedIterator.reduce while_::predicate f acc;

let toIterator ({ keyedIterator }: t 'k 'v): (Iterator.t ('k, 'v)) =>
  keyedIterator () |> KeyedIterator.toIterator;

let toKeyedIterator ({ keyedIterator }: t 'k 'v): (KeyedIterator.t 'k 'v) =>
  keyedIterator ();

let map (m: 'k => 'a => 'b) (map: t 'k 'a): (t 'k 'b) => {
  containsKey: map.containsKey,
  count: map.count,
  get: fun k => map.get k >>| m k,
  getOrRaise: fun k => {
    let v = map.getOrRaise k;
    m k v;
  },
  keyedIterator: map.keyedIterator >> KeyedIterator.mapValues m,
  sequence: map.sequence >> Sequence.map (fun (k, v) => (k, m k v)),
};

let toMap (map: t 'k 'v): (t 'k 'v) => map;

let toSequence ({ sequence }: t 'k 'v): (Sequence.t ('k, 'v)) =>
  sequence ();

let values ({ keyedIterator }: t 'k 'v): (Iterator.t 'v) =>
  keyedIterator () |> KeyedIterator.values;

let module KeyedReducer = KeyedReducer.Make2 {
  type nonrec t 'k 'v = t 'k 'v;
  let reduce = reduce;
};
