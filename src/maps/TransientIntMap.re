/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type k = int;
type t 'v = Transient.t (IntMap.t 'v);

let mutate ({ count, root }: IntMap.t 'v): (t 'v) =>
  Transient.create ({ count, root }: IntMap.t 'v);

let alterImpl
    (owner: Transient.Owner.t)
    (key: int)
    (f: option 'v => option 'v)
    ({ count, root } as map: IntMap.t 'v): (IntMap.t 'v) => {
  let alterResult = ref AlterResult.NoChange;
  let newRoot = root |> BitmapTrieIntMap.alter
    BitmapTrieIntMap.updateLevelNodeTransient
    owner
    alterResult
    0
    key
    f;

  switch !alterResult {
    | AlterResult.Added =>
        map.count = count + 1;
        map.root = newRoot;
        map;
    | AlterResult.NoChange => map
    | AlterResult.Replace =>
        map.root = newRoot;
        map;
    | AlterResult.Removed =>
        map.count = count - 1;
        map.root = newRoot;
        map;
  }
};

let alter
    (key: int)
    (f: option 'v => option 'v)
    (transient: t 'v): (t 'v) =>
  transient |> Transient.update2 alterImpl key f;

let containsKey (key: int) (transient: t 'v): bool =>
  transient |> Transient.get |> IntMap.containsKey key;

let count (transient: t 'v): int =>
  transient |> Transient.get |> IntMap.count;

let empty (): t 'v =>
  IntMap.empty () |> mutate;

let get (key: int) (transient: t 'v): (option 'v) =>
  transient |> Transient.get |> IntMap.get key;

let getOrDefault default::(default: 'v) (key: 'k) (transient: t 'v): 'v =>
  transient |> Transient.get |> IntMap.getOrDefault ::default key;

let getOrRaise (key: int) (transient: t 'v): 'v =>
  transient |> Transient.get |> IntMap.getOrRaise key;

let persist (transient: t 'v): (IntMap.t 'v) =>
  transient |> Transient.persist;

let putImpl
    (owner: Transient.Owner.t)
    (key: int)
    (value: 'v)
    ({ count, root } as map: IntMap.t 'v): (IntMap.t 'v) => {
  let alterResult = ref AlterResult.NoChange;
  let newRoot = root |> BitmapTrieIntMap.putWithResult
    BitmapTrieIntMap.updateLevelNodeTransient
    owner
    alterResult
    0
    key
    value;

  switch !alterResult {
    | AlterResult.Added =>
        map.count = count + 1;
        map.root = newRoot;
        map;
    | AlterResult.NoChange => map
    | AlterResult.Replace =>
        map.count = count + 1;
        map.root = newRoot;
        map;
    | AlterResult.Removed => failwith "invalid state"
  }
};

let put (key: int) (value: 'v) (transient: t 'v): (t 'v) =>
  transient |> Transient.update2 putImpl key value;

let remove (key: int) (transient: t 'v): (t 'v) =>
  transient |> alter key Functions.alwaysNone;

let removeAllImpl
    (_: Transient.Owner.t)
    (_: IntMap.t 'v): (IntMap.t 'v) => IntMap.empty ();

let removeAll (transient: t 'v): (t 'v) =>
    transient |> Transient.update removeAllImpl;
