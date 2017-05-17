/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'k 'v = Transient.t (HashMap.t 'k 'v);

let mutate ({ count, root, comparator, hash }: HashMap.t 'k 'v): (t 'k 'v) =>
  Transient.create ({ count, root, comparator, hash }: HashMap.t 'k 'v);

let alterImpl
    (owner: Transient.Owner.t)
    (key: 'k)
    (f: option 'v => option 'v)
    ({ count, root, comparator, hash } as map: HashMap.t 'k 'v): (HashMap.t 'k 'v) => {
  let hashKey = hash key;
  let alterResult = ref AlterResult.NoChange;
  let newRoot = root |> BitmapTrieMap.alter
    comparator
    BitmapTrieMap.updateLevelNodeTransient
    owner
    alterResult
    0
    hashKey
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
  };
};

let alter
    (key: 'k)
    (f: option 'v => option 'v)
    (transient: t 'k 'v): (t 'k 'v) =>
  transient |> Transient.update2 alterImpl key f;

let containsKey (key: 'k) (transient: t 'k 'v): bool =>
  transient |> Transient.get |> HashMap.containsKey key;

let count (transient: t 'k 'v): int =>
  transient |> Transient.get |> HashMap.count;

let emptyWith
    hash::(hash: Hash.t 'k)
    comparator::(comparator: Comparator.t 'k)
    (): (t 'k 'v) =>
  HashMap.emptyWith ::hash ::comparator |> mutate;

let get (key: 'k) (transient: t 'k 'v): (option 'v) =>
 transient |> Transient.get |> HashMap.get key;

let getOrDefault default::(default: 'v) (key: 'k) (transient: t 'k 'v): 'v =>
  transient |> Transient.get |> HashMap.getOrDefault ::default key;

let getOrRaise (key: 'k) (transient: t 'k 'v): 'v =>
  transient |> Transient.get |> HashMap.getOrRaise key;

let persist (transient: t 'k 'v): (HashMap.t 'k 'v) =>
  transient |> Transient.persist;

let putImpl
    (owner: Transient.Owner.t)
    (key: 'k)
    (value: 'v)
    ({ count, root, comparator, hash } as map: HashMap.t 'k 'v): (HashMap.t 'k 'v) => {
  let hashKey = hash key;
  let alterResult = ref AlterResult.NoChange;
  let newRoot = root |> BitmapTrieMap.putWithResult
    comparator
    BitmapTrieMap.updateLevelNodeTransient
    owner
    alterResult
    0
    hashKey
    key
    value;

  switch !alterResult {
    | AlterResult.Added =>
        map.count = count + 1;
        map.root = newRoot;
        map;
    | AlterResult.NoChange => map
    | AlterResult.Replace =>
        map.root = newRoot;
        map;
    | AlterResult.Removed => failwith "invalid state"
  };
};

let put (key: 'k) (value: 'v) (transient: t 'k 'v): (t 'k 'v) =>
  transient |> Transient.update2 putImpl key value;

let remove (key: 'k) (transient: t 'k 'v): (t 'k 'v) =>
  transient |> alter key Functions.alwaysNone;

let removeAllImpl
    (_: Transient.Owner.t)
    ({ comparator, hash }: HashMap.t 'k 'v): (HashMap.t 'k 'v) =>
  HashMap.emptyWith ::comparator ::hash;

let removeAll (transient: t 'k 'v): (t 'k 'v) =>
  transient |> Transient.update removeAllImpl;
