/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'k 'v = {
  mutable count: int,
  mutable root: BitmapTrieMap.t 'k 'v,
  comparator: Comparator.t 'k,
  hash: Hash.t 'k,
};

let containsKey (key: 'k) ({ root, comparator, hash }: t 'k 'v): bool => {
  let hashKey = hash key;
  root |> BitmapTrieMap.containsKey comparator 0 hashKey key;
};

let count ({ count }: t 'k 'v): int => count;

let get (key: 'k) ({ root, comparator, hash }: t 'k 'v): (option 'v) => {
  let hashKey = hash key;
  root |> BitmapTrieMap.get comparator 0 hashKey key;
};

let getOrDefault default::(default: 'v) (key: 'k) ({ root, comparator, hash }: t 'k 'v): 'v => {
  let hashKey = hash key;
  root |> BitmapTrieMap.getOrDefault comparator 0 default hashKey key;
};

let getOrRaise (key: 'k) ({ root, comparator, hash }: t 'k 'v): 'v => {
  let hashKey = hash key;
  root |> BitmapTrieMap.getOrRaise comparator 0 hashKey key;
};

let reduce
    while_::(predicate: 'acc => 'k => 'v => bool)
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    ({ root }: t 'k 'v): 'acc =>
  root |> BitmapTrieMap.reduce while_::predicate f acc;

let toSequence (selector: 'k => 'v => 'c)({ root }: t 'k 'v): (Sequence.t 'c) =>
  root |> BitmapTrieMap.toSequence selector;

let alter
    (key: 'k)
    (f: option 'v => option 'v)
    ({ count, root, comparator, hash } as map: t 'k 'v): (t 'k 'v) => {
  let hashKey = hash key;
  let alterResult = ref AlterResult.NoChange;
  let newRoot = root |> BitmapTrieMap.alter
    comparator
    BitmapTrieMap.updateLevelNodePersistent
    Transient.Owner.none
    alterResult
    0
    hashKey
    key
    f;

  switch !alterResult {
    | AlterResult.Added => { count: count + 1, root: newRoot, comparator, hash }
    | AlterResult.NoChange => map
    | AlterResult.Replace => { count, root: newRoot, comparator, hash }
    | AlterResult.Removed => { count: count - 1, root: newRoot, comparator, hash }
  };
};

let emptyWith
    hash::(hash: Hash.t 'k)
    comparator::(comparator: Comparator.t 'k): (t 'k 'v) => {
  count: 0,
  root: BitmapTrieMap.Empty,
  comparator,
  hash,
};

let put (key: 'k) (value: 'v) ({ count, root, comparator, hash } as map: t 'k 'v): (t 'k 'v) => {
  let hashKey = hash key;
  let alterResult = ref AlterResult.NoChange;
  let newRoot = root |> BitmapTrieMap.putWithResult
    comparator
    BitmapTrieMap.updateLevelNodePersistent
    Transient.Owner.none
    alterResult
    0
    hashKey
    key
    value;

  switch !alterResult {
    | AlterResult.Added => { count: count + 1, root: newRoot, comparator, hash }
    | AlterResult.NoChange => map
    | AlterResult.Replace => { count, root: newRoot, comparator, hash }
    | AlterResult.Removed => failwith "invalid state"
  };
};

let remove (key: 'k) (map: t 'k 'v): (t 'k 'v) =>
  map |> alter key Functions.alwaysNone;

let removeAll ({ comparator, hash }: t 'k 'v): (t 'k 'v) =>
  emptyWith hash::hash comparator::comparator;
