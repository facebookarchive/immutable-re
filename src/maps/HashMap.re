/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'k 'v = {
  count: int,
  root: BitmapTrieMap.t 'k 'v,
  comparator: Comparator.t 'k,
  hash: Hash.t 'k,
};

include (ImmMap.Make2 {
  type nonrec t 'k 'v = t 'k 'v;

  let containsKey (key: 'k) ({ root, comparator, hash }: t 'k 'v): bool => {
    let hashKey = hash key;
    root |> BitmapTrieMap.containsKey comparator 0 hashKey key;
  };

  let count ({ count }: t 'k 'v): int => count;

  let get (key: 'k) ({ root, comparator, hash }: t 'k 'v): (option 'v) => {
    let hashKey = hash key;
    root |> BitmapTrieMap.get comparator 0 hashKey key;
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
    if (predicate === Functions.alwaysTrue3) (root |> BitmapTrieMap.reduce f acc)
    else root |> BitmapTrieMap.reduceWhile predicate f acc;

  let reduceKeys
      while_::(predicate: 'acc => 'k => bool)
      (f: 'acc => 'k => 'acc)
      (acc: 'acc)
      ({ root }: t 'k 'v): 'acc =>
    if (predicate === Functions.alwaysTrue2) (root |> BitmapTrieMap.reduceKeys f acc)
    else root |> BitmapTrieMap.reduceKeysWhile predicate f acc;

  let reduceValues
      while_::(predicate: 'acc => 'v => bool)
      (f: 'acc => 'v => 'acc)
      (acc: 'acc)
      ({ root }: t 'k 'v): 'acc =>
    if (predicate === Functions.alwaysTrue2) (root |> BitmapTrieMap.reduceValues f acc)
    else root |> BitmapTrieMap.reduceValuesWhile predicate f acc;

  let toSequence (selector: 'k => 'v => 'c)({ root }: t 'k 'v): (Sequence.t 'c) =>
    root |> BitmapTrieMap.toSequence selector;
}: ImmMap.S2 with type t 'k 'v := t 'k 'v);

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

let module Transient = {
  type hashMap 'k 'v = t 'k 'v;

  type t 'k 'v = Transient.t (hashMap 'k 'v);

  let mutate (map: hashMap 'k 'v): (t 'k 'v) =>
    Transient.create map;

  let alterImpl
      (owner: Transient.Owner.t)
      (key: 'k)
      (f: option 'v => option 'v)
      ({ count, root, comparator, hash } as map: hashMap 'k 'v): (hashMap 'k 'v) => {
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
      | AlterResult.Added => { count: count + 1, root: newRoot, comparator, hash }
      | AlterResult.NoChange => map
      | AlterResult.Replace =>
          if (newRoot === root) map
          else { count, root: newRoot, comparator, hash }
      | AlterResult.Removed => { count: count - 1, root: newRoot, comparator, hash }
    };
  };

  let alter
      (key: 'k)
      (f: option 'v => option 'v)
      (transient: t 'k 'v): (t 'k 'v) =>
    transient |> Transient.update2 alterImpl key f;

  let containsKey (key: 'k) (transient: t 'k 'v): bool =>
    transient |> Transient.get |> containsKey key;

  let count (transient: t 'k 'v): int =>
    transient |> Transient.get |> count;

  let persistentEmptyWith = emptyWith;

  let emptyWith
      hash::(hash: Hash.t 'k)
      comparator::(comparator: Comparator.t 'k)
      (): (t 'k 'v) =>
    persistentEmptyWith hash::hash comparator::comparator |> mutate;

  let get (key: 'k) (transient: t 'k 'v): (option 'v) =>
   transient |> Transient.get |> get key;

  let getOrRaise (key: 'k) (transient: t 'k 'v): 'v =>
    transient |> Transient.get |> getOrRaise key;

  let isEmpty (transient: t 'k 'v): bool =>
    transient |> Transient.get |> isEmpty;

  let isNotEmpty (transient: t 'k 'v): bool =>
    transient |> Transient.get |> isNotEmpty;

  let persist (transient: t 'k 'v): (hashMap 'k 'v) =>
    transient |> Transient.persist;

  let putImpl
      (owner: Transient.Owner.t)
      (key: 'k)
      (value: 'v)
      ({ count, root, comparator, hash } as map: hashMap 'k 'v): (hashMap 'k 'v) => {
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
      | AlterResult.Added => { count: count + 1, root: newRoot, comparator, hash }
      | AlterResult.NoChange => map
      | AlterResult.Replace =>
          if (newRoot === root) map
          else { count, root: newRoot, comparator, hash }
      | AlterResult.Removed => failwith "invalid state"
    };
  };

  let put (key: 'k) (value: 'v) (transient: t 'k 'v): (t 'k 'v) =>
    transient |> Transient.update2 putImpl key value;

  let putAll (iter: KeyedIterable.t 'k 'v) (transient: t 'k 'v): (t 'k 'v) =>
    iter |> KeyedIterable.reduce (fun acc k v => acc |> put k v) transient;

  let putAllEntries (iter: Iterable.t ('k, 'v)) (transient: t 'k 'v): (t 'k 'v) => iter
    |> Iterable.reduce (fun acc (k, v) => acc |> put k v) transient;

  let remove (key: 'k) (transient: t 'k 'v): (t 'k 'v) =>
    transient |> alter key Functions.alwaysNone;

  let removeAllImpl
      (_: Transient.Owner.t)
      ({ comparator, hash }: hashMap 'k 'v): (hashMap 'k 'v) =>
    persistentEmptyWith comparator::comparator hash::hash;

  let removeAll (transient: t 'k 'v): (t 'k 'v) =>
    transient |> Transient.update removeAllImpl;
};

let mutate = Transient.mutate;

let putAll (iter: KeyedIterable.t 'k 'v) (map: t 'k 'v): (t 'k 'v) =>
  map |> mutate |> Transient.putAll iter |> Transient.persist;

let putAllEntries (iter: Iterable.t ('k, 'v)) (map: t 'k 'v): (t 'k 'v) =>
  map |> mutate |> Transient.putAllEntries iter |> Transient.persist;

let fromWith
    hash::(hash: Hash.t 'k)
    comparator::(comparator: Comparator.t 'k)
    (iter: KeyedIterable.t 'k 'v): (t 'k 'v) =>
  emptyWith hash::hash comparator::comparator |> putAll iter;

let fromEntriesWith
    hash::(hash: Hash.t 'k)
    comparator::(comparator: Comparator.t 'k)
    (iter: Iterable.t ('k, 'v)): (t 'k 'v) =>
  emptyWith hash::hash comparator::comparator |> putAllEntries iter;

let merge
    (f: 'k => (option 'vAcc) => (option 'v) => (option 'vAcc))
    (initialValue: t 'k 'vAcc)
    (next: t 'k 'v): (t 'k 'vAcc) => ImmSet.union (keysSet next) (keysSet initialValue)
  |> Iterable.reduce (
      fun acc key => {
        let result = f key (initialValue |> get key) (next |> get key);
        switch result {
          | None => acc |> Transient.remove key
          | Some value => acc |> Transient.put key value
        }
      }
    ) (mutate initialValue)
  |> Transient.persist;
