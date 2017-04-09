/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Functions.Operators;

type k = int;

type t 'v = {
  count: int,
  root: (BitmapTrieIntMap.t 'v),
};

let empty (): t 'v => { count: 0, root: BitmapTrieIntMap.Empty };

let alter (key: int) (f: option 'v => option 'v) ({ count, root } as map: t 'v): (t 'v) => {
  let alterResult = ref AlterResult.NoChange;
  let newRoot = root |> BitmapTrieIntMap.alter
    BitmapTrieIntMap.updateLevelNodePersistent
    Transient.Owner.none
    alterResult
    0
    key
    f;

  switch !alterResult {
    | AlterResult.Added => { count: count + 1, root: newRoot }
    | AlterResult.NoChange => map
    | AlterResult.Replace => { count, root: newRoot }
    | AlterResult.Removed => { count: count - 1, root: newRoot }
  }
};

let containsKey (key: int) ({ root }: t 'v): bool =>
  root |> BitmapTrieIntMap.containsKey 0 key;

let count ({ count }: t 'v): int => count;

let get (key: int) ({ root }: t 'v): (option 'v) =>
  root |> BitmapTrieIntMap.get 0 key;

let getOrRaise (key: int) ({ root }: t 'v): 'v =>
  root |> BitmapTrieIntMap.get 0 key |> Option.firstOrRaise;

let isEmpty ({ count }: t 'v): bool => count === 0;

let isNotEmpty ({ count }: t 'v): bool => count !== 0;

let put (key: int) (value: 'v) ({ count, root } as map: t 'v): (t 'v) => {
  let alterResult = ref AlterResult.NoChange;
  let newRoot = root |> BitmapTrieIntMap.putWithResult
    BitmapTrieIntMap.updateLevelNodePersistent
    Transient.Owner.none
    alterResult
    0
    key
    value;

  switch !alterResult {
    | AlterResult.Added => { count: count + 1, root: newRoot }
    | AlterResult.NoChange => map
    | AlterResult.Replace => { count, root: newRoot }
    | AlterResult.Removed => failwith "invalid state"
  }
};

let reduceImpl
    while_::(predicate: 'acc => int => 'v => bool)
    (f: 'acc => int => 'v => 'acc)
    (acc: 'acc)
    ({ root }: t 'v): 'acc =>
  if (predicate === Functions.alwaysTrue3) (BitmapTrieIntMap.reduce f acc root)
  else (BitmapTrieIntMap.reduceWhile predicate f acc root);

let reduce
    while_::(predicate: 'acc => int => 'v => bool)=Functions.alwaysTrue3
    (f: 'acc => int => 'v => 'acc)
    (acc: 'acc)
    (map: t 'v): 'acc =>
  reduceImpl while_::predicate f acc map;

let remove (key: int) (map: t 'v): (t 'v) =>
  map |> alter key Functions.alwaysNone;

let removeAll (_: t 'v): (t 'v) => empty ();

let iterator: Iterable.Iterator.t (int, 'v) (t 'v) = {
  reduce: fun while_::predicate f acc map => map |> reduce
    while_::(fun acc k v => predicate acc (k, v))
    (fun acc k v => f acc (k, v))
    acc
};

let toIterable (map: t 'v): (Iterable.t (int, 'v)) =>
  if (isEmpty map) (Iterable.empty ())
  else Iterable.Iterable map iterator;

let keyedIterator: KeyedIterable.KeyedIterator.t int 'v (t 'v) = { reduce: reduceImpl };

let toKeyedIterable (map: t 'v): (KeyedIterable.t int 'v) =>
  if (isEmpty map) (KeyedIterable.empty ())
  else KeyedIterable.KeyedIterable map keyedIterator;

let toKeySequence ({ root }: t 'v): (Sequence.t int) =>
  root |> BitmapTrieIntMap.toKeySequence;

let toSequence ({ root }: t 'v): (Sequence.t ((int, 'v))) =>
  root |> BitmapTrieIntMap.toSequence;

let keyCollectionOps (): Collection.Ops.t int (t 'v) => {
  count: count,
  toIterable: toKeyedIterable >> KeyedIterable.keys,
  toSequence: toKeySequence,
};

let toKeyCollection (map: t 'v): Collection.t int =>
  if (isEmpty map) (Collection.empty ())
  else (Collection.Collection map (keyCollectionOps ()));

let keys (map: t 'v): Iterable.t k =>
  map |> toKeyedIterable |> KeyedIterable.keys;

let values  (map: t 'v): Iterable.t 'v =>
  map |> toKeyedIterable |> KeyedIterable.values;

let keySetOps: ImmSet.Ops.t int (t 'v) = {
  contains: containsKey,
  count,
  toCollection: toKeyCollection,
  toIterable: keys,
  toSequence: toKeySequence,
};

let keySet (map: t 'v): (ImmSet.t int) =>
  if (isEmpty map) (ImmSet.empty ())
  else ImmSet.Set map keySetOps;

let keyedCollectionOps: KeyedCollection.Ops.t int 'v (t 'v) = {
  containsKey,
  count,
  keys,
  toIterable,
  toKeyedIterable,
  toSequence,
  values,
};

let toKeyedCollection (map: t 'v): (KeyedCollection.t int 'v) =>
  if (isEmpty map) (KeyedCollection.empty ())
  else KeyedCollection.KeyedCollection map keyedCollectionOps;

let mapOps: ImmMap.Ops.t int 'v (t 'v) = {
  containsKey,
  count,
  get,
  getOrRaise,
  keys,
  keySet,
  toIterable,
  toKeyedCollection,
  toKeyedIterable,
  toSequence,
  values,
};

let toMap (map: t 'v): (ImmMap.t int 'v) =>
  if (isEmpty map) (ImmMap.empty ())
  else ImmMap.Map map mapOps;

let module Transient = {
  type k = int;
  type intMap 'v = t 'v;
  type t 'v = Transient.t (intMap 'v);

  let mutate (map: intMap 'v): (t 'v) => Transient.create map;

  let alterImpl
      (owner: Transient.Owner.t)
      (key: int)
      (f: option 'v => option 'v)
      ({ count, root } as map: intMap 'v): (intMap 'v) => {
    let alterResult = ref AlterResult.NoChange;
    let newRoot = root |> BitmapTrieIntMap.alter
      BitmapTrieIntMap.updateLevelNodeTransient
      owner
      alterResult
      0
      key
      f;

    switch !alterResult {
      | AlterResult.Added => { count: count + 1, root: newRoot }
      | AlterResult.NoChange => map
      | AlterResult.Replace => if (root === newRoot) map else { count, root: newRoot }
      | AlterResult.Removed => { count: count - 1, root: newRoot }
    }
  };

  let alter
      (key: int)
      (f: option 'v => option 'v)
      (transient: t 'v): (t 'v) =>
    transient |> Transient.update2 alterImpl key f;

  let containsKey (key: int) (transient: t 'v): bool =>
    transient |> Transient.get |> containsKey key;

  let count (transient: t 'v): int =>
    transient |> Transient.get |> count;

  let persistentEmpty = empty;
  let empty (): t 'v =>
    empty () |> mutate;

  let get (key: int) (transient: t 'v): (option 'v) =>
    transient |> Transient.get |> get key;

  let getOrRaise (key: int) (transient: t 'v): 'v =>
    transient |> Transient.get |> getOrRaise key;

  let isEmpty (transient: t 'v): bool =>
    transient |> Transient.get |> isEmpty;

  let isNotEmpty (transient: t 'v): bool =>
    transient |> Transient.get |> isNotEmpty;

  let persist (transient: t 'v): (intMap 'v) =>
    transient |> Transient.persist;

  let putImpl
      (owner: Transient.Owner.t)
      (key: int)
      (value: 'v)
      ({ count, root } as map: intMap 'v): (intMap 'v) => {
    let alterResult = ref AlterResult.NoChange;
    let newRoot = root |> BitmapTrieIntMap.putWithResult
      BitmapTrieIntMap.updateLevelNodeTransient
      owner
      alterResult
      0
      key
      value;

    switch !alterResult {
      | AlterResult.Added => { count: count + 1, root: newRoot }
      | AlterResult.NoChange => map
      | AlterResult.Replace => if (root === newRoot) map else { count, root: newRoot }
      | AlterResult.Removed => failwith "invalid state"
    }
  };

  let put (key: int) (value: 'v) (transient: t 'v): (t 'v) =>
    transient |> Transient.update2 putImpl key value;

  let putAll
      (iter: KeyedIterable.t int 'v)
      (transient: t 'v): (t 'v) => iter
    |> KeyedIterable.reduce (fun acc k v => acc |> put k v) transient;

  let putAllEntries (iter: Iterable.t ('k, 'v)) (transient: t 'v): (t 'v) => iter
    |> Iterable.reduce (fun acc (k, v) => acc |> put k v) transient;

  let remove (key: int) (transient: t 'v): (t 'v) =>
    transient |> alter key Functions.alwaysNone;

  let removeAllImpl
      (_: Transient.Owner.t)
      (_: intMap 'v): (intMap 'v) => persistentEmpty ();

  let removeAll (transient: t 'v): (t 'v) =>
      transient |> Transient.update removeAllImpl;
};

let mutate = Transient.mutate;

let putAll (iter: KeyedIterable.t int 'v) (map: t 'v): (t 'v) => map
  |> mutate
  |> Transient.putAll iter
  |> Transient.persist;

let putAllEntries (iter: Iterable.t ('k, 'v)) (map: t 'v): (t 'v) => map
  |> mutate
  |> Transient.putAllEntries iter
  |> Transient.persist;

let from (iter: KeyedIterable.t int 'v): (t 'v) =>
  empty () |> putAll iter;

let fromEntries (iter: Iterable.t (k, 'v)): (t 'v) =>
  empty () |> putAllEntries iter;

let merge
    (f: k => (option 'vAcc) => (option 'v) => (option 'vAcc))
    (initialValue: t 'vAcc)
    (next: t 'v): (t 'vAcc) => ImmSet.union (keySet next) (keySet initialValue)
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
