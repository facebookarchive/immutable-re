/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

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

let isEmpty ({ count }: t 'v): bool => count == 0;

let isNotEmpty ({ count }: t 'v): bool => count != 0;

let put (key: int) (value: 'v) (map: t 'v): (t 'v) =>
  map |> alter key (Functions.return @@ Option.return @@ value);

let reduce
    while_::(predicate: 'acc => int => 'v => bool)=Functions.alwaysTrue3
    (f: 'acc => int => 'v => 'acc)
    (acc: 'acc)
    ({ root }: t 'v): 'acc =>
  if (predicate === Functions.alwaysTrue3) (BitmapTrieIntMap.reduce f acc root)
  else (BitmapTrieIntMap.reduceWhile predicate f acc root);

let remove (key: int) (map: t 'v): (t 'v) =>
  map |> alter key Functions.alwaysNone;

let removeAll (_: t 'v): (t 'v) => empty ();

let toIterator (map: t 'v): (Iterator.t (int, 'v)) =>
  if (isEmpty map) (Iterator.empty ())
  else {
    reduce: fun predicate f acc => map |> reduce
      while_::(fun acc k v => predicate acc (k, v))
      (fun acc k v => f acc (k, v))
      acc
  };

let toKeyedIterator (map: t 'v): (KeyedIterator.t int 'v) =>
  if (isEmpty map) (KeyedIterator.empty ())
  else {
    reduce: fun predicate f acc => reduce while_::predicate f acc map
  };

let toSequence ({ root }: t 'v): (Sequence.t ((int, 'v))) =>
  root |> BitmapTrieIntMap.toSequence;

let values ({ root }: t 'v): (Iterator.t 'v) =>
  root |> BitmapTrieIntMap.values;

let toMap (map: t 'v): (ImmMap.t int 'v) => {
  containsKey: fun k => containsKey k map,
  count: (count map),
  get: fun i => get i map,
  getOrRaise: fun i => getOrRaise i map,
  keyedIterator: fun () => toKeyedIterator map,
  sequence: fun () =>toSequence map,
};

let keys (map: t 'v): (ImmSet.t int) =>
  map |> toMap |> ImmMap.keys;

let module TransientIntMap = {
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

  let put (key: int) (value: 'v) (transient: t 'v): (t 'v) =>
    transient |> alter key (Functions.return @@ Option.return @@ value);

  let putAll
      (iter: KeyedIterator.t int 'v)
      (transient: t 'v): (t 'v) => iter
    |> KeyedIterator.reduce (fun acc k v => acc |> put k v) transient;

  let remove (key: int) (transient: t 'v): (t 'v) =>
    transient |> alter key Functions.alwaysNone;

  let removeAllImpl
      (_: Transient.Owner.t)
      (_: intMap 'v): (intMap 'v) => persistentEmpty ();

  let removeAll (transient: t 'v): (t 'v) =>
      transient |> Transient.update removeAllImpl;
};

let mutate = TransientIntMap.mutate;

let putAll (iter: KeyedIterator.t int 'v) (map: t 'v): (t 'v) => map
  |> mutate
  |> TransientIntMap.putAll iter
  |> TransientIntMap.persist;

let map (f: int => 'v => 'b) (map: t 'v): (t 'b) => map
  |> reduce
    (fun acc key value => acc |> TransientIntMap.put key (f key value))
    (mutate (empty ()))
  |> TransientIntMap.persist;

let from (iter: KeyedIterator.t int 'v): (t 'v) => putAll iter (empty ());

let merge
    (f: k => (option 'vAcc) => (option 'v) => (option 'vAcc))
    (initialValue: t 'vAcc)
    (next: t 'v): (t 'vAcc) => ImmSet.union (keys next) (keys initialValue)
  |> Iterator.reduce (
      fun acc key => {
        let result = f key (initialValue |> get key) (next |> get key);
        switch result {
          | None => acc |> TransientIntMap.remove key
          | Some value => acc |> TransientIntMap.put key value
        }
      }
    ) (mutate initialValue)
  |> TransientIntMap.persist;

let module KeyedReducer = KeyedReducer.Make1 {
  type nonrec k = k;
  type nonrec t 'v = t 'v;
  let reduce = reduce;
};
