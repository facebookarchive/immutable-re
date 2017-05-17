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
  mutable count: int,
  mutable root: (BitmapTrieIntMap.t 'v),
};

let containsKey (key: int) ({ root }: t 'v): bool =>
  root |> BitmapTrieIntMap.containsKey 0 key;

let count ({ count }: t 'v): int => count;

let get (key: int) ({ root }: t 'v): (option 'v) =>
  root |> BitmapTrieIntMap.get 0 key;

let getOrDefault default::(default: 'v) (key: int) ({ root }: t 'v): 'v =>
  root |> BitmapTrieIntMap.getOrDefault 0 default key;

let getOrRaise (key: int) ({ root }: t 'v): 'v =>
  root |> BitmapTrieIntMap.getOrRaise 0 key;

let reduce
    while_::(predicate: 'acc => int => 'v => bool)
    (f: 'acc => int => 'v => 'acc)
    (acc: 'acc)
    ({ root }: t 'v): 'acc =>
  BitmapTrieIntMap.reduce while_::predicate f acc root;

let toSequence (selector: int => 'v => 'c) ({ root }: t 'v): (Sequence.t 'c) =>
  root |> BitmapTrieIntMap.toSequence selector;

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

let empty (): t 'v => { count: 0, root: BitmapTrieIntMap.Empty };

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

let remove (key: int) (map: t 'v): (t 'v) =>
  map |> alter key Functions.alwaysNone;

let removeAll (_: t 'v): (t 'v) => empty ();
