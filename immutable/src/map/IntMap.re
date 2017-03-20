/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

let module BitmapTrieIntMap = {
  type t 'v =
    | Level int32 (array (t 'v)) Transient.Owner.t
    | Entry int 'v
    | Empty;

  type updateLevelNode 'v = Transient.Owner.t => int => (t 'v) => (t 'v) => (t 'v);

  type alterResult =
    | Added
    | NoChange
    | Removed
    | Replace;

  let rec alter
      (updateLevelNode: updateLevelNode 'v)
      (owner: Transient.Owner.t)
      (alterResult: ref alterResult)
      (depth: int)
      (key: int)
      (f: option 'v => option 'v)
      (map: t 'v): (t 'v) => switch map {
    | Entry entryKey entryValue when key == entryKey => switch (f @@ Option.return @@ entryValue) {
        | Some newEntryValue when newEntryValue === entryValue =>
            alterResult := NoChange;
            map;
        | Some newEntryValue =>
            alterResult := Replace;
            Entry key newEntryValue
        | None =>
            alterResult := Removed;
            Empty
      }
    | Entry entryKey _ => switch (f None) {
        | Some newEntryValue =>
            let bitmap = BitmapTrie.bitPos entryKey depth;
            Level bitmap [| map |] owner
              |> alter updateLevelNode owner alterResult depth key (Functions.return @@ Option.return @@ newEntryValue)
        | _ =>
            alterResult := NoChange;
            map;
      }
    | Level bitmap nodes _ =>
        let bit = BitmapTrie.bitPos key depth;
        let index = BitmapTrie.index bitmap bit;

        if (BitmapTrie.containsNode bitmap bit) {
          let childNode = nodes.(index);
          let newChildNode = childNode |> alter updateLevelNode owner alterResult (depth + 1) key f;

          switch !alterResult {
            | Added => map |> updateLevelNode owner index newChildNode;
            | NoChange => map
            | Replace => map |> updateLevelNode owner index newChildNode
            | Removed => switch newChildNode {
                | Empty =>
                    let nodes = nodes |> CopyOnWriteArray.removeAt index;
                    if (CopyOnWriteArray.count nodes > 0) (Level (Int32.logxor bitmap bit) nodes owner)
                    else Empty
                | _ => map |> updateLevelNode owner index newChildNode
              }
          }
        } else (switch (f None) {
          | Some newEntryValue =>
              alterResult := Added;
              let node = Entry key newEntryValue;
              let nodes = nodes |> CopyOnWriteArray.insertAt index node;
              Level (Int32.logor bitmap bit) nodes owner;
          | None =>
              alterResult := NoChange;
              map;
        })
    | Empty => switch (f None) {
        | None =>
            alterResult := NoChange;
            map
        | Some v =>
            alterResult := Added;
            Entry key v;
      }
  };

  let updateLevelNodePersistent
      (_: Transient.Owner.t)
      (index: int)
      (childNode: t 'v)
      (Level bitmap nodes _: (t 'v)): (t 'v) =>
    Level bitmap (CopyOnWriteArray.update index childNode nodes) Transient.Owner.none;

  let rec containsKey
      (depth: int)
      (key: int)
      (map: t 'v): bool => switch map {
    | Level bitmap nodes _ =>
        let bit = BitmapTrie.bitPos key depth;
        let index = BitmapTrie.index bitmap bit;

        (BitmapTrie.containsNode bitmap bit) &&
        (containsKey (depth + 1) key nodes.(index));
    | Entry entryKey _ => key == entryKey;
    | Empty => false;
  };

  let updateLevelNodeTransient
      (owner: Transient.Owner.t)
      (index: int)
      (childNode: t 'v)
      (Level bitmap nodes nodeOwner as node: (t 'v)): (t 'v) =>
    if (nodeOwner === owner) {
        nodes.(index) = childNode;
        node
    }
    else Level bitmap (CopyOnWriteArray.update index childNode nodes) owner;

  let rec containsWith
      (equality: Equality.t 'v)
      (depth: int)
      (key: int)
      (value: 'v)
      (map: t 'v): bool => switch map {
    | Level bitmap nodes _ =>
        let bit = BitmapTrie.bitPos key depth;
        let index = BitmapTrie.index bitmap bit;

        (BitmapTrie.containsNode bitmap bit) &&
        (containsWith equality (depth + 1) key value nodes.(index));
    | Entry entryKey entryValue =>
        key == entryKey && (equality value entryValue);
    | Empty => false;
  };

  let rec reduce (f: 'acc => int => 'v => 'acc) (acc: 'acc) (map: t 'v): 'acc => switch map {
    | Level _ nodes _ =>
        let reducer acc map => reduce f acc map;
        nodes |> CopyOnWriteArray.reduce reducer acc;
    | Entry key value => f acc key value;
    | Empty => acc;
  };

  let rec reduceWhileWithResult
      (shouldContinue: ref bool)
      (predicate: 'acc => int => 'v => bool)
      (f: 'acc => int => 'v => 'acc)
      (acc: 'acc)
      (map: t 'v): 'acc => switch map {
    | Level _ nodes _ =>
        let reducer acc node => node
          |> reduceWhileWithResult shouldContinue predicate f acc;
        let predicate acc node => !shouldContinue;

        nodes |> CopyOnWriteArray.reduceWhile predicate reducer acc
    | Entry key value =>
        if (!shouldContinue && (predicate acc key value)) (f acc key value)
        else acc
    | Empty => acc
  };

  let reduceWhile
      (predicate: 'acc => int => 'v => bool)
      (f: 'acc => int => 'v => 'acc)
      (acc: 'acc)
      (map: t 'k): 'acc => {
    let shouldContinue = ref true;
    let predicate acc k v => {
      let result = predicate acc k v;
      shouldContinue := result;
      result;
    };

    reduceWhileWithResult shouldContinue predicate f acc map;
  };

  let rec toSequence (map: t 'v): (Sequence.t (int, 'v)) => switch map {
    | Entry key value => Sequence.return (key, value)
    | Level _ nodes _ => nodes |> CopyOnWriteArray.toSequence |> Sequence.flatMap toSequence
    | Empty => Sequence.empty;
  };

  let rec get (depth: int) (key: int) (map: t 'v): (option 'v) => switch map {
    | Level bitmap nodes _ =>
        let bit = BitmapTrie.bitPos key depth;
        let index = BitmapTrie.index bitmap bit;

        if (BitmapTrie.containsNode bitmap bit) (get (depth + 1) key nodes.(index))
        else None;
    | Entry entryKey entryValue when key == entryKey => Some entryValue
    | _ => None
  };

  let rec values (map: t 'v): (Iterator.t 'v) => switch map {
    | Entry _ value => Iterator.return value
    | Level _ nodes _ => nodes |> CopyOnWriteArray.toIterator |> Iterator.flatMap values
    | Empty => Iterator.empty;
  };
};

type k = int;

type t 'v = {
  count: int,
  root: (BitmapTrieIntMap.t 'v),
};

let empty: t 'v = { count: 0, root: BitmapTrieIntMap.Empty };

let alter (key: int) (f: option 'v => option 'v) ({ count, root } as map: t 'v): (t 'v) => {
  let alterResult = ref BitmapTrieIntMap.NoChange;
  let newRoot = root |> BitmapTrieIntMap.alter
    BitmapTrieIntMap.updateLevelNodePersistent
    Transient.Owner.none
    alterResult
    0
    key
    f;

  switch !alterResult {
    | BitmapTrieIntMap.Added => { count: count + 1, root: newRoot }
    | BitmapTrieIntMap.NoChange => map
    | BitmapTrieIntMap.Replace => { count, root: newRoot }
    | BitmapTrieIntMap.Removed => { count: count - 1, root: newRoot }
  }
};

let containsKey (key: int) ({ root }: t 'v): bool =>
  root |> BitmapTrieIntMap.containsKey 0 key;

let containsWith (equality: Equality.t 'v) (key: int) (value: 'v) ({ root }: t 'v): bool =>
  root |> BitmapTrieIntMap.containsWith equality 0 key value;

let contains (key: int) (value: 'v) (map: t 'v): bool =>
  map |> containsWith Equality.structural key value;

let count ({ count }: t 'v): int => count;

let get (key: int) ({ root }: t 'v): (option 'v) =>
  root |> BitmapTrieIntMap.get 0 key;

let getOrRaise (key: int) ({ root }: t 'v): 'v =>
  root |> BitmapTrieIntMap.get 0 key |> Option.firstOrRaise;

let isEmpty ({ count }: t 'v): bool => count == 0;

let isNotEmpty ({ count }: t 'v): bool => count != 0;

let put (key: int) (value: 'v) (map: t 'v): (t 'v) =>
  map |> alter key (Functions.return @@ Option.return @@ value);

let reduce (f: 'vcc => int => 'v => 'vcc) (acc: 'vcc) ({ root }: t 'v): 'vcc =>
  root |> BitmapTrieIntMap.reduce f acc;

let reduceWhile
    (predicate: 'acc => int => 'v => bool)
    (f: 'acc => int => 'v => 'acc)
    (acc: 'acc)
    ({ root }: t 'v): 'acc =>
  root |> BitmapTrieIntMap.reduceWhile predicate f acc;

let remove (key: int) (map: t 'v): (t 'v) =>
  map |> alter key Functions.alwaysNone;

let removeAll (_: t 'v): (t 'v) => empty;

let toIterator (map: t 'v): (Iterator.t (int, 'v)) =>
  if (isEmpty map) Iterator.empty
  else {
    reduceWhile: fun predicate f acc => map |> reduceWhile
      (fun acc k v => predicate acc (k, v))
      (fun acc k v => f acc (k, v))
      acc
  };

let toKeyedIterator (map: t 'v): (KeyedIterator.t int 'v) =>
  if (isEmpty map) KeyedIterator.empty
  else {
    reduceWhile: fun predicate f acc => reduceWhile predicate f acc map
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
  keyedIterator: toKeyedIterator map,
  sequence: toSequence map,
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
    let alterResult = ref BitmapTrieIntMap.NoChange;
    let newRoot = root |> BitmapTrieIntMap.alter
      BitmapTrieIntMap.updateLevelNodeTransient
        owner
        alterResult
        0
        key
        f;

    switch !alterResult {
      | BitmapTrieIntMap.Added => { count: count + 1, root: newRoot }
      | BitmapTrieIntMap.NoChange => map
      | BitmapTrieIntMap.Replace => if (root === newRoot) map else { count, root: newRoot }
      | BitmapTrieIntMap.Removed => { count: count - 1, root: newRoot }
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
    empty |> mutate;

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
      (_: intMap 'v): (intMap 'v) => persistentEmpty;

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
    (mutate empty)
  |> TransientIntMap.persist;

let from (iter: KeyedIterator.t int 'v): (t 'v) => putAll iter empty;

let merge
    (f: int => (option 'vAcc) => (option 'v) => (option 'vAcc))
    (next: t 'v)
    (map: t 'vAcc): (t 'vAcc) =>
  ImmSet.union (keys map) (keys next)
    |> Iterator.reduce (
        fun acc key => {
          let result = f key (map |> get key) (next |> get key);
          switch result {
            | None => acc |> TransientIntMap.remove key
            | Some value => acc |> TransientIntMap.put key value
          }
        }
      )
      (mutate map)
    |> TransientIntMap.persist;
