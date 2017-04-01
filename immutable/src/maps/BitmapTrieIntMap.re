/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'v =
  | Level int32 (array (t 'v)) Transient.Owner.t
  | Entry int 'v
  | Empty;

type updateLevelNode 'v = Transient.Owner.t => int => (t 'v) => (t 'v) => (t 'v);

let rec putWithResult
    (updateLevelNode: updateLevelNode 'v)
    (owner: Transient.Owner.t)
    (alterResult: ref AlterResult.t)
    (depth: int)
    (key: int)
    (newEntryValue: 'v)
    (map: t 'v): (t 'v) => switch map {
  | Entry entryKey entryValue when key === entryKey =>
      if (newEntryValue === entryValue) {
        alterResult := AlterResult.NoChange;
        map;
      } else {
        alterResult := AlterResult.Replace;
        Entry key newEntryValue
      }
  | Entry entryKey _ =>
      let bitmap = BitmapTrie.bitPos entryKey depth;
      Level bitmap [| map |] owner
        |> putWithResult updateLevelNode owner alterResult depth key newEntryValue
  | Level bitmap nodes _ =>
      let bit = BitmapTrie.bitPos key depth;
      let index = BitmapTrie.index bitmap bit;

      if (BitmapTrie.containsNode bitmap bit) {
        let childNode = nodes.(index);
        let newChildNode = childNode
          |> putWithResult updateLevelNode owner alterResult (depth + 1) key newEntryValue;

        switch !alterResult {
          | AlterResult.Added => map |> updateLevelNode owner index newChildNode;
          | AlterResult.NoChange => map
          | AlterResult.Replace => map |> updateLevelNode owner index newChildNode
          | AlterResult.Removed => failwith "Illegal State"
        }
      }
      else {
        alterResult := AlterResult.Added;
        let node = Entry key newEntryValue;
        let nodes = nodes |> CopyOnWriteArray.insertAt index node;
        Level (Int32.logor bitmap bit) nodes owner;
      }
  | Empty =>
      alterResult := AlterResult.Added;
      Entry key newEntryValue;
};

let rec alter
    (updateLevelNode: updateLevelNode 'v)
    (owner: Transient.Owner.t)
    (alterResult: ref AlterResult.t)
    (depth: int)
    (key: int)
    (f: option 'v => option 'v)
    (map: t 'v): (t 'v) => switch map {
  | Entry entryKey entryValue when key === entryKey => switch (f @@ Option.return @@ entryValue) {
      | Some newEntryValue when newEntryValue === entryValue =>
          alterResult := AlterResult.NoChange;
          map;
      | Some newEntryValue =>
          alterResult := AlterResult.Replace;
          Entry key newEntryValue
      | None =>
          alterResult := AlterResult.Removed;
          Empty
    }
  | Entry entryKey _ => switch (f None) {
      | Some newEntryValue =>
          let bitmap = BitmapTrie.bitPos entryKey depth;
          Level bitmap [| map |] owner
            |> putWithResult updateLevelNode owner alterResult depth key newEntryValue
      | _ =>
          alterResult := AlterResult.NoChange;
          map;
    }
  | Level bitmap nodes _ =>
      let bit = BitmapTrie.bitPos key depth;
      let index = BitmapTrie.index bitmap bit;

      if (BitmapTrie.containsNode bitmap bit) {
        let childNode = nodes.(index);
        let newChildNode = childNode |> alter updateLevelNode owner alterResult (depth + 1) key f;

        switch !alterResult {
          | AlterResult.Added => map |> updateLevelNode owner index newChildNode;
          | AlterResult.NoChange => map
          | AlterResult.Replace => map |> updateLevelNode owner index newChildNode
          | AlterResult.Removed => switch newChildNode {
              | Empty =>
                  let nodes = nodes |> CopyOnWriteArray.removeAt index;
                  if (CopyOnWriteArray.count nodes > 0) (Level (Int32.logxor bitmap bit) nodes owner)
                  else Empty
              | _ => map |> updateLevelNode owner index newChildNode
            }
        }
      } else (switch (f None) {
        | Some newEntryValue =>
            alterResult := AlterResult.Added;
            let node = Entry key newEntryValue;
            let nodes = nodes |> CopyOnWriteArray.insertAt index node;
            Level (Int32.logor bitmap bit) nodes owner;
        | None =>
            alterResult := AlterResult.NoChange;
            map;
      })
  | Empty => switch (f None) {
      | None =>
          alterResult := AlterResult.NoChange;
          map
      | Some v =>
          alterResult := AlterResult.Added;
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
  | Entry entryKey _ => key === entryKey;
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
      let predicate _ _ => !shouldContinue;

      nodes |> CopyOnWriteArray.reduce while_::predicate reducer acc
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

let rec toKeySequence (map: t 'v): (Sequence.t int) => switch map {
  | Entry key value => Sequence.return key
  | Level _ nodes _ => nodes |> CopyOnWriteArray.toSequence |> Sequence.flatMap toKeySequence
  | Empty => Sequence.empty ();
};

let rec toSequence (map: t 'v): (Sequence.t (int, 'v)) => switch map {
  | Entry key value => Sequence.return (key, value)
  | Level _ nodes _ => nodes |> CopyOnWriteArray.toSequence |> Sequence.flatMap toSequence
  | Empty => Sequence.empty ();
};

let rec get (depth: int) (key: int) (map: t 'v): (option 'v) => switch map {
  | Level bitmap nodes _ =>
      let bit = BitmapTrie.bitPos key depth;
      let index = BitmapTrie.index bitmap bit;

      if (BitmapTrie.containsNode bitmap bit) (get (depth + 1) key nodes.(index))
      else None;
  | Entry entryKey entryValue when key === entryKey => Some entryValue
  | _ => None
};
