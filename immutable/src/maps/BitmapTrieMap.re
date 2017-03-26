/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'k 'v =
  | Level int32 (array (t 'k 'v)) Transient.Owner.t
  | Collision int (AVLTreeMap.t 'k 'v)
  | Entry int 'k 'v
  | Empty;

type updateLevelNode 'k 'v = Transient.Owner.t => int => (t 'k 'v) => (t 'k 'v) => (t 'k 'v);

let rec alter
    (comparator: Comparator.t 'k)
    (updateLevelNode: updateLevelNode 'k 'v)
    (owner: Transient.Owner.t)
    (alterResult: ref AlterResult.t)
    (depth: int)
    (hash: int)
    (key: 'k)
    (f: option 'v => option 'v)
    (map: t 'k 'v): (t 'k 'v) => switch map {
  | Entry entryHash entryKey entryValue when (hash === entryHash) =>
      if (Comparator.toEquality comparator key entryKey) (
        switch (f @@ Option.return @@ entryValue) {
          | Some newEntryValue when newEntryValue === entryValue =>
              alterResult := AlterResult.NoChange;
              map;
          | Some newEntryValue =>
              alterResult := AlterResult.Replace;
              Entry entryHash key newEntryValue;
          | None =>
              alterResult := AlterResult.Removed;
              Empty
        })
      else (switch (f None) {
        | Some value =>
            let map = AVLTreeMap.Empty
              |> AVLTreeMap.put comparator entryKey entryValue
              |> AVLTreeMap.put comparator key value;
            alterResult := AlterResult.Added;
            Collision entryHash map;
        | None =>
            alterResult := AlterResult.NoChange;
            map;
      });
  | Entry entryHash _ _ => switch (f None) {
      | Some newEntryValue =>
          let bitmap = BitmapTrie.bitPos entryHash depth;
          Level bitmap [| map |] owner
            |> alter comparator updateLevelNode owner alterResult depth hash key (Functions.return @@ Option.return @@ newEntryValue)
      | _ =>
          alterResult := AlterResult.NoChange;
          map
    }
  | Collision entryHash entryMap =>
      let collisionResult = ref AlterResult.NoChange;
      let newEntryMap = entryMap |> AVLTreeMap.alter comparator collisionResult key f;

      switch !collisionResult {
        | AlterResult.Added =>
            alterResult := AlterResult.Added;
            Collision entryHash newEntryMap;
        | AlterResult.NoChange =>
            alterResult := AlterResult.NoChange;
            map
        | AlterResult.Replace =>
            alterResult := Replace;
            Collision entryHash newEntryMap;
        | AlterResult.Removed => switch newEntryMap {
            | AVLTreeMap.Leaf k v =>
                alterResult := AlterResult.Removed;
                Entry entryHash k v;
            | _ =>
              alterResult := AlterResult.Removed;
              Collision entryHash newEntryMap;
          }
      };
  | Level bitmap nodes _ =>
      let bit = BitmapTrie.bitPos hash depth;
      let index = BitmapTrie.index bitmap bit;

      if (BitmapTrie.containsNode bitmap bit) {
        let childNode = nodes.(index);
        let newChildNode = childNode |> alter comparator updateLevelNode owner alterResult (depth + 1) hash key f;

        switch !alterResult {
          | AlterResult.Added => map |> updateLevelNode owner index newChildNode
          | AlterResult.NoChange => map
          | AlterResult.Removed =>
              if (newChildNode === Empty) {
                let nodes = nodes |> CopyOnWriteArray.removeAt index;
                if (CopyOnWriteArray.count nodes > 0) (Level (Int32.logxor bitmap bit) nodes owner)
                else Empty
              } else (updateLevelNode owner index newChildNode map)
          | Replace => map |> updateLevelNode owner index newChildNode
        }
      } else (switch (f None) {
        | Some newEntryValue =>
            alterResult := AlterResult.Added;
            let node = Entry hash key newEntryValue;
            let nodes = nodes |> CopyOnWriteArray.insertAt index node;
            Level (Int32.logor bitmap bit) nodes owner
        | None =>
            alterResult := AlterResult.NoChange;
            map
      })
  | Empty => switch (f None) {
      | None =>
          alterResult := AlterResult.NoChange;
          map;
      | Some v =>
          alterResult := AlterResult.Added;
          Entry hash key v;
    }
};

let updateLevelNodePersistent
    (_: Transient.Owner.t)
    (index: int)
    (childNode: t 'k 'v)
    (Level bitmap nodes _: (t 'k 'v)): (t 'k 'v) =>
  Level bitmap (CopyOnWriteArray.update index childNode nodes) Transient.Owner.none;

let updateLevelNodeTransient
    (owner: Transient.Owner.t)
    (index: int)
    (childNode: t 'k 'v)
    (Level bitmap nodes nodeOwner as node: (t 'k 'v)): (t 'k 'v) =>
  if (nodeOwner === owner) {
    nodes.(index) = childNode;
    node
  }
  else Level bitmap (CopyOnWriteArray.update index childNode nodes) owner;

let rec containsKey
    (comparator: Comparator.t 'k)
    (depth: int)
    (hash: int)
    (key: 'k)
    (set: t 'k 'v): bool => switch set {
  | Level bitmap nodes _ =>
      let bit = BitmapTrie.bitPos hash depth;
      let index = BitmapTrie.index bitmap bit;

      (BitmapTrie.containsNode bitmap bit) &&
      (containsKey comparator (depth + 1) hash key nodes.(index));
  | Collision entryHash entryMap =>
      (hash === entryHash) && (AVLTreeMap.containsKey comparator key entryMap);
  | Entry entryHash entryKey _ =>
      (hash === entryHash) && (Comparator.toEquality comparator entryKey key);
  | Empty => false;
};

let rec get
    (comparator: Comparator.t 'k)
    (depth: int)
    (hash: int)
    (key: 'k)
    (map: t 'k 'v): (option 'v) => switch map {
  | Level bitmap nodes _ =>
      let bit = BitmapTrie.bitPos hash depth;
      let index = BitmapTrie.index bitmap bit;

      if (BitmapTrie.containsNode bitmap bit) (get comparator (depth + 1) hash key nodes.(index))
      else None
  | Collision entryHash entryMap =>
      if (hash === entryHash) (AVLTreeMap.get comparator key entryMap)
      else None
  | Entry entryHash entryKey entryValue =>
      if ((hash === entryHash) && (Comparator.toEquality comparator entryKey key)) {
        Some entryValue
      } else None
  | Empty => None;
};

let rec getOrRaise
    (comparator: Comparator.t 'k)
    (depth: int)
    (hash: int)
    (key: 'k)
    (map: t 'k 'v): 'v => switch map {
  | Level bitmap nodes _ =>
      let bit = BitmapTrie.bitPos hash depth;
      let index = BitmapTrie.index bitmap bit;

      if (BitmapTrie.containsNode bitmap bit) (getOrRaise comparator (depth + 1) hash key nodes.(index))
      else (failwith "NotFound")
  | Collision entryHash entryMap =>
      if (hash === entryHash) (AVLTreeMap.getOrRaise comparator key entryMap)
      else (failwith "NotFound")
  | Entry entryHash entryKey entryValue =>
      if ((hash === entryHash) && (Comparator.toEquality comparator entryKey key)) {
        entryValue
      } else (failwith "NotFound")
  | Empty => failwith "NotFound";
};

let rec reduce (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) (map: t 'k 'v): 'acc => switch map {
  | Level _ nodes _ =>
      let reducer acc node => node |> reduce f acc;
      nodes |> CopyOnWriteArray.reduce reducer acc
  | Collision _ entryMap =>
      entryMap |> AVLTreeMap.reduce f acc
  | Entry _ entryKey entryValue =>
      f acc entryKey entryValue
  | Empty => acc
};

let rec reduceWhileWithResult
    (shouldContinue: ref bool)
    (predicate: 'acc => 'k => 'v => bool)
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    (map: t 'k 'v): 'acc => switch map {
  | Level _ nodes _ =>
      let reducer acc node => node
        |> reduceWhileWithResult shouldContinue predicate f acc;

      let predicate _ _ => !shouldContinue;

      nodes |> CopyOnWriteArray.reduce while_::predicate reducer acc
  | Collision _ entryMap =>
      entryMap |> AVLTreeMap.reduceWhileWithResult shouldContinue predicate f acc
  | Entry _ entryKey entryValue =>
      if (!shouldContinue && (predicate acc entryKey entryValue)) (f acc entryKey entryValue)
      else acc
  | Empty => acc
};

let reduceWhile
    (predicate: 'acc => 'k => 'v => bool)
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    (map: t 'k 'v): 'acc => {
  let shouldContinue = ref true;
  let predicate acc k v => {
    let result = predicate acc k v;
    shouldContinue := result;
    result;
  };

  reduceWhileWithResult shouldContinue predicate f acc map;
};

let rec toSequence (map: t 'k 'v): (Sequence.t ('k, 'v)) => switch map {
  | Level _ nodes _ => nodes |> CopyOnWriteArray.toSequence |> Sequence.flatMap toSequence
  | Collision _ entryMap => AVLTreeMap.toSequence entryMap
  | Entry _ entryKey entryValue => Sequence.return (entryKey, entryValue);
  | Empty => Sequence.empty ();
};

let rec values (map: t 'k 'v): (Iterator.t 'v) => switch map {
  | Level _ nodes _ => nodes |> CopyOnWriteArray.toIterator |> Iterator.flatMap values
  | Collision _ entryMap => AVLTreeMap.values entryMap
  | Entry _ _ entryValue => Iterator.return entryValue;
  | Empty => Iterator.empty ();
};
