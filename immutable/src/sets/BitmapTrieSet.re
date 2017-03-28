/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'a =
  | Level int32 (array (t 'a)) Transient.Owner.t
  | Collision int (AVLTreeSet.t 'a)
  | Entry int 'a
  | Empty;

type updateLevelNode 'a = Transient.Owner.t => int => (t 'a) => (t 'a) => (t 'a);

let rec add
    (comparator: Comparator.t 'a)
    (updateLevelNode: updateLevelNode 'a)
    (owner: Transient.Owner.t)
    (depth: int)
    (hash: int)
    (value: 'a)
    (set: t 'a): (t 'a) => switch set {
  | Level bitmap nodes _ =>
      let bit = BitmapTrie.bitPos hash depth;
      let index = BitmapTrie.index bitmap bit;

      if (BitmapTrie.containsNode bitmap bit) {
        let childNode = nodes.(index);
        let newChildNode = childNode |> add comparator updateLevelNode owner (depth + 1) hash value;

        if (childNode === newChildNode) set
        else (updateLevelNode owner index newChildNode set)
      } else {
        let entry = Entry hash value;
        let nodes = nodes |> CopyOnWriteArray.insertAt index entry;
        Level (Int32.logor bitmap bit) nodes owner;
      }
  | Collision entryHash entrySet when hash === entryHash =>
      let newEntrySet = entrySet |> AVLTreeSet.add comparator value;
      if (newEntrySet === entrySet) set else (Collision entryHash newEntrySet);
  | Collision entryHash _ =>
      let bitmap = BitmapTrie.bitPos entryHash depth;
      Level bitmap [| set |] owner |> add comparator updateLevelNode owner depth hash value;
  | Entry entryHash entryValue when hash === entryHash =>
      if (Comparator.toEquality comparator value entryValue) set
      else {
        let set = AVLTreeSet.Empty
          |> AVLTreeSet.add comparator entryValue
          |> AVLTreeSet.add comparator value;
        Collision entryHash set;
      };
  | Entry entryHash _ =>
      let bitmap = BitmapTrie.bitPos entryHash depth;
      Level bitmap [| set |] owner |> add comparator updateLevelNode owner depth hash value;
  | Empty => Entry hash value;
};

let rec contains
    (comparator: Comparator.t 'a)
    (depth: int)
    (hash: int)
    (value: 'a)
    (set: t 'a): bool => switch set {
  | Level bitmap nodes _ =>
      let bit = BitmapTrie.bitPos hash depth;
      let index = BitmapTrie.index bitmap bit;

      (BitmapTrie.containsNode bitmap bit) &&
      (contains comparator (depth + 1) hash value nodes.(index));
  | Collision entryHash entrySet =>
      (hash === entryHash) && (AVLTreeSet.contains comparator value entrySet);
  | Entry entryHash entryValue =>
      (hash === entryHash) && (Comparator.toEquality comparator entryValue value);
  | Empty => false;
};

let rec reduce (f: 'acc => 'a => 'acc) (acc: 'acc) (map: t 'a): 'acc => switch map {
  | Level _ nodes _ =>
      let reducer acc map => reduce f acc map;
      nodes |> CopyOnWriteArray.reduce reducer acc;
  | Collision _ entrySet => entrySet |> AVLTreeSet.reduce f acc;
  | Entry _ entryValue => f acc entryValue;
  | Empty => acc;
};

let rec reduceWhileWithResult
    (shouldContinue: ref bool)
    (predicate: 'acc => 'a => bool)
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (map: t 'a): 'acc => switch map {
  | Level _ nodes _ =>
      let reducer acc node => node
        |> reduceWhileWithResult shouldContinue predicate f acc;
      let predicate _ _ => !shouldContinue;

      nodes |> CopyOnWriteArray.reduce while_::predicate reducer acc
  | Collision _ entrySet => entrySet
      |> AVLTreeSet.reduceWhileWithResult shouldContinue predicate f acc
  | Entry _ entryValue =>
      if (!shouldContinue && (predicate acc entryValue)) (f acc entryValue)
      else acc
  | Empty => acc
};

let reduceWhile
    (predicate: 'acc => 'a => bool)
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (map: t 'a): 'acc => {
  let shouldContinue = ref true;
  let predicate acc v => {
    let result = predicate acc v;
    shouldContinue := result;
    result;
  };

  reduceWhileWithResult shouldContinue predicate f acc map;
};

let rec remove
    (comparator: Comparator.t 'a)
    (updateLevelNode: updateLevelNode 'a)
    (owner: Transient.Owner.t)
    (depth: int)
    (hash: int)
    (value: 'a)
    (set: t 'a): (t 'a) => switch set {
  | Level bitmap nodes _ =>
      let bit = BitmapTrie.bitPos hash depth;
      let index = BitmapTrie.index bitmap bit;

      if (BitmapTrie.containsNode bitmap bit) {
        let childNode = nodes.(index);
        let newChildNode = childNode |> remove comparator updateLevelNode owner (depth + 1) hash value;

        if (newChildNode === childNode) set
        else if (newChildNode === Empty) {
          let nodes = nodes |> CopyOnWriteArray.removeAt index;

          if (CopyOnWriteArray.count nodes > 0) (Level (Int32.logxor bitmap bit) nodes owner)
          else Empty;
        } else (updateLevelNode owner index newChildNode set);
      } else set;
  | Collision entryHash entrySet when hash === entryHash =>
      let newEntrySet = entrySet |> AVLTreeSet.remove comparator value;

      if (newEntrySet === entrySet) set else (switch newEntrySet {
        | AVLTreeSet.Leaf entryValue => (Entry entryHash entryValue)
        | _ => (Collision entryHash newEntrySet)
      });
  | Entry entryHash entryValue when (hash === entryHash) && (Comparator.toEquality comparator entryValue value) =>
      Empty;
  | _ => set
};

let rec toSequence (set: t 'a): (Sequence.t 'a) => switch set {
  | Level _ nodes _ => nodes |> CopyOnWriteArray.toSequence |> Sequence.flatMap toSequence
  | Collision _ entrySet => AVLTreeSet.toSequence entrySet;
  | Entry _ entryValue => Sequence.return entryValue;
  | Empty => Sequence.empty ();
};

let updateLevelNodePersistent
    (_: Transient.Owner.t)
    (index: int)
    (childNode: t 'a)
    (Level bitmap nodes _: (t 'a)): (t 'a) =>
  Level bitmap (CopyOnWriteArray.update index childNode nodes) Transient.Owner.none;

let updateLevelNodeTransient
    (owner: Transient.Owner.t)
    (index: int)
    (childNode: t 'a)
    (Level bitmap nodes nodeOwner as node: (t 'a)): (t 'a) =>
  if (nodeOwner === owner) {
    nodes.(index) = childNode;
    node
  }
  else Level bitmap (CopyOnWriteArray.update index childNode nodes) owner
