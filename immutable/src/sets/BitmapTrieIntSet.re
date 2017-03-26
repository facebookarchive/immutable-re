/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

/* FIXME: I'm fairly certain the BitmapTrie functions can be changed to properly sort IntSet */

type t =
  | Level int32 (array t) Transient.Owner.t
  | Entry int
  | Empty;

type updateLevelNode = Transient.Owner.t => int => t => t => t;

let rec add
    (updateLevelNode: updateLevelNode)
    (owner: Transient.Owner.t)
    (depth: int)
    (value: int)
    (set: t): t => switch set {
  | Level bitmap nodes _ =>
      let bit = BitmapTrie.bitPos value depth;
      let index = BitmapTrie.index bitmap bit;

      if (BitmapTrie.containsNode bitmap bit) {
        let childNode = nodes.(index);
        let newChildNode = childNode |> add updateLevelNode owner (depth + 1) value;

        if (childNode === newChildNode) set
        else (updateLevelNode owner index newChildNode set)
      } else {
        let entry = Entry value;
        let nodes = nodes |> CopyOnWriteArray.insertAt index entry;
        Level (Int32.logor bitmap bit) nodes owner;
      }
  | Entry entryValue =>
    if (value === entryValue) set
    else {
      let bitmap = BitmapTrie.bitPos entryValue depth;
      Level bitmap [| set |] owner |> add updateLevelNode owner depth value;
    }
  | Empty => Entry value;
};

let rec contains
    (depth: int)
    (value: int)
    (set: t): bool => switch set {
  | Level bitmap nodes _ =>
      let bit = BitmapTrie.bitPos value depth;
      let index = BitmapTrie.index bitmap bit;

      (BitmapTrie.containsNode bitmap bit) &&
      (contains (depth + 1) value nodes.(index));
  | Entry entryValue => value === entryValue
  | Empty => false;
};

let rec reduce (f: 'acc => int => 'acc) (acc: 'acc) (map: t): 'acc => switch map {
  | Level _ nodes _ =>
      let reducer acc map => reduce f acc map;
      nodes |> CopyOnWriteArray.reduce reducer acc;
  | Entry value => f acc value;
  | Empty => acc;
};

let rec reduceWhileWithResult
    (shouldContinue: ref bool)
    (predicate: 'acc => int => bool)
    (f: 'acc => int => 'acc)
    (acc: 'acc)
    (map: t): 'acc => switch map {
  | Level _ nodes _ =>
      let reducer acc node => node
        |> reduceWhileWithResult shouldContinue predicate f acc;
      let predicate _ _ => !shouldContinue;

      nodes |> CopyOnWriteArray.reduce while_::predicate reducer acc
  | Entry value =>
      if (!shouldContinue && (predicate acc value)) (f acc value)
      else acc
  | Empty => acc
};

let reduceWhile
    (predicate: 'acc => int => bool)
    (f: 'acc => int => 'acc)
    (acc: 'acc)
    (map: t): 'acc => {
  let shouldContinue = ref true;
  let predicate acc v => {
    let result = predicate acc v;
    shouldContinue := result;
    result;
  };

  reduceWhileWithResult shouldContinue predicate f acc map;
};

let rec remove
    (updateLevelNode: updateLevelNode)
    (owner: Transient.Owner.t)
    (depth: int)
    (value: int)
    (set: t): t => switch set {
  | Level bitmap nodes _ =>
      let bit = BitmapTrie.bitPos value depth;
      let index = BitmapTrie.index bitmap bit;

      if (BitmapTrie.containsNode bitmap bit) {
        let childNode = nodes.(index);
        let newChildNode = childNode |> remove updateLevelNode owner (depth + 1) value;

        if (newChildNode === childNode) set
        else if (newChildNode === Empty) {
          let nodes = nodes |> CopyOnWriteArray.removeAt index;

          if (CopyOnWriteArray.count nodes > 0) (Level (Int32.logxor bitmap bit) nodes owner)
          else Empty;
        } else (updateLevelNode owner index newChildNode set);
      } else set;
  | Entry entryValue when value === entryValue =>
      Empty;
  | _ => set
};

let rec toSequence (set: t): (Sequence.t int) => switch set {
  | Level _ nodes _ => nodes |> CopyOnWriteArray.toSequence |> Sequence.flatMap toSequence
  | Entry entryValue => Sequence.return entryValue;
  | Empty => Sequence.empty ();
};

let updateLevelNodePersistent
    (_: Transient.Owner.t)
    (index: int)
    (childNode: t)
    (Level bitmap nodes _: t): t =>
  Level bitmap (CopyOnWriteArray.update index childNode nodes) Transient.Owner.none;

let updateLevelNodeTransient
    (owner: Transient.Owner.t)
    (index: int)
    (childNode: t)
    (Level bitmap nodes nodeOwner as node: t): t =>
  if (nodeOwner === owner) {
    nodes.(index) = childNode;
    node
  }
  else Level bitmap (CopyOnWriteArray.update index childNode nodes) owner;
