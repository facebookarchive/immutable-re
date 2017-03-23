/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

let module BitmapTrieSet = {
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
    | Collision entryHash entrySet when hash == entryHash =>
        let newEntrySet = entrySet |> AVLTreeSet.add comparator value;
        if (newEntrySet === entrySet) set else (Collision entryHash newEntrySet);
    | Collision entryHash _ =>
        let bitmap = BitmapTrie.bitPos entryHash depth;
        Level bitmap [| set |] owner |> add comparator updateLevelNode owner depth hash value;
    | Entry entryHash entryValue when hash == entryHash =>
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
        (hash == entryHash) && (AVLTreeSet.contains comparator value entrySet);
    | Entry entryHash entryValue =>
        (hash == entryHash) && (Comparator.toEquality comparator entryValue value);
    | Empty => false;
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
    | Collision entryHash entrySet when hash == entryHash =>
        let newEntrySet = entrySet |> AVLTreeSet.remove comparator value;

        if (newEntrySet === entrySet) set else (switch newEntrySet {
          | AVLTreeSet.Leaf entryValue => (Entry entryHash entryValue)
          | _ => (Collision entryHash newEntrySet)
        });
    | Entry entryHash entryValue when (hash == entryHash) && (Comparator.toEquality comparator entryValue value) =>
        Empty;
    | _ => set
  };

  let rec toSequence (set: t 'a): (Sequence.t 'a) => switch set {
    | Level _ nodes _ => nodes |> CopyOnWriteArray.toSequence |> Sequence.flatMap toSequence
    | Collision _ entrySet => AVLTreeSet.toSequence entrySet;
    | Entry _ entryValue => Sequence.return entryValue;
    | Empty => Sequence.empty;
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
};

type t 'a = {
  count: int,
  root: BitmapTrieSet.t 'a,
  comparator: Comparator.t 'a,
  hash: Hash.t 'a,
};

let add (value: 'a) ({ count, root, hash, comparator } as set: t 'a): (t 'a) => {
  let keyHash = hash value;
  let newRoot = root |> BitmapTrieSet.add
    comparator
    BitmapTrieSet.updateLevelNodePersistent
    Transient.Owner.none
    0
    keyHash
    value;

  if (newRoot === root) set
  else { count: count + 1, root: newRoot, hash, comparator };
};

let contains (value: 'a) ({ root, hash, comparator }: t 'a): bool => {
  let keyHash = hash value;
  root |> BitmapTrieSet.contains comparator 0 keyHash value;
};

let count ({ count }: t 'a): int => count;

let emptyWith
    hash::(hash: Hash.t 'a)
    comparator::(comparator: Comparator.t 'a): (t 'a) => {
  count: 0,
  root: BitmapTrieSet.Empty,
  comparator,
  hash,
};

let isEmpty ({ count }: t 'a): bool => count == 0;

let isNotEmpty ({ count }: t 'a): bool => count != 0;

let remove (value: 'a) ({ count, root, hash, comparator } as set: t 'a): (t 'a) => {
  let keyHash = hash value;
  let newRoot = root |> BitmapTrieSet.remove
    comparator
    BitmapTrieSet.updateLevelNodePersistent
    Transient.Owner.none
    0
    keyHash
    value;

  if (newRoot === root) set
  else { count: count - 1, root: newRoot, hash, comparator };
};

let removeAll ({ hash, comparator }: t 'a): (t 'a) =>
  emptyWith hash::hash comparator::comparator;

/* FIXME: Shouldn't use sequences to implement all these.
 * They're way slow.
 */

let toSequence ({ root } as set: t 'a): (Sequence.t 'a) =>
  if (isEmpty set) Sequence.empty
  else root |> BitmapTrieSet.toSequence;

/** FIXME: Implement this correctly. ecchh */
let reduce
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (set: t 'a): 'acc =>
  set |> toSequence |> Sequence.reduce while_::predicate f acc;

let toIterator (set: t 'a): (Iterator.t 'a) =>
  if (isEmpty set) Iterator.empty
  else { reduce: fun predicate f acc => reduce while_::predicate f acc set };

let toKeyedIterator (set: t 'a): (KeyedIterator.t 'a 'a) =>
  if (isEmpty set) KeyedIterator.empty
  else { reduce: fun predicate f acc => set |> reduce
    while_::(fun acc next => predicate acc next next)
    (fun acc next => f acc next next)
    acc
  };

let toSet (set: t 'a): (ImmSet.t 'a) =>
  if (isEmpty set) ImmSet.empty
  else {
    contains: fun v => contains v set,
    count: count set,
    iterator: toIterator set,
    sequence: toSequence set,
  };

let equals (this: t 'a) (that: t 'a): bool =>
  ImmSet.equals (toSet this) (toSet that);

let hash ({ hash, comparator } as set: t 'a): int =>
  set |> reduce (fun acc next => acc + hash next) 0;

let toMap (set: t 'a): (ImmMap.t 'a 'a) =>
  set |> toSet |> ImmMap.ofSet;

let module TransientHashSet = {
  type hashSet 'a = t 'a;

  type t 'a = Transient.t (hashSet 'a);

  let mutate (set: hashSet 'a): (t 'a) =>
    Transient.create set;

  let addImpl
      (owner: Transient.Owner.t)
      (value: 'a)
      ({ count, root, hash, comparator } as set: hashSet 'a): (hashSet 'a) => {
    let keyHash = hash value;
    if (set |> contains value) set
    else {
      let newRoot = root |> BitmapTrieSet.add
        comparator
        BitmapTrieSet.updateLevelNodeTransient
        owner
        0
        keyHash
        value;

      { count: count + 1, root: newRoot, hash, comparator };
    }
  };

  let add (value: 'a) (transient: t 'a): (t 'a) =>
    transient |> Transient.update1 addImpl value;

  let addAllImpl
      (owner: Transient.Owner.t)
      (iter: Iterator.t 'a)
      ({ count, root, hash, comparator } as set: hashSet 'a): (hashSet 'a) => {
    let newCount = ref count;

    let newRoot = iter |> Iterator.reduce (fun acc value => {
      let keyHash = hash value;

      if (acc |> BitmapTrieSet.contains comparator 0 keyHash value) acc
      else  {
        let newRoot = acc |> BitmapTrieSet.add
          comparator
          BitmapTrieSet.updateLevelNodeTransient
          owner
          0
          keyHash
          value;

        newCount := !newCount + 1;
        newRoot
      }
    }) root;

    if (!newCount == count) set
    else { count: !newCount, root: newRoot, hash, comparator };
  };

  let addAll (iter: Iterator.t 'a) (transient: t 'a): (t 'a) =>
    transient |> Transient.update1 addAllImpl iter;

  let contains (value: 'a) (transient: t 'a): bool =>
    transient |> Transient.get |> contains value;

  let count (transient: t 'a): int =>
    transient |> Transient.get |> count;

  let persistentEmptyWith = emptyWith;

  let emptyWith
      hash::(hash: Hash.t 'a)
      comparator::(comparator: Comparator.t 'a)
      (): (t 'a) =>
    persistentEmptyWith hash::hash comparator::comparator |>  mutate;

  let isEmpty (transient: t 'a): bool =>
    transient |> Transient.get |> isEmpty;

  let isNotEmpty (transient: t 'a): bool =>
    transient |> Transient.get |> isNotEmpty;

  let persist (transient: t 'a): (hashSet 'a) =>
    transient |> Transient.persist;

  let removeImpl
      (owner: Transient.Owner.t)
      (value: 'a)
      ({ count, root, hash, comparator } as set: hashSet 'a): (hashSet 'a) => {
    let keyHash = hash value;
    let newRoot = root |> BitmapTrieSet.remove
      comparator
      BitmapTrieSet.updateLevelNodeTransient
      owner
      0
      keyHash
      value;

    if (newRoot === root) set
    else { count: count - 1, root: newRoot, hash, comparator };
  };

  let remove (value: 'a) (transient: t 'a): (t 'a) =>
    transient |> Transient.update1 removeImpl value;

  let removeAllImpl
      (_: Transient.Owner.t)
      ({ hash, comparator }: hashSet 'a): (hashSet 'a) =>
    persistentEmptyWith hash::hash comparator::comparator;

  let removeAll (transient: t 'a): (t 'a) =>
    transient |> Transient.update removeAllImpl;
};

let mutate = TransientHashSet.mutate;

let addAll (iter: Iterator.t 'a) (set: t 'a): (t 'a) =>
  set |> mutate |> TransientHashSet.addAll iter |> TransientHashSet.persist;

let intersect ({ hash, comparator } as this: t 'a) (that: t 'a): (t 'a) =>
  /* FIXME: Makes this more efficient */
  emptyWith hash::hash comparator::comparator
    |> addAll (ImmSet.intersect (toSet this) (toSet that));

let subtract ({ hash, comparator } as this: t 'a) (that: t 'a): (t 'a) =>
  /* FIXME: Makes this more efficient */
  emptyWith hash::hash comparator::comparator
    |> addAll (ImmSet.subtract (toSet this) (toSet that));

let union ({ hash, comparator } as this: t 'a) (that: t 'a): (t 'a) =>
  /* FIXME: Makes this more efficient */
  emptyWith hash::hash comparator::comparator
    |> addAll (ImmSet.union (toSet this) (toSet that));

let module Reducer = Reducer.Make1 {
  type nonrec t 'a = t 'a;
  let reduce = reduce;
};
