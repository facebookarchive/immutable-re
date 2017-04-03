/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

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

let isEmpty ({ count }: t 'a): bool => count === 0;

let isNotEmpty ({ count }: t 'a): bool => count !== 0;

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

let reduceImpl
    while_::(predicate: 'acc => 'a => bool)
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    ({ root }: t 'a): 'acc =>
  if (predicate === Functions.alwaysTrue2) (BitmapTrieSet.reduce f acc root)
  else (BitmapTrieSet.reduceWhile predicate f acc root);

let reduce
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (set: t 'a): 'acc =>
  reduceImpl while_::predicate f acc set;

let iterator: Iterable.Iterator.t 'a (t 'a) = { reduce: reduceImpl };

let toIterable (set: t 'a): (Iterable.t 'a) =>
  if (isEmpty set) (Iterable.empty ())
  else Iterable.Iterable set iterator;

let toSequence ({ root } as set: t 'a): (Sequence.t 'a) =>
  if (isEmpty set) (Sequence.empty ())
  else root |> BitmapTrieSet.toSequence;

let collectionOps: Collection.Ops.t 'a (t 'a) = {
  count,
  toIterable,
  toSequence,
};

let toCollection (set: t 'a): (Collection.t 'a) =>
  if (isEmpty set) (Collection.empty ())
  else Collection.Collection set collectionOps;

let setOps: ImmSet.Ops.t 'a (t 'a) = {
  contains,
  count,
  toCollection,
  toIterable,
  toSequence,
};

let toSet (set: t 'a): (ImmSet.t 'a) =>
  if (isEmpty set) (ImmSet.empty ())
  else ImmSet.Set set setOps;

let equals (this: t 'a) (that: t 'a): bool =>
  ImmSet.equals (toSet this) (toSet that);

let hash ({ hash, comparator } as set: t 'a): int =>
  set |> reduce (fun acc next => acc + hash next) 0;

let module Transient = {
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
      (iter: Iterable.t 'a)
      ({ count, root, hash, comparator } as set: hashSet 'a): (hashSet 'a) => {
    let newCount = ref count;

    let newRoot = iter |> Iterable.reduce (fun acc value => {
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

    if (!newCount === count) set
    else { count: !newCount, root: newRoot, hash, comparator };
  };

  let addAll (iter: Iterable.t 'a) (transient: t 'a): (t 'a) =>
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

let mutate = Transient.mutate;

let addAll (iter: Iterable.t 'a) (set: t 'a): (t 'a) =>
  set |> mutate |> Transient.addAll iter |> Transient.persist;

let fromWith
    hash::(hash: Hash.t 'a)
    comparator::(comparator: Comparator.t 'a)
    (iterable: Iterable.t 'a): (t 'a) =>
  emptyWith hash::hash comparator::comparator |> addAll iterable;

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
