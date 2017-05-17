/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'a = Transient.t (HashSet.t 'a);

let mutate ({ count, root } as set: HashSet.t 'a): (t 'a) =>
  Transient.create { ...set, count, root };

let addImpl
    (owner: Transient.Owner.t)
    (value: 'a)
    ({ count, root, hash, comparator } as set: HashSet.t 'a): (HashSet.t 'a) => {
  let keyHash = hash value;
  if (set |> HashSet.contains value) set
  else {
    let newRoot = root |> BitmapTrieSet.add
      comparator
      BitmapTrieSet.updateLevelNodeTransient
      owner
      0
      keyHash
      value;

    set.count = count + 1;
    set.root = newRoot;
    set;
  }
};

let add (value: 'a) (transient: t 'a): (t 'a) =>
  transient |> Transient.update1 addImpl value;

let contains (value: 'a) (transient: t 'a): bool =>
  transient |> Transient.get |> HashSet.contains value;

let count (transient: t 'a): int =>
  transient |> Transient.get |> HashSet.count;

let emptyWith
    hash::(hash: Hash.t 'a)
    comparator::(comparator: Comparator.t 'a)
    (): (t 'a) =>
  HashSet.emptyWith ::hash ::comparator |>  mutate;

let persist (transient: t 'a): (HashSet.t 'a) =>
  transient |> Transient.persist;

let removeImpl
    (owner: Transient.Owner.t)
    (value: 'a)
    ({ count, root, hash, comparator } as set: HashSet.t 'a): (HashSet.t 'a) => {
  let keyHash = hash value;
  let newRoot = root |> BitmapTrieSet.remove
    comparator
    BitmapTrieSet.updateLevelNodeTransient
    owner
    0
    keyHash
    value;

  /* FIXME: Is this right? */
  if (newRoot === root) set
  else { count: count - 1, root: newRoot, hash, comparator };
};

let remove (value: 'a) (transient: t 'a): (t 'a) =>
  transient |> Transient.update1 removeImpl value;

let removeAllImpl
    (_: Transient.Owner.t)
    ({ hash, comparator }: HashSet.t 'a): (HashSet.t 'a) =>
  HashSet.emptyWith hash::hash comparator::comparator;

let removeAll (transient: t 'a): (t 'a) =>
  transient |> Transient.update removeAllImpl;
