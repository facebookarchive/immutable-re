/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type a = int;

type t = Transient.t IntSet.t;

let mutate ({ count, root }: IntSet.t): t =>
  Transient.create ({ count, root }: IntSet.t);

let addImpl
    (owner: Transient.Owner.t)
    (value: int)
    ({ count, root } as set: IntSet.t): IntSet.t =>
  if (set |> IntSet.contains value) set
  else {
    let newRoot = root |> BitmapTrieIntSet.add
      BitmapTrieIntSet.updateLevelNodeTransient
      owner
      0
      value;

    set.count = count + 1;
    set.root = newRoot;
    set
  };

let add (value: int) (transient: t): t =>
  transient |> Transient.update1 addImpl value;

let contains (value: int) (transient: t): bool =>
  transient |> Transient.get |> IntSet.contains value;

let count (transient: t): int =>
  transient |> Transient.get |> IntSet.count;

let empty (): t =>
  IntSet.empty () |> mutate;

let persist (transient: t): IntSet.t =>
  transient |> Transient.persist;

let removeImpl
    (owner: Transient.Owner.t)
    (value: int)
    ({ count, root } as set: IntSet.t): IntSet.t => {
  let newRoot = root |> BitmapTrieIntSet.remove
    BitmapTrieIntSet.updateLevelNodeTransient
    owner
    0
    value;

  if (newRoot === root) set
  else { count: count - 1, root: newRoot };
};

let remove (value: int) (transient: t): t =>
  transient |> Transient.update1 removeImpl value;

let removeAllImpl
  (_: Transient.Owner.t)
  (_: IntSet.t): IntSet.t => IntSet.empty ();

let removeAll (transient: t): t =>
  transient |> Transient.update removeAllImpl;
