/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type a = int;

type t = {
  count: int,
  root: BitmapTrieIntSet.t,
};

let contains (value: int) ({ root }: t): bool =>
  root |> BitmapTrieIntSet.contains 0 value;

let count ({ count }: t): int => count;

let reduce
    while_::(predicate: 'acc => int => bool)
    (f: 'acc => int => 'acc)
    (acc: 'acc)
    ({ root }: t): 'acc =>
  BitmapTrieIntSet.reduce while_::predicate f acc root;

let toSequence ({ root }: t): (Sequence.t int) =>
  root |> BitmapTrieIntSet.toSequence;

let add (value: int) ({ count, root } as set: t): t => {
  let newRoot = root |> BitmapTrieIntSet.add
    BitmapTrieIntSet.updateLevelNodePersistent
    Transient.Owner.none
    0
    value;

  if (newRoot === root) set
  else { count: count + 1, root: newRoot };
};

let emptyInstance: t = { count: 0, root: BitmapTrieIntSet.Empty };

let empty (): t => emptyInstance;

let hash (set: t): int => set
  |> reduce while_::Functions.alwaysTrue2 (fun acc next => acc + next) 0;

let remove (value: int) ({ count, root } as set: t): t => {
  let newRoot = root |> BitmapTrieIntSet.remove
    BitmapTrieIntSet.updateLevelNodePersistent
    Transient.Owner.none
    0
    value;

  if (newRoot === root) set
  else { count: count - 1, root: newRoot };
};

let removeAll (_: t): t =>
  emptyInstance;

let module Transient = {
  type a = int;

  type intSet = t;

  type t = Transient.t intSet;

  let mutate (set: intSet): t =>
    Transient.create set;

  let addImpl
      (owner: Transient.Owner.t)
      (value: int)
      ({ count, root } as set: intSet): intSet => {
    if (set |> contains value) set
    else {
      let newRoot = root |> BitmapTrieIntSet.add
        BitmapTrieIntSet.updateLevelNodeTransient
        owner
        0
        value;

      { count: count + 1, root: newRoot };
    }
  };

  let add (value: int) (transient: t): t =>
    transient |> Transient.update1 addImpl value;

  let addAllImpl
      (owner: Transient.Owner.t)
      (iter: Iterable.t int)
      ({ count, root } as set: intSet): intSet => {
    let newCount = ref count;

    let newRoot = iter |> Iterable.reduce while_::Functions.alwaysTrue2 (fun acc value => {
      if (acc |> BitmapTrieIntSet.contains 0 value) acc
      else  {
        let newRoot = acc |> BitmapTrieIntSet.add
          BitmapTrieIntSet.updateLevelNodeTransient
          owner
          0
          value;

        newCount := !newCount + 1;
        newRoot
      }
    }) root;

    if (!newCount === count) set
    else { count: !newCount, root: newRoot };
  };

  let addAll (iter: Iterable.t int) (transient: t): t =>
    transient |> Transient.update1 addAllImpl iter;

  let contains (value: int) (transient: t): bool =>
    transient |> Transient.get |> contains value;

  let count (transient: t): int =>
    transient |> Transient.get |> count;

  let empty (): t =>
    emptyInstance |> mutate;

  let isEmpty (transient: t): bool =>
    count transient === 0;

  let isNotEmpty (transient: t): bool =>
    count transient !== 0;

  let persist (transient: t): intSet =>
    transient |> Transient.persist;

  let removeImpl
      (owner: Transient.Owner.t)
      (value: int)
      ({ count, root } as set: intSet): intSet => {
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
      (_: intSet): intSet => emptyInstance;

  let removeAll (transient: t): t =>
    transient |> Transient.update removeAllImpl;
};

let mutate = Transient.mutate;
