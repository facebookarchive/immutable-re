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

let add (value: int) ({ count, root } as set: t): t => {
  let newRoot = root |> BitmapTrieIntSet.add
    BitmapTrieIntSet.updateLevelNodePersistent
    Transient.Owner.none
    0
    value;

  if (newRoot === root) set
  else { count: count + 1, root: newRoot };
};

let contains (value: int) ({ root }: t): bool =>
  root |> BitmapTrieIntSet.contains 0 value;

let count ({ count }: t): int => count;

let empty: t = { count: 0, root: BitmapTrieIntSet.Empty };

let isEmpty ({ count }: t): bool => count === 0;

let isNotEmpty ({ count }: t): bool => count !== 0;

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
  empty;

let toSequence ({ root }: t): (Sequence.t int) =>
  root |> BitmapTrieIntSet.toSequence;

let reduceImpl
    while_::(predicate: 'acc => int => bool)
    (f: 'acc => int => 'acc)
    (acc: 'acc)
    ({ root }: t): 'acc =>
  if (predicate === Functions.alwaysTrue2) (BitmapTrieIntSet.reduce f acc root)
  else (BitmapTrieIntSet.reduceWhile predicate f acc root);

let reduce
    while_::(predicate: 'acc => int => bool)=Functions.alwaysTrue2
    (f: 'acc => int => 'acc)
    (acc: 'acc)
    (set: t): 'acc =>
  reduceImpl while_::predicate f acc set;

let iterator: Iterable.Iterator.t 'a t = { reduce: reduceImpl };

let toIterable (set: t): (Iterable.t int) =>
  if (isEmpty set) (Iterable.empty ())
  else Iterable.Iterable set iterator;

let collectionOps: Collection.Ops.t int t = {
  count,
  toIterable,
  toSequence,
};

let toCollection (set: t): (Collection.t int) =>
  if (isEmpty set) (Collection.empty ())
  else Collection.Collection set collectionOps;

let setOps: ImmSet.Ops.t int t = {
  contains,
  count,
  toCollection,
  toIterable,
  toSequence,
};

let toSet (set: t): (ImmSet.t int) =>
  if (isEmpty set) (ImmSet.empty ())
  else ImmSet.Set set setOps;

let equals (this: t) (that: t): bool =>
  ImmSet.equals (toSet this) (toSet that);

let hash (set: t): int => set
  |> reduce (fun acc next => acc + next) 0;

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

    let newRoot = iter |> Iterable.reduce (fun acc value => {
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

  let persistentEmpty = empty;

  let empty (): t =>
    empty |> mutate;

  let isEmpty (transient: t): bool =>
    transient |> Transient.get |> isEmpty;

  let isNotEmpty (transient: t): bool =>
    transient |> Transient.get |> isNotEmpty;

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
      (_: intSet): intSet => persistentEmpty;

  let removeAll (transient: t): t =>
    transient |> Transient.update removeAllImpl;
};

let mutate = Transient.mutate;

let addAll (iter: Iterable.t int) (set: t): t =>
  set |> mutate |> Transient.addAll iter |> Transient.persist;

let from (iter: Iterable.t int): t =>
  empty |> addAll iter;

let intersect (this: t) (that: t): t =>
  /* FIXME: Improve this implementation */
  ImmSet.intersect (toSet this) (toSet that) |> from;

let subtract (this: t) (that: t): t =>
  /* FIXME: Improve this implementation */
  ImmSet.subtract (toSet this) (toSet that) |> from;

let union (this: t) (that: t): t =>
  /* FIXME: Improve this implementation */
  ImmSet.union (toSet this) (toSet that) |> from;
