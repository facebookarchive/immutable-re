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
  mutable count: int,
  mutable root: BitmapTrieIntSet.t,
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
