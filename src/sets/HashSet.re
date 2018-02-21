/***
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
type t('a) = {
  mutable count: int,
  mutable root: BitmapTrieSet.t('a),
  comparator: Comparator.t('a),
  hash: Hash.t('a)
};

let contains = (value: 'a, {root, hash, comparator}: t('a)) : bool => {
  let keyHash = hash(value);
  root |> BitmapTrieSet.contains(comparator, 0, keyHash, value)
};

let count = ({count}: t('a)) : int => count;

let reduce =
    (~while_ as predicate: ('acc, 'a) => bool, f: ('acc, 'a) => 'acc, acc: 'acc, {root}: t('a))
    : 'acc =>
  BitmapTrieSet.reduce(~while_=predicate, f, acc, root);

let toSequence = ({root}: t('a)) : Sequence.t('a) => root |> BitmapTrieSet.toSequence;

let add = (value: 'a, {count, root, hash, comparator} as set: t('a)) : t('a) => {
  let keyHash = hash(value);
  let newRoot =
    root
    |> BitmapTrieSet.add(
         comparator,
         BitmapTrieSet.updateLevelNodePersistent,
         Transient.Owner.none,
         0,
         keyHash,
         value
       );
  if (newRoot === root) {
    set
  } else {
    {count: count + 1, root: newRoot, hash, comparator}
  }
};

let emptyWith = (~hash: Hash.t('a), ~comparator: Comparator.t('a)) : t('a) => {
  count: 0,
  root: BitmapTrieSet.Empty,
  comparator,
  hash
};

let hash = ({hash} as set: t('a)) : int =>
  set |> reduce(~while_=Functions.alwaysTrue2, (acc, next) => acc + hash(next), 0);

let remove = (value: 'a, {count, root, hash, comparator} as set: t('a)) : t('a) => {
  let keyHash = hash(value);
  let newRoot =
    root
    |> BitmapTrieSet.remove(
         comparator,
         BitmapTrieSet.updateLevelNodePersistent,
         Transient.Owner.none,
         0,
         keyHash,
         value
       );
  if (newRoot === root) {
    set
  } else {
    {count: count - 1, root: newRoot, hash, comparator}
  }
};

let removeAll = ({hash, comparator}: t('a)) : t('a) => emptyWith(~hash, ~comparator);
