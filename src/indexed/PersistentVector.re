/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'a = {
  left: array 'a,
  middle: IndexedTrie.t 'a,
  right: array 'a,
};

let count ({ left, middle, right }: t 'a): int => {
  let leftCount = CopyOnWriteArray.count left;
  let middleCount = IndexedTrie.count middle;
  let rightCount = CopyOnWriteArray.count right;

  leftCount + middleCount + rightCount;
};

let getOrRaise (index: int) ({ left, middle, right }: t 'a): 'a => {
  let leftCount = CopyOnWriteArray.count left;
  let middleCount = IndexedTrie.count middle;

  let rightIndex = index - middleCount - leftCount;

  if (index < leftCount) left.(index)
  else if (rightIndex >= 0) right.(rightIndex)
  else {
    let index = index - leftCount;
    middle |> IndexedTrie.get index;
  }
};

let reduce
    while_::(predicate: 'acc => 'a => bool)
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    ({ left, middle, right }: t 'a): 'acc =>
  if (predicate === Functions.alwaysTrue2) {
    let rec trieReducer acc node => node |> IndexedTrie.reduce
      triePredicate::Functions.alwaysTrue2
      ::trieReducer
      while_::Functions.alwaysTrue2
      f
      acc;

    let acc = left |> CopyOnWriteArray.reduce while_::Functions.alwaysTrue2 f acc;
    let acc = trieReducer acc middle;
    let acc = right |> CopyOnWriteArray.reduce while_::Functions.alwaysTrue2 f acc;
    acc;
  }
  else {
    let shouldContinue = ref true;

    let predicate acc next => {
      let result = predicate acc next;
      shouldContinue := result;
      result;
    };

    let triePredicate _ _ => !shouldContinue;
    let rec trieReducer acc =>
      IndexedTrie.reduce ::triePredicate ::trieReducer while_::predicate f acc;

    let acc = left |> CopyOnWriteArray.reduce while_::predicate f acc;
    let acc = if (!shouldContinue) (trieReducer acc middle) else acc;
    if (!shouldContinue) (CopyOnWriteArray.reduce while_::predicate f acc right) else acc;
  };

let reduceReversed
    while_::(predicate: 'acc => 'a => bool)
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    ({ left, middle, right }: t 'a): 'acc =>
  if (predicate === Functions.alwaysTrue2) {
    let rec trieReducer acc node => node |> IndexedTrie.reduceReversed
      triePredicate::Functions.alwaysTrue2
      ::trieReducer
      while_::Functions.alwaysTrue2
      f
      acc;

    let acc = right |> CopyOnWriteArray.reduceReversed while_::Functions.alwaysTrue2 f acc;
    let acc = trieReducer acc middle;
    let acc = left |> CopyOnWriteArray.reduceReversed while_::Functions.alwaysTrue2 f acc;
    acc;
  }
  else {
    let shouldContinue = ref true;

    let predicate acc next => {
      let result = predicate acc next;
      shouldContinue := result;
      result;
    };

    let triePredicate _ _ => !shouldContinue;
    let rec trieReducer acc =>
      IndexedTrie.reduceReversed ::triePredicate ::trieReducer while_::predicate f acc;

    let acc = right |> CopyOnWriteArray.reduceReversed while_::predicate f acc;
    let acc = if (!shouldContinue) (trieReducer acc middle) else acc;
    if (!shouldContinue) (CopyOnWriteArray.reduceReversed while_::predicate f acc left) else acc;
  };

let toSequence ({ left, middle, right }: t 'a): (Sequence.t 'a) => Sequence.concat [
  CopyOnWriteArray.toSequence left,
  IndexedTrie.toSequence middle,
  CopyOnWriteArray.toSequence right,
];

let toSequenceReversed ({ left, middle, right }: t 'a): (Sequence.t 'a) => Sequence.concat [
  CopyOnWriteArray.toSequenceReversed right,
  IndexedTrie.toSequenceReversed middle,
  CopyOnWriteArray.toSequenceReversed left,
];

let emptyInstance: t 'a = {
  left: [||],
  middle: IndexedTrie.empty,
  right: [||],
};

let empty (): t 'a => emptyInstance;

let tailIsFull (arr: array 'a): bool =>
  (CopyOnWriteArray.count arr) === IndexedTrie.width;

let tailIsNotFull (arr: array 'a): bool =>
  (CopyOnWriteArray.count arr) !== IndexedTrie.width;

let addFirst (value: 'a) ({ left, middle, right }: t 'a): (t 'a) =>
  if ((tailIsFull left) && (CopyOnWriteArray.count right !== 0)) {
    left: [| value |],
    middle: IndexedTrie.addFirstLeaf
      IndexedTrie.updateLevelPersistent
      Transient.Owner.none
      left
      middle,
    right,
  }
  else if ((tailIsFull left) && (CopyOnWriteArray.count right === 0)) {
    left: [| value |],
    middle,
    right: left,
  }
  else {
    left: left |> CopyOnWriteArray.addFirst value,
    middle,
    right,
  };

let addLast (value: 'a) ({ left, middle, right }: t 'a): (t 'a) =>
  /* If right is empty, then middle is also empty */
  if ((tailIsNotFull left) && (CopyOnWriteArray.count right === 0)) {
    left: left |> CopyOnWriteArray.addLast value,
    middle,
    right,
  }
  else if (tailIsNotFull right) {
    left,
    middle,
    right: right |> CopyOnWriteArray.addLast value,
  }
  else {
    left,
    middle: IndexedTrie.addLastLeaf
      IndexedTrie.updateLevelPersistent
      Transient.Owner.none
      right
      middle,
    right: [| value |],
  };

let removeAll (_: t 'a): (t 'a) => emptyInstance;

let removeFirstOrRaise ({ left, middle, right }: t 'a): (t 'a) => {
  let leftCount = CopyOnWriteArray.count left;
  let middleCount = IndexedTrie.count middle;
  let rightCount = CopyOnWriteArray.count right;

  if (leftCount > 1) {
    left: CopyOnWriteArray.removeFirstOrRaise left,
    middle,
    right,
  }
  else if (middleCount > 0) {
    let firstLeaf = ref IndexedTrie.Empty;
    let middle = IndexedTrie.removeFirstLeaf
      IndexedTrie.updateLevelPersistent
      Transient.Owner.none
      firstLeaf
      middle;

    let (IndexedTrie.Leaf _ left) = !firstLeaf;
    { left, middle, right };
  }
  else if (rightCount > 0) {
    left: right,
    middle,
    right: [||],
  }
  else if (leftCount === 1) (empty ())
  else failwith "vector is empty";
};

let removeLastOrRaise ({ left, middle, right }: t 'a): (t 'a) => {
  let leftCount = CopyOnWriteArray.count left;
  let middleCount = IndexedTrie.count middle;
  let rightCount = CopyOnWriteArray.count right;

    if (rightCount > 1) {
      left,
      middle,
      right: CopyOnWriteArray.removeLastOrRaise right,
    }
    else if (middleCount > 0) {
      let lastLeaf = ref IndexedTrie.Empty;
      let middle = IndexedTrie.removeLastLeaf
          IndexedTrie.updateLevelPersistent
          Transient.Owner.none
          lastLeaf
          middle;

      let (IndexedTrie.Leaf _ right) = !lastLeaf;
      { left, middle, right };
    }
    else if (rightCount === 1) {
      left,
      middle,
      right: [||],
    }
    else if (leftCount > 0) {
      left: CopyOnWriteArray.removeLastOrRaise left,
      middle,
      right,
    }
    else failwith "vector is empty";
  };

let return (value: 'a): (t 'a) =>
  empty () |> addLast value;

let update
  (index: int)
    (value: 'a)
    ({ left, middle, right } as vector: t 'a): (t 'a) => {
  Preconditions.failIfOutOfRange (count vector) index;

  let leftCount = CopyOnWriteArray.count left;
  let middleCount = IndexedTrie.count middle;

  let rightIndex = index - middleCount - leftCount;

  if (index < leftCount) {
    left: left |>  CopyOnWriteArray.update index value,
    middle,
    right,
  }
  else if (rightIndex >= 0) {
    left,
    middle,
    right: right |> CopyOnWriteArray.update rightIndex value,
  }
  else {
    let index = (index - leftCount);
    let middle = middle |> IndexedTrie.update
      IndexedTrie.updateLevelPersistent
      IndexedTrie.updateLeafPersistent
      Transient.Owner.none
      index
      value;

    { left, middle, right }
  };
};

let updateWith
    (index: int)
    (f: 'a => 'a)
    ({ left, middle, right } as vector: t 'a): (t 'a) => {
  Preconditions.failIfOutOfRange (count vector) index;

  let leftCount = CopyOnWriteArray.count left;
  let middleCount = IndexedTrie.count middle;

  let rightIndex = index - middleCount - leftCount;

  if (index < leftCount) {
    left: left |>  CopyOnWriteArray.updateWith index f,
    middle,
    right,
  }
  else if (rightIndex >= 0) {
    left,
    middle,
    right: right |> CopyOnWriteArray.updateWith rightIndex f,
  }
  else {
    let index = (index - leftCount);
    let middle = middle |> IndexedTrie.updateWith
      IndexedTrie.updateLevelPersistent
      IndexedTrie.updateLeafPersistent
      Transient.Owner.none
      index
      f;

    { left, middle, right }
  };
};
