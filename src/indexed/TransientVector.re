/***
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
module TransientVectorImpl = {
  type t('a) = {
    mutable left: array('a),
    mutable leftCount: int,
    mutable middle: IndexedTrie.t('a),
    mutable right: array('a),
    mutable rightCount: int
  };
  let tailCopyAndExpand = (arr: array('a)) : array('a) => {
    let arrCount = CopyOnWriteArray.count(arr);
    let retval = Array.make(IndexedTrie.width, arr[0]);
    Array.blit(arr, 0, retval, 0, min(arrCount, IndexedTrie.width));
    retval
  };
  let tailIsEmpty = (count: int) : bool => count === 0;
  let tailIsFull = (count: int) : bool => count === IndexedTrie.width;
  let tailIsNotEmpty = (count: int) : bool => count !== 0;
  let tailIsNotFull = (count: int) : bool => count !== IndexedTrie.width;
  let tailAddFirst = (value: 'a, arr: array('a)) : array('a) => {
    let arr =
      if (CopyOnWriteArray.count(arr) === 0) {
        Array.make(IndexedTrie.width, value)
      } else {
        arr
      };
    let rec loop = (index) =>
      if (index > 0) {
        arr[index] = arr[index - 1];
        loop(index - 1)
      } else {
        ()
      };
    loop(CopyOnWriteArray.lastIndexOrRaise(arr));
    arr[0] = value;
    arr
  };
  let tailRemoveFirst = (arr: array('a)) : array('a) => {
    let countArr = CopyOnWriteArray.count(arr);
    let rec loop = (index) =>
      if (index < countArr) {
        arr[index - 1] = arr[index];
        loop(index + 1)
      } else {
        arr
      };
    loop(1)
  };
  let tailUpdate = (index: int, value: 'a, arr: array('a)) : array('a) => {
    let arr =
      if (CopyOnWriteArray.count(arr) === 0) {
        Array.make(IndexedTrie.width, value)
      } else {
        arr
      };
    arr[index] = value;
    arr
  };
  let count = ({leftCount, middle, rightCount}: t('a)) : int => {
    let middleCount = IndexedTrie.count(middle);
    leftCount + middleCount + rightCount
  };
  let empty = () => {
    left: [||],
    leftCount: 0,
    middle: IndexedTrie.empty,
    right: [||],
    rightCount: 0
  };
  let addFirst =
      (
        owner: Transient.Owner.t,
        value: 'a,
        {left, leftCount, middle, rightCount} as transientVec: t('a)
      )
      : t('a) => {
    if (tailIsFull(leftCount) && tailIsNotEmpty(rightCount)) {
      transientVec.left = Array.make(IndexedTrie.width, value);
      transientVec.leftCount = 1;
      transientVec.middle =
        IndexedTrie.addFirstLeaf(IndexedTrie.updateLevelTransient, owner, left, middle)
    } else if (tailIsFull(leftCount) && tailIsEmpty(rightCount)) {
      transientVec.left = Array.make(IndexedTrie.width, value);
      transientVec.leftCount = 1;
      transientVec.right = left;
      transientVec.rightCount = leftCount
    } else {
      transientVec.left = left |> tailAddFirst(value);
      transientVec.leftCount = leftCount + 1
    };
    transientVec
  };
  let addLast =
      (
        owner: Transient.Owner.t,
        value: 'a,
        {left, leftCount, middle, right, rightCount} as transientVec: t('a)
      )
      : t('a) => {
    /* If right is empty, then middle is also empty */
    if (tailIsNotFull(leftCount) && tailIsEmpty(rightCount)) {
      transientVec.left = left |> tailUpdate(leftCount, value);
      transientVec.leftCount = leftCount + 1
    } else if (tailIsNotFull(rightCount)) {
      transientVec.right = right |> tailUpdate(rightCount, value);
      transientVec.rightCount = rightCount + 1
    } else {
      transientVec.middle =
        IndexedTrie.addLastLeaf(IndexedTrie.updateLevelTransient, owner, right, middle);
      transientVec.right = Array.make(IndexedTrie.width, value);
      transientVec.rightCount = 1
    };
    transientVec
  };
  let removeFirstOrRaise =
      (
        owner: Transient.Owner.t,
        {left, leftCount, middle, right, rightCount} as transientVec: t('a)
      )
      : t('a) => {
    if (leftCount > 1) {
      transientVec.left = tailRemoveFirst(left);
      transientVec.leftCount = leftCount - 1
    } else if (IndexedTrie.count(middle) > 0) {
      let firstLeaf = ref(IndexedTrie.Empty);
      let middle =
        IndexedTrie.removeFirstLeaf(IndexedTrie.updateLevelTransient, owner, firstLeaf, middle);
      let IndexedTrie.Leaf(leftOwner, left) = firstLeaf^;
      let leftCount = CopyOnWriteArray.count(left);
      let left =
        if (leftOwner === owner && leftCount === IndexedTrie.width) {
          left
        } else {
          tailCopyAndExpand(left)
        };
      transientVec.left = left;
      transientVec.leftCount = leftCount;
      transientVec.middle = middle
    } else if (rightCount > 0) {
      transientVec.left = right;
      transientVec.leftCount = rightCount;
      transientVec.right = Array.make(IndexedTrie.width, right[0]);
      transientVec.rightCount = 0
    } else if (leftCount === 1) {
      transientVec.leftCount = 0
    } else {
      failwith("vector is empty")
    };
    transientVec
  };
  let removeLastOrRaise =
      (owner: Transient.Owner.t, {leftCount, middle, rightCount} as transientVec: t('a))
      : t('a) => {
    if (rightCount > 1) {
      transientVec.rightCount = rightCount - 1
    } else if (IndexedTrie.count(middle) > 0) {
      let lastLeaf = ref(IndexedTrie.Empty);
      let middle =
        IndexedTrie.removeLastLeaf(IndexedTrie.updateLevelTransient, owner, lastLeaf, middle);
      let IndexedTrie.Leaf(rightOwner, right) = lastLeaf^;
      let rightCount = CopyOnWriteArray.count(right);
      let right =
        if (rightOwner === owner && rightCount === IndexedTrie.width) {
          right
        } else {
          tailCopyAndExpand(right)
        };
      transientVec.middle = middle;
      transientVec.right = right;
      transientVec.rightCount = rightCount
    } else if (rightCount === 1) {
      transientVec.rightCount = 0
    } else if (leftCount > 0) {
      transientVec.leftCount = leftCount - 1
    } else {
      failwith("vector is empty")
    };
    transientVec
  };
  let getOrRaise = (index: int, {left, leftCount, middle, right, _}: t('a)) : 'a => {
    let middleCount = IndexedTrie.count(middle);
    let rightIndex = index - middleCount - leftCount;
    if (index < leftCount) {
      left[index]
    } else if (rightIndex >= 0) {
      right[rightIndex]
    } else {
      let index = index - leftCount;
      middle |> IndexedTrie.get(index)
    }
  };
  let getOrDefault = (~default: 'a, index: int, vector: t('a)) : 'a =>
    if (index < 0 || index >= count(vector)) {
      default
    } else {
      getOrRaise(index, vector)
    };
  let get = (index: int, vector: t('a)) : option('a) => {
    let trieCount = count(vector);
    Preconditions.noneIfIndexOutOfRange(trieCount, index, Functions.flip(getOrRaise, vector))
  };
  let last = (vector: t('a)) : option('a) => get(count(vector) - 1, vector);
  let lastOrRaise = (vector: t('a)) : 'a => getOrRaise(count(vector) - 1, vector);
  let update =
      (
        owner: Transient.Owner.t,
        index: int,
        value: 'a,
        {left, leftCount, middle, right} as transientVec: t('a)
      )
      : t('a) => {
    let middleCount = IndexedTrie.count(middle);
    let rightIndex = index - middleCount - leftCount;
    if (index < leftCount) {
      transientVec.left = left |> tailUpdate(index, value)
    } else if (rightIndex >= 0) {
      transientVec.right = right |> tailUpdate(rightIndex, value)
    } else {
      let index = index - leftCount;
      let middle =
        middle
        |> IndexedTrie.update(
             IndexedTrie.updateLevelTransient,
             IndexedTrie.updateLeafTransient,
             owner,
             index,
             value
           );
      transientVec.middle = middle
    };
    transientVec
  };
  let updateWith =
      (
        owner: Transient.Owner.t,
        index: int,
        f: 'a => 'a,
        {left, leftCount, middle, right} as transientVec: t('a)
      )
      : t('a) => {
    let middleCount = IndexedTrie.count(middle);
    let rightIndex = index - middleCount - leftCount;
    if (index < leftCount) {
      transientVec.left = left |> tailUpdate(index, f(left[index]))
    } else if (rightIndex >= 0) {
      transientVec.right = right |> tailUpdate(rightIndex, f(right[rightIndex]))
    } else {
      let index = index - leftCount;
      let middle =
        middle
        |> IndexedTrie.updateWith(
             IndexedTrie.updateLevelTransient,
             IndexedTrie.updateLeafTransient,
             owner,
             index,
             f
           );
      transientVec.middle = middle
    };
    transientVec
  };
};

type t('a) = Transient.t(TransientVectorImpl.t('a));

module Owner = Transient.Owner;

let mutate = ({left, middle, right}: PersistentVector.t('a)) : t('a) =>
  Transient.create(
    {
      left:
        if (CopyOnWriteArray.count(left) > 0) {
          TransientVectorImpl.tailCopyAndExpand(left)
        } else {
          [||]
        },
      leftCount: CopyOnWriteArray.count(left),
      middle,
      right:
        if (CopyOnWriteArray.count(right) > 0) {
          TransientVectorImpl.tailCopyAndExpand(right)
        } else {
          [||]
        },
      rightCount: CopyOnWriteArray.count(right)
    }: TransientVectorImpl.t('a)
  );

let addFirst = (value: 'a, transient: t('a)) : t('a) =>
  transient |> Transient.update1(TransientVectorImpl.addFirst, value);

let addLast = (value: 'a, transient: t('a)) : t('a) =>
  transient |> Transient.update1(TransientVectorImpl.addLast, value);

let count = (transient: t('a)) : int => transient |> Transient.get |> TransientVectorImpl.count;

let empty = () => PersistentVector.empty() |> mutate;

let tailCompress = (count: int, arr: array('a)) : array('a) => {
  let arrCount = CopyOnWriteArray.count(arr);
  if (arrCount === count) {
    arr
  } else if (arrCount > 0) {
    let retval = Array.make(count, arr[0]);
    Array.blit(arr, 0, retval, 0, count);
    retval
  } else {
    [||]
  }
};

let persist = (transient: t('a)) : PersistentVector.t('a) => {
  let {left, leftCount, middle, right, rightCount}: TransientVectorImpl.t('a) =
    transient |> Transient.persist;
  {left: left |> tailCompress(leftCount), middle, right: right |> tailCompress(rightCount)}
};

let removeImpl = (_: Transient.Owner.t, _: TransientVectorImpl.t('a)) =>
  TransientVectorImpl.empty();

let removeAll = (transient: t('a)) : t('a) => transient |> Transient.update(removeImpl);

let removeFirstOrRaise = (transient: t('a)) : t('a) =>
  transient |> Transient.update(TransientVectorImpl.removeFirstOrRaise);

let removeLastOrRaise = (transient: t('a)) : t('a) =>
  transient |> Transient.update(TransientVectorImpl.removeLastOrRaise);

let get = (index: int, transient: t('a)) : option('a) =>
  transient |> Transient.get |> TransientVectorImpl.get(index);

let getOrDefault = (~default: 'v, index: int, transient: t('a)) : 'a =>
  transient |> Transient.get |> TransientVectorImpl.getOrDefault(~default, index);

let getOrRaise = (index: int, transient: t('a)) : 'a =>
  transient |> Transient.get |> TransientVectorImpl.getOrRaise(index);

let first = (transient: t('a)) : option('a) =>
  transient |> Transient.get |> TransientVectorImpl.get(0);

let firstOrRaise = (transient: t('a)) : 'a =>
  transient |> Transient.get |> TransientVectorImpl.getOrRaise(0);

let last = (transient: t('a)) : option('a) =>
  transient |> Transient.get |> TransientVectorImpl.last;

let lastOrRaise = (transient: t('a)) : 'a =>
  transient |> Transient.get |> TransientVectorImpl.lastOrRaise;

let update = (index: int, value: 'a, transient: t('a)) : t('a) =>
  transient |> Transient.update2(TransientVectorImpl.update, index, value);

let updateAllImpl =
    (
      owner: Transient.Owner.t,
      f: (int, 'a) => 'a,
      {left, leftCount, middle, right, rightCount} as transientVec: TransientVectorImpl.t('a)
    )
    : TransientVectorImpl.t('a) => {
  let index = ref(0);
  let updater = (value) => {
    let result = f(index^, value);
    index := index^ + 1;
    result
  };
  for (i in 0 to leftCount - 1) {
    left[i] = updater(left[i])
  };
  let middle =
    middle
    |> IndexedTrie.updateAll(
         IndexedTrie.updateLevelTransient,
         IndexedTrie.updateLeafTransient,
         owner,
         updater
       );
  for (i in 0 to rightCount - 1) {
    right[i] = updater(right[i])
  };
  transientVec.middle = middle;
  transientVec
};

let updateAll = (f: (int, 'a) => 'a, transient: t('a)) : t('a) =>
  transient |> Transient.update1(updateAllImpl, f);

let updateWith = (index: int, f: 'a => 'a, transient: t('a)) : t('a) =>
  transient |> Transient.update2(TransientVectorImpl.updateWith, index, f);

/* Unimplemented functions */
let insertAt = (index: int, value: 'a, transient: t('a)) : t('a) => failwith("Not Implemented");

let removeAt = (index: int, transient: t('a)) : t('a) => failwith("Not Implemented");
