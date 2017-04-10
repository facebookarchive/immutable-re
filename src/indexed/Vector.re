/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Functions.Operators;
open Option.Operators;

let module VectorImpl = {
  module type VectorBase = {
    type t 'a;

    let addFirst: Transient.Owner.t => 'a => (t 'a) => (t 'a);
    let addLast: Transient.Owner.t => 'a => (t 'a) => (t 'a);
    let count: (t 'a) => int;
    let empty: unit => (t 'a);
    let getOrRaiseUnsafe: int => (t 'a) => 'a;
    let removeFirstOrRaise: Transient.Owner.t => (t 'a) => (t 'a);
    let removeLastOrRaise: Transient.Owner.t => (t 'a) => (t 'a);
    let updateUnsafe: Transient.Owner.t => int => 'a => (t 'a) => (t 'a);
    let updateWithUnsafe: Transient.Owner.t => int => ('a => 'a) => (t 'a) => (t 'a);
  };

  module type S = {
    type t 'a;

    let addFirst: Transient.Owner.t => 'a => (t 'a) => (t 'a);
    let addFirstAll: Transient.Owner.t => (Sequence.t 'a) => (t 'a) => (t 'a);
    let addLast: Transient.Owner.t => 'a => (t 'a) => (t 'a);
    let addLastAll: Transient.Owner.t => (Sequence.t 'a) => (t 'a) => (t 'a);
    let count: (t 'a) => int;
    let empty: unit => (t 'a);
    let first: (t 'a) => (option 'a);
    let firstOrRaise: (t 'a) => 'a;
    let get: int => (t 'a) => (option 'a);
    let getOrRaise: int => (t 'a) => 'a;
    let isEmpty: (t 'a) => bool;
    let isNotEmpty: (t 'a) => bool;
    let last: (t 'a) => (option 'a);
    let lastOrRaise: (t 'a) => 'a;
    let removeAll: (t 'a) => (t 'a);
    let removeFirstOrRaise: Transient.Owner.t => (t 'a) => (t 'a);
    let removeLastOrRaise: Transient.Owner.t => (t 'a) => (t 'a);
    let update: Transient.Owner.t => int => 'a => (t 'a) => (t 'a);
    let updateWith: Transient.Owner.t => int => ('a => 'a) => (t 'a) => (t 'a);
  };

  let module Make = fun (X: VectorBase) => {
    type t 'a = X.t 'a;

    let addFirstAll (owner: Transient.Owner.t) (iter: Iterable.t 'a) (vector: t 'a): (t 'a) => iter
      |> Iterable.reduce
        (fun acc next => acc |> X.addFirst owner next)
        vector;

    let addLastAll (owner: Transient.Owner.t) (iter: Iterable.t 'a) (vector: t 'a): (t 'a) => iter
      |> Iterable.reduce
        (fun acc next => acc |> X.addLast owner next)
        vector;

    let addFirst = X.addFirst;
    let addLast = X.addLast;
    let count = X.count;

    let get (index: int) (vector: t 'a): (option 'a) => {
      let trieCount = count vector;
      Preconditions.noneIfIndexOutOfRange
        trieCount
        index
        (Functions.flip X.getOrRaiseUnsafe vector);
    };

    let getOrRaise (index: int) (vector: t 'a): 'a => {
      Preconditions.failIfOutOfRange (X.count vector) index;
      X.getOrRaiseUnsafe index vector;
    };

    let empty (): (t 'a) => X.empty ();

    let first (vector: t 'a): (option 'a) => get 0 vector;

    let firstOrRaise (vector: t 'a): 'a =>
      getOrRaise 0 vector;

    let isEmpty (vector: t 'a): bool =>
      (X.count vector) === 0;

    let isNotEmpty (vector: t 'a): bool =>
      (X.count vector) !== 0;

    let last (vector: t 'a): (option 'a) => get ((X.count vector) - 1) vector;

    let lastOrRaise (vector: t 'a): 'a =>
      getOrRaise ((X.count vector) - 1) vector;

    let removeAll (_: t 'a): (t 'a) => X.empty ();

    let removeFirstOrRaise = X.removeFirstOrRaise;

    let removeLastOrRaise = X.removeLastOrRaise;

    let update (owner: Transient.Owner.t) (index: int) (value: 'a) (vector: t 'a): (t 'a) => {
      Preconditions.failIfOutOfRange (X.count vector) index;
      X.updateUnsafe owner index value vector;
    };

    let updateWith (owner: Transient.Owner.t) (index: int) (f: 'a => 'a) (vector: t 'a): (t 'a) => {
      Preconditions.failIfOutOfRange (X.count vector) index;
      X.updateWithUnsafe owner index f vector;
    };
  };
};

type t 'a = {
  left: array 'a,
  middle: IndexedTrie.t 'a,
  right: array 'a,
};

let empty () => {
  left: [||],
  middle: IndexedTrie.empty,
  right: [||],
};

let module PersistentVector = VectorImpl.Make {
  type nonrec t 'a = t 'a;

  let tailIsFull (arr: array 'a): bool => (CopyOnWriteArray.count arr) === IndexedTrie.width;
  let tailIsNotFull (arr: array 'a): bool => (CopyOnWriteArray.count arr) !== IndexedTrie.width;

  let count ({ left, middle, right }: t 'a): int => {
    let leftCount = CopyOnWriteArray.count left;
    let middleCount = IndexedTrie.count middle;
    let rightCount = CopyOnWriteArray.count right;

    leftCount + middleCount + rightCount;
  };

  let empty () => empty ();

  let addFirst (_: Transient.Owner.t) (value: 'a) ({ left, middle, right }: t 'a): (t 'a) =>
    if ((tailIsFull left) && (CopyOnWriteArray.isNotEmpty right)) {
      left: [| value |],
      middle: IndexedTrie.addFirstLeafUsingMutator IndexedTrie.updateLevelPersistent Transient.Owner.none left middle,
      right,
    }
    else if ((tailIsFull left) && (CopyOnWriteArray.isEmpty right)) {
      left: [| value |],
      middle,
      right: left,
    }
    else {
      left: left |> CopyOnWriteArray.addFirst value,
      middle,
      right,
    };

  let addLast (_: Transient.Owner.t) (value: 'a) ({ left, middle, right }: t 'a): (t 'a) =>
    /* If right is empty, then middle is also empty */
    if ((tailIsNotFull left) && (CopyOnWriteArray.isEmpty right)) {
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
      middle: IndexedTrie.addLastLeafUsingMutator IndexedTrie.updateLevelPersistent Transient.Owner.none right middle,
      right: [| value |],
    };

  let removeFirstOrRaise (_: Transient.Owner.t) ({ left, middle, right }: t 'a): (t 'a) => {
    let leftCount = CopyOnWriteArray.count left;
    let middleCount = IndexedTrie.count middle;
    let rightCount = CopyOnWriteArray.count right;

    if (leftCount > 1) {
      left: CopyOnWriteArray.removeFirstOrRaise left,
      middle,
      right,
    }
    else if (middleCount > 0) {
      let (IndexedTrie.Leaf _ left, middle) =
        IndexedTrie.removeFirstLeafUsingMutator IndexedTrie.updateLevelPersistent Transient.Owner.none middle;
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

  let removeLastOrRaise (_: Transient.Owner.t) ({ left, middle, right }: t 'a): (t 'a) => {
    let leftCount = CopyOnWriteArray.count left;
    let middleCount = IndexedTrie.count middle;
    let rightCount = CopyOnWriteArray.count right;

    if (rightCount > 1) {
      left,
      middle,
      right: CopyOnWriteArray.removeLastOrRaise right,
    }
    else if (middleCount > 0) {
      let (middle, IndexedTrie.Leaf _ right) =
        IndexedTrie.removeLastLeafUsingMutator IndexedTrie.updateLevelPersistent Transient.Owner.none middle;
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

  let getOrRaiseUnsafe (index: int) ({ left, middle, right }: t 'a): 'a => {
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

  let updateUnsafe
      (_: Transient.Owner.t)
      (index: int)
      (value: 'a)
      ({ left, middle, right }: t 'a): (t 'a) => {
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
      let middle = middle |> IndexedTrie.updateUsingMutator
        IndexedTrie.updateLevelPersistent
        IndexedTrie.updateLeafPersistent
        Transient.Owner.none
        index
        value;

      { left, middle, right }
    };
  };

  let updateWithUnsafe
      (_: Transient.Owner.t)
      (index: int)
      (f: 'a => 'a)
      ({ left, middle, right }: t 'a): (t 'a) => {
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
      let middle = middle |> IndexedTrie.updateWithUsingMutator
        IndexedTrie.updateLevelPersistent
        IndexedTrie.updateLeafPersistent
        Transient.Owner.none
        index
        f;

      { left, middle, right }
    };
  };
};

type transientVectorImpl 'a = {
  mutable left: array 'a,
  mutable leftCount: int,
  mutable middle: IndexedTrie.t 'a,
  mutable right: array 'a,
  mutable rightCount: int,
};

let tailCopyAndExpand (arr: array 'a): (array 'a) => {
  let arrCount = CopyOnWriteArray.count arr;
  let retval = Array.make IndexedTrie.width arr.(0);
  Array.blit arr 0 retval 0 (min arrCount IndexedTrie.width);
  retval;
};

let module TransientVectorImpl = VectorImpl.Make {
  type t 'a = transientVectorImpl 'a;

  let tailIsEmpty (count: int): bool => count === 0;
  let tailIsFull (count: int): bool => count === IndexedTrie.width;
  let tailIsNotEmpty (count: int): bool => count !== 0;
  let tailIsNotFull (count: int): bool => count !== IndexedTrie.width;

  let tailAddFirst (value: 'a) (arr: array 'a): (array 'a) => {
    let arr =
      if (CopyOnWriteArray.isEmpty arr) (Array.make IndexedTrie.width value)
      else arr;

    let rec loop index =>
      if (index > 0) {
        arr.(index) = arr.(index - 1);
        loop (index - 1);
      }
      else ();

    loop (CopyOnWriteArray.lastIndexOrRaise arr);
    arr.(0) = value;
    arr;
  };

  let tailRemoveFirst (arr: array 'a): (array 'a) => {
    let countArr = CopyOnWriteArray.count arr;
    let rec loop index =>
      if (index < countArr) {
        arr.(index - 1) = arr.(index);
        loop (index + 1);
      }
      else arr;

    loop 1;
  };

  let tailUpdate (index: int) (value: 'a) (arr: array 'a): (array 'a) => {
    let arr =
      if (CopyOnWriteArray.isEmpty arr) (Array.make IndexedTrie.width value)
      else arr;

    arr.(index) = value;
    arr;
  };

  let count ({ leftCount, middle, rightCount }: t 'a): int => {
    let middleCount = IndexedTrie.count middle;
    leftCount + middleCount + rightCount;
  };

  let empty () => {
    left: [||],
    leftCount: 0,
    middle: IndexedTrie.empty,
    right: [||],
    rightCount: 0,
  };

  let addFirst
      (owner: Transient.Owner.t)
      (value: 'a)
      ({
        left,
        leftCount,
        middle,
        rightCount,
      } as transientVec: t 'a): (t 'a) => {
    if ((tailIsFull leftCount) && (tailIsNotEmpty rightCount)) {
      transientVec.left = Array.make IndexedTrie.width value;
      transientVec.leftCount = 1;
      transientVec.middle = IndexedTrie.addFirstLeafUsingMutator
        IndexedTrie.updateLevelTransient
        owner
        left
        middle;
    }
    else if ((tailIsFull leftCount) && (tailIsEmpty rightCount)) {
      transientVec.left = Array.make IndexedTrie.width value;
      transientVec.leftCount = 1;
      transientVec.right = left;
      transientVec.rightCount = leftCount;
    }
    else {
      transientVec.left = left |> tailAddFirst value;
      transientVec.leftCount = leftCount + 1;
    };

    transientVec
  };

  let addLast
      (owner: Transient.Owner.t)
      (value: 'a)
      ({
        left,
        leftCount,
        middle,
        right,
        rightCount,
      } as transientVec: t 'a): (t 'a) => {
    /* If right is empty, then middle is also empty */
    if ((tailIsNotFull leftCount) && (tailIsEmpty rightCount)) {
      transientVec.left = left |> tailUpdate leftCount value;
      transientVec.leftCount = leftCount + 1;
    }
    else if (tailIsNotFull rightCount) {
      transientVec.right = right |> tailUpdate rightCount value;
      transientVec.rightCount = rightCount + 1;
    }
    else {
      transientVec.middle = IndexedTrie.addLastLeafUsingMutator
        IndexedTrie.updateLevelTransient
        owner
        right
        middle;
      transientVec.right = Array.make IndexedTrie.width value;
      transientVec.rightCount = 1;
    };

    transientVec
  };

  let removeFirstOrRaise
      (owner: Transient.Owner.t)
      ({
        left,
        leftCount,
        middle,
        right,
        rightCount,
      } as transientVec: t 'a): (t 'a) => {
    if (leftCount > 1) {
      transientVec.left = tailRemoveFirst left;
      transientVec.leftCount = leftCount - 1;
    }
    else if ((IndexedTrie.count middle) > 0) {
      let (IndexedTrie.Leaf leftOwner left, middle) = middle
        |> IndexedTrie.removeFirstLeafUsingMutator IndexedTrie.updateLevelTransient owner;
      let leftCount = CopyOnWriteArray.count left;

      let left =
        if (leftOwner === owner && leftCount === IndexedTrie.width) left
        else tailCopyAndExpand left;

      transientVec.left = left;
      transientVec.leftCount = leftCount;
      transientVec.middle = middle;
    }
    else if (rightCount > 0) {
      transientVec.left = right;
      transientVec.leftCount = rightCount;
      transientVec.right = Array.make IndexedTrie.width right.(0);
      transientVec.rightCount = 0;
    }
    else if (leftCount === 1) {
      transientVec.leftCount = 0;
    }
    else failwith "vector is empty";

    transientVec
  };

  let removeLastOrRaise
      (owner: Transient.Owner.t)
      ({
        leftCount,
        middle,
        rightCount,
      } as transientVec: t 'a): (t 'a) => {
    if (rightCount > 1) {
      transientVec.rightCount = rightCount - 1;
    }
    else if ((IndexedTrie.count middle) > 0) {
      let (middle, IndexedTrie.Leaf rightOwner right) = middle
        |> IndexedTrie.removeLastLeafUsingMutator IndexedTrie.updateLevelTransient owner;
      let rightCount = CopyOnWriteArray.count right;

      let right =
        if (rightOwner === owner && rightCount === IndexedTrie.width) right
        else tailCopyAndExpand right;

      transientVec.middle = middle;
      transientVec.right = right;
      transientVec.rightCount = rightCount;
    }
    else if (rightCount === 1) {
      transientVec.rightCount = 0;
    }
    else if (leftCount > 0) {
      transientVec.leftCount = leftCount - 1;
    }
    else failwith "vector is empty";

    transientVec
  };

  let getOrRaiseUnsafe
      (index: int)
      ({
        left,
        leftCount,
        middle,
        right,
        _,
      }: t 'a): 'a => {
    let middleCount = IndexedTrie.count middle;
    let rightIndex = index - middleCount - leftCount;

    if (index < leftCount) left.(index)
    else if (rightIndex >= 0) right.(rightIndex)
    else {
      let index = index - leftCount;
      middle |> IndexedTrie.get index;
    }
  };

  let updateUnsafe
      (owner: Transient.Owner.t)
      (index: int)
      (value: 'a)
      ({
        left,
        leftCount,
        middle,
        right,
      } as transientVec: t 'a): (t 'a) => {
    let middleCount = IndexedTrie.count middle;
    let rightIndex = index - middleCount - leftCount;

    if (index < leftCount) {
      transientVec.left = left |> tailUpdate index value;
    }
    else if (rightIndex >= 0) {
      transientVec.right = right |> tailUpdate rightIndex value;
    }
    else {
      let index = (index - leftCount);
      let middle = middle |> IndexedTrie.updateUsingMutator
        IndexedTrie.updateLevelTransient
        IndexedTrie.updateLeafTransient
        owner
        index
        value;

      transientVec.middle = middle;
    };

    transientVec
  };

  let updateWithUnsafe
      (owner: Transient.Owner.t)
      (index: int)
      (f: 'a => 'a)
      ({
        left,
        leftCount,
        middle,
        right,
      } as transientVec: t 'a): (t 'a) => {
    let middleCount = IndexedTrie.count middle;
    let rightIndex = index - middleCount - leftCount;

    if (index < leftCount) {
      transientVec.left = left |> tailUpdate index (f left.(index));
    }
    else if (rightIndex >= 0) {
      transientVec.right = right |> tailUpdate rightIndex (f right.(rightIndex));
    }
    else {
      let index = (index - leftCount);
      let middle = middle |> IndexedTrie.updateWithUsingMutator
        IndexedTrie.updateLevelTransient
        IndexedTrie.updateLeafTransient
        owner
        index
        f;

      transientVec.middle = middle;
    };

    transientVec
  };
};

let addFirst value => PersistentVector.addFirst Transient.Owner.none value;
let addLast value => PersistentVector.addLast Transient.Owner.none value;
let count = PersistentVector.count;
let first = PersistentVector.first;
let firstOrRaise = PersistentVector.firstOrRaise;
let get = PersistentVector.get;
let getOrRaise = PersistentVector.getOrRaise;
let isEmpty = PersistentVector.isEmpty;
let isNotEmpty = PersistentVector.isNotEmpty;
let last = PersistentVector.last;
let lastOrRaise = PersistentVector.lastOrRaise;
let removeAll = PersistentVector.removeAll;
let removeFirstOrRaise vector => PersistentVector.removeFirstOrRaise Transient.Owner.none vector;
let removeLastOrRaise vector => PersistentVector.removeLastOrRaise Transient.Owner.none vector;
let update index => PersistentVector.update Transient.Owner.none index;
let updateWith index => PersistentVector.updateWith Transient.Owner.none index;

module Transient = {
  type vector 'a = t 'a;
  type t 'a = Transient.t (TransientVectorImpl.t 'a);

  let module Owner = Transient.Owner;

  let mutate ({ left, middle, right }: vector 'a): (t 'a) => Transient.create {
    left: if (CopyOnWriteArray.count left > 0) (tailCopyAndExpand left) else [||],
    leftCount: CopyOnWriteArray.count left,
    middle,
    right: if (CopyOnWriteArray.count right > 0) (tailCopyAndExpand right) else [||],
    rightCount: CopyOnWriteArray.count right,
  };

  let addFirst (value: 'a) (transient: t 'a): (t 'a) =>
    transient |> Transient.update1 TransientVectorImpl.addFirst value;

  let addFirstAll (iter: Iterable.t 'a) (transient: t 'a): (t 'a) =>
    transient |> Transient.update1 TransientVectorImpl.addFirstAll iter;

  let addLast (value: 'a) (transient: t 'a): (t 'a) =>
    transient |> Transient.update1 TransientVectorImpl.addLast value;

  let addLastAll (iter: Iterable.t 'a) (transient: t 'a): (t 'a) =>
    transient |> Transient.update1 TransientVectorImpl.addLastAll iter;

  let count (transient: t 'a): int =>
    transient |> Transient.get |> TransientVectorImpl.count;

  let empty () => empty () |> mutate;

  let isEmpty (transient: t 'a): bool =>
    transient |> Transient.get |> TransientVectorImpl.isEmpty;

  let isNotEmpty (transient: t 'a): bool =>
    transient |> Transient.get |> TransientVectorImpl.isNotEmpty;

  let tailCompress (count: int) (arr: array 'a): (array 'a) => {
    let arrCount = CopyOnWriteArray.count arr;

    if (arrCount === count) arr
    else if (arrCount > 0) {
      let retval = Array.make count arr.(0);
      Array.blit arr 0 retval 0 count;
      retval;
    }
    else [||];
  };

  let persist (transient: t 'a): (vector 'a) => {
    let {
      left,
      leftCount,
      middle,
      right,
      rightCount,
    } = transient |> Transient.persist;

    {
      left: left |> tailCompress leftCount,
      middle,
      right: right |> tailCompress rightCount,
    }
  };

  let removeImpl
      (_: Transient.Owner.t)
      (vec: transientVectorImpl 'a) =>
    TransientVectorImpl.removeAll vec;

  let removeAll (transient: t 'a): (t 'a) =>
      transient |> Transient.update removeImpl;

  let removeFirstOrRaise (transient: t 'a): (t 'a) =>
    transient |> Transient.update TransientVectorImpl.removeFirstOrRaise;

  let removeLastOrRaise (transient: t 'a): (t 'a) =>
    transient |> Transient.update TransientVectorImpl.removeLastOrRaise;

  let get (index: int) (transient: t 'a): (option 'a) =>
    transient |> Transient.get |> TransientVectorImpl.get index;

  let getOrRaise (index: int) (transient: t 'a): 'a =>
    transient |> Transient.get |> TransientVectorImpl.getOrRaise index;

  let first (transient: t 'a): (option 'a) =>
    transient |> Transient.get |> TransientVectorImpl.first;

  let firstOrRaise (transient: t 'a): 'a =>
    transient |> Transient.get |> TransientVectorImpl.firstOrRaise;

  let last (transient: t 'a): (option 'a) =>
    transient |> Transient.get |> TransientVectorImpl.last;

  let lastOrRaise (transient: t 'a): 'a =>
    transient |> Transient.get |> TransientVectorImpl.lastOrRaise;

  let update (index: int) (value: 'a) (transient: t 'a): (t 'a) =>
    transient |> Transient.update2 TransientVectorImpl.update index value;

  let updateAllImpl
      (owner: Transient.Owner.t)
      (f: int => 'a => 'a)
      ({
        left,
        leftCount,
        middle,
        right,
        rightCount
      } as transientVec: transientVectorImpl 'a): (transientVectorImpl 'a) => {
    let index = ref 0;
    let updater value => {
      let result = f !index value;
      index := !index + 1;
      result;
    };

    for i in 0 to (leftCount - 1) { left.(i) = updater left.(i) };

    let middle = middle |> IndexedTrie.updateAllUsingMutator
      IndexedTrie.updateLevelTransient
      IndexedTrie.updateLeafTransient
      owner
      updater;

    for i in 0 to (rightCount - 1) { right.(i) = updater right.(i) };

    transientVec.middle = middle;
    transientVec
  };

  let updateAll (f: int => 'a => 'a) (transient: t 'a): (t 'a) =>
    transient |> Transient.update1 updateAllImpl f;

  let updateWith (index: int) (f: 'a => 'a) (transient: t 'a): (t 'a) =>
    transient |> Transient.update2 TransientVectorImpl.updateWith index f;

  /* Unimplemented functions */
  let insertAt (index: int) (value: 'a) (transient: t 'a): (t 'a) =>
    failwith "Not Implemented";

  let removeAt (index: int) (transient: t 'a): (t 'a) =>
    failwith "Not Implemented";
};

let mutate = Transient.mutate;

let addFirstAll (iter: Iterable.t 'a) (vec: t 'a): (t 'a) => vec
  |> mutate
  |> Transient.addFirstAll iter
  |> Transient.persist;

let addLastAll (iter: Iterable.t 'a) (vec: t 'a): (t 'a) => vec
  |> mutate
  |> Transient.addLastAll iter
  |> Transient.persist;

let from (iter: Iterable.t 'a): (t 'a) =>
  empty () |> addLastAll iter;

let fromReverse (iter: Iterable.t 'a): (t 'a) =>
  empty () |> addFirstAll iter;

let init (count: int) (f: int => 'a): (t 'a) => IntRange.create start::0 count::count
  |> IntRange.reduce (fun acc next =>
      acc |> Transient.addLast (f next)) (mutate (empty ())
    )
  |> Transient.persist;

let reduceImpl (f: 'acc => 'a => 'acc) (acc: 'acc) ({ left, middle, right }: t 'a): 'acc => {
  let acc = left |> CopyOnWriteArray.reduce f acc;
  let acc = middle |> IndexedTrie.reduce f acc;
  let acc = right |> CopyOnWriteArray.reduce f acc;
  acc;
};

let reduceWhile
    while_::(predicate: 'acc => 'a => bool)
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    ({ left, middle, right }: t 'a): 'acc => {
  let shouldContinue = ref true;
  let predicate acc next => {
    let result = predicate acc next;
    shouldContinue := result;
    result;
  };

  let triePredicate _ _ => !shouldContinue;
  let rec trieReducer acc =>
    IndexedTrie.reduceWhileWithResult triePredicate trieReducer predicate f acc;

  let acc = left |> CopyOnWriteArray.reduce while_::predicate f acc;

  let acc =
    if (!shouldContinue) (IndexedTrie.reduceWhileWithResult
      triePredicate
      trieReducer
      predicate
      f
      acc
      middle
    )
    else acc;

  let acc =
    if (!shouldContinue) (CopyOnWriteArray.reduce while_::predicate f acc right)
    else acc;

  acc;
};

let reduce
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (vec: t 'a): 'acc =>
  if (predicate === Functions.alwaysTrue2) (reduceImpl f acc vec)
  else (reduceWhile while_::predicate f acc vec);

let reduceWithIndexImpl (f: 'acc => int => 'a => 'acc) (acc: 'acc) (vec: t 'a): 'acc => {
  /* kind of a hack, but a lot less code to write */
  let index = ref 0;
  let reducer acc next => {
    let acc = f acc !index next;
    index := !index + 1;
    acc
  };

  reduce reducer acc vec;
};

let reduceWithIndexWhile
    while_::(predicate: 'acc => int => 'a => bool)
    (f: 'acc => int => 'a => 'acc)
    (acc: 'acc)
    (vec: t 'a): 'acc => {
  /* kind of a hack, but a lot less code to write */
  let index = ref 0;
  let reducer acc next => {
    let acc = f acc !index next;
    index := !index + 1;
    acc
  };

  let predicate acc next => predicate acc !index next;

  reduceWhile while_::predicate reducer acc vec;
};

let reduceWithIndex
    while_::(predicate: 'acc => int => 'a => bool)=Functions.alwaysTrue3
    (f: 'acc => int => 'a => 'acc)
    (acc: 'acc)
    (vec: t 'a): 'acc =>
  if (predicate === Functions.alwaysTrue3) (reduceWithIndexImpl f acc vec)
  else (reduceWithIndexWhile while_::predicate f acc vec);

let reduceReversedImpl (f: 'acc => 'a => 'acc) (acc: 'acc) ({ left, middle, right }: t 'a): 'acc => {
  let acc = right |> CopyOnWriteArray.reduceReversed f acc;
  let acc = middle |> IndexedTrie.reduceReversed f acc;
  let acc = left |> CopyOnWriteArray.reduceReversed f acc;
  acc;
};

let reduceReversedWhile
    while_::(predicate: 'acc => 'a => bool)
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    ({ left, middle, right }: t 'a): 'acc => {
  let shouldContinue = ref true;
  let predicate acc next => {
    let result = predicate acc next;
    shouldContinue := result;
    result;
  };

  let triePredicate _ _ => !shouldContinue;
  let rec trieReducer acc =>
    IndexedTrie.reduceReversedWhileWithResult triePredicate trieReducer predicate f acc;

  let acc = right |> CopyOnWriteArray.reduceReversed while_::predicate f acc;

  let acc =
    if (!shouldContinue) (IndexedTrie.reduceReversedWhileWithResult
      triePredicate
      trieReducer
      predicate
      f
      acc
      middle
    )
    else acc;

  let acc =
    if (!shouldContinue) (CopyOnWriteArray.reduceReversed while_::predicate f acc left)
    else acc;

  acc;
};

let reduceReversed
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (vec: t 'a): 'acc =>
  if (predicate === Functions.alwaysTrue2) (reduceReversedImpl f acc vec)
  else (reduceReversedWhile while_::predicate f acc vec);

let reduceReversedWithIndexImpl (f: 'acc => int => 'a => 'acc) (acc: 'acc) (vec: t 'a): 'acc => {
  /* kind of a hack, but a lot less code to write */
  let index = ref (count vec - 1);
  let reducer acc next => {
    let acc = f acc !index next;
    index := !index - 1;
    acc
  };

  reduceReversed reducer acc vec;
};

let reduceReversedWithIndexWhile
    while_::(predicate: 'acc => int => 'a => bool)
    (f: 'acc => int => 'a => 'acc)
    (acc: 'acc)
    (vec: t 'a): 'acc => {
  /* kind of a hack, but a lot less code to write */
  let index = ref (count vec - 1);
  let reducer acc next => {
    let acc = f acc !index next;
    index := !index - 1;
    acc
  };

  let predicate acc next =>
    predicate acc !index next;

  reduceReversedWhile while_::predicate reducer acc vec;
};

let reduceReversedWithIndex
    while_::(predicate: 'acc => int => 'a => bool)=Functions.alwaysTrue3
    (f: 'acc => int => 'a => 'acc)
    (acc: 'acc)
    (vec: t 'a): 'acc =>
  if (predicate === Functions.alwaysTrue3) (reduceReversedWithIndexImpl f acc vec)
  else (reduceReversedWithIndexWhile while_::predicate f acc vec);

let return (value: 'a): (t 'a) =>
  empty () |> addLast value;

let skip (skipCount: int) ({ left, middle, right } as vec: t 'a): (t 'a) => {
  let vectorCount = count vec;
  let leftCount = CopyOnWriteArray.count left;
  let middleCount = IndexedTrie.count middle;

  if (skipCount >= vectorCount) (empty ())
  else if (skipCount <= 0) vec
  else if (skipCount < leftCount) {
    left: left |> CopyOnWriteArray.skip skipCount,
    middle,
    right,
  }
  else if (skipCount === leftCount) {
    let (IndexedTrie.Leaf _ left, middle) = IndexedTrie.removeFirstLeafUsingMutator
      IndexedTrie.updateLevelPersistent
      Transient.Owner.none
      middle;

    { left, middle, right }
  }
  else if (skipCount - leftCount < middleCount) {
    let skipCount = skipCount - leftCount;
    let (left, middle) = IndexedTrie.skip Transient.Owner.none skipCount middle;
    { left, middle, right }
  }
  else {
    let skipCount = skipCount - leftCount - middleCount;
    {
      left:  right |> CopyOnWriteArray.skip skipCount,
      middle: IndexedTrie.empty,
      right: [||],
    }
  }
};

let take (takeCount: int) ({ left, middle, right } as vec: t 'a): (t 'a) => {
  let vectorCount = count vec;
  let leftCount = CopyOnWriteArray.count left;
  let middleCount = IndexedTrie.count middle;

  if (takeCount >= vectorCount) vec
  else if (takeCount <= leftCount) {
    left: left |> CopyOnWriteArray.take takeCount,
    middle: IndexedTrie.empty,
    right: [||],
  }
  else if (takeCount - leftCount < middleCount) {
    let takeCount = takeCount - leftCount;
    let (middle, right) = IndexedTrie.take Transient.Owner.none takeCount middle;
    { left, middle, right }
  }
  else if (takeCount - leftCount === middleCount) {
    let (middle, IndexedTrie.Leaf _ right) = IndexedTrie.removeLastLeafUsingMutator
      IndexedTrie.updateLevelPersistent
      Transient.Owner.none
      middle;

    { left, middle, right }
  }
  else {
    let takeCount = takeCount - leftCount - middleCount;
    { left, middle, right: right |> CopyOnWriteArray.take takeCount }
  }
};

let slice start::(start: int)=0 end_::(end_: option int)=? (vec: t 'a): (t 'a) => {
  let vecCount = count vec;

  let end_ = switch end_ {
    | Some end_ => end_
    | None => vecCount
  };

  let start = if (start < 0) (start + vecCount) else start;
  let end_ = if (end_ < 0) (end_ + vecCount) else end_;

  let skipCount = start;
  let takeCount = max (end_ - start) 0;

  if (skipCount === 0 && takeCount === vecCount) vec
  else if (takeCount === 0) (empty ())
  else vec |> skip skipCount |> take takeCount;
};

let iterator: Iterable.Iterator.t 'a (t 'a) = { reduce: reduceWhile };

let toIterable (vec: t 'a): (Iterable.t 'a) =>
  if (isEmpty vec) (Iterable.empty ())
  else Iterable.Iterable vec iterator;

let iteratorReversed: Iterable.Iterator.t 'a (t 'a) = { reduce: reduceReversedWhile };

let toIterableReversed (vec: t 'a): (Iterable.t 'a) =>
  if (isEmpty vec) (Iterable.empty ())
  else Iterable.Iterable vec iteratorReversed;

let keyedIterator: KeyedIterable.KeyedIterator.t int 'a (t 'a) = { reduce: reduceWithIndexWhile };

let toKeyedIterable (vec: t 'a): (KeyedIterable.t int 'a) =>
  if (isEmpty vec) (KeyedIterable.empty ())
  else KeyedIterable.KeyedIterable vec keyedIterator;

let keyedIteratorReversed: KeyedIterable.KeyedIterator.t int 'a (t 'a) = { reduce: reduceReversedWithIndexWhile };

let toKeyedIterableReversed (vec: t 'a): (KeyedIterable.t int 'a) =>
  if (isEmpty vec) (KeyedIterable.empty ())
  else KeyedIterable.KeyedIterable vec keyedIterator;
  let containsKey (index: int) (arr: t 'a): bool =>
    index >= 0 && index < count arr;

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

let toSequenceWithIndex (vec: t 'a): (Sequence.t (int, 'a)) => Sequence.zip2With
  (fun a b => (a, b))
  (IntRange.create start::0 count::(count vec) |> IntRange.toSequence)
  (toSequence vec);

let toSequenceWithIndexReversed (vec: t 'a): (Sequence.t (int, 'a)) => Sequence.zip2With
  (fun a b => (a, b))
  (IntRange.create start::0 count::(count vec) |> IntRange.toSequenceReversed)
  (toSequenceReversed vec);

let collectionOps: Collection.Ops.t 'a (t 'a) = {
  count,
  toIterable,
  toSequence,
};

let toCollection (vec: t 'a): (Collection.t 'a) =>
  if (isEmpty vec) (Collection.empty ())
  else Collection.Collection vec collectionOps;

let seqCollectionOps: SequentialCollection.Ops.t 'a (t 'a) = {
  count,
  first,
  firstOrRaise,
  toCollection,
  toIterable,
  toSequence,
};

let toSequentialCollection (vector: t 'a): (SequentialCollection.t 'a) =>
  if (isEmpty vector) (SequentialCollection.empty ())
  else SequentialCollection.SequentialCollection vector seqCollectionOps;

let keys (arr: t 'a): (Iterable.t int) =>
  IntRange.create start::0 count::(count arr) |> IntRange.toIterable;

let keysReversed (arr: t 'a): (Iterable.t int) =>
  IntRange.create start::0 count::(count arr) |> IntRange.toIterableReversed;

let keySet (arr: t 'a): (ImmSet.t int) =>
  IntRange.create start::0 count::(count arr) |> IntRange.toSet;

let navigableKeySet (arr: t 'a): (NavigableSet.t int) =>
  IntRange.create start::0 count::(count arr) |> IntRange.toNavigableSet;

let keyedCollectionOps (): KeyedCollection.Ops.t int 'a (t 'a) => {
  containsKey,
  count,
  keys,
  toIterable: toKeyedIterable >> KeyedIterable.toIterable,
  toKeyedIterable,
  toSequence: toSequenceWithIndex,
  values: toIterable,
};

let toKeyedCollection (arr: t 'a): (KeyedCollection.t int 'a) =>
  if (isEmpty arr) (KeyedCollection.empty ())
  else KeyedCollection.KeyedCollection arr (keyedCollectionOps ());

let mapOps (): ImmMap.Ops.t int 'a (t 'a) => {
  containsKey: fun index arr => index >= 0 && index < count arr,
  count,
  get,
  getOrRaise,
  keys,
  keySet,
  toIterable: toKeyedIterable >> KeyedIterable.toIterable,
  toKeyedCollection,
  toKeyedIterable,
  toSequence: toSequenceWithIndex,
  values: toIterable,
};

let toMap (vec: t 'a): (ImmMap.t int 'a) =>
  if (isEmpty vec) (ImmMap.empty ())
  else ImmMap.Map vec (mapOps ());

let navCollectionOps: NavigableCollection.Ops.t 'a (t 'a) = {
  count,
  first,
  firstOrRaise,
  last,
  lastOrRaise,
  toCollection,
  toSequentialCollection,
  toIterable,
  toIterableReversed,
  toSequence,
  toSequenceReversed,
};

let toNavigableCollection (set: (t 'a)): (NavigableCollection.t 'a) =>
  if (isEmpty set) (NavigableCollection.empty ())
  else NavigableCollection.NavigableCollection set navCollectionOps;

let navigableKeyedCollectionOps (): NavigableKeyedCollection.Ops.t int 'v (t 'v) =>  {
  containsKey,
  count,
  first: first >> Option.map (fun v => (0, v)),
  firstOrRaise: fun arr => (0, firstOrRaise arr),
  firstKey: first >> Option.map (fun v => 0),
  firstKeyOrRaise: fun arr => {
    firstOrRaise arr |> ignore;
    0
  },
  firstValue: first,
  firstValueOrRaise: firstOrRaise,
  keys,
  keysReversed,
  last: fun arr => arr |> last |> Option.map (fun v => ((count arr) - 1, v)),
  lastOrRaise: fun arr => ((count arr) - 1, lastOrRaise arr),
  lastKey: fun arr => arr |> last |> Option.map (fun v => (count arr) - 1),
  lastKeyOrRaise: fun arr => {
    lastOrRaise arr |> ignore;
    (count arr) - 1;
  },
  lastValue: last,
  lastValueOrRaise: lastOrRaise,
  toIterable: toKeyedIterable >> KeyedIterable.toIterable,
  toIterableReversed: toKeyedIterableReversed >> KeyedIterable.toIterable,
  toKeyedCollection,
  toKeyedIterable,
  toKeyedIterableReversed,
  toSequence: toSequenceWithIndex,
  toSequenceReversed: toSequenceWithIndexReversed,
  values: toIterable,
  valuesReversed: toIterableReversed,
};

let toNavigableKeyedCollection (arr: t 'a): (NavigableKeyedCollection.t int 'a) =>
  if (isEmpty arr) (NavigableKeyedCollection.empty ())
  else NavigableKeyedCollection.NavigableKeyedCollection arr (navigableKeyedCollectionOps ());

let navigableMapOps (): NavigableMap.Ops.t int 'v (t 'v) =>  {
  containsKey,
  count,
  first: first >> Option.map (fun v => (0, v)),
  firstOrRaise: fun arr => (0, firstOrRaise arr),
  firstKey: first >> Option.map (fun v => 0),
  firstKeyOrRaise: fun arr => {
    firstOrRaise arr |> ignore;
    0
  },
  firstValue: first,
  firstValueOrRaise: firstOrRaise,
  get,
  getOrRaise,
  keys,
  keysReversed,
  keySet,
  navigableKeySet,
  last: fun arr => arr |> last |> Option.map (fun v => ((count arr) - 1, v)),
  lastOrRaise: fun arr => ((count arr) - 1, lastOrRaise arr),
  lastKey: fun arr => arr |> last |> Option.map (fun v => (count arr) - 1),
  lastKeyOrRaise: fun arr => {
    lastOrRaise arr |> ignore;
    (count arr) - 1;
  },
  lastValue: last,
  lastValueOrRaise: lastOrRaise,
  toIterable: toKeyedIterable >> KeyedIterable.toIterable,
  toIterableReversed: toKeyedIterableReversed >> KeyedIterable.toIterable,
  toKeyedCollection,
  toKeyedIterable,
  toKeyedIterableReversed,
  toMap,
  toNavigableKeyedCollection,
  toSequence: toSequenceWithIndex,
  toSequenceReversed: toSequenceWithIndexReversed,
  values: toIterable,
  valuesReversed: toIterableReversed,
};

let toNavigableMap (arr: t 'a): (NavigableMap.t int 'a) =>
  if (isEmpty arr) (NavigableMap.empty ())
  else NavigableMap.NavigableMap arr (navigableMapOps ());

let updateAll (f: int => 'a => 'a) (vec: t 'a): (t 'a) => vec
  |> mutate
  |> Transient.updateAll f
  |> Transient.persist;

/* Unimplemented functions */
let concat (vectors: list (t 'a)): (t 'a) =>
  failwith "Not Implemented";

let insertAt (index: int) (value: 'a) (vec: t 'a): (t 'a) =>
  failwith "Not Implemented";

let removeAt (index: int) (vec: t 'a): (t 'a) =>
  failwith "Not Implemented";
