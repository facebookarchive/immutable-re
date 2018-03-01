/***
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
type t('a) = array('a);

let count = (arr: t('a)) : int => Array.length(arr);

let getOrRaise = (index: int, arr: t('a)) : 'a => arr[index];

let reduce =
    (~while_ as predicate: ('acc, 'a) => bool, f: ('acc, 'a) => 'acc, acc: 'acc, arr: t('a))
    : 'acc => {
  let arrCount = count(arr);
  let rec loop = (acc, index) =>
    if (index < arrCount) {
      let next = arr[index];
      if (predicate(acc, next)) {
        let acc = f(acc, arr[index]);
        loop(acc, index + 1)
      } else {
        acc
      }
    } else {
      acc
    };
  loop(acc, 0)
};

let reduceReversed =
    (~while_ as predicate: ('acc, 'a) => bool, f: ('acc, 'a) => 'acc, acc: 'acc, arr: t('a))
    : 'acc => {
  let arrCount = count(arr);
  let rec loop = (acc, index) =>
    if (index >= 0) {
      let next = arr[index];
      if (predicate(acc, next)) {
        let acc = f(acc, arr[index]);
        loop(acc, index - 1)
      } else {
        acc
      }
    } else {
      acc
    };
  loop(acc, arrCount - 1)
};

let toSequence = (arr: t('a)) : Sequence.t('a) => {
  let arrCount = count(arr);
  let rec loop = (index, ()) =>
    if (index < arrCount) {
      Sequence.yield(arr[index], loop(index + 1))
    } else {
      Sequence.empty()
    };
  loop(0, ())
};

let toSequenceReversed = (arr: t('a)) : Sequence.t('a) => {
  let rec loop = (index, ()) =>
    if (index < 0) {
      Sequence.empty()
    } else {
      Sequence.yield(arr[index], loop(index - 1))
    };
  loop(count(arr) - 1, ())
};

let lastIndexOrRaise = (arr: t('a)) : int => {
  let lastIndex = count(arr) - 1;
  if (lastIndex >= 0) {
    lastIndex
  } else {
    failwith("empty")
  }
};

let addFirst = (item: 'a, arr: t('a)) : t('a) => {
  let count = count(arr);
  let retval = Array.make(count + 1, item);
  Array.blit(arr, 0, retval, 1, count);
  retval
};

let addLast = (item: 'a, arr: t('a)) : t('a) => {
  let count = count(arr);
  let retval = Array.make(count + 1, item);
  Array.blit(arr, 0, retval, 0, count);
  retval
};

let empty = () : t('a) => [||];

let init = Array.init;

let insertAt = (index: int, item: 'a, arr: t('a)) : t('a) => {
  let count = count(arr);
  Preconditions.failIfOutOfRange(count + 1, index);
  let retval = Array.make(count + 1, item);
  Array.blit(arr, 0, retval, 0, index);
  Array.blit(arr, index, retval, index + 1, count - index);
  retval
};

let ofUnsafe = (arr: array('a)) : t('a) => arr;

let removeLastOrRaise = (arr: t('a)) : t('a) => {
  let count = count(arr);
  if (count === 0) {
    failwith("Array is empty")
  } else if (count === 1) {
    [||]
  } else {
    Array.sub(arr, 0, count - 1)
  }
};

let removeAt = (index: int, arr: t('a)) : t('a) => {
  let count = count(arr);
  Preconditions.failIfOutOfRange(count, index);
  let newLength = count - 1;
  let anyItem = arr[0];
  let retval = Array.make(newLength, anyItem);
  Array.blit(arr, 0, retval, 0, index);
  Array.blit(arr, index + 1, retval, index, newLength - index);
  retval
};

let removeFirstOrRaise = (arr: t('a)) : t('a) => removeAt(0, arr);

let skip = (startIndex: int, arr: t('a)) : t('a) => {
  let arrCount = count(arr);
  if (startIndex < 0) {
    failwith("startIndex is < 0")
  } else if (startIndex >= arrCount) {
    [||]
  } else {
    let newCount = arrCount - startIndex;
    Array.sub(arr, startIndex, newCount)
  }
};

let take = (newCount: int, arr: t('a)) : t('a) =>
  if (newCount < 0) {
    failwith("count is < 0")
  } else if (newCount === 0) {
    [||]
  } else {
    Array.sub(arr, 0, newCount)
  };

let update = (index: int, item: 'a, arr: t('a)) : t('a) => {
  let arrCount = count(arr);
  Preconditions.failIfOutOfRange(arrCount, index);
  let clone = Array.copy(arr);
  clone[index] = item;
  clone
};

let updateWith = (index: int, f: 'a => 'a, arr: t('a)) : t('a) => {
  let count = count(arr);
  Preconditions.failIfOutOfRange(count, index);
  let clone = Array.copy(arr);
  clone[index] = f(arr[index]);
  clone
};
