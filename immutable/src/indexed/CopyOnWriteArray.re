/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Option.Operators;

type t 'a = array 'a;

let count (arr: t 'a): int => Array.length arr;

let addFirst (item: 'a) (arr: t 'a): (t 'a) => {
  let count = count arr;

  let retval = Array.make (count + 1) item;
  Array.blit arr 0 retval 1 count;
  retval
};

let addFirstAll (iter: Iterator.t 'a) (arr: t 'a): (t 'a) =>
  /* FIXME: This implemenation is particularly bad. We can improve it
   * by using dynamic array allocations.
   */
  iter |> Iterator.reduce (fun acc next => acc |> addFirst next) arr;

let addLast (item: 'a) (arr: t 'a): (t 'a) => {
  let count = count arr;

  let retval = Array.make (count + 1) item;
  Array.blit arr 0 retval 0 count;
  retval
};

let addLastAll (iter: Iterator.t 'a) (arr: t 'a): (t 'a) =>
  /* FIXME: This implemenation is particularly bad. We can improve it
   * by using dynamic array allocations.
   */
  iter |> Iterator.reduce (fun acc next => acc |> addLast next) arr;

let empty: (t 'a) = [||];

let from (iter: Iterator.t 'a): (t 'a) =>
  [||] |> addLastAll iter;

let fromReverse (iter: Iterator.t 'a): (t 'a) =>
  [||] |> addFirstAll iter;

let getOrRaiseFlipped (arr: t 'a) (index: int): 'a =>
  arr.(index);

let get (index: int) (arr: t 'a): (option 'a) =>
  Preconditions.noneIfIndexOutOfRange (count arr) index (getOrRaiseFlipped arr);

let getOrRaise (index: int) (arr: t 'a): 'a => arr.(index);

let first (arr: t 'a): (option 'a) => get 0 arr;

let firstOrRaise (arr: t 'a): 'a => getOrRaise 0 arr;

let lastIndexOrRaise (arr: t 'a): int => {
  let lastIndex = count arr - 1;
  if (lastIndex > 0) lastIndex
  else failwith "empty";
};

let last (arr: t 'a): (option 'a) => {
  let lastIndex = count arr - 1;
  if (lastIndex >= 0) (get lastIndex arr)
  else None;
};

let lastOrRaise (arr: t 'a): 'a => {
  let lastIndex = count arr - 1;
  arr.(lastIndex)
};

let init = Array.init;

let insertAt (index: int) (item: 'a) (arr: t 'a): (t 'a) => {
  let count = count arr;

  Preconditions.failIfOutOfRange (count + 1) index;

  let retval = Array.make (count + 1) item;
  Array.blit arr 0 retval 0 index;
  Array.blit arr index retval (index + 1) (count - index);

  retval;
};

let isEmpty (arr: t 'a): bool => (count arr) == 0;

let isNotEmpty (arr: t 'a): bool => (count arr) != 0;

let concat (arrays: list (array 'a)): (array 'a) => {
  let newCount = arrays |> ImmList.reduce (fun acc i => acc + count i) 0;

  if (newCount == 0) [||]
  else {
    let retval = Array.make newCount (ImmList.findOrRaise isNotEmpty arrays).(0);

    ImmList.reduce (fun index next => {
      let countNext = count next;
      Array.blit next 0 retval index countNext;
      index + countNext;
    }) 0 arrays |> ignore;

    retval;
  };
};

let ofUnsafe (arr: array 'a): (t 'a) => arr;

let range
    (startIndex: int)
    (newCount: option int)
    (arr: t 'a): (t 'a) => {
  let newCount = newCount |? (count arr) - startIndex;

  if (startIndex == 0 && newCount == (count arr)) arr
  else Array.sub arr startIndex newCount;
};

let reduce
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (arr: t 'a): 'acc => {
  let arrCount = count arr;
  let rec loop acc index =>
    if (index < arrCount) {
      let next = arr.(index);

      if (predicate acc next) {
        let acc = f acc arr.(index);
        loop acc (index + 1);
      }
      else acc
    }
    else acc;

  loop acc 0;
};

let reduceWithIndex
    while_::(predicate: 'acc => int => 'a => bool)=Functions.alwaysTrue3
    (f: 'acc => int => 'a => 'acc)
    (acc: 'acc)
    (arr: t 'a): 'acc => {
  let arrCount = count arr;
  let rec loop acc index =>
    if (index < arrCount) {
      let next = arr.(index);

      if (predicate acc index next) {
        let acc = f acc index next;
        loop acc (index + 1);
      }
      else acc;
    }
    else acc;

  loop acc 0;
};

let reduceRight
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (arr: t 'a): 'acc => {
  let arrCount = count arr;
  let rec loop acc index =>
    if (index >= 0) {
      let next = arr.(index);

      if (predicate acc next) {
        let acc = f acc arr.(index);
        loop acc (index - 1);
      }
      else acc
    }
    else acc;

  loop acc (arrCount - 1);
};

let reduceRightWithIndex
    while_::(predicate: 'acc => int => 'a => bool)=Functions.alwaysTrue3
    (f: 'acc => int => 'a => 'acc)
    (acc: 'acc)
    (arr: t 'a): 'acc => {
  let arrLastIndex = lastIndexOrRaise arr;
  let rec loop acc index =>
    if (index >= 0) {
      let next = arr.(index);

      if (predicate acc index next) {
        let acc = f acc index next;
        loop acc (index - 1);
      }
      else acc
    }
    else acc;

  loop acc arrLastIndex;
};

let map (f: 'a => 'b) (arr: t 'a): (t 'b) =>
  if (isNotEmpty arr) {
    let initialValue = f arr.(0);
    let retval = Array.make (count arr) initialValue;
    arr |> reduce (fun acc next => { retval.(acc) = f next; acc + 1 }) 0 |> ignore;
    retval;
  }
  else [||];

let mapWithIndex (f: int => 'a => 'b) (arr: t 'a): (t 'b) =>
  if (isNotEmpty arr) {
    let initialValue = f 0 arr.(0);
    let retval = Array.make (count arr) initialValue;
    arr |> reduce (fun acc next => { retval.(acc) = f acc next; acc + 1 }) 0 |> ignore;
    retval;
  }
  else [||];

let mapReverse (f: 'a => 'b) (arr: t 'a): (t 'b) =>
  if (isNotEmpty arr) {
    let initialValue = f arr.(0);
    let retval = Array.make (count arr) initialValue;
    arr |> reduceRight (fun acc next => { retval.(acc) = f next; acc + 1 }) 0 |> ignore;
    retval;
  }
  else [||];

let mapReverseWithIndex (f: int => 'a => 'b) (arr: t 'a): (t 'b) =>
  if (isNotEmpty arr) {
    let arrCount = count arr;
    let initialValue = f 0 arr.(0);
    let retval = Array.make arrCount initialValue;
    arr |> reduce (fun acc next => { retval.(arrCount - acc - 1) = f acc next; acc + 1 }) 0 |> ignore;
    retval;
  }
  else [||];

let removeAll (_: t 'a): (t 'a) => empty;

let removeLastOrRaise (arr: t 'a): (t 'a) => {
  let count = count arr;

  if (count == 0) (failwith "Array is empty")
  else if (count == 1) [||]
  else Array.sub arr 0 (count - 1);
};

let removeAt (index: int) (arr: t 'a): (t 'a) => {
  let count = count arr;

  Preconditions.failIfOutOfRange count index;

  let newLength = count - 1;
  let anyItem = arr.(0);

  let retval = Array.make newLength anyItem;
  Array.blit arr 0 retval 0 index;
  Array.blit arr (index + 1) retval index (newLength - index);

  retval
};

let removeFirstOrRaise (arr: t 'a): (t 'a) =>
  removeAt 0 arr;

let return (value: 'a): (t 'a) => [| value |];

let skip (startIndex: int) (arr: t 'a): (t 'a) => {
  let newCount = (count arr) - startIndex;
  Array.sub arr startIndex newCount;
};

let take (newCount: int) (arr: t 'a): (t 'a) =>
  Array.sub arr 0 newCount;

let toIterator (arr: t 'a): (Iterator.t 'a) =>
  if (isEmpty arr) Iterator.empty
  else { reduce: fun predicate f acc => reduce while_::predicate f acc arr };

let toIteratorRight (arr: t 'a): (Iterator.t 'a) =>
  if (isEmpty arr) Iterator.empty
  else { reduce: fun predicate f acc => reduceRight while_::predicate f acc arr };

let toKeyedIterator (arr: t 'a): (KeyedIterator.t int 'a) =>
  if (isEmpty arr) KeyedIterator.empty
  else { reduce: fun predicate f acc => reduceWithIndex while_::predicate f acc arr };

let toKeyedIteratorRight (arr: t 'a): (KeyedIterator.t int 'a) =>
  if (isEmpty arr) KeyedIterator.empty
  else { reduce: fun predicate f acc => reduceRightWithIndex while_::predicate f acc arr };

let toSequenceRight (arr: t 'a): (Sequence.t 'a) =>
  if (isEmpty arr) Sequence.empty
  else {
    let rec loop index => fun () =>
      if (index < 0) Sequence.Completed
      else Sequence.Next arr.(index) (loop (index - 1));
    loop (count arr - 1);
  };

let toSequence (arr: t 'a): (Sequence.t 'a) =>
  if (isEmpty arr) Sequence.empty
  else {
    let arrCount = count arr;
    let rec loop index => fun () =>
      if (index < arrCount) (Sequence.Next arr.(index) (loop (index + 1)))
      else Sequence.Completed;
    loop 0;
  };

let toSequenceWithIndex (arr: t 'a): (Sequence.t (int, 'a)) => {
  let arrCount = count arr;
  let rec loop index => fun () =>
    if (index < arrCount) (Sequence.Next (index, arr.(index)) (loop (index + 1)))
    else Sequence.Completed;
  loop 0;
};

let update (index: int) (item: 'a) (arr: t 'a): (t 'a) => {
  let arrCount = count arr;

  Preconditions.failIfOutOfRange arrCount index;

  let clone = Array.copy arr;
  clone.(index) = item;
  clone
};

let updateAll (f: int => 'a => 'a) (arr: t 'a): (t 'a) => {
  let arrCount = count arr;
  let clone = Array.copy arr;
  let rec loop index =>
    if (index < arrCount) {
      clone.(index) = f index arr.(index);
      loop (index + 1);
    }
    else clone;

  loop 0;
};

let updateWith (index: int) (f: 'a => 'a) (arr: t 'a): (t 'a) => {
  let count = count arr;

  Preconditions.failIfOutOfRange count index;

  let clone = Array.copy arr;
  clone.(index) = f arr.(index);
  clone
};

let toMap (arr: t 'a): (ImmMap.t int 'a) => {
  containsKey: fun index => index >= 0 && index < count arr,
  count: count arr,
  get: fun i => get i arr,
  getOrRaise: fun index => getOrRaise index arr,
  keyedIterator: toKeyedIterator arr,
  sequence: toSequenceWithIndex arr,
};

let module Reducer = Reducer.Make1 {
  type t 'a = array 'a;
  let reduce = reduce;
};
