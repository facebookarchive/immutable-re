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

let compareWith
    (valueCompare: Comparator.t 'a)
    (this: t 'a)
    (that: t 'a): Ordering.t => {
  let thisCount = count this;
  let thatCount = count that;

  let loopCount = min thisCount thatCount;

  let rec loop index =>
    if (index < loopCount) {
      let cmp = valueCompare this.(index) that.(index);

      if (cmp === Ordering.equal) (loop (index + 1))
      else cmp
    }
    else if (index < thisCount) Ordering.greaterThan
    else if (index < thatCount) Ordering.lessThan
    else Ordering.equal;
  loop 0;
};

let compare (this: t 'a) (that: t 'a): Ordering.t =>
  compareWith Comparator.structural this that;

let empty: (t 'a) = [||];

let equalsWith
    (valueEquals: Equality.t 'a)
    (this: t 'a)
    (that: t 'a): bool => {
  let thisCount = count this;
  let thatCount = count that;

  let loopCount = min thisCount thatCount;

  let rec loop index =>
    if (index < loopCount) (
      if (valueEquals this.(index) that.(index)) (loop (index + 1))
      else false
    )
    else  true;

  if (this === that) true
  else if (thisCount != thatCount) false
  else loop 0;
};

let equals (this: t 'a) (that: t 'a): bool =>
  equalsWith Equality.structural this that;

let every (f: 'a => bool) (arr: t 'a): bool => {
  let arrCount = count arr;
  let rec loop index =>
    if (index >= arrCount) true
    else if (f arr.(index)) (loop (index + 1))
    else false;

  loop 0;
};

let everyWithIndex (f: int => 'a => bool) (arr: t 'a): bool => {
  let arrCount = count arr;
  let rec loop index =>
    if (index >= arrCount) true
    else if (f index arr.(index)) (loop (index + 1))
    else false;

  loop 0;
};

let find (f: 'a => bool) (arr: t 'a): (option 'a) => {
  let arrCount = count arr;

  let rec loop index =>
    if (index < arrCount) {
      let v = arr.(index);

      if (f v) (Some v)
      else loop (index + 1)
    }
    else None;

  loop 0;
};

let findOrRaise (f: 'a => bool) (arr: t 'a): 'a => {
  let arrCount = count arr;

  let rec loop index =>
    if (index < arrCount) {
      let v = arr.(index);

      if (f v) v
      else loop (index + 1)
    }
    else failwith "not found";

  loop 0;
};

let findWithIndex (f: int => 'a => bool) (arr: t 'a): (option 'a) => {
  let arrCount = count arr;

  let rec loop index =>
    if (index < arrCount) {
      let v = arr.(index);

      if (f index v) (Some v)
      else loop (index + 1)
    }
    else None;

  loop 0;
};

let findWithIndexOrRaise (f: int => 'a => bool) (arr: t 'a): 'a => {
  let arrCount = count arr;

  let rec loop index =>
    if (index < arrCount) {
      let v = arr.(index);

      if (f index v) v
      else loop (index + 1)
    }
    else failwith "not found";

  loop 0;
};

let forEach (f: 'a => unit) (arr: t 'a): 'acc =>
  arr |> Array.iter f;

let forEachWithIndex (f: int => 'a => unit) (arr: t 'a): 'acc =>
  arr |> Array.iteri f;

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
  if (lastIndex > 0) (get lastIndex arr)
  else None;
};

let lastOrRaise (arr: t 'a): 'a => {
  let lastIndex = count arr - 1;
  arr.(lastIndex)
};

let indexOf (f: 'a => bool) (arr: t 'a): (option int) => {
  let arrCount = count arr;

  let rec loop index =>
    if (index < arrCount) {
      let v = arr.(index);

      if (f v) (Some index)
      else loop (index + 1)
    }
    else None;

  loop 0;
};

let indexOfOrRaise (f: 'a => bool) (arr: t 'a): int => {
  let arrCount = count arr;

  let rec loop index =>
    if (index < arrCount) {
      let v = arr.(index);

      if (f v) index
      else loop (index + 1)
    }
    else failwith "not found";

  loop 0;
};

let indexOfWithIndex (f: int => 'a => bool) (arr: t 'a): (option int) => {
  let arrCount = count arr;

  let rec loop index =>
    if (index < arrCount) {
      let v = arr.(index);

      if (f index v) (Some index)
      else loop (index + 1)
    }
    else None;

  loop 0;
};

let indexOfWithIndexOrRaise (f: int => 'a => bool) (arr: t 'a): int => {
  let arrCount = count arr;

  let rec loop index =>
    if (index < arrCount) {
      let v = arr.(index);

      if (f index v) index
      else loop (index + 1)
    }
    else failwith "not found";

  loop 0;
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

let none (f: 'a => bool) (arr: t 'a): bool => {
  let arrCount = count arr;
  let rec loop index =>
    if (index >= arrCount) true
    else if (f arr.(index)) false
    else loop (index + 1);

  loop 0;
};

let noneWithIndex (f: int => 'a => bool) (arr: t 'a): bool => {
  let arrCount = count arr;
  let rec loop index =>
    if (index >= arrCount) true
    else if (f index arr.(index)) false
    else loop (index + 1);

  loop 0;
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

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) (arr: t 'a): 'acc =>
  Array.fold_left f acc arr;

let reduceWithIndex (f: 'acc => int => 'a => 'acc) (acc: 'acc) (arr: t 'a): 'acc => {
  let arrCount = count arr;
  let rec loop acc index =>
    if (index < arrCount) {
      let acc = f acc index arr.(index);
      loop acc (index + 1);
    }
    else acc;

  loop acc 0;
};

let reduceRight (f: 'acc => 'a => 'acc) (acc: 'acc) (arr: t 'a): 'acc =>
  Array.fold_right (Functions.flip f) arr acc;

let forEachRight (f: 'a => unit) (arr: t 'a): unit =>
  arr |> reduceRight (fun _ next => f next) ();

let reduceRightWithIndex (f: 'acc => int => 'a => 'acc) (acc: 'acc) (arr: t 'a): 'acc => {
  let arrLastIndex = lastIndexOrRaise arr;
  let rec loop acc index =>
    if (index >= 0) {
      let acc = f acc index arr.(index);
      loop acc (index - 1);
    }
    else acc;

  loop acc arrLastIndex;
};

let forEachRightWithIndex (f: int => 'a => unit) (arr: t 'a): unit =>
  arr |> reduceRightWithIndex (fun _ index next => f index next) ();

let hashWith (hash: Hash.t 'a) (arr: t 'a): int =>
  arr |> reduce (Hash.reducer hash) Hash.initialValue;

let hash (arr: t 'a): int =>
  hashWith Hash.structural arr;

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

let removeLast (arr: t 'a): (t 'a) => {
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

let removeFirst (arr: t 'a): (t 'a) =>
  removeAt 0 arr;

let return (value: 'a): (t 'a) => [| value |];

let reverse (arr: t 'a): (t 'a) => {
  let count = count arr;
  Array.init count (fun i => arr.(count - i - 1))
};

let skip (startIndex: int) (arr: t 'a): (t 'a) => {
  let newCount = (count arr) - startIndex;
  Array.sub arr startIndex newCount;
};

let some (f: 'a => bool) (arr: t 'a): bool => {
  let arrCount = count arr;

  let rec loop index =>
    if (index >= arrCount) false
    else if (f arr.(index)) true
    else loop (index + 1);

  loop 0;
};

let containsWith (valueEquals: Equality.t 'a) (value: 'a) (arr: t 'a): bool =>
  some (valueEquals value) arr;

let contains (value: 'a) (list: t 'a): bool =>
  containsWith Equality.structural value list;

let someWithIndex (f: int => 'a => bool) (arr: t 'a): bool => {
  let arrCount = count arr;

  let rec loop index =>
    if (index >= arrCount) false
    else if (f index arr.(index)) true
    else loop (index + 1);

  loop 0;
};

let take (newCount: int) (arr: t 'a): (t 'a) =>
  Array.sub arr 0 newCount;

let toIterator (arr: t 'a): (Iterator.t 'a) =>
  if (isEmpty arr) Iterator.empty
  else { reduce: fun f acc => reduce f acc arr };

let toIteratorRight (arr: t 'a): (Iterator.t 'a) =>
  if (isEmpty arr) Iterator.empty
  else { reduce: fun f acc => reduceRight f acc arr };

let toKeyedIterator (arr: t 'a): (KeyedIterator.t int 'a) =>
  if (isEmpty arr) KeyedIterator.empty
  else { reduce: fun f acc => reduceWithIndex f acc arr };

let toKeyedIteratorRight (arr: t 'a): (KeyedIterator.t int 'a) =>
  if (isEmpty arr) KeyedIterator.empty
  else { reduce: fun f acc => reduceRightWithIndex f acc arr };

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
  containsWith: fun equals index value =>
    if (index >= 0 && index < count arr) (equals arr.(index) value)
    else false,
  containsKey: fun index => index >= 0 && index < count arr,
  count: count arr,
  every: fun f => everyWithIndex f arr,
  find: fun f => indexOfWithIndex f arr >>| fun index => (index, arr.(index)),
  findOrRaise: fun f => {
    let index = indexOfWithIndexOrRaise f arr;
    (index, arr.(index))
  },
  forEach: fun f => forEachWithIndex f arr,
  get: fun i => get i arr,
  getOrRaise: fun index => getOrRaise index arr,
  none: fun f => noneWithIndex f arr,
  reduce: fun f acc => reduceWithIndex f acc arr,
  some: fun f => someWithIndex f arr,
  toSequence: toSequenceWithIndex arr,
  values: toIterator arr,
};
