/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Functions;
open Indexed;
open Option.Operators;
open Preconditions;
open Seq;

type copyOnWriteArray 'a = array 'a;

let count (arr: copyOnWriteArray 'a): int => Array.length arr;

let addFirst (item: 'a) (arr: copyOnWriteArray 'a): (copyOnWriteArray 'a) => {
  let count = count arr;

  let retval = Array.make (count + 1) item;
  Array.blit arr 0 retval 1 count;
  retval
};

let addLast (item: 'a) (arr: copyOnWriteArray 'a): (copyOnWriteArray 'a) => {
  let count = count arr;

  let retval = Array.make (count + 1) item;
  Array.blit arr 0 retval 0 count;
  retval
};

let add = addLast;

let concat (arrays: list (array 'a)): (array 'a) => {
  let newCount = arrays |> ImmList.reduce (fun acc i => acc + count i) 0;

  newCount == 0 ? [||] : {
    let retval = Array.make newCount (ImmList.first arrays).(0);

    ImmList.reduce (fun index next => {
      let countNext = count next;
      Array.blit next 0 retval index countNext;
      index + countNext;
    }) 0 |> ignore;

    retval;
  };
};

let empty: (copyOnWriteArray 'a) = [||];

let every (f: 'a => bool) (arr: copyOnWriteArray 'a): bool => {
  let arrCount = count arr;
  let rec loop i =>
    i >= arrCount ? true :
    (f arr.(i)) ? loop (i + 1) :
    false;

  loop 0;
};

let first (arr: copyOnWriteArray 'a): 'a => arr.(0);

let fromSeq (length: int) (defaultValue: 'a) (seq: seq 'a): (array 'a) => {
  let seq = ref seq;

  let f (index: int) => switch (!seq ()) {
    | Next value next =>
        seq := next;
        value
    | Completed => defaultValue
  };

  Array.init length f
};

let get (index: int) (arr: copyOnWriteArray 'a): 'a => arr.(index);

let lastIndex (arr: copyOnWriteArray 'a): int => count arr - 1;
let last (arr: copyOnWriteArray 'a): 'a => arr.(lastIndex arr);

let init = Array.init;

let insertAt (index: int) (item: 'a) (arr: copyOnWriteArray 'a): (copyOnWriteArray 'a) => {
  let count = count arr;

  Preconditions.failIfOutOfRange (count + 1) index;

  let retval = Array.make (count + 1) item;
  Array.blit arr 0 retval 0 index;
  Array.blit arr index retval (index + 1) (count - index);

  retval;
};

let isEmpty (arr: copyOnWriteArray 'a): bool => (count arr) == 0;

let isNotEmpty (arr: copyOnWriteArray 'a): bool => (count arr) != 0;

let none (f: 'a => bool) (arr: copyOnWriteArray 'a): bool => {
  let arrCount = count arr;
  let rec loop i =>
    i >= arrCount ? true :
    (f arr.(i)) ? false :
    loop (i + 1);

  loop 0;
};

let ofUnsafe (arr: array 'a): (copyOnWriteArray 'a) => arr;

let range
    (startIndex: int)
    (newCount: option int)
    (arr: copyOnWriteArray 'a): (copyOnWriteArray 'a) => {
  let newCount = newCount |? (count arr) - startIndex;
  startIndex == 0 && newCount == (count arr)
    ? arr
    : Array.sub arr startIndex newCount;
};

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) (arr: copyOnWriteArray 'a): 'acc =>
  Array.fold_left f acc arr;

let reduceRight (f: 'acc => 'a => 'acc) (acc: 'acc) (arr: copyOnWriteArray 'a): 'acc =>
  Array.fold_right (flip f) arr acc;

let map (f: 'a => 'b) (arr: copyOnWriteArray 'a): (copyOnWriteArray 'b) => isNotEmpty arr
  ? {
    let initialValue = f arr.(0);
    let retval = Array.make (count arr) initialValue;
    arr |> reduce (fun acc next => { retval.(acc) = f next; acc + 1 }) 0 |> ignore;
    retval;
  }: [||];

let mapReverse (f: 'a => 'b) (arr: copyOnWriteArray 'a): (copyOnWriteArray 'b) => isNotEmpty arr
  ? {
    let initialValue = f arr.(0);
    let retval = Array.make (count arr) initialValue;
    arr |> reduceRight (fun acc next => { retval.(acc) = f next; acc + 1 }) 0 |> ignore;
    retval;
  }: [||];

let removeAll (arr: copyOnWriteArray 'a): (copyOnWriteArray 'a) => empty;

let removeLast (arr: copyOnWriteArray 'a): (copyOnWriteArray 'a) => {
  let count = count arr;

  count == 0 ? failwith "Array is empty" :
  count == 1 ? [||] :
  Array.sub arr 0 (count - 1)
};

let removeAt (index: int) (arr: copyOnWriteArray 'a): (copyOnWriteArray 'a) => {
  let count = count arr;

  Preconditions.failIfOutOfRange count index;

  let newLength = count - 1;
  let anyItem = arr.(0);

  let retval = Array.make newLength anyItem;
  Array.blit arr 0 retval 0 index;
  Array.blit arr (index + 1) retval index (newLength - index);

  retval
};

let removeFirst (arr: copyOnWriteArray 'a): (copyOnWriteArray 'a) =>
  removeAt 0 arr;

let reverse (arr: copyOnWriteArray 'a): (copyOnWriteArray 'a) => {
  let count = count arr;
  Array.init count (fun i => arr.(count - i - 1))
};

let skip (startIndex: int) (arr: copyOnWriteArray 'a): (copyOnWriteArray 'a) => {
  let newCount = (count arr) - startIndex;
  Array.sub arr startIndex newCount;
};

let some (f: 'a => bool) (arr: copyOnWriteArray 'a): bool => {
  let arrCount = count arr;
  let rec loop i =>
    i >= arrCount ? false :
    (f arr.(i)) ? true :
    loop (i + 1);

  loop 0;
};

let take (newCount: int) (arr: copyOnWriteArray 'a): (copyOnWriteArray 'a) =>
  Array.sub arr 0 newCount;

let split (index: int) (arr: copyOnWriteArray 'a): (copyOnWriteArray 'a, copyOnWriteArray 'a) =>
  (take index arr, skip index arr);

let toSeqDecrementing (startIndex: int) (endIndex: int) (arr: copyOnWriteArray 'a): (seq 'a) => {
  let rec toSeqAtIndex (index: int) => fun () => index > endIndex
    ? Next arr.(index) (toSeqAtIndex (index - 1))
    : Completed;

  toSeqAtIndex startIndex
};

let toSeqIncrementing (startIndex: int) (endIndex: int) (arr: copyOnWriteArray 'a): (seq 'a) => {
  let rec toSeqAtIndex (index: int) => fun () => index < endIndex
    ? Next arr.(index) (toSeqAtIndex (index + 1))
    : Completed;

  toSeqAtIndex startIndex
};

let toSeqInRange (startIndex: int) (endIndex: int) (arr: copyOnWriteArray 'a): seq 'a => {
  let count = count arr;

  (startIndex <= endIndex) && (endIndex <= count) && (startIndex >= 0) ?
    arr |> toSeqIncrementing startIndex endIndex :

  (startIndex > endIndex) && (startIndex < count) && (endIndex >= -1) ?
    arr |> toSeqDecrementing startIndex endIndex :

  failwith "Index out of bounds";
};

let toSeqReversed (arr: copyOnWriteArray 'a): (seq 'a) =>
  toSeqInRange ((count arr) - 1) (-1) arr;

let toSeq (arr: copyOnWriteArray 'a): (seq 'a) =>
  toSeqInRange 0 (count arr) arr;

let toIndexed (arr: copyOnWriteArray 'a): indexed 'a => ({
  count: count arr,
  rseq: toSeqReversed arr,
  seq: toSeq arr,
  tryGet: fun (index: int) => index >= 0 && index < (count arr)
    ? Some (arr.(index))
    : None,
});

let tryGet (index: int) (arr: copyOnWriteArray 'a): (option 'a) =>
  Preconditions.noneIfIndexOutOfRange (count arr) index (flip get arr);

let tryFirst (arr: copyOnWriteArray 'a): (option 'a) => tryGet 0 arr;

let tryLast (arr: copyOnWriteArray 'a): (option 'a) => tryGet ((count arr) - 1) arr;

let update (index: int) (item: 'a) (arr: copyOnWriteArray 'a): (copyOnWriteArray 'a) => {
  let count = count arr;

  Preconditions.failIfOutOfRange count index;

  let clone = Array.copy arr;
  clone.(index) = item;
  clone
};
