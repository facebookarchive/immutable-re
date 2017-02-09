/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Comparator;
open Equality;
open Functions;
open Hash;
open Indexed;
open Option.Operators;
open Ordering;
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

let addFirstAll (seq: seq 'a) (arr: copyOnWriteArray 'a): (copyOnWriteArray 'a) =>
  /* FIXME: This implemenation is particularly bad. We can improve it
   * by using dynamic array allocations.
   */
  seq |> Seq.reduce (fun acc next => acc |> addFirst next) arr;

let addLast (item: 'a) (arr: copyOnWriteArray 'a): (copyOnWriteArray 'a) => {
  let count = count arr;

  let retval = Array.make (count + 1) item;
  Array.blit arr 0 retval 0 count;
  retval
};

let addLastAll (seq: seq 'a) (arr: copyOnWriteArray 'a): (copyOnWriteArray 'a) =>
  /* FIXME: This implemenation is particularly bad. We can improve it
   * by using dynamic array allocations.
   */
  seq |> Seq.reduce (fun acc next => acc |> addLast next) arr;

let rec compareWith
    (valueCompare: comparator 'a)
    (this: copyOnWriteArray 'a)
    (that: copyOnWriteArray 'a): ordering => {
  let thisCount = count this;
  let thatCount = count that;

  let loopCount = min thisCount thatCount;

  let rec loop index =>
    index < loopCount ? switch (valueCompare this.(index) that.(index)) {
      | Equal => loop (index + 1)
      | x => x
    } :
    index < thisCount ? Ordering.greaterThan :
    index < thatCount ? Ordering.lessThan :
    Ordering.equal;
  loop 0;
};

let compare (this: copyOnWriteArray 'a) (that: copyOnWriteArray 'a): ordering =>
  compareWith Comparator.structural this that;

let empty: (copyOnWriteArray 'a) = [||];

let rec equalsWith
    (valueEquals: equality 'a)
    (this: copyOnWriteArray 'a)
    (that: copyOnWriteArray 'a): bool => {
  let thisCount = count this;
  let thatCount = count that;

  let loopCount = min thisCount thatCount;

  let rec loop index => index < loopCount
    ? (valueEquals this.(index) that.(index) ? loop (index + 1) : false)
    : true;

  this === that ? true :
  thisCount != thatCount ? false :
  loop 0;
};

let equals (this: copyOnWriteArray 'a) (that: copyOnWriteArray 'a): bool =>
  equalsWith Equality.structural this that;

let every (f: 'a => bool) (arr: copyOnWriteArray 'a): bool => {
  let arrCount = count arr;
  let rec loop i =>
    i >= arrCount ? true :
    (f arr.(i)) ? loop (i + 1) :
    false;

  loop 0;
};

let find (f: 'a => bool) (arr: copyOnWriteArray 'a): 'a => {
  let arrCount = count arr;

  let rec loop index => index < arrCount ? {
    let v = arr.(index);
    f v ? v : loop (index + 1)
  } : failwith "not found";

  loop 0;
};

let first (arr: copyOnWriteArray 'a): 'a => arr.(0);

let forEach (f: 'a => unit) (arr: copyOnWriteArray 'a): 'acc =>
  arr |> Array.iter f;

let forEachWithIndex (f: int => 'a => unit) (arr: copyOnWriteArray 'a): 'acc =>
  arr |> Array.iteri f;

let fromSeq (seq: seq 'a): (copyOnWriteArray 'a) =>
  [||] |> addLastAll seq;

let fromSeqReversed (seq: seq 'a): (copyOnWriteArray 'a) =>
  [||] |> addFirstAll seq;

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

let concat (arrays: list (array 'a)): (array 'a) => {
  let newCount = arrays |> ImmList.reduce (fun acc i => acc + count i) 0;

  newCount == 0 ? [||] : {
    let retval = Array.make newCount (ImmList.find isNotEmpty arrays).(0);

    ImmList.reduce (fun index next => {
      let countNext = count next;
      Array.blit next 0 retval index countNext;
      index + countNext;
    }) 0 |> ignore;

    retval;
  };
};

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

let reduceWithIndex (f: 'acc => int => 'a => 'acc) (acc: 'acc) (arr: copyOnWriteArray 'a): 'acc => {
  let arrCount = count arr;
  let rec loop acc index => index < arrCount ? {
    let acc = f acc index arr.(index);
    loop acc (index + 1);
  } : acc;

  loop acc 0;
};

let reduceRight (f: 'acc => 'a => 'acc) (acc: 'acc) (arr: copyOnWriteArray 'a): 'acc =>
  Array.fold_right (flip f) arr acc;

let forEachReverse (f: 'a => unit) (arr: copyOnWriteArray 'a): unit =>
  arr |> reduceRight (fun _ next => f next) ();

let reduceRightWithIndex (f: 'acc => int => 'a => 'acc) (acc: 'acc) (arr: copyOnWriteArray 'a): 'acc => {
  let arrLastIndex = lastIndex arr;
  let rec loop acc index => index >= 0 ? {
    let acc = f acc index arr.(index);
    loop acc (index - 1);
  } : acc;

  loop acc arrLastIndex;
};

let forEachReverseWithIndex (f: int => 'a => unit) (arr: copyOnWriteArray 'a): unit =>
  arr |> reduceRightWithIndex (fun _ index next => f index next) ();

let hashWith (hash: hash 'a) (arr: copyOnWriteArray 'a): int =>
  arr |> reduce (Hash.reducer hash) Hash.initialValue;

let hash (arr: copyOnWriteArray 'a): int =>
  hashWith Hash.structural arr;

let map (f: 'a => 'b) (arr: copyOnWriteArray 'a): (copyOnWriteArray 'b) => isNotEmpty arr
  ? {
    let initialValue = f arr.(0);
    let retval = Array.make (count arr) initialValue;
    arr |> reduce (fun acc next => { retval.(acc) = f next; acc + 1 }) 0 |> ignore;
    retval;
  }: [||];

let mapWithIndex (f: int => 'a => 'b) (arr: copyOnWriteArray 'a): (copyOnWriteArray 'b) => isNotEmpty arr
  ? {
    let initialValue = f 0 arr.(0);
    let retval = Array.make (count arr) initialValue;
    arr |> reduce (fun acc next => { retval.(acc) = f acc next; acc + 1 }) 0 |> ignore;
    retval;
  }: [||];

let mapReverse (f: 'a => 'b) (arr: copyOnWriteArray 'a): (copyOnWriteArray 'b) => isNotEmpty arr
  ? {
    let initialValue = f arr.(0);
    let retval = Array.make (count arr) initialValue;
    arr |> reduceRight (fun acc next => { retval.(acc) = f next; acc + 1 }) 0 |> ignore;
    retval;
  }: [||];

let mapReverseWithIndex (f: int => 'a => 'b) (arr: copyOnWriteArray 'a): (copyOnWriteArray 'b) => isNotEmpty arr
  ? {
    let arrCount = count arr;
    let initialValue = f 0 arr.(0);
    let retval = Array.make arrCount initialValue;
    arr |> reduce (fun acc next => { retval.(arrCount - acc - 1) = f acc next; acc + 1 }) 0 |> ignore;
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

let toSeqReversed (arr: copyOnWriteArray 'a): (seq 'a) => {
  let rec loop index => fun () => index < 0
    ? Completed
    : Next arr.(index) (loop (index - 1));
  loop (count arr - 1);
};

let toSeq (arr: copyOnWriteArray 'a): (seq 'a) => {
  let arrCount = count arr;
  let rec loop index => fun () => index < arrCount
    ? Next arr.(index) (loop (index + 1))
    : Completed;
  loop 0;
};

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

let tryFind (f: 'a => bool) (arr: copyOnWriteArray 'a): (option 'a) => {
  let arrCount = count arr;

  let rec loop index => index < arrCount ? {
    let v = arr.(index);
    f v ? Some v : loop (index + 1)
  } : None;

  loop 0;
};

let tryFirst (arr: copyOnWriteArray 'a): (option 'a) => tryGet 0 arr;

let tryLast (arr: copyOnWriteArray 'a): (option 'a) => tryGet ((count arr) - 1) arr;

let update (index: int) (item: 'a) (arr: copyOnWriteArray 'a): (copyOnWriteArray 'a) => {
  let count = count arr;

  Preconditions.failIfOutOfRange count index;

  let clone = Array.copy arr;
  clone.(index) = item;
  clone
};
