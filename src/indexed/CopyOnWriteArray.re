/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Functions.Operators;

type t 'a = array 'a;

let count (arr: t 'a): int => Array.length arr;

let addFirst (item: 'a) (arr: t 'a): (t 'a) => {
  let count = count arr;

  let retval = Array.make (count + 1) item;
  Array.blit arr 0 retval 1 count;
  retval
};

let addLast (item: 'a) (arr: t 'a): (t 'a) => {
  let count = count arr;

  let retval = Array.make (count + 1) item;
  Array.blit arr 0 retval 0 count;
  retval
};

let empty (): (t 'a) => [||];

let getOrRaiseFlipped (arr: t 'a) (index: int): 'a =>
  arr.(index);

let get (index: int) (arr: t 'a): (option 'a) =>
  Preconditions.noneIfIndexOutOfRange (count arr) index (getOrRaiseFlipped arr);

let getOrRaise (index: int) (arr: t 'a): 'a => arr.(index);

let first (arr: t 'a): (option 'a) => get 0 arr;

let firstOrRaise (arr: t 'a): 'a => getOrRaise 0 arr;

let lastIndexOrRaise (arr: t 'a): int => {
  let lastIndex = count arr - 1;
  if (lastIndex >= 0) lastIndex
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

let isEmpty (arr: t 'a): bool => (count arr) === 0;

let isNotEmpty (arr: t 'a): bool => (count arr) !== 0;

let ofUnsafe (arr: array 'a): (t 'a) => arr;

let reduceImpl
    while_::(predicate: 'acc => 'a => bool)
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

let reduce
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (arr: t 'a): 'acc =>
  reduceImpl while_::predicate f acc arr;

let reduceWithIndexImpl
    while_::(predicate: 'acc => int => 'a => bool)
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

let reduceWithIndex
    while_::(predicate: 'acc => int => 'a => bool)=Functions.alwaysTrue3
    (f: 'acc => int => 'a => 'acc)
    (acc: 'acc)
    (arr: t 'a): 'acc => reduceWithIndexImpl while_::predicate f acc arr;

let reduceReversedImpl
    while_::(predicate: 'acc => 'a => bool)
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

let reduceReversed
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (arr: t 'a): 'acc =>
  reduceReversedImpl while_::predicate f acc arr;

let reduceReversedWithIndexImpl
    while_::(predicate: 'acc => int => 'a => bool)
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

let reduceReversedWithIndex
    while_::(predicate: 'acc => int => 'a => bool)=Functions.alwaysTrue3
    (f: 'acc => int => 'a => 'acc)
    (acc: 'acc)
    (arr: t 'a): 'acc =>
  reduceReversedWithIndexImpl while_::predicate f acc arr;

let removeLastOrRaise (arr: t 'a): (t 'a) => {
  let count = count arr;

  if (count === 0) (failwith "Array is empty")
  else if (count === 1) [||]
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

let skip (startIndex: int) (arr: t 'a): (t 'a) => {
  let arrCount = count arr;

  if (startIndex < 0) (failwith "startIndex is < 0")
  else if (startIndex >= arrCount) [||]
  else {
    let newCount = arrCount - startIndex;
    Array.sub arr startIndex newCount;
  };
};

let take (newCount: int) (arr: t 'a): (t 'a) => {
  let arrCount = count arr;

  if (newCount < 0) (failwith "count is < 0")
  else if (newCount === 0) [||]
  else Array.sub arr 0 newCount;
};

let iterator: Iterable.Iterator.t 'a (array 'a) = { reduce: reduceImpl };

let toIterable (arr: t 'a): (Iterable.t 'a) =>
  if (isEmpty arr) (Iterable.empty ())
  else Iterable.Iterable arr iterator;

let iteratorReversed: Iterable.Iterator.t 'a (array 'a) = { reduce: reduceReversedImpl };

let toIterableReversed (arr: t 'a): (Iterable.t 'a) =>
  if (isEmpty arr) (Iterable.empty ())
  else Iterable.Iterable arr iteratorReversed;

let keyedIterator: KeyedIterable.KeyedIterator.t int 'a (array 'a) = { reduce: reduceWithIndexImpl };

let toKeyedIterable (arr: t 'a): (KeyedIterable.t int 'a) =>
  if (isEmpty arr) (KeyedIterable.empty ())
  else KeyedIterable.KeyedIterable arr keyedIterator;

let keyedIteratorReversed: KeyedIterable.KeyedIterator.t int 'a (array 'a) = { reduce: reduceReversedWithIndexImpl };

let toKeyedIterableReversed (arr: t 'a): (KeyedIterable.t int 'a) =>
  if (isEmpty arr) (KeyedIterable.empty ())
  else KeyedIterable.KeyedIterable arr keyedIteratorReversed;

let toSequenceReversed (arr: t 'a): (Sequence.t 'a) =>
  if (isEmpty arr) (Sequence.empty ())
  else {
    let rec loop index => fun () =>
      if (index < 0) Sequence.Completed
      else Sequence.Next arr.(index) (loop (index - 1));
    loop (count arr - 1);
  };

let toSequence (arr: t 'a): (Sequence.t 'a) =>
  if (isEmpty arr) (Sequence.empty ())
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

let toSequenceWithIndexReversed (arr: t 'a): (Sequence.t (int, 'a)) => {
  let arrCount = count arr;
  let rec loop index => fun () =>
    if (index >= 0) (Sequence.Next (index, arr.(index)) (loop (index - 1)))
    else Sequence.Completed;
  loop (arrCount - 1);
};

let collectionOps: Collection.Ops.t 'a (t 'a) = {
  count,
  toIterable,
  toSequence,
};

let toCollection (arr: t 'a): (Collection.t 'a) =>
  if (isEmpty arr) (Collection.empty ())
  else Collection.Collection arr collectionOps;

let seqCollectionOps: SequentialCollection.Ops.t 'a (t 'a) = {
  count,
  first,
  firstOrRaise,
  toCollection,
  toIterable,
  toSequence,
};

let toSequentialCollection (arr: t 'a): (SequentialCollection.t 'a) =>
  if (isEmpty arr) (SequentialCollection.empty ())
  else SequentialCollection.SequentialCollection arr seqCollectionOps;

let containsKey (index: int) (arr: t 'a): bool =>
  index >= 0 && index < count arr;

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

let mapOps (): ImmMap.Ops.t int 'v (t 'v) => {
  containsKey,
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

let toMap (arr: t 'a): (ImmMap.t int 'a) =>
  if (isEmpty arr) (ImmMap.empty ())
  else ImmMap.Map arr (mapOps ());

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

let toNavigableCollection (arr: t 'a): (NavigableCollection.t 'a) =>
  if (isEmpty arr) (NavigableCollection.empty ())
  else NavigableCollection.NavigableCollection arr navCollectionOps;

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

let update (index: int) (item: 'a) (arr: t 'a): (t 'a) => {
  let arrCount = count arr;

  Preconditions.failIfOutOfRange arrCount index;

  let clone = Array.copy arr;
  clone.(index) = item;
  clone
};

let updateWith (index: int) (f: 'a => 'a) (arr: t 'a): (t 'a) => {
  let count = count arr;

  Preconditions.failIfOutOfRange count index;

  let clone = Array.copy arr;
  clone.(index) = f arr.(index);
  clone
};
