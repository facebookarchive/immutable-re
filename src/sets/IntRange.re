/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type a = int;

type t = {
  count: int,
  start: int,
};

let emptyInstance: t = {
  start: 0,
  count: 0,
};

let empty (): t => emptyInstance;

let create start::(start: int) count::(count: int): t => {
  Preconditions.failIf "count must be >= 0" (count < 0);
  if (count === 0) emptyInstance
  else { start, count };
};

let compare
    ({ count: thisCount, start: thisStart }: t)
    ({ count: thatCount, start: thatStart }: t): Ordering.t =>
  if (thisStart > thatStart) Ordering.greaterThan
  else if (thisStart < thatStart) Ordering.lessThan
  else if (thisCount > thatCount) Ordering.greaterThan
  else if (thisCount < thatCount) Ordering.lessThan
  else Ordering.equal;

let contains (value: int) ({ count, start }: t): bool =>
  value >= start && value < (start + count);

let equals (this: t) (that: t): bool =>
  this.start === that.start && this.count === that.count;

let first ({ count, start }: t): (option int) =>
  if (count === 0) None
  else (Some start);

let firstOrRaise ({ count, start }: t): int =>
  if (count === 0) (failwith "empty")
  else start;

let last ({ count, start }: t): (option int) =>
  if (count === 0) None
  else (start + count - 1) |> Option.return;

let lastOrRaise ({ count, start }: t): int =>
  if (count === 0) (failwith "empty")
  else start + count - 1;

include (Collection.Make {
  type nonrec a = int;
  type nonrec t = t;

  let count ({ count }: t): int => count;

  let reduce
      while_::(predicate: 'acc => int => bool)
      (f: 'acc => int => 'acc)
      (acc: 'acc)
      ({ count, start }: t): 'acc => {
    let rec recurse predicate f start count acc =>
      if (count === 0) acc
      else if (predicate acc start |> not) acc
      else {
        let acc = f acc start;
        recurse predicate f (start + 1) (count - 1) acc;
      };
    recurse predicate f start count acc;
  };

  let toSequence ({ count, start }: t): (Sequence.t int) => {
    let rec recurse start count => fun () =>
      if (count === 0) Sequence.Completed
      else Sequence.Next start (recurse (start + 1) (count - 1));
    recurse start count
  };
}: Collection.S with type t := t and type a := a);

let reduceReversedImpl
    while_::(predicate: 'acc => int => bool)
    (f: 'acc => int => 'acc)
    (acc: 'acc)
    ({ count, start }: t): 'acc => {
  let rec recurse predicate f start count acc =>
    if (count === 0) acc
    else if (predicate acc start |> not) acc
    else {
      let acc = f acc start;
      recurse predicate f (start - 1) (count - 1) acc;
    };
  recurse predicate f (start + count - 1) count acc;
};

let reduceReversed
    while_::(predicate: 'acc => int => bool)=Functions.alwaysTrue2
    (f: 'acc => int => 'acc)
    (acc: 'acc)
    (set: t): 'acc =>
  reduceReversedImpl while_::predicate f acc set;

let hash ({ start, count }: t): int =>
  start + count;

let toSequenceReversed ({ count, start }: t): (Sequence.t int) => {
  let rec recurse start count => fun () =>
    if (count === 0) Sequence.Completed
    else Sequence.Next start (recurse (start - 1) (count - 1));
  recurse (start + count - 1) count
};

let iterableReversedBase: Iterable.s t int = { reduce: reduceReversedImpl };

let toIterableReversed (set: t): (Iterable.t int) =>
  if (isEmpty set) (Iterable.empty ())
  else Iterable.create iterableReversedBase set;

let seqCollectionOps: SequentialCollection.Ops.t int t = {
  count,
  first,
  firstOrRaise,
  toCollection,
  toIterable,
  toSequence,
};

let toSequentialCollection (set: t): (SequentialCollection.t int) =>
  if (isEmpty set) (SequentialCollection.empty ())
  else SequentialCollection.SequentialCollection set seqCollectionOps;

let navCollectionOps: NavigableCollection.Ops.t int t = {
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

let toNavigableCollection (set: t): (NavigableCollection.t int) =>
  if (isEmpty set) (NavigableCollection.empty ())
  else NavigableCollection.NavigableCollection set navCollectionOps;

let setOps: ImmSet.Ops.t int t = {
  contains,
  count,
  toCollection,
  toIterable,
  toSequence,
};

let toSet (set: t): (ImmSet.t int) =>
  if (isEmpty set) (ImmSet.empty ())
  else ImmSet.Set set setOps;

let navigableSetOps: NavigableSet.Ops.t int t = {
  contains,
  count,
  first,
  firstOrRaise,
  last,
  lastOrRaise,
  toCollection,
  toSequentialCollection,
  toIterable,
  toIterableReversed,
  toNavigableCollection,
  toSequence,
  toSequenceReversed,
  toSet,
};

let toNavigableSet (set: t): (NavigableSet.t int) =>
  if (isEmpty set) (NavigableSet.empty ())
  else NavigableSet.NavigableSet set navigableSetOps;
