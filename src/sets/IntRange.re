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

include (NavigableSet.Make {
  type nonrec a = int;
  type nonrec t = t;

  let contains (value: int) ({ count, start }: t): bool =>
    value >= start && value < (start + count);

  let count ({ count }: t): int => count;

  let firstOrRaise ({ count, start }: t): int =>
    if (count === 0) (failwith "empty")
    else start;

  let lastOrRaise ({ count, start }: t): int =>
    if (count === 0) (failwith "empty")
    else start + count - 1;

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

  let reduceReversed
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

  let toSequence ({ count, start }: t): (Sequence.t int) => {
    let rec recurse start count => fun () =>
      if (count === 0) Sequence.Completed
      else Sequence.Next start (recurse (start + 1) (count - 1));
    recurse start count
  };

  let toSequenceReversed ({ count, start }: t): (Sequence.t int) => {
    let rec recurse start count => fun () =>
      if (count === 0) Sequence.Completed
      else Sequence.Next start (recurse (start - 1) (count - 1));
    recurse (start + count - 1) count
  };
}: NavigableSet.S with type t := t and type a := a);

let emptyInstance: t = {
  start: 0,
  count: 0,
};

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

let empty (): t => emptyInstance;

let equals (this: t) (that: t): bool =>
  this.start === that.start && this.count === that.count;

let hash ({ start, count }: t): int =>
  start + count;
