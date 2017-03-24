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

let empty: t = {
  start: 0,
  count: 0,
};

let create start::(start: int) count::(count: int): t => {
  Preconditions.failIf "count must be >= 0" (count < 0);
  if (count == 0) empty
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

let count ({ count }: t): int => count;

let equals (this: t) (that: t): bool =>
  this.start == that.start && this.count == that.count;

let first ({ count, start }: t): (option int) =>
  if (count == 0) None
  else (Some start);

let firstOrRaise ({ count, start }: t): int =>
  if (count == 0) (failwith "empty")
  else start;

let isEmpty ({ count }: t): bool => count == 0;

let isNotEmpty ({ count }: t): bool => count != 0;

let last ({ count, start }: t): (option int) =>
  if (count == 0) None
  else (start + count - 1) |> Option.return;

let lastOrRaise ({ count, start }: t): int =>
  if (count == 0) (failwith "empty")
  else start + count - 1;

let reduce
    while_::(predicate: 'acc => int => bool)=Functions.alwaysTrue2
    (f: 'acc => int => 'acc)
    (acc: 'acc)
    ({ count, start }: t): 'acc => {
  let rec recurse predicate f start count acc =>
    if (count == 0) acc
    else if (predicate acc start |> not) acc
    else {
      let acc = f acc start;
      recurse predicate f (start + 1) (count - 1) acc;
    };
  recurse predicate f start count acc;
};

let reduceRight
    while_::(predicate: 'acc => int => bool)=Functions.alwaysTrue2
    (f: 'acc => int => 'acc)
    (acc: 'acc)
    ({ count, start }: t): 'acc => {
  let rec recurse predicate f start count acc =>
    if (count == 0) acc
    else if (predicate acc start |> not) acc
    else {
      let acc = f acc start;
      recurse predicate f (start - 1) (count - 1) acc;
    };
  recurse predicate f (start + count - 1) count acc;
};

let hash ({ start, count }: t): int =>
  start + count;

let toSequence ({ count, start }: t): (Sequence.t int) => {
  let rec recurse start count => fun () =>
    if (count == 0) Sequence.Completed
    else Sequence.Next start (recurse (start + 1) (count - 1));
  recurse start count
};

let toSequenceRight ({ count, start }: t): (Sequence.t int) => {
  let rec recurse start count => fun () =>
    if (count == 0) Sequence.Completed
    else Sequence.Next start (recurse (start - 1) (count - 1));
  recurse (start + count - 1) count
};

let toIterator (set: t): (Iterator.t int) =>
  if (isEmpty set) Iterator.empty
  else { reduce: fun predicate f acc => reduce while_::predicate f acc set };

let toIteratorRight (set: t): (Iterator.t int) =>
  if (isEmpty set) Iterator.empty
  else { reduce: fun predicate f acc => reduceRight while_::predicate f acc set };

let toSet (set: t): (ImmSet.t int) => {
  contains: fun v => contains v set,
  count: count set,
  iterator: toIterator set,
  sequence: toSequence set,
};

let toMap (set: t): (ImmMap.t int int) =>
  set |> toSet |> ImmMap.ofSet;

let module Reducer = Reducer.Make {
  type a = int;
  type nonrec t = t;
  let reduce = reduce;
};
