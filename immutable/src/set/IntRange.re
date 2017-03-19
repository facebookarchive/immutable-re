/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t = {
  count: int,
  start: int,
};

let empty: t = {
  start: 0,
  count: 0,
};

let create (start: int) (count: int): t => {
  Preconditions.failIf "count must be >= 0" (count < 0);
  if (count == 0) empty
  else { start, count };
};

let contains (value: int) ({ count, start }: t): bool =>
  value >= start && value < (start + count);

let count ({ count }: t): int => count;

let equals (this: t) (that: t): bool =>
  this.start == that.start && this.count == that.count;

let every (f: int => bool) ({ count, start }: t): bool => {
  let rec recurse f start count =>
     if (count == 0) true
     else if (f start) (recurse f (start + 1) (count - 1))
     else false;

  recurse f start count;
};

let find (f: int => bool) ({ count, start }: t): (option int) => {
  let rec recurse f start count =>
    if (count == 0) None
    else if (f start) (Some start)
    else recurse f (start + 1) (count - 1);

  recurse f start count;
};

let findOrRaise (f: int => bool) ({ count, start }: t): int => {
  let rec recurse f start count =>
    if (count == 0) (failwith "not found")
    else if (f start) start
    else recurse f (start + 1) (count - 1);

  recurse f start count;
};

let findRight (f: int => bool) ({ count, start }: t): (option int) => {
  let rec recurse f start count =>
    if (count == 0) None
    else if (f start) (Some start)
    else recurse f (start - 1) (count - 1);

  recurse f (count - 1) count;
};

let findRightOrRaise (f: int => bool) ({ count, start }: t): int => {
  let rec recurse f start count =>
    if (count == 0) (failwith "not found")
    else if (f start) start
    else recurse f (start - 1) (count - 1);

  recurse f (count - 1) count;
};

let first ({ count, start }: t): (option int) =>
  if (count == 0) None
  else (Some start);

let firstOrRaise ({ count, start }: t): int =>
  if (count == 0) (failwith "empty")
  else start;

let forEach (f: int => unit) ({ count, start }: t) => {
  let rec recurse f start count =>
    if (count == 0) ()
    else {
      f start;
      recurse f (start + 1) (count - 1);
    };
  recurse f start count;
};

let forEachWhile (predicate: int => bool) (f: int => unit) ({ count, start }: t) => {
  let rec recurse f start count =>
    if (count == 0) ()
    else if (predicate start) {
      f start;
      recurse f (start + 1) (count - 1);
    }
    else ();
  recurse f start count;
};

let forEachRight (f: int => unit) ({ count, start }: t) => {
  let rec recurse f start count =>
    if (count == 0) ()
    else {
      f start;
      recurse f (start - 1) (count - 1);
    };
  recurse f (count - 1) count;
};

let forEachRightWhile (predicate: int => bool) (f: int => unit) ({ count, start }: t) => {
  let rec recurse f start count =>
    if (count == 0) ()
    else if (predicate start) {
      f start;
      recurse f (start - 1) (count - 1);
    }
    else ();
  recurse f (count - 1) count;
};

let isEmpty ({ count }: t): bool => count == 0;

let isNotEmpty ({ count }: t): bool => count != 0;

let last ({ count, start }: t): (option int) =>
  if (count == 0) None
  else (start + count - 1) |> Option.return;

let lastOrRaise ({ count, start }: t): int =>
  if (count == 0) (failwith "empty")
  else start + count - 1;

let none (f: int => bool) ({ count, start }: t): bool => {
  let rec recurse f start count =>
     if (count == 0) true
     else if (f start) false
     else (recurse f (start + 1) (count - 1));

  recurse f start count;
};

let reduce (f: 'acc => int => 'acc) (acc: 'acc) ({ count, start }: t): 'acc => {
  let rec recurse f start count acc =>
    if (count == 0) acc
    else {
      let acc = f acc start;
      recurse f (start + 1) (count - 1) acc;
    };
  recurse f start count acc;
};

let reduceWhile
    (predicate: 'acc => int => bool)
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

let reduceRight (f: 'acc => int => 'acc) (acc: 'acc) ({ count, start }: t): 'acc => {
  let rec recurse f start count acc =>
    if (count == 0) acc
    else {
      let acc = f acc start;
      recurse f (start - 1) (count - 1) acc;
    };
  recurse f (start + count - 1) count acc;
};

let reduceRightWhile
    (predicate: 'acc => int => bool)
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

let hash (set: t): int =>
  set |> reduce (Hash.reducer Hash.structural) Hash.initialValue;

let some (f: int => bool) ({ count, start }: t): bool => {
  let rec recurse f start count =>
     if (count == 0) false
     else if (f start) true
     else (recurse f (start + 1) (count - 1));

  recurse f start count;
};

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
  else { reduceWhile: fun predicate f acc => reduceWhile predicate f acc set };

let toIteratorRight (set: t): (Iterator.t int) =>
  if (isEmpty set) Iterator.empty
  else { reduceWhile: fun predicate f acc => reduceRightWhile predicate f acc set };

let toKeyedIterator (set: t): (KeyedIterator.t int int) =>
  if (isEmpty set) KeyedIterator.empty
  else { reduceWhile: fun predicate f acc => set |> reduceWhile
    (fun acc next => predicate acc next next)
    (fun acc next => f acc next next)
    acc
  };

let toKeyedIteratorRight (set: t): (KeyedIterator.t int int) =>
  if (isEmpty set) KeyedIterator.empty
  else { reduceWhile: fun predicate f acc => set |> reduceRightWhile
    (fun acc next => predicate acc next next)
    (fun acc next => f acc next next)
    acc
  };

let toSet (set: t): (ImmSet.t int) => {
  contains: fun v => contains v set,
  count: count set,
  iterator: toIterator set,
  sequence: toSequence set,
};

let toMap (set: t): (ImmMap.t int int) =>
  set |> toSet |> ImmMap.ofSet;
