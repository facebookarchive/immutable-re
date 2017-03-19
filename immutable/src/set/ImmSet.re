/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Functions.Operators;

type t 'a = {
  contains: 'a => bool,
  count: int,
  iterator: (Iterator.t 'a),
  sequence: (Sequence.t 'a),
};

let contains (value: 'a) ({ contains }: t 'a): bool =>
  contains value;

let count ({ count }: t 'a): int => count;

let empty: (t 'a) = {
  contains: fun _ => false,
  count: 0,
  iterator: Iterator.empty,
  sequence: Sequence.empty,
};

let every (f: 'a => bool) ({ iterator }: t 'a): bool =>
  iterator |> Iterator.every f;

let equals (that: t 'a) (this: t 'a): bool =>
  if (this === that) true
  else if (this.count != that.count) false
  else this |> every that.contains;

let find (f: 'a => bool) ({ iterator }: t 'a): (option 'a) =>
  iterator |> Iterator.find f;

let findOrRaise (f: 'a => bool) ({ iterator }: t 'a): 'a =>
  iterator |> Iterator.findOrRaise f;

let forEach (f: 'a => unit) ({ iterator }: t 'a): unit =>
  iterator |> Iterator.forEach f;

let forEachWhile (predicate: 'a => bool) (f: 'a => unit) ({ iterator }: t 'a): unit =>
  iterator |> Iterator.forEachWhile predicate f;

let hashWith (hash: Hash.t 'a) ({ iterator }: t 'a): int =>
  iterator |> Iterator.reduce (fun acc next => acc + hash next) 0;

let hash (set: t 'a): int =>
  hashWith Hash.structural set;

let isEmpty ({ count }: t 'a): bool =>
  count == 0;

let isNotEmpty ({ count }: t 'a): bool =>
  count != 0;

let none (f: 'a => bool) ({ iterator }: t 'a): bool =>
  iterator |> Iterator.none f;

let ofOptionWith (equals: Equality.t 'a) (opt: option 'a): (t 'a) => {
  contains: fun v => Option.containsWith equals v opt,
  count: Option.count opt,
  iterator: Iterator.ofOption opt,
  sequence: Sequence.ofOption opt,
};

let ofOption (opt: option 'a): (t 'a) =>
  ofOptionWith Equality.structural opt;

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) ({ iterator }: t 'a): 'acc =>
  iterator |> Iterator.reduce f acc;

let reduceWhile
    (predicate: 'acc => 'a => bool)
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    ({ iterator }: t 'a): 'acc =>
  iterator |> Iterator.reduceWhile predicate f acc;

let some (f: 'a => bool) ({ iterator }: t 'a): bool =>
  iterator |> Iterator.some f;

let toIterator ({ iterator } as set: t 'a): (Iterator.t 'a) =>
  if (set === empty) Iterator.empty
  else iterator;

let toKeyedIterator ({ iterator }: t 'a): (KeyedIterator.t 'a 'a) =>
  if (iterator == Iterator.empty) KeyedIterator.empty
  else {
    reduceWhile: fun predicate f acc => iterator |> Iterator.reduceWhile
      (fun acc next => predicate acc next next)
      (fun acc next => f acc next next)
      acc
  };

let toSequence ({ sequence }: t 'a): (Sequence.t 'a) => sequence;

let toSet (set: t 'a): (t 'a) => set;

let intersect (this: t 'a) (that: t 'a): (Iterator.t 'a) =>
  this |> toIterator |> Iterator.filter (that.contains);

let subtract (this: t 'a) (that: t 'a): (Iterator.t 'a) =>
  this |> toIterator |> Iterator.filter (that.contains >> not);

let union (this: t 'a) (that: t 'a): (Iterator.t 'a) => Iterator.concat [
  this |> toIterator,
  subtract that this,
];
