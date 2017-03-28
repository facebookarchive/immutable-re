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
  iterator: unit => (Iterator.t 'a),
  sequence: unit => (Sequence.t 'a),
};

let contains (value: 'a) ({ contains }: t 'a): bool =>
  contains value;

let count ({ count }: t 'a): int => count;

let empty (): (t 'a) => {
  contains: fun _ => false,
  count: 0,
  iterator: Iterator.empty,
  sequence: Sequence.empty,
};

let equals (this: t 'a) (that: t 'a): bool =>
  if (this === that) true
  else if (this.count !== that.count) false
  else this.iterator () |> Iterator.Reducer.every that.contains;

let isEmpty ({ count }: t 'a): bool =>
  count === 0;

let isNotEmpty ({ count }: t 'a): bool =>
  count !== 0;

let reduce
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    ({ iterator }: t 'a): 'acc =>
  iterator () |> Iterator.reduce while_::predicate f acc;

let toIterator ({ iterator }: t 'a): (Iterator.t 'a) =>
  iterator ();

let toSequence ({ sequence }: t 'a): (Sequence.t 'a) =>
  sequence ();

let toSet (set: t 'a): (t 'a) => set;

let intersect (this: t 'a) (that: t 'a): (Iterator.t 'a) =>
  this |> toIterator |> Iterator.filter (that.contains);

let subtract (this: t 'a) (that: t 'a): (Iterator.t 'a) =>
  this |> toIterator |> Iterator.filter (that.contains >> not);

let union (this: t 'a) (that: t 'a): (Iterator.t 'a) => Iterator.concat [
  this |> toIterator,
  subtract that this,
];

let module Reducer = Reducer.Make1 {
  type nonrec t 'a = t 'a;
  let reduce = reduce;
};
