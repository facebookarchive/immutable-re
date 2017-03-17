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
  every: ('a => bool) => bool,
  find: ('a => bool) => 'a,
  forEach: ('a => unit) => unit,
  none: ('a => bool) =>  bool,
  reduce: 'acc . ('acc => 'a => 'acc) => 'acc => 'acc,
  some: ('a => bool) => bool,
  toSequence: (Sequence.t 'a),
  tryFind: ('a => bool) => (option 'a),
};

let contains (value: 'a) ({ contains }: t 'a): bool =>
  contains value;

let count ({ count }: t 'a): int => count;

let empty: (t 'a) = {
  contains: fun _ => false,
  count: 0,
  every: fun _ => true,
  find: fun _ => failwith "set is empty",
  forEach: fun _ => (),
  none: fun _ => true,
  reduce: fun _ acc => acc,
  some: fun _ => false,
  toSequence: Sequence.empty,
  tryFind: Functions.alwaysNone,
};

let equals (that: t 'a) (this: t 'a): bool =>
  if (this === that) true
  else if (this.count != that.count) false
  else this.every that.contains;

let every (f: 'a => bool) ({ every }: t 'a): bool =>
  every f;

let find (f: 'a => bool) ({ find }: t 'a): 'a =>
  find f;

let forEach (f: 'a => unit) ({ forEach }: t 'a): unit =>
  forEach f;

let hashWith (hash: Hash.t 'a) ({ reduce }: t 'a): int =>
  reduce (fun acc next => acc + hash next) 0;

let hash (set: t 'a): int =>
  hashWith Hash.structural set;

let isEmpty ({ count }: t 'a): bool =>
  count == 0;

let isNotEmpty ({ count }: t 'a): bool =>
  count != 0;

let none (f: 'a => bool) ({ none }: t 'a): bool =>
  none f;

let ofOptionWith (equals: Equality.t 'a) (opt: option 'a): (t 'a) => {
  contains: fun v => Option.containsWith equals v opt,
  count: Option.count opt,
  every: fun f => Option.every f opt,
  find: fun f => Option.find f opt,
  forEach: fun f => Option.forEach f opt,
  none: fun f => Option.none f opt,
  reduce: fun f acc => Option.reduce f acc opt,
  some: fun f => Option.some f opt,
  toSequence: Sequence.ofOption opt,
  tryFind: fun f => Option.tryFind f opt,
};

let ofOption (opt: option 'a): (t 'a) =>
  ofOptionWith Equality.structural opt;

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) ({ reduce }: t 'a): 'acc =>
  reduce f acc;

let some (f: 'a => bool) ({ some }: t 'a): bool =>
  some f;

let toIterator ({ reduce } as set: t 'a): (Iterator.t 'a) =>
  if (set === empty) Iterator.empty
  else {
    reduce: reduce
  };

let toKeyedIterator ({ reduce } as set: t 'a): (KeyedIterator.t 'a 'a) =>
  if (set == empty) KeyedIterator.empty
  else {
    reduce: fun f acc => reduce
      (fun acc next => f acc next next)
      acc
    };

let toSequence ({ toSequence }: t 'a): (Sequence.t 'a) => toSequence;

let tryFind (f: 'a => bool) ({ tryFind }: t 'a): (option 'a) =>
  tryFind f;

let intersect (this: t 'a) (that: t 'a): (Iterator.t 'a) =>
  this |> toIterator |> Iterator.filter (that.contains);

let subtract (this: t 'a) (that: t 'a): (Iterator.t 'a) =>
  this |> toIterator |> Iterator.filter (that.contains >> not);

let union (this: t 'a) (that: t 'a): (Iterator.t 'a) => Iterator.concat [
  this |> toIterator,
  subtract that this,
];
