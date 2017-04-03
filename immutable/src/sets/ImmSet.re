/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Functions;
open Functions.Operators;

let module Ops = {
  type t 'a 'set = {
    contains: 'a => 'set => bool,
    count: 'set => int,
    toCollection: 'set => Collection.t 'a,
    toIterable: 'set => Iterable.t 'a,
    toSequence: 'set => Sequence.t 'a,
  };
};

type t 'a =
  | Empty
  | Set 'set (Ops.t 'a 'set): t 'a;

let contains (value: 'a) (set: t 'a): bool => switch set {
  | Empty => false
  | Set set { contains } => contains value set
};

let count (set: t 'a): int => switch set {
  | Empty => 0
  | Set set { count } => count set
};

let empty (): (t 'a) => Empty;

let isEmpty (set: t 'a): bool =>
  (count set) === 0;

let isNotEmpty (set: t 'a): bool =>
  (count set) !== 0;

let toCollection (set: t 'a): (Collection.t 'a) => switch set {
  | Empty => Collection.empty ()
  | Set set { toCollection } => toCollection set
};

let toIterable (set: t 'a): (Iterable.t 'a) => switch set {
  | Empty => Iterable.empty ()
  | Set set { toIterable } => toIterable set
};

let toSequence (set: t 'a): (Sequence.t 'a) => switch set {
  | Empty => Sequence.empty ()
  | Set set { toSequence } => toSequence set
};

let toSet (set: t 'a): (t 'a) => set;

let equals (this: t 'a) (that: t 'a): bool => switch (this, that) {
  | (Set _ _, Set _ _) =>
      if (this === that) true
      else if ((count this) !== (count that)) false
      else this |> toIterable |> Iterable.every (flip contains that)
  | _ => false
};

let reduce
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (set: t 'a): 'acc =>
  set |> toIterable |> Iterable.reduce while_::predicate f acc;

let intersect (this: t 'a) (that: t 'a): (Iterable.t 'a) =>
  this |> toIterable |> Iterable.filter (flip contains that);

let subtract (this: t 'a) (that: t 'a): (Iterable.t 'a) =>
  this |> toIterable |> Iterable.filter (flip contains that >> not);

let union (this: t 'a) (that: t 'a): (Iterable.t 'a) => Iterable.concat [
  this |> toIterable,
  subtract that this,
];
