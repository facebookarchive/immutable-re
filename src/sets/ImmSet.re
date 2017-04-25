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

type s 'set 'a = {
  contains: 'a => 'set => bool,
  count: 'set => int,
  reduce: 'acc . while_::('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => 'set => 'acc,
  toSequence: 'set => Sequence.t 'a,
};

type t 'a =
  | Empty
  | Instance 'set (s 'set 'a): t 'a;

type set 'a = t 'a;

module type S = {
  type a;
  type t;

  include Collection.S with type a := a and type t := t;
  include Equatable.S with type t := t;

  let contains: a => t => bool;
  let toSet: t => set a;
};

module type S1 = {
  type t 'a;

  include Collection.S1 with type t 'a := t 'a;

  let contains: 'a => (t 'a) => bool;
  let equals: Equality.t (t 'a);
  let toSet: (t 'a) => set 'a;
};

let module Make = fun (Base: {
  type a;
  type t;

  let contains: a => t => bool;
  let count: t => int;
  let reduce: while_::('acc => a => bool) => ('acc => a => 'acc) => 'acc => t => 'acc;
  let toSequence: t => Sequence.t a;
}) => ({
  include Base;

  include (Collection.Make Base: Collection.S with type t := t and type a := a);

  let setBase: s t a = {
    contains,
    count,
    reduce: Base.reduce,
    toSequence,
  };

  let equals (this: t) (that: t): bool =>
    if (this === that) true
    else if ((count this) !== (count that)) false
    else this |> reduce
      while_::(fun acc _ => acc) (fun _ => flip contains that) true;

  let toSet (set: t): (set a) =>
    if (isEmpty set) Empty
    else Instance set setBase;

}: S with type t := Base.t and type a := Base.a);

let module Make1 = fun (Base: {
  type t 'a;

  let contains: 'a => t 'a => bool;
  let count: t 'a => int;
  let reduce: while_::('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => t 'a => 'acc;
  let toSequence: t 'a => Sequence.t 'a;
}) => ({
  include Base;

  include (Collection.Make1 Base: Collection.S1 with type t 'a := t 'a);

  let setBase: s (t 'a) 'a = {
    contains,
    count,
    reduce: Base.reduce,
    toSequence,
  };

  let equals (this: t 'a) (that: t 'a): bool =>
    if (this === that) true
    else if ((count this) !== (count that)) false
    else this |> reduce
      while_::(fun acc _ => acc) (fun _ => flip contains that) true;

  let toSet (set: t 'a): (set 'a) =>
    if (isEmpty set) Empty
    else Instance set setBase;

}: S1 with type t 'a := Base.t 'a);

include(Make1 {
  type nonrec t 'a = t 'a;

  let contains (value: 'a) (set: t 'a): bool => switch set {
    | Empty => false
    | Instance set { contains } => contains value set
  };

  let count (set: t 'a): int => switch set {
    | Empty => 0
    | Instance set { count } => count set
  };

  let reduce
      while_::(predicate: 'acc => 'a => bool)
      (f: 'acc => 'a => 'acc)
      (acc: 'acc)
      (collection: t 'a): 'acc => switch collection {
    | Empty => acc
    | Instance collection { reduce } =>
        collection |> reduce while_::predicate f acc;
  };

  let toSequence (set: t 'a): (Sequence.t 'a) => switch set {
    | Empty => Sequence.empty ()
    | Instance set { toSequence } => toSequence set
  };
}: S1 with type t 'a := t 'a);

let empty (): (t 'a) => Empty;

let intersect (this: t 'a) (that: t 'a): (Iterable.t 'a) =>
  this |> toIterable |> Iterable.filter (flip contains that);

let subtract (this: t 'a) (that: t 'a): (Iterable.t 'a) =>
  this |> toIterable |> Iterable.filter (flip contains that >> not);

let toSet (set: t 'a): (t 'a) => set;

let union (this: t 'a) (that: t 'a): (Iterable.t 'a) => Iterable.concat [
  this |> toIterable,
  subtract that this,
];
