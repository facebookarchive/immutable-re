/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type s 'collection 'a = {
  count: 'collection => int,
  reduce: 'acc . while_::('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => 'collection => 'acc,
  toSequence: 'collection => Sequence.t 'a,
};

type t 'a =
  | Empty
  | Instance 'collection (s 'collection 'a): t 'a;

type collection 'a = t 'a;

module type SGeneric = {
  type elt 'a;
  type t 'a;

  include Iterable.SGeneric with type elt 'a := elt 'a and type t 'a := t 'a;

  let count: t 'a => int;
  let isEmpty: t 'a => bool;
  let isNotEmpty: t 'a => bool;
  let toCollection: t 'a => collection (elt 'a);
  let toSequence: t 'a => (Sequence.t (elt 'a));
};

module type S = {
  type a;
  type t;

  include Iterable.S with type a := a and type t := t;

  let count: t => int;
  let isEmpty: t => bool;
  let isNotEmpty: t => bool;
  let toCollection: t => collection a;
  let toSequence: t => (Sequence.t a);
};

module type S1 = {
  type t 'a;

  include Iterable.S1 with type t 'a := t 'a;

  let count: (t 'a) => int;
  let isEmpty: (t 'a) => bool;
  let isNotEmpty: (t 'a) => bool;
  let toCollection: (t 'a) => (collection 'a);
  let toSequence: (t 'a) => (Sequence.t 'a);
};

let module MakeGeneric = fun (Base: {
  type elt 'a;
  type t 'a;

  let count: t 'a => int;
  let reduce: while_::('acc => elt 'a => bool) => ('acc => elt 'a => 'acc) => 'acc => t 'a => 'acc;
  let toSequence: t 'a => Sequence.t (elt 'a);
}) => ({
  include Base;

  let isEmpty (collection: t 'a): bool =>
    (count collection) === 0;

  let isNotEmpty (collection: t 'a): bool =>
    (count collection) !== 0;

  include (Iterable.MakeGeneric {
    type nonrec elt 'a = elt 'a;
    type nonrec t 'a = t 'a;

    let isEmpty = isEmpty;
    let reduce = reduce;
  }: Iterable.SGeneric with type t 'a := t 'a and type elt 'a := elt 'a);

  let collectionBase: s (t 'a) (elt 'a) = {
    count,
    reduce: Base.reduce,
    toSequence,
  };

  let toCollection (collection: t 'a): (collection (elt 'a)) =>
    if (isEmpty collection) Empty
    else Instance collection collectionBase;
}: SGeneric with type t 'a := Base.t 'a and type elt 'a := Base.elt 'a);

let module Make = fun (Base: {
  type a;
  type t;

  let count: t => int;
  let reduce: while_::('acc => a => bool) => ('acc => a => 'acc) => 'acc => t => 'acc;
  let toSequence: t => Sequence.t a;
}) => ((MakeGeneric {
  type t 'a   = Base.t;
  type elt 'a = Base.a;

  let count = Base.count;
  let reduce = Base.reduce;
  let toSequence = Base.toSequence;
}): S with type t := Base.t and type a := Base.a);

let module Make1 = fun (Base: {
  type t 'a;

  let count: t 'a => int;
  let reduce: while_::('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => t 'a => 'acc;
  let toSequence: t 'a => Sequence.t 'a;
}) => ((MakeGeneric {
  type t 'a   = Base.t 'a;
  type elt 'a = 'a;

  let count = Base.count;
  let reduce = Base.reduce;
  let toSequence = Base.toSequence;
}): S1 with type t 'a := Base.t 'a);

include(Make1 {
  type nonrec t 'a = t 'a;

  let count (collection: t 'a): int => switch collection {
    | Empty => 0
    | Instance collection { count } => count collection
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

  let toSequence (collection: t 'a): (Sequence.t 'a) => switch collection {
    | Empty => Sequence.empty ()
    | Instance collection { toSequence } => toSequence collection
  };
}: S1 with type t 'a := t 'a);

let empty (): (t 'a) => Empty;

let toCollection (collection: t 'a): (t 'a) =>
  collection;
