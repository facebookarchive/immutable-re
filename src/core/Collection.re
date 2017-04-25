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

let module Make = fun (Base: {
  type a;
  type t;

  let count: t => int;
  let reduce: while_::('acc => a => bool) => ('acc => a => 'acc) => 'acc => t => 'acc;
  let toSequence: t => Sequence.t a;
}) => ({
  include Base;

  let isEmpty (collection: t): bool =>
    (count collection) === 0;

  let isNotEmpty (collection: t): bool =>
    (count collection) !== 0;

  include (Iterable.Make {
    type nonrec a = a;
    type nonrec t = t;

    let isEmpty = isEmpty;
    let reduce = reduce;
  }: Iterable.S with type t := t and type a := a);

  let collectionBase: s t a = {
    count,
    reduce: Base.reduce,
    toSequence,
  };

  let toCollection (collection: t): (collection a) =>
    if (isEmpty collection) Empty
    else Instance collection collectionBase;

}: S with type t := Base.t and type a := Base.a);

let module Make1 = fun (Base: {
  type t 'a;

  let count: t 'a => int;
  let reduce: while_::('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => t 'a => 'acc;
  let toSequence: t 'a => Sequence.t 'a;
}) => ({
  include Base;

  let isEmpty (collection: t 'a): bool =>
    (count collection) === 0;

  let isNotEmpty (collection: t 'a): bool =>
    (count collection) !== 0;

  include (Iterable.Make1 {
    type nonrec t 'a = t 'a;

    let isEmpty = isEmpty;
    let reduce = reduce;
  }: Iterable.S1 with type t 'a := t 'a);

  let toSequence (collection: t 'a): (Sequence.t 'a) =>
    if (isEmpty collection) (Sequence.empty ())
    else (Base.toSequence collection);

  let collectionBase: s (t 'a) 'a = {
    count,
    reduce: Base.reduce,
    toSequence,
  };

  let toCollection (collection: t 'a): (collection 'a) =>
    if (isEmpty collection) Empty
    else Instance collection collectionBase;
}: S1 with type t 'a := Base.t 'a);

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
