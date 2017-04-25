/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type s 'collection 'a  = {
  count: 'collection => int,
  first: 'collection => (option 'a),
  firstOrRaise: 'collection => 'a,
  reduce: 'acc . while_::('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => 'collection => 'acc,
  toSequence: 'collection => Sequence.t 'a,
};

type t 'a =
  | Empty
  | Instance 'collection (s 'collection 'a): t 'a;

type sequentialCollection 'a = t 'a;

module type S = {
  type a;
  type t;

  include Collection.S with type a := a and type t := t;

  let first: t => (option a);
  let firstOrRaise: t => a;
  let toSequentialCollection: t => (sequentialCollection a);
};

module type S1 = {
  type t 'a;

  include Collection.S1 with type t 'a := t 'a;

  let first: (t 'a) => (option 'a);
  let firstOrRaise: (t 'a) => 'a;
  let toSequentialCollection: (t 'a) => (sequentialCollection 'a);
};

let module Make = fun (Base: {
  type a;
  type t;

  let count: t => int;
  let first: t => (option a);
  let firstOrRaise: t => a;
  let reduce: while_::('acc => a => bool) => ('acc => a => 'acc) => 'acc => t => 'acc;
  let toSequence: t => Sequence.t a;
}) => ({
  include Base;

  include (Collection.Make Base: Collection.S with type t := t and type a := a);

  let sequentialCollectionBase: s t a = {
    count,
    first,
    firstOrRaise,
    reduce: Base.reduce,
    toSequence,
  };

  let toSequentialCollection (collection: t): (sequentialCollection a) =>
    if (isEmpty collection) Empty
    else Instance collection sequentialCollectionBase;

}: S with type t := Base.t and type a := Base.a);

let module Make1 = fun (Base: {
  type t 'a;

  let count: t 'a => int;
  let first: t 'a => (option 'a);
  let firstOrRaise: t 'a => 'a;
  let reduce: while_::('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => t 'a => 'acc;
  let toSequence: t 'a => Sequence.t 'a;
}) => ({
  include Base;

  include (Collection.Make1 Base: Collection.S1 with type t 'a := t 'a);

  let sequentialCollectionBase: s (t 'a) 'a = {
    count,
    first,
    firstOrRaise,
    reduce: Base.reduce,
    toSequence,
  };

  let toSequentialCollection (collection: t 'a): (sequentialCollection 'a) =>
    if (isEmpty collection) Empty
    else Instance collection sequentialCollectionBase;

}: S1 with type t 'a := Base.t 'a);

include(Make1 {
  type nonrec t 'a = t 'a;

  let count (collection: t 'a): int => switch collection {
    | Empty => 0
    | Instance collection { count } => count collection
  };

  let first (collection: t 'a): (option 'a) => switch collection {
    | Empty => None
    | Instance collection { first } => first collection
  };

  let firstOrRaise (collection: t 'a): 'a => switch collection {
    | Empty => failwith "empty"
    | Instance collection { firstOrRaise } => firstOrRaise collection
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

let toSequentialCollection (collection: t 'a): (t 'a) =>
  collection;
