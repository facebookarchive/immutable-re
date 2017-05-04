/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type s 'keyed 'k 'v = {
  containsKey: 'k => 'keyed => bool,
  count: 'keyed => int,
  reduce: 'acc . (while_::('acc => 'k => 'v => bool) => ('acc => 'k => 'v => 'acc) => 'acc => 'keyed => 'acc),
  toSequence: 'c . ('k => 'v => 'c) => 'keyed => Sequence.t 'c,
};

type t 'k 'v =
  | Empty
  | Instance 'keyed (s 'keyed 'k 'v): t 'k 'v;

type keyedCollection 'k 'v = t 'k 'v;

module type S1 = {
  type k;
  type t 'v;

  include KeyedIterable.S1 with type k := k and type t 'v := t 'v;

  let containsKey: k => t 'v => bool;
  let count: t 'v => int;
  let isEmpty: (t 'v) => bool;
  let isNotEmpty: (t 'v) => bool;
  let keysCollection: (t 'v) => Collection.t k;
  let keysSequence: (t 'v) => Sequence.t k;
  let toKeyedCollection: (t 'v) => (keyedCollection k 'v);
  let toSequence: (k => 'v => 'c) => (t 'v) => (Sequence.t 'c);
  let valuesCollection: (t 'v) => Collection.t 'v;
  let valuesSequence: (t 'v) => Sequence.t 'v;
};

module type S2 = {
  type t 'k 'v;

  include KeyedIterable.S2 with type t 'k 'v := t 'k 'v;

  let containsKey: 'k => t 'k 'v => bool;
  let count: t 'k 'v => int;
  let isEmpty: (t 'k 'v) => bool;
  let isNotEmpty: (t 'k 'v) => bool;
  let keysCollection: (t 'k 'v) => Collection.t 'k;
  let keysSequence: (t 'k 'v) => Sequence.t 'k;
  let toKeyedCollection: (t 'k 'v) => (keyedCollection 'k 'v);
  let toSequence: ('k => 'v => 'c) => (t 'k 'v) => (Sequence.t 'c);
  let valuesCollection: (t 'k 'v) => Collection.t 'v;
  let valuesSequence: (t 'k 'v) => Sequence.t 'v;
};

let module Make1 = fun (Base: {
  type k;
  type t 'v;

  let containsKey: k => t 'v => bool;
  let count: t 'v => int;
  let reduce: while_::('acc => k => 'v => bool) => ('acc => k => 'v => 'acc) => 'acc => t 'v => 'acc;
  let reduceKeys: while_::('acc => k => bool) => ('acc => k => 'acc) => 'acc => (t 'v) => 'acc;
  let reduceValues: while_::('acc => 'v => bool) => ('acc => 'v => 'acc) => 'acc => (t 'v) => 'acc;
  let toSequence: (k => 'v => 'c) => (t 'v) => Sequence.t 'c;
}) => ({
  include Base;

  let isEmpty (keyed: t 'v): bool =>
    count keyed === 0;

  let isNotEmpty (keyed: t 'v): bool =>
    count keyed !== 0;

  include (KeyedIterable.Make1 {
    type nonrec k = k;
    type nonrec t 'v = t 'v;

    let isEmpty = isEmpty;
    let reduce = reduce;
    let reduceKeys = reduceKeys;
    let reduceValues = reduceValues;
  }: KeyedIterable.S1 with type t 'v := t 'v and type k := k);

  let keysSequence (keyed: t 'v): (Sequence.t k) =>
    if (isEmpty keyed) (Sequence.empty ())
    else toSequence Functions.getKey keyed;

  let keysCollectionImpl: Collection.s (t 'v) k = {
    count,
    reduce: Base.reduceKeys,
    toSequence: keysSequence,
  };

  let keysCollection (keyed: t 'v): (Collection.t k) =>
    if (isEmpty keyed) (Collection.empty ())
    else Collection.Instance keyed keysCollectionImpl;

  let valuesSequence (keyed: t 'v): (Sequence.t 'v) =>
    if (isEmpty keyed) (Sequence.empty ())
    else toSequence Functions.getValue keyed;

  let valuesCollectionImpl: Collection.s (t 'v) 'v = {
    count,
    reduce: Base.reduceValues,
    toSequence: valuesSequence,
  };

  let valuesCollection (keyed: t 'v): (Collection.t 'v) =>
    if (isEmpty keyed) (Collection.empty ())
    else Collection.Instance keyed valuesCollectionImpl;

  let keyedCollectionBase: s (t 'v) k 'v = {
    containsKey,
    count,
    reduce: Base.reduce,
    toSequence,
  };

  let toKeyedCollection (keyed: t 'v): (keyedCollection k 'v) =>
    if (isEmpty keyed) Empty
    else Instance keyed keyedCollectionBase;
}: S1 with type k := Base.k and type t 'v := Base.t 'v);

let module Make2 = fun (Base: {
  type t 'k 'v;

  let containsKey: 'k => t 'k 'v => bool;
  let count: t 'k 'v => int;
  let reduce: while_::('acc => 'k => 'v => bool) => ('acc => 'k => 'v => 'acc) => 'acc => t 'k 'v => 'acc;
  let reduceKeys: while_::('acc => 'k => bool) => ('acc => 'k => 'acc) => 'acc => t 'k 'v => 'acc;
  let reduceValues: while_::('acc => 'v => bool) => ('acc => 'v => 'acc) => 'acc => t 'k 'v => 'acc;
  let toSequence: ('k => 'v => 'c) => (t 'k 'v) => Sequence.t 'c;
}) => ({
  include Base;

  let isEmpty (keyed: t 'k 'v): bool =>
    count keyed === 0;

  let isNotEmpty (keyed: t 'k 'v): bool =>
    count keyed !== 0;

  include (KeyedIterable.Make2 {
    type nonrec t 'k 'v = t 'k 'v;

    let isEmpty = isEmpty;
    let reduce = reduce;
    let reduceKeys = reduceKeys;
    let reduceValues = reduceValues;
  }: KeyedIterable.S2 with type t 'k 'v := t 'k 'v);

  let keysSequence (keyed: t 'k 'v): (Sequence.t 'k) =>
    if (isEmpty keyed) (Sequence.empty ())
    else toSequence Functions.getKey keyed;

  let keysCollectionImpl: Collection.s (t 'k 'v) 'k = {
    count,
    reduce: Base.reduceKeys,
    toSequence: keysSequence,
  };

  let keysCollection (keyed: t 'k 'v): (Collection.t 'k) =>
    if (isEmpty keyed) (Collection.empty ())
    else Collection.Instance keyed keysCollectionImpl;

  let valuesSequence (keyed: t 'k 'v): (Sequence.t 'v) =>
    if (isEmpty keyed) (Sequence.empty ())
    else toSequence Functions.getValue keyed;

  let valuesCollectionImpl: Collection.s (t 'k 'v) 'v = {
    count,
    reduce: Base.reduceValues,
    toSequence: valuesSequence,
  };

  let valuesCollection (keyed: t 'k 'v): (Collection.t 'v) =>
    if (isEmpty keyed) (Collection.empty ())
    else Collection.Instance keyed valuesCollectionImpl;

  let keyedCollectionBase: s (t 'k 'v) 'k 'v = {
    containsKey,
    count,
    reduce: Base.reduce,
    toSequence,
  };

  let toKeyedCollection (keyed: t 'k 'v): (keyedCollection 'k 'v) =>
    if (isEmpty keyed) Empty
    else Instance keyed keyedCollectionBase;
}: S2 with type t 'k 'v := Base.t 'k 'v);

include (Make2 {
  type nonrec t 'k 'v = t 'k 'v;

  let containsKey (key: 'k) (keyed: t 'k 'v): bool => switch keyed {
    | Empty => false
    | Instance keyed { containsKey } => containsKey key keyed
  };

  let count (keyed: t 'k 'v): int => switch keyed {
    | Empty => 0
    | Instance keyed { count } => count keyed
  };

  include (KeyedReducer.Make2 {
    type nonrec t 'k 'v = t 'k 'v;

    let reduce
        while_::(predicate:'acc => 'k => 'v => bool)
        (f: 'acc => 'k => 'v => 'acc)
        (acc: 'acc)
        (iter: t 'k 'v): 'acc => switch iter {
      | Empty => acc
      | Instance iter { reduce } => iter |> reduce while_::predicate f acc;
    };
  }: KeyedReducer.S2 with type t 'k 'v:= t 'k 'v);

  let toSequence (selector: 'k => 'v => 'c) (keyed: t 'k 'v): Sequence.t 'c => switch keyed {
    | Empty => Sequence.empty ()
    | Instance keyed { toSequence } => toSequence selector keyed
  };
}: S2 with type t 'k 'v := t 'k 'v);

let empty (): (t 'k 'v) => Empty;

let toKeyedCollection (keyed: t 'k 'v): (t 'k 'v) => keyed;
