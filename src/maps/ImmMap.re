/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type s 'map 'k 'v = {
  containsKey: 'k => 'map => bool,
  count: 'map => int,
  get: 'k => 'map => (option 'v),
  getOrRaise: 'k => 'map => 'v,
  keys: 'map => Iterable.t 'k,
  keysCollection: 'map => Collection.t 'k,
  keysSequence: 'map => Sequence.t 'k,
  keysSet: 'map => ImmSet.t 'k,
  reduce: 'acc . (while_::('acc => 'k => 'v => bool) => ('acc => 'k => 'v => 'acc) => 'acc => 'map => 'acc),
  reduceKeys: 'acc . while_::('acc => 'k => bool) => ('acc => 'k => 'acc) => 'acc => 'map => 'acc,
  reduceValues: 'acc . while_::('acc => 'v => bool) => ('acc => 'v => 'acc) => 'acc => 'map => 'acc,
  toIterable: 'map => Iterable.t ('k, 'v),
  toKeyedCollection: 'map => KeyedCollection.t 'k 'v,
  toKeyedIterable: 'map => KeyedIterable.t 'k 'v,
  toSequence: 'map => Sequence.t ('k, 'v),
  values: 'map => Iterable.t 'v,
  valuesCollection: 'map => Collection.t 'v,
  valuesSequence: 'map => Sequence.t 'v,
};

type t 'k 'v =
  | Empty
  | Instance 'map (s 'map 'k 'v): t 'k 'v;

let containsKey (key: 'k) (map: t 'k 'v): bool => switch map {
  | Empty => false
  | Instance map { containsKey } => containsKey key map
};

let count (map: t 'k 'v): int => switch map {
  | Empty => 0
  | Instance map { count } => count map
};

let empty (): (t 'k 'v) => Empty;

let get (key: 'k) (map: t 'k 'v): (option 'v) => switch map {
  | Empty => None
  | Instance map { get } => get key map
};

let getOrRaise (key: 'k) (map: t 'k 'v): 'v => switch map {
  | Empty => failwith "not found"
  | Instance map { getOrRaise } => getOrRaise key map
};

let keys (map: t 'k 'v): Iterable.t 'k => switch map {
  | Empty => Iterable.empty ()
  | Instance map { keys } => keys map
};

let keysCollection (map: t 'k 'v): Collection.t 'k => switch map {
  | Empty => Collection.empty ()
  | Instance map { keysCollection } => keysCollection map
};

let keysSequence (map: t 'k 'v): Sequence.t 'k => switch map {
  | Empty => Sequence.empty ()
  | Instance map { keysSequence } => keysSequence map
};

let keysSet (map: t 'k 'v): ImmSet.t 'k => switch map {
  | Empty => ImmSet.empty ()
  | Instance map { keysSet } => keysSet map
};

let isEmpty (map: t 'k 'v): bool =>
  (count map) === 0;

let isNotEmpty (map: t 'k 'v): bool =>
  (count map) !== 0;

let reduce
    while_::(predicate: 'acc => 'k => 'v => bool)=Functions.alwaysTrue3
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    (map: t 'k 'v): 'acc => switch map {
  | Empty => acc
  | Instance map { reduce } => reduce while_::predicate f acc map
};

let reduceKeys
    while_::(predicate: 'acc => 'k => bool)=Functions.alwaysTrue2
    (f: 'acc => 'k => 'acc)
    (acc: 'acc)
    (map: t 'k 'v): 'acc => switch map {
  | Empty => acc
  | Instance map { reduceKeys } => reduceKeys while_::predicate f acc map
};

let reduceValues
    while_::(predicate: 'acc => 'v => bool)=Functions.alwaysTrue2
    (f: 'acc => 'v => 'acc)
    (acc: 'acc)
    (map: t 'k 'v): 'acc => switch map {
  | Empty => acc
  | Instance map { reduceValues } => reduceValues while_::predicate f acc map
};

let toKeyedCollection (map: t 'k 'v): (KeyedCollection.t 'k 'v) => switch map {
  | Empty => KeyedCollection.empty ()
  | Instance map { toKeyedCollection } => toKeyedCollection map
};

let toKeyedIterable (map: t 'k 'v): (KeyedIterable.t 'k 'v) => switch map {
  | Empty => KeyedIterable.empty ()
  | Instance map { toKeyedIterable } => toKeyedIterable map
};

let toIterable (map: t 'k 'v): (Iterable.t ('k, 'v)) =>
  map |> toKeyedIterable |> KeyedIterable.toIterable;

let toMap (map: t 'k 'v): (t 'k 'v) => map;

let toSequence (map: t 'k 'v): (Sequence.t ('k, 'v)) => switch map {
  | Empty => Sequence.empty ()
  | Instance map { toSequence } => toSequence map
};

let values (map: t 'k 'v): Iterable.t 'v => switch map {
  | Empty => Iterable.empty ()
  | Instance map { values } => values map
};

let valuesCollection (map: t 'k 'v): Collection.t 'v => switch map {
  | Empty => Collection.empty ()
  | Instance map { valuesCollection } => valuesCollection map
};

let valuesSequence (map: t 'k 'v): Sequence.t 'v => switch map {
  | Empty => Sequence.empty ()
  | Instance map { valuesSequence } => valuesSequence map
};

type map 'k 'v = t 'k 'v;
module type S1 = {
  type k;
  type t 'v;

  include KeyedCollection.S1 with type k := k and type t 'v := t 'v;

  let get: k => (t 'v) => (option 'v);
  let getOrRaise: k => (t 'v) => 'v;
  let keysSet: (t 'v) => (ImmSet.t k);
  let toMap: (t 'v) => map k 'v;
};

module type S2 = {
  type t 'k 'v;

  include KeyedCollection.S2 with type t 'k 'v := t 'k 'v;

  let get: 'k => (t 'k 'v) => (option 'v);
  let getOrRaise: 'k => (t 'k 'v) => 'v;
  let keysSet: (t 'k 'v) => (ImmSet.t 'k);
  let toMap: (t 'k 'v) => map 'k 'v;
};

let module Make1 = fun (Base: {
  type k;
  type t 'v;

  let containsKey: k => t 'v => bool;
  let count: t 'v => int;
  let get: k => (t 'v) => (option 'v);
  let getOrRaise: k => (t 'v) => 'v;
  let keysSequence: (t 'v) => Sequence.t k;
  let reduce: while_::('acc => k => 'v => bool) => ('acc => k => 'v => 'acc) => 'acc => t 'v => 'acc;
  let reduceKeys: while_::('acc => k => bool) => ('acc => k => 'acc) => 'acc => (t 'v) => 'acc;
  let reduceValues: while_::('acc => 'v => bool) => ('acc => 'v => 'acc) => 'acc => (t 'v) => 'acc;
  let toSequence: (t 'v) => Sequence.t (k, 'v);
  let valuesSequence: (t 'v) => Sequence.t 'v;
}) => ({
  include Base;

  include (KeyedCollection.Make1 Base: KeyedCollection.S1 with type k := k and type t 'v := t 'v);

  let keysSetBase: ImmSet.s (t 'v) k = {
    contains: containsKey,
    count,
    reduce: Base.reduceKeys,
    toCollection: keysCollection,
    toIterable: keys,
    toSequence: keysSequence,
  };

  let keysSet (map: t 'v): (ImmSet.t k) =>
    if (isEmpty map) (ImmSet.empty ())
    else (ImmSet.Instance map keysSetBase);

  let mapBase: s (t 'v) k 'v = {
    containsKey,
    count,
    get,
    getOrRaise,
    keys,
    keysCollection,
    keysSequence,
    keysSet,
    reduce: Base.reduce,
    reduceKeys: Base.reduceKeys,
    reduceValues: Base.reduceValues,
    toIterable,
    toKeyedCollection,
    toKeyedIterable,
    toSequence,
    values,
    valuesCollection,
    valuesSequence,
  };

  let toMap (map: t 'v): (map k 'v) =>
    if (isEmpty map) (empty ())
    else (Instance map mapBase);

}: S1 with type k := Base.k and type t 'v := Base.t 'v);

let module Make2 = fun (Base: {
  type t 'k 'v;

  let containsKey: 'k => t 'k 'v => bool;
  let count: t 'k 'v => int;
  let get: 'k => (t 'k 'v) => (option 'v);
  let getOrRaise: 'k => (t 'k 'v) => 'v;
  let keysSequence: (t 'k 'v) => Sequence.t 'k;
  let reduce: while_::('acc => 'k => 'v => bool) => ('acc => 'k => 'v => 'acc) => 'acc => t 'k 'v => 'acc;
  let reduceKeys: while_::('acc => 'k => bool) => ('acc => 'k => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceValues: while_::('acc => 'v => bool) => ('acc => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let toSequence: (t 'k 'v) => Sequence.t ('k, 'v);
  let valuesSequence: (t 'k 'v) => Sequence.t 'v;
}) => ({
  include Base;

  include (KeyedCollection.Make2 Base: KeyedCollection.S2 with type t 'k 'v := t 'k 'v);

  let keysSetBase: ImmSet.s (t 'k 'v) 'k = {
    contains: containsKey,
    count,
    reduce: Base.reduceKeys,
    toCollection: keysCollection,
    toIterable: keys,
    toSequence: keysSequence,
  };

  let keysSet (map: t 'k 'v): (ImmSet.t 'k) =>
    if (isEmpty map) (ImmSet.empty ())
    else (ImmSet.Instance map keysSetBase);

  let mapBase: s (t 'k 'v) 'k 'v = {
    containsKey,
    count,
    get,
    getOrRaise,
    keys,
    keysCollection,
    keysSequence,
    keysSet,
    reduce: Base.reduce,
    reduceKeys: Base.reduceKeys,
    reduceValues: Base.reduceValues,
    toIterable,
    toKeyedCollection,
    toKeyedIterable,
    toSequence,
    values,
    valuesCollection,
    valuesSequence,
  };

  let toMap (map: t 'k 'v): (map 'k 'v) =>
    if (isEmpty map) (empty ())
    else (Instance map mapBase);

}: S2 with type t 'k 'v := Base.t 'k 'v);
