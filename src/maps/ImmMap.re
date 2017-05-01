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
  getOrDefault: default::'v => 'k => 'map => 'v,
  getOrRaise: 'k => 'map => 'v,
  reduce: 'acc . (while_::('acc => 'k => 'v => bool) => ('acc => 'k => 'v => 'acc) => 'acc => 'map => 'acc),
  reduceKeys: 'acc . while_::('acc => 'k => bool) => ('acc => 'k => 'acc) => 'acc => 'map => 'acc,
  reduceValues: 'acc . while_::('acc => 'v => bool) => ('acc => 'v => 'acc) => 'acc => 'map => 'acc,
  toSequence: 'c . ('k => 'v => 'c) => 'map => Sequence.t 'c,
};

type t 'k 'v =
  | Empty
  | Instance 'map (s 'map 'k 'v): t 'k 'v;

type map 'k 'v = t 'k 'v;
module type S1 = {
  type k;
  type t 'v;

  include KeyedCollection.S1 with type k := k and type t 'v := t 'v;

  let get: k => (t 'v) => (option 'v);
  let getOrDefault: default::'v => k => (t 'v) => 'v;
  let getOrRaise: k => (t 'v) => 'v;
  let keysSet: (t 'v) => (ImmSet.t k);
  let toMap: (t 'v) => map k 'v;
};

module type S2 = {
  type t 'k 'v;

  include KeyedCollection.S2 with type t 'k 'v := t 'k 'v;

  let get: 'k => (t 'k 'v) => (option 'v);
  let getOrDefault: default::'v => 'k => (t 'k 'v) => 'v;
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
  let getOrDefault: default::'v => k => (t 'v) => 'v;
  let getOrRaise: k => (t 'v) => 'v;
  let reduce: while_::('acc => k => 'v => bool) => ('acc => k => 'v => 'acc) => 'acc => t 'v => 'acc;
  let reduceKeys: while_::('acc => k => bool) => ('acc => k => 'acc) => 'acc => (t 'v) => 'acc;
  let reduceValues: while_::('acc => 'v => bool) => ('acc => 'v => 'acc) => 'acc => (t 'v) => 'acc;
  let toSequence: (k => 'v => 'c) => (t 'v) => Sequence.t 'c;
}) => ({
  include Base;

  include (KeyedCollection.Make1 Base: KeyedCollection.S1 with type k := k and type t 'v := t 'v);

  let keysSetBase: ImmSet.s (t 'v) k = {
    contains: containsKey,
    count,
    reduce: Base.reduceKeys,
    toSequence: keysSequence,
  };

  let keysSet (map: t 'v): (ImmSet.t k) =>
    if (isEmpty map) (ImmSet.empty ())
    else (ImmSet.Instance map keysSetBase);

  let mapBase: s (t 'v) k 'v = {
    containsKey,
    count,
    get,
    getOrDefault,
    getOrRaise,
    reduce: Base.reduce,
    reduceKeys: Base.reduceKeys,
    reduceValues: Base.reduceValues,
    toSequence,
  };

  let toMap (map: t 'v): (map k 'v) =>
    if (isEmpty map) Empty
    else Instance map mapBase;

}: S1 with type k := Base.k and type t 'v := Base.t 'v);

let module Make2 = fun (Base: {
  type t 'k 'v;

  let containsKey: 'k => t 'k 'v => bool;
  let count: t 'k 'v => int;
  let get: 'k => (t 'k 'v) => (option 'v);
  let getOrDefault: default::'v => 'k => (t 'k 'v) => 'v;
  let getOrRaise: 'k => (t 'k 'v) => 'v;
  let reduce: while_::('acc => 'k => 'v => bool) => ('acc => 'k => 'v => 'acc) => 'acc => t 'k 'v => 'acc;
  let reduceKeys: while_::('acc => 'k => bool) => ('acc => 'k => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceValues: while_::('acc => 'v => bool) => ('acc => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let toSequence: ('k => 'v => 'c) => (t 'k 'v) => Sequence.t 'c;
}) => ({
  include Base;

  include (KeyedCollection.Make2 Base: KeyedCollection.S2 with type t 'k 'v := t 'k 'v);

  let keysSetBase: ImmSet.s (t 'k 'v) 'k = {
    contains: containsKey,
    count,
    reduce: Base.reduceKeys,
    toSequence: keysSequence,
  };

  let keysSet (map: t 'k 'v): (ImmSet.t 'k) =>
    if (isEmpty map) (ImmSet.empty ())
    else (ImmSet.Instance map keysSetBase);

  let mapBase: s (t 'k 'v) 'k 'v = {
    containsKey,
    count,
    get,
    getOrDefault,
    getOrRaise,
    reduce: Base.reduce,
    reduceKeys: Base.reduceKeys,
    reduceValues: Base.reduceValues,
    toSequence,
  };

  let toMap (map: t 'k 'v): (map 'k 'v) =>
    if (isEmpty map) Empty
    else Instance map mapBase;

}: S2 with type t 'k 'v := Base.t 'k 'v);

include (Make2 {
  type nonrec t 'k 'v = t 'k 'v;

  let containsKey (key: 'k) (map: t 'k 'v): bool => switch map {
    | Empty => false
    | Instance map { containsKey } => containsKey key map
  };

  let count (map: t 'k 'v): int => switch map {
    | Empty => 0
    | Instance map { count } => count map
  };

  let get (key: 'k) (map: t 'k 'v): (option 'v) => switch map {
    | Empty => None
    | Instance map { get } => get key map
  };

  let getOrDefault default::(default: 'v) (key: 'k) (map: t 'k 'v): 'v => switch map {
    | Empty => default
    | Instance map { getOrDefault } => getOrDefault ::default key map
  };

  let getOrRaise (key: 'k) (map: t 'k 'v): 'v => switch map {
    | Empty => failwith "not found"
    | Instance map { getOrRaise } => getOrRaise key map
  };

  let reduce
      while_::(predicate: 'acc => 'k => 'v => bool)
      (f: 'acc => 'k => 'v => 'acc)
      (acc: 'acc)
      (map: t 'k 'v): 'acc => switch map {
    | Empty => acc
    | Instance map { reduce } => reduce while_::predicate f acc map
  };

  let reduceKeys
      while_::(predicate: 'acc => 'k => bool)
      (f: 'acc => 'k => 'acc)
      (acc: 'acc)
      (map: t 'k 'v): 'acc => switch map {
    | Empty => acc
    | Instance map { reduceKeys } => reduceKeys while_::predicate f acc map
  };

  let reduceValues
      while_::(predicate: 'acc => 'v => bool)
      (f: 'acc => 'v => 'acc)
      (acc: 'acc)
      (map: t 'k 'v): 'acc => switch map {
    | Empty => acc
    | Instance map { reduceValues } => reduceValues while_::predicate f acc map
  };

  let toSequence (selector: 'k => 'v => 'c) (map: t 'k 'v): (Sequence.t 'c) => switch map {
    | Empty => Sequence.empty ()
    | Instance map { toSequence } => toSequence selector map
  };
}: S2 with type t 'k 'v := t 'k 'v);

let empty (): (t 'k 'v) => Empty;

let toMap (map: t 'k 'v): (t 'k 'v) => map;
