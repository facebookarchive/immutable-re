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
  toSequence: 'c . ('k => 'v => 'c) => 'map => Sequence.t 'c,
};

type t 'k 'v =
  | Empty
  | Instance 'map (s 'map 'k 'v): t 'k 'v;

type map 'k 'v = t 'k 'v;

module type SGeneric = {
  type t 'k 'v;
  type k 'k;
  type v 'v;

  include KeyedCollection.SGeneric with type t 'k 'v := t 'k 'v and type k 'k := k 'k  and type v 'v := v 'v;

  let get: k 'k => (t 'k 'v) => (option (v 'v));
  let getOrDefault: default::(v 'v) => (k 'k) => (t 'k 'v) => (v 'v);
  let getOrRaise: (k 'k) => (t 'k 'v) => (v 'v);
  let keysSet: (t 'k 'v) => (ImmSet.t (k 'k));
  let toMap: (t 'k 'v) => map (k 'k) (v 'v);
};

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

let module MakeGeneric = fun (Base: {
  type t 'k 'v;
  type k 'k;
  type v 'v;

  let containsKey: k 'k => t 'k 'v => bool;
  let count: t 'k 'v => int;
  let get: k 'k => (t 'k 'v) => (option (v 'v));
  let getOrDefault: default::(v 'v) => k 'k => (t 'k 'v) => v 'v;
  let getOrRaise: k 'k => (t 'k 'v) => v 'v;
  let reduce: while_::('acc => k 'k => v 'v => bool) => ('acc => k 'k => v 'v => 'acc) => 'acc => t 'k 'v => 'acc;
  let reduceKeys: while_::('acc => k 'k => bool) => ('acc => k 'k => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceValues: while_::('acc => v 'v => bool) => ('acc => v 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let toSequence: (k 'k => v 'v => 'c) => (t 'k 'v) => Sequence.t 'c;
}) => ({
  include Base;

  include (KeyedCollection.MakeGeneric Base: KeyedCollection.SGeneric with type t 'k 'v := t 'k 'v and type k 'k := k 'k and type v 'v := v 'v);

  let keysSetBase: ImmSet.s (t 'k 'v) (k 'k) = {
    contains: containsKey,
    count,
    reduce: Base.reduceKeys,
    toSequence: keysSequence,
  };

  let keysSet (map: t 'k 'v): (ImmSet.t (k 'k)) =>
    if (isEmpty map) (ImmSet.empty ())
    else (ImmSet.Instance map keysSetBase);

  let mapBase: s (t 'k 'v) (k 'k) (v 'v) = {
    containsKey,
    count,
    get,
    getOrDefault,
    getOrRaise,
    reduce: Base.reduce,
    toSequence,
  };

  let toMap (map: t 'k 'v): (map (k 'k) (v 'v)) =>
    if (isEmpty map) Empty
    else Instance map mapBase;

}: SGeneric with type t 'k 'v := Base.t 'k 'v and type k 'k := Base.k 'k and type v 'v := Base.v 'v);

include (MakeGeneric {
  type nonrec t 'k 'v = t 'k 'v;
  type k 'k  = 'k;
  type v 'v = 'v;

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

  include (KeyedReducer.MakeGeneric {
    type nonrec t 'k 'v = t 'k 'v;
    type k 'k = 'k;
    type v 'v = 'v;

    let reduce
        while_::(predicate: 'acc => 'k => 'v => bool)
        (f: 'acc => 'k => 'v => 'acc)
        (acc: 'acc)
        (map: t 'k 'v): 'acc => switch map {
      | Empty => acc
      | Instance map { reduce } => reduce while_::predicate f acc map
    };
  }: KeyedReducer.S2 with type t 'k 'v := t 'k 'v);

  let toSequence (selector: 'k => 'v => 'c) (map: t 'k 'v): (Sequence.t 'c) => switch map {
    | Empty => Sequence.empty ()
    | Instance map { toSequence } => toSequence selector map
  };
}: S2 with type t 'k 'v := t 'k 'v);

let empty (): (t 'k 'v) => Empty;

let toMap (map: t 'k 'v): (t 'k 'v) => map;
