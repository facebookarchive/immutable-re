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
  firstOrRaise: 'c . ('k => 'v => 'c) => 'map => 'c,
  get: 'k => 'map => (option 'v),
  getOrDefault: default::'v => 'k => 'map => 'v,
  getOrRaise: 'k => 'map => 'v,
  reduce: 'acc . (while_::('acc => 'k => 'v => bool) => ('acc => 'k => 'v => 'acc) => 'acc => 'map => 'acc),
  toSequence: 'c . ('k => 'v => 'c) => 'map => Sequence.t 'c,
};

type t 'k 'v =
  | Empty
  | Instance 'map (s 'map 'k 'v) (s 'map 'k 'v): t 'k 'v;

type navigableMap 'k 'v = t 'k 'v;

module type SGeneric = {
  type t 'k 'v;
  type k 'k;
  type v 'v;

  include NavigableKeyedCollection.SGeneric with type t 'k 'v := t 'k 'v and type k 'k := k 'k and type v 'v := v 'v;
  include ImmMap.SGeneric with type t 'k 'v := t 'k 'v and type k 'k := k 'k and type v 'v := v 'v;

  let keysNavigableSet: (t 'k 'v) => (NavigableSet.t (k 'k));
  let keysNavigableSetReversed: (t 'k 'v) => (NavigableSet.t (k 'k));
  let keysSet: (t 'k 'v) => (ImmSet.t (k 'k));
  let toMapReversed: (t 'k 'v) => ImmMap.t (k 'k) (v 'v);
  let toNavigableMap: (t 'k 'v) => navigableMap (k 'k) (v 'v);
  let toNavigableMapReversed: (t 'k 'v) => navigableMap (k 'k) (v 'v);
};

module type S1 = {
  type k;
  type t 'v;

  include NavigableKeyedCollection.S1 with type k := k and type t 'v := t 'v;
  include ImmMap.S1 with type k := k and type t 'v := t 'v;

  let keysNavigableSet: (t 'v) => (NavigableSet.t k);
  let keysNavigableSetReversed: (t 'v) => (NavigableSet.t k);
  let keysSet: (t 'v) => (ImmSet.t k);
  let toMapReversed: (t 'v) => ImmMap.t k 'v;
  let toNavigableMap: (t 'v) => navigableMap k 'v;
  let toNavigableMapReversed: (t 'v) => navigableMap k 'v;
};

module type S2 = {
  type t 'k 'v;

  include NavigableKeyedCollection.S2 with type t 'k 'v := t 'k 'v;
  include ImmMap.S2 with type t 'k 'v := t 'k 'v;

  let keysNavigableSet: (t 'k 'v) => (NavigableSet.t 'k);
  let keysNavigableSetReversed: (t 'k 'v) => (NavigableSet.t 'k);
  let keysSet: (t 'k 'v) => (ImmSet.t 'k);
  let toMapReversed: (t 'k 'v) => ImmMap.t 'k 'v;
  let toNavigableMap: (t 'k 'v) => navigableMap 'k 'v;
  let toNavigableMapReversed: (t 'k 'v) => navigableMap 'k 'v;
};

module type Base = {
  type t 'k 'v;
  type k 'k;
  type v 'v;

  let containsKey: k 'k => t 'k 'v => bool;
  let count: t 'k 'v => int;
  let firstOrRaise: (k 'k => v 'v => 'c) => (t 'k 'v) => 'c;
  let get: k 'k => (t 'k 'v) => (option (v 'v));
  let getOrDefault: default::(v 'v) => (k 'k) => (t 'k 'v) => (v 'v);
  let getOrRaise: (k 'k) => (t 'k 'v) => (v 'v);
  let lastOrRaise: (k 'k => v 'v => 'c) => (t 'k 'v) => 'c;
  let reduce: while_::('acc => k 'k => v 'v => bool) => ('acc => k 'k => v 'v => 'acc) => 'acc => t 'k 'v => 'acc;
  let reduceReversed: while_::('acc => k 'k => v 'v => bool) => ('acc => k 'k => v 'v => 'acc) => 'acc => t 'k 'v => 'acc;
  let reduceKeys: while_::('acc => k 'k => bool) => ('acc => k 'k => 'acc) => 'acc => t 'k 'v => 'acc;
  let reduceKeysReversed: while_::('acc => k 'k => bool) => ('acc => k 'k => 'acc) => 'acc => t 'k 'v => 'acc;
  let reduceValues: while_::('acc => v 'v => bool) => ('acc => v 'v => 'acc) => 'acc => t 'k 'v => 'acc;
  let reduceValuesReversed: while_::('acc => v 'v => bool) => ('acc => v 'v => 'acc) => 'acc => t 'k 'v => 'acc;
  let toSequence: (k 'k => v 'v => 'c) => (t 'k 'v) => Sequence.t 'c;
  let toSequenceReversed: (k 'k => v 'v => 'c) => (t 'k 'v) => Sequence.t 'c;
};

let module MakeGeneric = fun (Base: Base) => ({
  include Base;

  include (ImmMap.MakeGeneric Base: ImmMap.SGeneric with type t 'k 'v := t 'k 'v and type k 'k := k 'k and type v 'v := v 'v);
  include (NavigableKeyedCollection.MakeGeneric Base: NavigableKeyedCollection.SGeneric with type t 'k 'v := t 'k 'v and type k 'k := k 'k and type v 'v := v 'v);

  let module ReversedImmMap = ImmMap.MakeGeneric {
    type nonrec t 'k 'v = t 'k 'v;
    type nonrec k 'k = k 'k;
    type nonrec v 'v = v 'v;

    let containsKey = containsKey;
    let count = count;
    let get = get;
    let getOrDefault = getOrDefault;
    let getOrRaise = getOrRaise;
    let reduce = Base.reduceReversed;
    let reduceKeys = Base.reduceKeysReversed;
    let reduceValues = Base.reduceValuesReversed;
    let toSequence = toSequenceReversed;
  };

  let toMapReversed = ReversedImmMap.toMap;

  let firstKeyOrRaise collection => firstOrRaise Functions.getKey collection;
  let lastKeyOrRaise collection => lastOrRaise Functions.getKey collection;

  let keysNavigableSetImpl: NavigableSet.s (t 'k 'v) (k 'k) = {
    contains: containsKey,
    count,
    firstOrRaise: firstKeyOrRaise,
    reduce: Base.reduceKeys,
    toSequence: keysSequence,
  };

  let keysNavigableSetReversedImpl: NavigableSet.s (t 'k 'v) (k 'k) = {
    contains: containsKey,
    count,
    firstOrRaise: lastKeyOrRaise,
    reduce: Base.reduceKeysReversed,
    toSequence: keysSequenceReversed,
  };

  let keysNavigableSet (map: t 'k 'v): NavigableSet.t (k 'k) =>
    if (isEmpty map) (NavigableSet.empty ())
    else NavigableSet.Instance map keysNavigableSetImpl keysNavigableSetReversedImpl;

  let keysNavigableSetReversed (map: t 'k 'v): NavigableSet.t (k 'k) =>
    if (isEmpty map) (NavigableSet.empty ())
    else NavigableSet.Instance map keysNavigableSetReversedImpl keysNavigableSetImpl;

  let sequentialMapImpl: s (t 'k 'v) (k 'k) (v 'v) = {
    containsKey,
    count,
    firstOrRaise,
    get,
    getOrDefault,
    getOrRaise,
    reduce: Base.reduce,
    toSequence,
  };

  let sequentialMapReversedImpl: s (t 'k 'v) (k 'k) (v 'v) = {
    containsKey,
    count,
    firstOrRaise: lastOrRaise,
    get,
    getOrDefault,
    getOrRaise,
    reduce: Base.reduceReversed,
    toSequence: toSequenceReversed,
  };

  let toNavigableMap (map: t 'k 'v): (navigableMap (k 'k) (v 'v)) =>
    if (isEmpty map) Empty
    else Instance map sequentialMapImpl sequentialMapReversedImpl;

  let toNavigableMapReversed (map: t 'k 'v): (navigableMap (k 'k) (v 'v)) =>
    if (isEmpty map) Empty
    else Instance map sequentialMapReversedImpl sequentialMapImpl;
}: SGeneric with type t 'k 'v := Base.t 'k 'v and type k 'k := Base.k 'k and type v 'v := Base.v 'v);

let containsKey (key: 'k) (map: t 'k 'v): bool => switch map {
  | Empty => false
  | Instance map { containsKey } _ => containsKey key map
};

let count (map: t 'k 'v): int => switch map {
  | Empty => 0
  | Instance map { count } _ => count map
};

let empty (): (t 'k 'v) => Empty;

let firstOrRaise (selector: 'k => 'v => 'c) (map: t 'k 'v): 'c => switch map {
  | Empty => failwith "empty"
  | Instance map { firstOrRaise } _ => firstOrRaise selector map
};

let get (key: 'k) (map: t 'k 'v): (option 'v) => switch map {
  | Empty => None
  | Instance map { get } _ => get key map
};

let getOrDefault default::(default: 'v) (key: 'k) (map: t 'k 'v): 'v => switch map {
  | Empty => default
  | Instance map { getOrDefault } _ => getOrDefault ::default key map
};

let getOrRaise (key: 'k) (map: t 'k 'v): 'v => switch map {
  | Empty => failwith "not found"
  | Instance map { getOrRaise } _ => getOrRaise key map
};

let lastOrRaise (selector: 'k => 'v => 'c) (map: t 'k 'v): 'c => switch map {
  | Empty => failwith "empty"
  | Instance map _ { firstOrRaise } => firstOrRaise selector map
};

let reduce
    while_::(predicate:'acc => 'k => 'v => bool)
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    (iter: t 'k 'v): 'acc => switch iter {
  | Empty => acc
  | Instance iter { reduce } _ => iter |> reduce while_::predicate f acc;
};

let reduceReversed
    while_::(predicate:'acc => 'k => 'v => bool)
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    (iter: t 'k 'v): 'acc => switch iter {
  | Empty => acc
  | Instance iter _ { reduce } => iter |> reduce while_::predicate f acc;
};

let toSequence (selector: 'k => 'v => 'c) (map: t 'k 'v): (Sequence.t 'c) => switch map {
  | Empty => Sequence.empty ()
  | Instance map { toSequence } _ => toSequence selector map
};

let toSequenceReversed (selector: 'k => 'v => 'c) (map: t 'k 'v): (Sequence.t 'c) => switch map {
  | Empty => Sequence.empty ()
  | Instance map _ { toSequence } => toSequence selector map
};
