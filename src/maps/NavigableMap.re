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
  first: 'map => (option ('k, 'v)),
  firstOrRaise: 'map => ('k, 'v),
  firstKey: 'map => (option 'k),
  firstKeyOrRaise: 'map => 'k,
  firstValue: 'map => (option 'v),
  firstValueOrRaise: 'map => 'v,
  get: 'k => 'map => (option 'v),
  getOrRaise: 'k => 'map => 'v,
  keysSequence: 'map => Sequence.t 'k,
  reduce: 'acc . (while_::('acc => 'k => 'v => bool) => ('acc => 'k => 'v => 'acc) => 'acc => 'map => 'acc),
  reduceKeys: 'acc . while_::('acc => 'k => bool) => ('acc => 'k => 'acc) => 'acc => 'map => 'acc,
  reduceValues: 'acc . while_::('acc => 'v => bool) => ('acc => 'v => 'acc) => 'acc => 'map => 'acc,
  toSequence: 'map => Sequence.t ('k, 'v),
  valuesSequence: 'map => Sequence.t 'v,
};

type t 'k 'v =
  | Empty
  | Instance 'map (s 'map 'k 'v) (s 'map 'k 'v): t 'k 'v;

type navigableMap 'k 'v = t 'k 'v;
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

let module Make1 = fun (Base: {
  type k;
  type t 'v;

  let containsKey: k => t 'v => bool;
  let count: t 'v => int;
  let first: (t 'v) => (option (k, 'v));
  let firstOrRaise: (t 'v) => (k, 'v);
  let firstKey: (t 'v) => (option k);
  let firstKeyOrRaise: (t 'v) => k;
  let firstValue: (t 'v) => (option 'v);
  let firstValueOrRaise: (t 'v) => 'v;
  let get: k => (t 'v) => (option 'v);
  let getOrRaise: k => (t 'v) => 'v;
  let keysSequence: (t 'v) => Sequence.t k;
  let keysSequenceReversed: (t 'v) => Sequence.t k;
  let last: (t 'v) => (option (k, 'v));
  let lastOrRaise: (t 'v) => (k, 'v);
  let lastKey: (t 'v) => (option k);
  let lastKeyOrRaise: (t 'v) => k;
  let lastValue: (t 'v) => (option 'v);
  let lastValueOrRaise: (t 'v) => 'v;
  let reduce: while_::('acc => k => 'v => bool) => ('acc => k => 'v => 'acc) => 'acc => t 'v => 'acc;
  let reduceReversed: while_::('acc => k => 'v => bool) => ('acc => k => 'v => 'acc) => 'acc => t 'v => 'acc;
  let reduceKeys: while_::('acc => k => bool) => ('acc => k => 'acc) => 'acc => t 'v => 'acc;
  let reduceKeysReversed: while_::('acc => k => bool) => ('acc => k => 'acc) => 'acc => t 'v => 'acc;
  let reduceValues: while_::('acc => 'v => bool) => ('acc => 'v => 'acc) => 'acc => t 'v => 'acc;
  let reduceValuesReversed: while_::('acc => 'v => bool) => ('acc => 'v => 'acc) => 'acc => t 'v => 'acc;
  let toSequence: (t 'v) => Sequence.t (k, 'v);
  let toSequenceReversed: (t 'v) => Sequence.t (k, 'v);
  let valuesSequence: (t 'v) => Sequence.t 'v;
  let valuesSequenceReversed: (t 'v) => Sequence.t 'v;
}) => ({
  include Base;

  include (ImmMap.Make1 Base: ImmMap.S1 with type k := k and type t 'v := t 'v);
  include (NavigableKeyedCollection.Make1 Base: NavigableKeyedCollection.S1 with type k := k and type t 'v := t 'v);

  let module ReversedImmMap = ImmMap.Make1 {
    type nonrec k = k;
    type nonrec t 'v = t 'v;

    let containsKey = containsKey;
    let count = count;
    let get = get;
    let getOrRaise = getOrRaise;
    let keysSequence = keysSequenceReversed;
    let reduce = Base.reduceReversed;
    let reduceKeys = Base.reduceKeysReversed;
    let reduceValues = Base.reduceValuesReversed;
    let toSequence = toSequenceReversed;
    let valuesSequence = valuesSequenceReversed;
  };

  let keysSetReversed = ReversedImmMap.keysSet;
  let toMapReversed = ReversedImmMap.toMap;

  let keysNavigableSetImpl: NavigableSet.s (t 'v) k = {
    contains: containsKey,
    count,
    first: firstKey,
    firstOrRaise: firstKeyOrRaise,
    reduce: Base.reduceKeys,
    toCollection: keysCollection,
    toIterable: keys,
    toNavigableCollection: keysNavigableCollection,
    toSequence: keysSequence,
    toSequentialCollection: keysSequentialCollection,
    toSet: keysSet,
  };

  let keysNavigableSetReversedImpl: NavigableSet.s (t 'v) k = {
    contains: containsKey,
    count,
    first: lastKey,
    firstOrRaise: lastKeyOrRaise,
    reduce: Base.reduceKeysReversed,
    toCollection: keysCollectionReversed,
    toIterable: keysReversed,
    toNavigableCollection: keysNavigableCollectionReversed,
    toSequence: keysSequenceReversed,
    toSequentialCollection: keysSequentialCollectionReversed,
    toSet: keysSetReversed,
  };

  let keysNavigableSet (map: t 'v): NavigableSet.t k =>
    if (isEmpty map) (NavigableSet.empty ())
    else NavigableSet.Instance map keysNavigableSetImpl keysNavigableSetReversedImpl;

  let keysNavigableSetReversed (map: t 'v): NavigableSet.t k =>
    if (isEmpty map) (NavigableSet.empty ())
    else NavigableSet.Instance map keysNavigableSetReversedImpl keysNavigableSetImpl;

  let sequentialMapImpl: s (t 'v) k 'v = {
    containsKey,
    count,
    first,
    firstOrRaise,
    firstKey,
    firstKeyOrRaise,
    firstValue,
    firstValueOrRaise,
    get,
    getOrRaise,
    keysSequence,
    reduce: Base.reduce,
    reduceKeys: Base.reduceKeys,
    reduceValues: Base.reduceValues,
    toSequence,
    valuesSequence,
  };

  let sequentialMapReversedImpl: s (t 'v) k 'v = {
    containsKey,
    count,
    first: last,
    firstOrRaise: lastOrRaise,
    firstKey: lastKey,
    firstKeyOrRaise: lastKeyOrRaise,
    firstValue: lastValue,
    firstValueOrRaise: lastValueOrRaise,
    get,
    getOrRaise,
    keysSequence: keysSequenceReversed,
    reduce: Base.reduceReversed,
    reduceKeys: Base.reduceKeysReversed,
    reduceValues: Base.reduceValuesReversed,
    toSequence: toSequenceReversed,
    valuesSequence: valuesSequenceReversed,
  };

  let toNavigableMap (map: t 'v): (navigableMap k 'v) =>
    if (isEmpty map) Empty
    else Instance map sequentialMapImpl sequentialMapReversedImpl;

  let toNavigableMapReversed (map: t 'v): (navigableMap k 'v) =>
    if (isEmpty map) Empty
    else Instance map sequentialMapReversedImpl sequentialMapImpl;

}: S1 with type k := Base.k and type t 'v := Base.t 'v);

let module Make2 = fun (Base: {
  type t 'k 'v;

  let containsKey: 'k => t 'k 'v => bool;
  let count: t 'k 'v => int;
  let first: (t 'k 'v) => (option ('k, 'v));
  let firstOrRaise: (t 'k 'v) => ('k, 'v);
  let firstKey: (t 'k 'v) => (option 'k);
  let firstKeyOrRaise: (t 'k 'v) => 'k;
  let firstValue: (t 'k 'v) => (option 'v);
  let firstValueOrRaise: (t 'k 'v) => 'v;
  let get: 'k => (t 'k 'v) => (option 'v);
  let getOrRaise: 'k => (t 'k 'v) => 'v;
  let keysSequence: (t 'k 'v) => Sequence.t 'k;
  let keysSequenceReversed: (t 'k 'v) => Sequence.t 'k;
  let last: (t 'k 'v) => (option ('k, 'v));
  let lastOrRaise: (t 'k 'v) => ('k, 'v);
  let lastKey: (t 'k 'v) => (option 'k);
  let lastKeyOrRaise: (t 'k 'v) => 'k;
  let lastValue: (t 'k 'v) => (option 'v);
  let lastValueOrRaise: (t 'k 'v) => 'v;
  let reduce: while_::('acc => 'k => 'v => bool) => ('acc => 'k => 'v => 'acc) => 'acc => t 'k 'v => 'acc;
  let reduceReversed: while_::('acc => 'k => 'v => bool) => ('acc => 'k => 'v => 'acc) => 'acc => t 'k 'v => 'acc;
  let reduceKeys: while_::('acc => 'k => bool) => ('acc => 'k => 'acc) => 'acc => t 'k 'v => 'acc;
  let reduceKeysReversed: while_::('acc => 'k => bool) => ('acc => 'k => 'acc) => 'acc => t 'k 'v => 'acc;
  let reduceValues: while_::('acc => 'v => bool) => ('acc => 'v => 'acc) => 'acc => t 'k 'v => 'acc;
  let reduceValuesReversed: while_::('acc => 'v => bool) => ('acc => 'v => 'acc) => 'acc => t 'k 'v => 'acc;
  let toSequence: (t 'k 'v) => Sequence.t ('k, 'v);
  let toSequenceReversed: (t 'k 'v) => Sequence.t ('k, 'v);
  let valuesSequence: (t 'k 'v) => Sequence.t 'v;
  let valuesSequenceReversed: (t 'k 'v) => Sequence.t 'v;
}) => ({
  include Base;

  include (ImmMap.Make2 Base: ImmMap.S2 with type t 'k 'v := t 'k 'v);
  include (NavigableKeyedCollection.Make2 Base: NavigableKeyedCollection.S2 with type t 'k 'v := t 'k 'v);

  let module ReversedImmMap = ImmMap.Make2 {
    type nonrec t 'k 'v = t 'k 'v;

    let containsKey = containsKey;
    let count = count;
    let get = get;
    let getOrRaise = getOrRaise;
    let keysSequence = keysSequenceReversed;
    let reduce = Base.reduceReversed;
    let reduceKeys = Base.reduceKeysReversed;
    let reduceValues = Base.reduceValuesReversed;
    let toSequence = toSequenceReversed;
    let valuesSequence = valuesSequenceReversed;
  };

  let keysSetReversed = ReversedImmMap.keysSet;
  let toMapReversed = ReversedImmMap.toMap;

  let keysNavigableSetImpl: NavigableSet.s (t 'k 'v) 'k = {
    contains: containsKey,
    count,
    first: firstKey,
    firstOrRaise: firstKeyOrRaise,
    reduce: Base.reduceKeys,
    toCollection: keysCollection,
    toIterable: keys,
    toNavigableCollection: keysNavigableCollection,
    toSequence: keysSequence,
    toSequentialCollection: keysSequentialCollection,
    toSet: keysSet,
  };

  let keysNavigableSetReversedImpl: NavigableSet.s (t 'k 'v) 'k = {
    contains: containsKey,
    count,
    first: lastKey,
    firstOrRaise: lastKeyOrRaise,
    reduce: Base.reduceKeysReversed,
    toCollection: keysCollectionReversed,
    toIterable: keysReversed,
    toNavigableCollection: keysNavigableCollectionReversed,
    toSequence: keysSequenceReversed,
    toSequentialCollection: keysSequentialCollectionReversed,
    toSet: keysSetReversed,
  };

  let keysNavigableSet (map: t 'k 'v): NavigableSet.t 'k =>
    if (isEmpty map) (NavigableSet.empty ())
    else NavigableSet.Instance map keysNavigableSetImpl keysNavigableSetReversedImpl;

  let keysNavigableSetReversed (map: t 'k 'v): NavigableSet.t 'k =>
    if (isEmpty map) (NavigableSet.empty ())
    else NavigableSet.Instance map keysNavigableSetReversedImpl keysNavigableSetImpl;

  let sequentialMapImpl: s (t 'k 'v) 'k 'v = {
    containsKey,
    count,
    first,
    firstOrRaise,
    firstKey,
    firstKeyOrRaise,
    firstValue,
    firstValueOrRaise,
    get,
    getOrRaise,
    keysSequence,
    reduce: Base.reduce,
    reduceKeys: Base.reduceKeys,
    reduceValues: Base.reduceValues,
    toSequence,
    valuesSequence,
  };

  let sequentialMapReversedImpl: s (t 'k 'v) 'k 'v = {
    containsKey,
    count,
    first: last,
    firstOrRaise: lastOrRaise,
    firstKey: lastKey,
    firstKeyOrRaise: lastKeyOrRaise,
    firstValue: lastValue,
    firstValueOrRaise: lastValueOrRaise,
    get,
    getOrRaise,
    keysSequence: keysSequenceReversed,
    reduce: Base.reduceReversed,
    reduceKeys: Base.reduceKeysReversed,
    reduceValues: Base.reduceValuesReversed,
    toSequence: toSequenceReversed,
    valuesSequence: valuesSequenceReversed,
  };

  let toNavigableMap (map: t 'k 'v): (navigableMap 'k 'v) =>
    if (isEmpty map) Empty
    else Instance map sequentialMapImpl sequentialMapReversedImpl;

  let toNavigableMapReversed (map: t 'k 'v): (navigableMap 'k 'v) =>
    if (isEmpty map) Empty
    else Instance map sequentialMapReversedImpl sequentialMapImpl;

}: S2 with type t 'k 'v := Base.t 'k 'v);

include (Make2 {
  type nonrec t 'k 'v = t 'k 'v;

  let containsKey (key: 'k) (map: t 'k 'v): bool => switch map {
    | Empty => false
    | Instance map { containsKey } _ => containsKey key map
  };

  let count (map: t 'k 'v): int => switch map {
    | Empty => 0
    | Instance map { count } _ => count map
  };

  let first (map: t 'k 'v): option ('k, 'v) => switch map {
    | Empty => None
    | Instance map { first } _ => first map
  };

  let firstOrRaise (map: t 'k 'v): ('k, 'v) => switch map {
    | Empty => failwith "empty"
    | Instance map { firstOrRaise } _ => firstOrRaise map
  };

  let firstKey (map: t 'k 'v): option 'k => switch map {
    | Empty => None
    | Instance map { firstKey } _ => firstKey map
  };

  let firstKeyOrRaise (map: t 'k 'v): 'k => switch map {
    | Empty => failwith "empty"
    | Instance map { firstKeyOrRaise } _ => firstKeyOrRaise map
  };

  let firstValue (map: t 'k 'v): option 'v => switch map {
    | Empty => None
    | Instance map { firstValue } _ => firstValue map
  };

  let firstValueOrRaise (map: t 'k 'v): 'v => switch map {
    | Empty => failwith "empty"
    | Instance map { firstValueOrRaise } _ => firstValueOrRaise map
  };

  let get (key: 'k) (map: t 'k 'v): (option 'v) => switch map {
    | Empty => None
    | Instance map { get } _ => get key map
  };

  let getOrRaise (key: 'k) (map: t 'k 'v): 'v => switch map {
    | Empty => failwith "not found"
    | Instance map { getOrRaise } _ => getOrRaise key map
  };


  let keysSequence (map: t 'k 'v): Sequence.t 'k => switch map {
    | Empty => Sequence.empty ()
    | Instance map { keysSequence } _ => keysSequence map
  };

  let keysSequenceReversed (map: t 'k 'v): Sequence.t 'k => switch map {
    | Empty => Sequence.empty ()
    | Instance map _ { keysSequence } => keysSequence map
  };

  let last (map: t 'k 'v): option ('k, 'v) => switch map {
    | Empty => None
    | Instance map _ { first } => first map
  };

  let lastOrRaise (map: t 'k 'v): ('k, 'v) => switch map {
    | Empty => failwith "empty"
    | Instance map _ { firstOrRaise } => firstOrRaise map
  };

  let lastKey (keyed: t 'k 'v): option 'k => switch keyed {
    | Empty => None
    | Instance keyed _ { firstKey } => firstKey keyed
  };

  let lastKeyOrRaise (keyed: t 'k 'v): 'k => switch keyed {
    | Empty => failwith "empty"
    | Instance keyed _ { firstKeyOrRaise } => firstKeyOrRaise keyed
  };

  let lastValue (keyed: t 'k 'v): option 'v => switch keyed {
    | Empty => None
    | Instance keyed _ { firstValue } => firstValue keyed
  };

  let lastValueOrRaise (keyed: t 'k 'v): 'v => switch keyed {
    | Empty => failwith "empty"
    | Instance keyed _ { firstValueOrRaise } => firstValueOrRaise keyed
  };

  let reduce
      while_::(predicate: 'acc => 'k => 'v => bool)
      (f: 'acc => 'k => 'v => 'acc)
      (acc: 'acc)
      (map: t 'k 'v): 'acc => switch map {
    | Empty => acc
    | Instance map { reduce } _ => reduce while_::predicate f acc map
  };

  let reduceReversed
      while_::(predicate: 'acc => 'k => 'v => bool)
      (f: 'acc => 'k => 'v => 'acc)
      (acc: 'acc)
      (map: t 'k 'v): 'acc => switch map {
    | Empty => acc
    | Instance map _ { reduce } => reduce while_::predicate f acc map
  };

  let reduceKeys
      while_::(predicate: 'acc => 'k => bool)
      (f: 'acc => 'k => 'acc)
      (acc: 'acc)
      (map: t 'k 'v): 'acc => switch map {
    | Empty => acc
    | Instance map { reduceKeys } _ => reduceKeys while_::predicate f acc map
  };

  let reduceKeysReversed
      while_::(predicate: 'acc => 'k => bool)
      (f: 'acc => 'k => 'acc)
      (acc: 'acc)
      (map: t 'k 'v): 'acc => switch map {
    | Empty => acc
    | Instance map _ { reduceKeys } => reduceKeys while_::predicate f acc map
  };

  let reduceValues
      while_::(predicate: 'acc => 'v => bool)
      (f: 'acc => 'v => 'acc)
      (acc: 'acc)
      (map: t 'k 'v): 'acc => switch map {
    | Empty => acc
    | Instance map { reduceValues } _ => reduceValues while_::predicate f acc map
  };

  let reduceValuesReversed
      while_::(predicate: 'acc => 'v => bool)
      (f: 'acc => 'v => 'acc)
      (acc: 'acc)
      (map: t 'k 'v): 'acc => switch map {
    | Empty => acc
    | Instance map _ { reduceValues } => reduceValues while_::predicate f acc map
  };

  let toSequence (map: t 'k 'v): (Sequence.t ('k, 'v)) => switch map {
    | Empty => Sequence.empty ()
    | Instance map { toSequence } _ => toSequence map
  };

  let toSequenceReversed (map: t 'k 'v): (Sequence.t ('k, 'v)) => switch map {
    | Empty => Sequence.empty ()
    | Instance map _ { toSequence } => toSequence map
  };

  let valuesSequence (map: t 'k 'v): Sequence.t 'v => switch map {
    | Empty => Sequence.empty ()
    | Instance map { valuesSequence } _ => valuesSequence map
  };

  let valuesSequenceReversed (map: t 'k 'v): Sequence.t 'v => switch map {
    | Empty => Sequence.empty ()
    | Instance map _ { valuesSequence } => valuesSequence map
  };
}: S2 with type t 'k 'v := t 'k 'v);

let empty (): (t 'k 'v) => Empty;

let toNavigableMap (map: t 'k 'v): (t 'k 'v) => map;
