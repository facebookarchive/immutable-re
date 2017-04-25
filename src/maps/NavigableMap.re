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
  keys: 'map => Iterable.t 'k,
  keysCollection: 'map => Collection.t 'k,
  keysNavigableCollection: 'map => (NavigableCollection.t 'k),
  keysNavigableSet: 'map => (NavigableSet.t 'k),
  keysSequence: 'map => Sequence.t 'k,
  keysSequentialCollection: 'map => (SequentialCollection.t 'k),
  keysSet: 'map => ImmSet.t 'k,
  reduce: 'acc . (while_::('acc => 'k => 'v => bool) => ('acc => 'k => 'v => 'acc) => 'acc => 'map => 'acc),
  reduceKeys: 'acc . while_::('acc => 'k => bool) => ('acc => 'k => 'acc) => 'acc => 'map => 'acc,
  reduceValues: 'acc . while_::('acc => 'v => bool) => ('acc => 'v => 'acc) => 'acc => 'map => 'acc,
  toIterable: 'map => Iterable.t ('k, 'v),
  toKeyedCollection: 'map => KeyedCollection.t 'k 'v,
  toKeyedIterable: 'map => KeyedIterable.t 'k 'v,
  toMap: 'map => ImmMap.t 'k 'v,
  toNavigableKeyedCollection: 'map => NavigableKeyedCollection.t 'k 'v,
  toSequence: 'map => Sequence.t ('k, 'v),
  values: 'map => Iterable.t 'v,
  valuesCollection: 'map => Collection.t 'v,
  valuesNavigableCollection: 'map => NavigableCollection.t 'v,
  valuesSequence: 'map => Sequence.t 'v,
  valuesSequentialCollection: 'map => SequentialCollection.t 'v,
};

type t 'k 'v =
  | Empty
  | Instance 'map (s 'map 'k 'v) (s 'map 'k 'v): t 'k 'v;

let containsKey (key: 'k) (map: t 'k 'v): bool => switch map {
  | Empty => false
  | Instance map { containsKey } _ => containsKey key map
};

let count (map: t 'k 'v): int => switch map {
  | Empty => 0
  | Instance map { count } _ => count map
};

let empty (): (t 'k 'v) => Empty;

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

let isEmpty (map: t 'k 'v): bool =>
  (count map) === 0;

let isNotEmpty (map: t 'k 'v): bool =>
  (count map) !== 0;

let keys (map: t 'k 'v): (Iterable.t 'k) => switch map {
  | Empty => Iterable.empty ()
  | Instance map { keys } _ => keys map
};

let keysReversed (map: t 'k 'v): (Iterable.t 'k) => switch map {
  | Empty => Iterable.empty ()
  | Instance map _ { keys } => keys map
};

let keysCollection (map: t 'k 'v): Collection.t 'k => switch map {
  | Empty => Collection.empty ()
  | Instance map { keysCollection } _ => keysCollection map
};

let keysCollectionReversed (map: t 'k 'v): Collection.t 'k => switch map {
  | Empty => Collection.empty ()
  | Instance map _ { keysCollection } => keysCollection map
};

let keysNavigableCollection (map: t 'k 'v): NavigableCollection.t 'k => switch map {
  | Empty => NavigableCollection.empty ()
  | Instance map { keysNavigableCollection } _ => keysNavigableCollection map
};

let keysNavigableCollectionReversed (map: t 'k 'v): NavigableCollection.t 'k => switch map {
  | Empty => NavigableCollection.empty ()
  | Instance map _ { keysNavigableCollection } => keysNavigableCollection map
};

let keysNavigableSet (map: t 'k 'v): NavigableSet.t 'k => switch map {
  | Empty => NavigableSet.empty ()
  | Instance map { keysNavigableSet } _ => keysNavigableSet map
};

let keysNavigableSetReversed (map: t 'k 'v): NavigableSet.t 'k => switch map {
  | Empty => NavigableSet.empty ()
  | Instance map _ { keysNavigableSet } => keysNavigableSet map
};

let keysSequence (map: t 'k 'v): Sequence.t 'k => switch map {
  | Empty => Sequence.empty ()
  | Instance map { keysSequence } _ => keysSequence map
};

let keysSequenceReversed (map: t 'k 'v): Sequence.t 'k => switch map {
  | Empty => Sequence.empty ()
  | Instance map _ { keysSequence } => keysSequence map
};

let keysSequentialCollection (map: t 'k 'v): SequentialCollection.t 'k => switch map {
  | Empty => SequentialCollection.empty ()
  | Instance map { keysSequentialCollection } _ => keysSequentialCollection map
};

let keysSequentialCollectionReversed (map: t 'k 'v): SequentialCollection.t 'k => switch map {
  | Empty => SequentialCollection.empty ()
  | Instance map _ { keysSequentialCollection } => keysSequentialCollection map
};

let keysSet (map: t 'k 'v): ImmSet.t 'k => switch map {
  | Empty => ImmSet.empty ()
  | Instance map { keysSet } _ => keysSet map
};

let keysSetReversed (map: t 'k 'v): ImmSet.t 'k => switch map {
  | Empty => ImmSet.empty ()
  | Instance map _ { keysSet } => keysSet map
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
    while_::(predicate: 'acc => 'k => 'v => bool)=Functions.alwaysTrue3
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    (map: t 'k 'v): 'acc => switch map {
  | Empty => acc
  | Instance map { reduce } _ => reduce while_::predicate f acc map
};

let reduceReversed
    while_::(predicate: 'acc => 'k => 'v => bool)=Functions.alwaysTrue3
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    (map: t 'k 'v): 'acc => switch map {
  | Empty => acc
  | Instance map _ { reduce } => reduce while_::predicate f acc map
};

let reduceKeys
    while_::(predicate: 'acc => 'k => bool)=Functions.alwaysTrue2
    (f: 'acc => 'k => 'acc)
    (acc: 'acc)
    (map: t 'k 'v): 'acc => switch map {
  | Empty => acc
  | Instance map { reduceKeys } _ => reduceKeys while_::predicate f acc map
};

let reduceKeysReversed
    while_::(predicate: 'acc => 'k => bool)=Functions.alwaysTrue2
    (f: 'acc => 'k => 'acc)
    (acc: 'acc)
    (map: t 'k 'v): 'acc => switch map {
  | Empty => acc
  | Instance map _ { reduceKeys } => reduceKeys while_::predicate f acc map
};

let reduceValues
    while_::(predicate: 'acc => 'v => bool)=Functions.alwaysTrue2
    (f: 'acc => 'v => 'acc)
    (acc: 'acc)
    (map: t 'k 'v): 'acc => switch map {
  | Empty => acc
  | Instance map { reduceValues } _ => reduceValues while_::predicate f acc map
};

let reduceValuesReversed
    while_::(predicate: 'acc => 'v => bool)=Functions.alwaysTrue2
    (f: 'acc => 'v => 'acc)
    (acc: 'acc)
    (map: t 'k 'v): 'acc => switch map {
  | Empty => acc
  | Instance map _ { reduceValues } => reduceValues while_::predicate f acc map
};

let toIterable (map: t 'k 'v): (Iterable.t ('k, 'v)) =>switch map {
  | Empty => Iterable.empty ()
  | Instance map { toIterable } _ => toIterable map
};

let toIterableReversed (map: t 'k 'v): (Iterable.t ('k, 'v)) =>switch map {
  | Empty => Iterable.empty ()
  | Instance map _ { toIterable } => toIterable map
};

let toKeyedCollection (map: t 'k 'v): (KeyedCollection.t 'k 'v) => switch map {
  | Empty => KeyedCollection.empty ()
  | Instance map { toKeyedCollection } _ => toKeyedCollection map
};

let toKeyedCollectionReversed (map: t 'k 'v): (KeyedCollection.t 'k 'v) => switch map {
  | Empty => KeyedCollection.empty ()
  | Instance map _ { toKeyedCollection } => toKeyedCollection map
};

let toKeyedIterable (map: t 'k 'v): (KeyedIterable.t 'k 'v) => switch map {
  | Empty => KeyedIterable.empty ()
  | Instance map { toKeyedIterable } _ => toKeyedIterable map
};

let toKeyedIterableReversed (map: t 'k 'v): (KeyedIterable.t 'k 'v) => switch map {
  | Empty => KeyedIterable.empty ()
  | Instance map _ { toKeyedIterable } => toKeyedIterable map
};

let toMap (map: t 'k 'v): (ImmMap.t 'k 'v) => switch map {
  | Empty => ImmMap.empty ()
  | Instance map { toMap } _ => toMap map
};

let toMapReversed (map: t 'k 'v): (ImmMap.t 'k 'v) => switch map {
  | Empty => ImmMap.empty ()
  | Instance map _ { toMap } => toMap map
};

let toNavigableKeyedCollection (map: t 'k 'v): (NavigableKeyedCollection.t 'k 'v) => switch map {
  | Empty => NavigableKeyedCollection.empty ()
  | Instance map { toNavigableKeyedCollection } _ => toNavigableKeyedCollection map
};

let toNavigableKeyedCollectionReversed (map: t 'k 'v): (NavigableKeyedCollection.t 'k 'v) => switch map {
  | Empty => NavigableKeyedCollection.empty ()
  | Instance map _ { toNavigableKeyedCollection } => toNavigableKeyedCollection map
};

let toNavigableMap (map: t 'k 'v): (t 'k 'v) => map;

let toNavigableMapReversed (map: t 'k 'v): (t 'k 'v) => switch map {
  | Empty => Empty
  | Instance map impl implReversed => Instance map implReversed impl
};

let toSequence (map: t 'k 'v): (Sequence.t ('k, 'v)) => switch map {
  | Empty => Sequence.empty ()
  | Instance map { toSequence } _ => toSequence map
};

let toSequenceReversed (map: t 'k 'v): (Sequence.t ('k, 'v)) => switch map {
  | Empty => Sequence.empty ()
  | Instance map _ { toSequence } => toSequence map
};

let values (map: t 'k 'v): (Iterable.t 'v) => switch map {
  | Empty => Iterable.empty ()
  | Instance map { values } _ => values map
};

let valuesReversed (map: t 'k 'v): (Iterable.t 'v) => switch map {
  | Empty => Iterable.empty ()
  | Instance map _ { values } => values map
};

let valuesCollection (map: t 'k 'v): Collection.t 'v => switch map {
  | Empty => Collection.empty ()
  | Instance map { valuesCollection } _ => valuesCollection map
};

let valuesCollectionReversed (map: t 'k 'v): Collection.t 'v => switch map {
  | Empty => Collection.empty ()
  | Instance map _ { valuesCollection } => valuesCollection map
};

let valuesNavigableCollection (map: t 'k 'v): NavigableCollection.t 'v => switch map {
  | Empty => NavigableCollection.empty ()
  | Instance map { valuesNavigableCollection } _ => valuesNavigableCollection map
};

let valuesNavigableCollectionReversed (map: t 'k 'v): NavigableCollection.t 'v => switch map {
  | Empty => NavigableCollection.empty ()
  | Instance map _ { valuesNavigableCollection } => valuesNavigableCollection map
};

let valuesSequence (map: t 'k 'v): Sequence.t 'v => switch map {
  | Empty => Sequence.empty ()
  | Instance map { valuesSequence } _ => valuesSequence map
};

let valuesSequenceReversed (map: t 'k 'v): Sequence.t 'v => switch map {
  | Empty => Sequence.empty ()
  | Instance map _ { valuesSequence } => valuesSequence map
};

let valuesSequentialCollection (map: t 'k 'v): SequentialCollection.t 'v => switch map {
  | Empty => SequentialCollection.empty ()
  | Instance map { valuesSequentialCollection } _ => valuesSequentialCollection map
};

let valuesSequentialCollectionReversed (map: t 'k 'v): SequentialCollection.t 'v => switch map {
  | Empty => SequentialCollection.empty ()
  | Instance map _ { valuesSequentialCollection } => valuesSequentialCollection map
};

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
    keys,
    keysCollection,
    keysNavigableCollection,
    keysNavigableSet,
    keysSequence,
    keysSequentialCollection,
    keysSet,
    reduce: Base.reduce,
    reduceKeys: Base.reduceKeys,
    reduceValues: Base.reduceValues,
    toIterable,
    toKeyedCollection,
    toKeyedIterable,
    toMap,
    toNavigableKeyedCollection,
    toSequence,
    values,
    valuesCollection,
    valuesNavigableCollection,
    valuesSequence,
    valuesSequentialCollection,
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
    keys: keysReversed,
    keysCollection: keysCollectionReversed,
    keysNavigableCollection: keysNavigableCollectionReversed,
    keysNavigableSet: keysNavigableSetReversed,
    keysSequence: keysSequenceReversed,
    keysSequentialCollection: keysSequentialCollectionReversed,
    keysSet: keysSetReversed,
    reduce: Base.reduceReversed,
    reduceKeys: Base.reduceKeysReversed,
    reduceValues: Base.reduceValuesReversed,
    toIterable: toIterableReversed,
    toKeyedCollection: toKeyedCollectionReversed,
    toKeyedIterable: toKeyedIterableReversed,
    toMap: toMapReversed,
    toNavigableKeyedCollection: toNavigableKeyedCollectionReversed,
    toSequence: toSequenceReversed,
    values: valuesReversed,
    valuesCollection: valuesCollectionReversed,
    valuesNavigableCollection: valuesNavigableCollectionReversed,
    valuesSequence: valuesSequenceReversed,
    valuesSequentialCollection: valuesSequentialCollectionReversed,
  };

  let toNavigableMap (map: t 'v): (navigableMap k 'v) =>
    if (isEmpty map) (empty ())
    else Instance map sequentialMapImpl sequentialMapReversedImpl;

  let toNavigableMapReversed (map: t 'v): (navigableMap k 'v) =>
    if (isEmpty map) (empty ())
    else Instance map sequentialMapReversedImpl sequentialMapImpl;

}: S1 with type k := Base.k and type t 'v := Base.t 'v);
