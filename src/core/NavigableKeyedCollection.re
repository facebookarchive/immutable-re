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
  first: 'keyed => (option ('k, 'v)),
  firstOrRaise: 'keyed => ('k, 'v),
  firstKey: 'keyed => (option 'k),
  firstKeyOrRaise: 'keyed => 'k,
  firstValue: 'keyed => (option 'v),
  firstValueOrRaise: 'keyed => 'v,
  keysSequence: 'keyed => Sequence.t 'k,
  reduce: 'acc . (while_::('acc => 'k => 'v => bool) => ('acc => 'k => 'v => 'acc) => 'acc => 'keyed => 'acc),
  reduceKeys: 'acc . while_::('acc => 'k => bool) => ('acc => 'k => 'acc) => 'acc => 'keyed => 'acc,
  reduceValues: 'acc . while_::('acc => 'v => bool) => ('acc => 'v => 'acc) => 'acc => 'keyed => 'acc,
  toSequence: 'keyed => Sequence.t ('k, 'v),
  valuesSequence: 'keyed => Sequence.t 'v,
};

type t 'k 'v =
  | Empty
  | Instance 'keyed (s 'keyed 'k 'v) (s 'keyed 'k 'v): t 'k 'v;

type navigableKeyedCollection 'k 'v = t 'k 'v;

module type S1 = {
  type k;
  type t 'v;

  include KeyedCollection.S1 with type k := k and type t 'v := t 'v;

  let first: (t 'v) => (option (k, 'v));
  let firstOrRaise: (t 'v) => (k, 'v);
  let firstKey: (t 'v) => (option k);
  let firstKeyOrRaise: (t 'v) => k;
  let firstValue: (t 'v) => (option 'v);
  let firstValueOrRaise: (t 'v) => 'v;
  let keysCollectionReversed: (t 'v) => (Collection.t k);
  let keysNavigableCollection: (t 'v) => (NavigableCollection.t k);
  let keysNavigableCollectionReversed: (t 'v) => (NavigableCollection.t k);
  let keysSequentialCollection: (t 'v) => (SequentialCollection.t k);
  let keysSequentialCollectionReversed: (t 'v) => (SequentialCollection.t k);
  let keysReversed: (t 'v) => (Iterable.t k);
  let last: (t 'v) => (option (k, 'v));
  let lastOrRaise: (t 'v) => (k, 'v);
  let lastKey: (t 'v) => (option k);
  let lastKeyOrRaise: (t 'v) => k;
  let lastValue: (t 'v) => (option 'v);
  let lastValueOrRaise: (t 'v) => 'v;
  let reduceReversed: while_::('acc => k => 'v => bool)? => ('acc => k => 'v => 'acc) => 'acc => (t 'v) => 'acc;
  let reduceKeysReversed: while_::('acc => k => bool)? => ('acc => k => 'acc) => 'acc => (t 'v) => 'acc;
  let reduceValuesReversed: while_::('acc => 'v => bool)? => ('acc => 'v => 'acc) => 'acc => (t 'v) => 'acc;
  let toIterableReversed: t 'v => Iterable.t (k, 'v);
  let toKeyedCollectionReversed: t 'v => KeyedCollection.t k 'v;
  let toKeyedIterableReversed: t 'v => KeyedIterable.t k 'v;
  let toNavigableKeyedCollection: t 'v => navigableKeyedCollection k 'v;
  let toNavigableKeyedCollectionReversed: t 'v => navigableKeyedCollection k 'v;
  let toSequenceReversed: (t 'v) => (Sequence.t (k, 'v));
  let valuesCollectionReversed: (t 'v) => (Collection.t 'v);
  let valuesNavigableCollection: (t 'v) => (NavigableCollection.t 'v);
  let valuesNavigableCollectionReversed: (t 'v) => (NavigableCollection.t 'v);
  let valuesSequentialCollection: (t 'v) => (SequentialCollection.t 'v);
  let valuesSequentialCollectionReversed: (t 'v) => (SequentialCollection.t 'v);
  let valuesReversed: (t 'v) => (Iterable.t 'v);
};

module type S2 = {
  type t 'k 'v;

  include KeyedCollection.S2 with type t 'k 'v := t 'k 'v;

  let first: (t 'k 'v) => (option ('k, 'v));
  let firstOrRaise: (t 'k 'v) => ('k, 'v);
  let firstKey: (t 'k 'v) => (option 'k);
  let firstKeyOrRaise: (t 'k 'v) => 'k;
  let firstValue: (t 'k 'v) => (option 'v);
  let firstValueOrRaise: (t 'k 'v) => 'v;
  let keysCollectionReversed: (t 'k 'v) => (Collection.t 'k);
  let keysNavigableCollection: (t 'k 'v) => (NavigableCollection.t 'k);
  let keysNavigableCollectionReversed: (t 'k 'v) => (NavigableCollection.t 'k);
  let keysSequentialCollection: (t 'k 'v) => (SequentialCollection.t 'k);
  let keysSequentialCollectionReversed: (t 'k 'v) => (SequentialCollection.t 'k);
  let keysReversed: (t 'k 'v) => (Iterable.t 'k);
  let last: (t 'k 'v) => (option ('k, 'v));
  let lastOrRaise: (t 'k 'v) => ('k, 'v);
  let lastKey: (t 'k 'v) => (option 'k);
  let lastKeyOrRaise: (t 'k 'v) => 'k;
  let lastValue: (t 'k 'v) => (option 'v);
  let lastValueOrRaise: (t 'k 'v) => 'v;
  let reduceReversed: while_::('acc => 'k => 'v => bool)? => ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceKeysReversed: while_::('acc => 'k => bool)? => ('acc => 'k => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceValuesReversed: while_::('acc => 'v => bool)? => ('acc => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let toIterableReversed: t 'k 'v => Iterable.t ('k, 'v);
  let toKeyedCollectionReversed: t 'k 'v => KeyedCollection.t 'k 'v;
  let toKeyedIterableReversed: t 'k 'v => KeyedIterable.t 'k 'v;
  let toNavigableKeyedCollection: t 'k 'v => navigableKeyedCollection 'k 'v;
  let toNavigableKeyedCollectionReversed: t 'k 'v => navigableKeyedCollection 'k 'v;
  let toSequenceReversed: (t 'k 'v) => (Sequence.t ('k, 'v));
  let valuesCollectionReversed: (t 'k 'v) => (Collection.t 'v);
  let valuesNavigableCollection: (t 'k 'v) => (NavigableCollection.t 'v);
  let valuesNavigableCollectionReversed: (t 'k 'v) => (NavigableCollection.t 'v);
  let valuesSequentialCollection: (t 'k 'v) => (SequentialCollection.t 'v);
  let valuesSequentialCollectionReversed: (t 'k 'v) => (SequentialCollection.t 'v);
  let valuesReversed: (t 'k 'v) => (Iterable.t 'v);
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

  include (KeyedCollection.Make1 Base: KeyedCollection.S1 with type k := k and type t 'v := t 'v);

  let module ReversedKeyedCollection = KeyedCollection.Make1 {
    type nonrec k = k;
    type nonrec t 'v = t 'v;

    let containsKey = containsKey;
    let count = count;
    let keysSequence = keysSequenceReversed;
    let reduce = Base.reduceReversed;
    let reduceKeys = Base.reduceKeysReversed;
    let reduceValues = Base.reduceValuesReversed;
    let toSequence = toSequenceReversed;
    let valuesSequence = valuesSequenceReversed;
  };

  let keysCollectionReversed = ReversedKeyedCollection.keysCollection;
  let keysReversed = ReversedKeyedCollection.keys;
  let reduceReversed = ReversedKeyedCollection.reduce;
  let reduceKeysReversed = ReversedKeyedCollection.reduceKeys;
  let reduceValuesReversed = ReversedKeyedCollection.reduceValues;
  let toIterableReversed = ReversedKeyedCollection.toIterable;
  let toKeyedCollectionReversed = ReversedKeyedCollection.toKeyedCollection;
  let toKeyedIterableReversed = ReversedKeyedCollection.toKeyedIterable;
  let toSequenceReversed = ReversedKeyedCollection.toSequence;
  let valuesCollectionReversed = ReversedKeyedCollection.valuesCollection;
  let valuesReversed = ReversedKeyedCollection.values;

  let keysSequentialCollectionBase: SequentialCollection.s (t 'v) k = {
    count,
    first: firstKey,
    firstOrRaise: firstKeyOrRaise,
    reduce: Base.reduceKeys,
    toSequence: keysSequence,
  };

  let keysSequentialCollectionReversedBase: SequentialCollection.s (t 'v) k = {
    count,
    first: lastKey,
    firstOrRaise: lastKeyOrRaise,
    reduce: Base.reduceKeysReversed,
    toSequence: keysSequenceReversed,
  };

  let keysSequentialCollection (collection: t 'v): (SequentialCollection.t k) =>
    if (isEmpty collection) (SequentialCollection.empty ())
    else SequentialCollection.Instance collection keysSequentialCollectionBase;

  let keysSequentialCollectionReversed (collection: t 'v): (SequentialCollection.t k) =>
    if (isEmpty collection) (SequentialCollection.empty ())
    else SequentialCollection.Instance collection keysSequentialCollectionReversedBase;

  let keysNavigableCollection (collection: t 'v): (NavigableCollection.t k) =>
    if (isEmpty collection) (NavigableCollection.empty ())
    else NavigableCollection.Instance collection keysSequentialCollectionBase keysSequentialCollectionReversedBase;

  let keysNavigableCollectionReversed (collection: t 'v): (NavigableCollection.t k) =>
    if (isEmpty collection) (NavigableCollection.empty ())
    else NavigableCollection.Instance collection keysSequentialCollectionReversedBase keysSequentialCollectionBase;

  let valuesSequentialCollectionBase: SequentialCollection.s (t 'v) 'v = {
    count,
    first: firstValue,
    firstOrRaise: firstValueOrRaise,
    reduce: Base.reduceValues,
    toSequence: valuesSequence,
  };

  let valuesSequentialCollectionReversedBase: SequentialCollection.s (t 'v) 'v = {
    count,
    first: lastValue,
    firstOrRaise: lastValueOrRaise,
    reduce: Base.reduceValuesReversed,
    toSequence: valuesSequenceReversed,
  };

  let valuesSequentialCollection (collection: t 'v): (SequentialCollection.t 'v) =>
    if (isEmpty collection) (SequentialCollection.empty ())
    else SequentialCollection.Instance collection valuesSequentialCollectionBase;

  let valuesSequentialCollectionReversed (collection: t 'v): (SequentialCollection.t 'v) =>
    if (isEmpty collection) (SequentialCollection.empty ())
    else SequentialCollection.Instance collection valuesSequentialCollectionReversedBase;

  let valuesNavigableCollection (collection: t 'v): (NavigableCollection.t 'v) =>
    if (isEmpty collection) (NavigableCollection.empty ())
    else NavigableCollection.Instance collection valuesSequentialCollectionBase valuesSequentialCollectionReversedBase;

  let valuesNavigableCollectionReversed (collection: t 'v): (NavigableCollection.t 'v) =>
    if (isEmpty collection) (NavigableCollection.empty ())
    else NavigableCollection.Instance collection valuesSequentialCollectionReversedBase valuesSequentialCollectionBase;

  let sequentialKeyedCollectionBase: s (t 'v) k 'v  = {
    containsKey,
    count,
    first,
    firstOrRaise,
    firstKey,
    firstKeyOrRaise,
    firstValue,
    firstValueOrRaise,
    keysSequence,
    reduce: Base.reduce,
    reduceKeys: Base.reduceKeys,
    reduceValues: Base.reduceValues,
    toSequence,
    valuesSequence,
  };

  let sequentialKeyedCollectionReversedBase: s (t 'v) k 'v  = {
    containsKey,
    count,
    first: last,
    firstOrRaise: lastOrRaise,
    firstKey: lastKey,
    firstKeyOrRaise: lastKeyOrRaise,
    firstValue: lastValue,
    firstValueOrRaise: lastValueOrRaise,
    keysSequence: keysSequenceReversed,
    reduce: Base.reduceReversed,
    reduceKeys: Base.reduceKeysReversed,
    reduceValues: Base.reduceValuesReversed,
    toSequence: toSequenceReversed,
    valuesSequence: valuesSequenceReversed,
  };

  let toNavigableKeyedCollection (keyed: t 'v): navigableKeyedCollection k 'v =>
    if (isEmpty keyed) Empty
    else Instance keyed sequentialKeyedCollectionBase sequentialKeyedCollectionReversedBase;

  let toNavigableKeyedCollectionReversed (keyed: t 'v): navigableKeyedCollection k 'v =>
    if (isEmpty keyed) Empty
    else Instance keyed sequentialKeyedCollectionReversedBase sequentialKeyedCollectionBase;

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

  include (KeyedCollection.Make2 Base: KeyedCollection.S2 with type t 'k 'v := t 'k 'v);

  let module ReversedKeyedCollection = KeyedCollection.Make2 {
    type nonrec t 'k 'v = t 'k 'v;

    let containsKey = containsKey;
    let count = count;
    let keysSequence = keysSequenceReversed;
    let reduce = Base.reduceReversed;
    let reduceKeys = Base.reduceKeysReversed;
    let reduceValues = Base.reduceValuesReversed;
    let toSequence = toSequenceReversed;
    let valuesSequence = valuesSequenceReversed;
  };

  let keysCollectionReversed = ReversedKeyedCollection.keysCollection;
  let keysReversed = ReversedKeyedCollection.keys;
  let reduceReversed = ReversedKeyedCollection.reduce;
  let reduceKeysReversed = ReversedKeyedCollection.reduceKeys;
  let reduceValuesReversed = ReversedKeyedCollection.reduceValues;
  let toIterableReversed = ReversedKeyedCollection.toIterable;
  let toKeyedCollectionReversed = ReversedKeyedCollection.toKeyedCollection;
  let toKeyedIterableReversed = ReversedKeyedCollection.toKeyedIterable;
  let toSequenceReversed = ReversedKeyedCollection.toSequence;
  let valuesCollectionReversed = ReversedKeyedCollection.valuesCollection;
  let valuesReversed = ReversedKeyedCollection.values;

  let keysSequentialCollectionBase: SequentialCollection.s (t 'k 'v) 'k = {
    count,
    first: firstKey,
    firstOrRaise: firstKeyOrRaise,
    reduce: Base.reduceKeys,
    toSequence: keysSequence,
  };

  let keysSequentialCollectionReversedBase: SequentialCollection.s (t 'k 'v) 'k = {
    count,
    first: lastKey,
    firstOrRaise: lastKeyOrRaise,
    reduce: Base.reduceKeysReversed,
    toSequence: keysSequenceReversed,
  };

  let keysSequentialCollection (collection: t 'k 'v): (SequentialCollection.t 'k) =>
    if (isEmpty collection) (SequentialCollection.empty ())
    else SequentialCollection.Instance collection keysSequentialCollectionBase;

  let keysSequentialCollectionReversed (collection: t 'k 'v): (SequentialCollection.t 'k) =>
    if (isEmpty collection) (SequentialCollection.empty ())
    else SequentialCollection.Instance collection keysSequentialCollectionReversedBase;

  let keysNavigableCollection (collection: t 'k 'v): (NavigableCollection.t 'k) =>
    if (isEmpty collection) (NavigableCollection.empty ())
    else NavigableCollection.Instance collection keysSequentialCollectionBase keysSequentialCollectionReversedBase;

  let keysNavigableCollectionReversed (collection: t 'k 'v): (NavigableCollection.t 'k) =>
    if (isEmpty collection) (NavigableCollection.empty ())
    else NavigableCollection.Instance collection keysSequentialCollectionReversedBase keysSequentialCollectionBase;

  let valuesSequentialCollectionBase: SequentialCollection.s (t 'k 'v) 'v = {
    count,
    first: firstValue,
    firstOrRaise: firstValueOrRaise,
    reduce: Base.reduceValues,
    toSequence: valuesSequence,
  };

  let valuesSequentialCollectionReversedBase: SequentialCollection.s (t 'k 'v) 'v = {
    count,
    first: lastValue,
    firstOrRaise: lastValueOrRaise,
    reduce: Base.reduceValuesReversed,
    toSequence: valuesSequenceReversed,
  };

  let valuesSequentialCollection (collection: t 'k 'v): (SequentialCollection.t 'v) =>
    if (isEmpty collection) (SequentialCollection.empty ())
    else SequentialCollection.Instance collection valuesSequentialCollectionBase;

  let valuesSequentialCollectionReversed (collection: t 'k 'v): (SequentialCollection.t 'v) =>
    if (isEmpty collection) (SequentialCollection.empty ())
    else SequentialCollection.Instance collection valuesSequentialCollectionReversedBase;

  let valuesNavigableCollection (collection: t 'k 'v): (NavigableCollection.t 'v) =>
    if (isEmpty collection) (NavigableCollection.empty ())
    else NavigableCollection.Instance collection valuesSequentialCollectionBase valuesSequentialCollectionReversedBase;

  let valuesNavigableCollectionReversed (collection: t 'k 'v): (NavigableCollection.t 'v) =>
    if (isEmpty collection) (NavigableCollection.empty ())
    else NavigableCollection.Instance collection valuesSequentialCollectionReversedBase valuesSequentialCollectionBase;

  let sequentialKeyedCollectionBase: s (t 'k 'v) 'k 'v  = {
    containsKey,
    count,
    first,
    firstOrRaise,
    firstKey,
    firstKeyOrRaise,
    firstValue,
    firstValueOrRaise,
    keysSequence,
    reduce: Base.reduce,
    reduceKeys: Base.reduceKeys,
    reduceValues: Base.reduceValues,
    toSequence,
    valuesSequence,
  };

  let sequentialKeyedCollectionReversedBase: s (t 'k 'v) 'k 'v  = {
    containsKey,
    count,
    first: last,
    firstOrRaise: lastOrRaise,
    firstKey: lastKey,
    firstKeyOrRaise: lastKeyOrRaise,
    firstValue: lastValue,
    firstValueOrRaise: lastValueOrRaise,
    keysSequence: keysSequenceReversed,
    reduce: Base.reduceReversed,
    reduceKeys: Base.reduceKeysReversed,
    reduceValues: Base.reduceValuesReversed,
    toSequence: toSequenceReversed,
    valuesSequence: valuesSequenceReversed,
  };

  let toNavigableKeyedCollection (keyed: t 'k 'v): navigableKeyedCollection 'k 'v =>
    if (isEmpty keyed) Empty
    else Instance keyed sequentialKeyedCollectionBase sequentialKeyedCollectionReversedBase;

  let toNavigableKeyedCollectionReversed (keyed: t 'k 'v): navigableKeyedCollection 'k 'v =>
    if (isEmpty keyed) Empty
    else Instance keyed sequentialKeyedCollectionReversedBase sequentialKeyedCollectionBase;

}: S2 with type t 'k 'v := Base.t 'k 'v);

include (Make2 {
  type nonrec t 'k 'v = t 'k 'v;

  let containsKey (key: 'k) (keyed: t 'k 'v): bool => switch keyed {
    | Empty => false
    | Instance keyed { containsKey } _ => containsKey key keyed
  };

  let count (keyed: t 'k 'v): int => switch keyed {
    | Empty => 0
    | Instance keyed { count } _ => count keyed
  };

  let first (keyed: t 'k 'v): option ('k, 'v) => switch keyed {
    | Empty => None
    | Instance keyed { first } _ => first keyed
  };

  let firstOrRaise (keyed: t 'k 'v): ('k, 'v) => switch keyed {
    | Empty => failwith "empty"
    | Instance keyed { firstOrRaise } _ => firstOrRaise keyed
  };

  let firstKey (keyed: t 'k 'v): option 'k => switch keyed {
    | Empty => None
    | Instance keyed { firstKey } _ => firstKey keyed
  };

  let firstKeyOrRaise (keyed: t 'k 'v): 'k => switch keyed {
    | Empty => failwith "empty"
    | Instance keyed { firstKeyOrRaise } _ => firstKeyOrRaise keyed
  };

  let firstValue (keyed: t 'k 'v): option 'v => switch keyed {
    | Empty => None
    | Instance keyed { firstValue } _ => firstValue keyed
  };

  let firstValueOrRaise (keyed: t 'k 'v): 'v => switch keyed {
    | Empty => failwith "empty"
    | Instance keyed { firstValueOrRaise } _ => firstValueOrRaise keyed
  };

  let keysSequence (keyed: t 'k 'v): Sequence.t 'k => switch keyed {
    | Empty => Sequence.empty ()
    | Instance keyed { keysSequence } _ => keysSequence keyed
  };

  let keysSequenceReversed (keyed: t 'k 'v): Sequence.t 'k => switch keyed {
    | Empty => Sequence.empty ()
    | Instance keyed _ { keysSequence } => keysSequence keyed
  };

  let last (keyed: t 'k 'v): option ('k, 'v) => switch keyed {
    | Empty => None
    | Instance keyed _ { first } => first keyed
  };

  let lastOrRaise (keyed: t 'k 'v): ('k, 'v) => switch keyed {
    | Empty => failwith "empty"
    | Instance keyed _ { firstOrRaise } => firstOrRaise keyed
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

  let reduceKeys
      while_::(predicate:'acc => 'k => bool)
      (f: 'acc => 'k => 'acc)
      (acc: 'acc)
      (iter: t 'k 'v): 'acc => switch iter {
    | Empty => acc
    | Instance iter { reduceKeys } _ => iter |> reduceKeys while_::predicate f acc;
  };

  let reduceKeysReversed
      while_::(predicate:'acc => 'k => bool)
      (f: 'acc => 'k => 'acc)
      (acc: 'acc)
      (iter: t 'k 'v): 'acc => switch iter {
    | Empty => acc
    | Instance iter _ { reduceKeys } => iter |> reduceKeys while_::predicate f acc;
  };

  let reduceValues
      while_::(predicate:'acc => 'v => bool)
      (f: 'acc => 'v => 'acc)
      (acc: 'acc)
      (iter: t 'k 'v): 'acc => switch iter {
    | Empty => acc
    | Instance iter { reduceValues } _ => iter |> reduceValues while_::predicate f acc;
  };

  let reduceValuesReversed
      while_::(predicate:'acc => 'v => bool)
      (f: 'acc => 'v => 'acc)
      (acc: 'acc)
      (iter: t 'k 'v): 'acc => switch iter {
    | Empty => acc
    | Instance iter _ { reduceValues } => iter |> reduceValues while_::predicate f acc;
  };

  let toSequence (keyed: t 'k 'v): Sequence.t ('k, 'v) => switch keyed {
    | Empty => Sequence.empty ()
    | Instance keyed { toSequence } _ => toSequence keyed
  };

  let toSequenceReversed (keyed: t 'k 'v): Sequence.t ('k, 'v) => switch keyed {
    | Empty => Sequence.empty ()
    | Instance keyed _ { toSequence } => toSequence keyed
  };

  let valuesSequence (keyed: t 'k 'v): Sequence.t 'v => switch keyed {
    | Empty => Sequence.empty ()
    | Instance keyed { valuesSequence } _ => valuesSequence keyed
  };

  let valuesSequenceReversed (keyed: t 'k 'v): Sequence.t 'v => switch keyed {
    | Empty => Sequence.empty ()
    | Instance keyed _ { valuesSequence } => valuesSequence keyed
  };
}: S2 with type t 'k 'v := t 'k 'v);

let empty (): (t 'k 'v) => Empty;

let toNavigableKeyedCollection (keyed: t 'k 'v): t 'k 'v => keyed;
