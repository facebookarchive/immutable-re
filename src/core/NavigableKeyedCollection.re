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
  firstOrRaise: 'c . ('k => 'v => 'c) => 'keyed => 'c,
  reduce: 'acc . (while_::('acc => 'k => 'v => bool) => ('acc => 'k => 'v => 'acc) => 'acc => 'keyed => 'acc),
  reduceKeys: 'acc . while_::('acc => 'k => bool) => ('acc => 'k => 'acc) => 'acc => 'keyed => 'acc,
  reduceValues: 'acc . while_::('acc => 'v => bool) => ('acc => 'v => 'acc) => 'acc => 'keyed => 'acc,
  toSequence: 'c . ('k => 'v => 'c) => 'keyed => Sequence.t 'c,
};

type t 'k 'v =
  | Empty
  | Instance 'keyed (s 'keyed 'k 'v) (s 'keyed 'k 'v): t 'k 'v;

type navigableKeyedCollection 'k 'v = t 'k 'v;

module type S1 = {
  type k;
  type t 'v;

  include KeyedCollection.S1 with type k := k and type t 'v := t 'v;

  let first: (k => 'v => 'c) => (t 'v) => (option 'c);
  let firstOrRaise: (k => 'v => 'c) => (t 'v) => 'c;
  let keysCollectionReversed: (t 'v) => (Collection.t k);
  let keysNavigableCollection: (t 'v) => (NavigableCollection.t k);
  let keysNavigableCollectionReversed: (t 'v) => (NavigableCollection.t k);
  let keysReversed: (t 'v) => (Iterable.t k);
  let keysSequentialCollection: (t 'v) => (SequentialCollection.t k);
  let keysSequentialCollectionReversed: (t 'v) => (SequentialCollection.t k);
  let keysSequenceReversed: (t 'v) => Sequence.t k;
  let last: (k => 'v => 'c) => (t 'v) => (option 'c);
  let lastOrRaise: (k => 'v => 'c) => (t 'v) => 'c;
  let reduceReversed: while_::('acc => k => 'v => bool)? => ('acc => k => 'v => 'acc) => 'acc => (t 'v) => 'acc;
  let reduceKeysReversed: while_::('acc => k => bool)? => ('acc => k => 'acc) => 'acc => (t 'v) => 'acc;
  let reduceValuesReversed: while_::('acc => 'v => bool)? => ('acc => 'v => 'acc) => 'acc => (t 'v) => 'acc;
  let toIterableReversed: (k => 'v => 'c) => t 'v => Iterable.t 'c;
  let toKeyedCollectionReversed: t 'v => KeyedCollection.t k 'v;
  let toKeyedIterableReversed: t 'v => KeyedIterable.t k 'v;
  let toNavigableKeyedCollection: t 'v => navigableKeyedCollection k 'v;
  let toNavigableKeyedCollectionReversed: t 'v => navigableKeyedCollection k 'v;
  let toSequenceReversed: (k => 'v => 'c) => (t 'v) => (Sequence.t 'c);
  let valuesCollectionReversed: (t 'v) => (Collection.t 'v);
  let valuesNavigableCollection: (t 'v) => (NavigableCollection.t 'v);
  let valuesNavigableCollectionReversed: (t 'v) => (NavigableCollection.t 'v);
  let valuesReversed: (t 'v) => (Iterable.t 'v);
  let valuesSequentialCollection: (t 'v) => (SequentialCollection.t 'v);
  let valuesSequentialCollectionReversed: (t 'v) => (SequentialCollection.t 'v);
  let valuesSequenceReversed: (t 'v) => (Sequence.t 'v);
};

module type S2 = {
  type t 'k 'v;

  include KeyedCollection.S2 with type t 'k 'v := t 'k 'v;

  let first: ('k => 'v => 'c) => (t 'k 'v) => (option 'c);
  let firstOrRaise: ('k => 'v => 'c) => (t 'k 'v) => 'c;
  let keysCollectionReversed: (t 'k 'v) => (Collection.t 'k);
  let keysNavigableCollection: (t 'k 'v) => (NavigableCollection.t 'k);
  let keysNavigableCollectionReversed: (t 'k 'v) => (NavigableCollection.t 'k);
  let keysReversed: (t 'k 'v) => (Iterable.t 'k);
  let keysSequentialCollection: (t 'k 'v) => (SequentialCollection.t 'k);
  let keysSequentialCollectionReversed: (t 'k 'v) => (SequentialCollection.t 'k);
  let keysSequenceReversed: (t 'k 'v) => Sequence.t 'k;
  let last: ('k => 'v => 'c) => (t 'k 'v) => (option 'c);
  let lastOrRaise: ('k => 'v => 'c) => (t 'k 'v) => 'c;
  let reduceReversed: while_::('acc => 'k => 'v => bool)? => ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceKeysReversed: while_::('acc => 'k => bool)? => ('acc => 'k => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceValuesReversed: while_::('acc => 'v => bool)? => ('acc => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let toIterableReversed: ('k => 'v => 'c) => t 'k 'v => Iterable.t 'c;
  let toKeyedCollectionReversed: t 'k 'v => KeyedCollection.t 'k 'v;
  let toKeyedIterableReversed: t 'k 'v => KeyedIterable.t 'k 'v;
  let toNavigableKeyedCollection: t 'k 'v => navigableKeyedCollection 'k 'v;
  let toNavigableKeyedCollectionReversed: t 'k 'v => navigableKeyedCollection 'k 'v;
  let toSequenceReversed: ('k => 'v => 'c) => (t 'k 'v) => (Sequence.t 'c);
  let valuesCollectionReversed: (t 'k 'v) => (Collection.t 'v);
  let valuesNavigableCollection: (t 'k 'v) => (NavigableCollection.t 'v);
  let valuesNavigableCollectionReversed: (t 'k 'v) => (NavigableCollection.t 'v);
  let valuesReversed: (t 'k 'v) => (Iterable.t 'v);
  let valuesSequentialCollection: (t 'k 'v) => (SequentialCollection.t 'v);
  let valuesSequentialCollectionReversed: (t 'k 'v) => (SequentialCollection.t 'v);
  let valuesSequenceReversed: (t 'k 'v) => (Sequence.t 'v);
};

let module Make1 = fun (Base: {
  type k;
  type t 'v;

  let containsKey: k => t 'v => bool;
  let count: t 'v => int;
  let firstOrRaise: (k => 'v => 'c) => (t 'v) => 'c;
  let lastOrRaise: (k => 'v => 'c) => (t 'v) => 'c;
  let reduce: while_::('acc => k => 'v => bool) => ('acc => k => 'v => 'acc) => 'acc => t 'v => 'acc;
  let reduceReversed: while_::('acc => k => 'v => bool) => ('acc => k => 'v => 'acc) => 'acc => t 'v => 'acc;
  let reduceKeys: while_::('acc => k => bool) => ('acc => k => 'acc) => 'acc => t 'v => 'acc;
  let reduceKeysReversed: while_::('acc => k => bool) => ('acc => k => 'acc) => 'acc => t 'v => 'acc;
  let reduceValues: while_::('acc => 'v => bool) => ('acc => 'v => 'acc) => 'acc => t 'v => 'acc;
  let reduceValuesReversed: while_::('acc => 'v => bool) => ('acc => 'v => 'acc) => 'acc => t 'v => 'acc;
  let toSequence: (k => 'v => 'c) => (t 'v) => Sequence.t 'c;
  let toSequenceReversed: (k => 'v => 'c) => (t 'v) => Sequence.t 'c;
}) => ({
  include Base;

  include (KeyedCollection.Make1 Base: KeyedCollection.S1 with type k := k and type t 'v := t 'v);

  let first (selector: k => 'v => 'c) (keyed: t 'v): (option 'c) =>
    if (isEmpty keyed) None
    else Some (firstOrRaise selector keyed);

  let last (selector: k => 'v => 'c) (keyed: t 'v): (option 'c) =>
    if (isEmpty keyed) None
    else Some (lastOrRaise selector keyed);

  let module ReversedKeyedCollection = KeyedCollection.Make1 {
    type nonrec k = k;
    type nonrec t 'v = t 'v;

    let containsKey = containsKey;
    let count = count;
    let reduce = Base.reduceReversed;
    let reduceKeys = Base.reduceKeysReversed;
    let reduceValues = Base.reduceValuesReversed;
    let toSequence = toSequenceReversed;
  };

  let keysCollectionReversed = ReversedKeyedCollection.keysCollection;
  let keysReversed = ReversedKeyedCollection.keys;
  let keysSequenceReversed = ReversedKeyedCollection.keysSequence;
  let reduceReversed = ReversedKeyedCollection.reduce;
  let reduceKeysReversed = ReversedKeyedCollection.reduceKeys;
  let reduceValuesReversed = ReversedKeyedCollection.reduceValues;
  let toIterableReversed = ReversedKeyedCollection.toIterable;
  let toKeyedCollectionReversed = ReversedKeyedCollection.toKeyedCollection;
  let toKeyedIterableReversed = ReversedKeyedCollection.toKeyedIterable;
  let toSequenceReversed = ReversedKeyedCollection.toSequence;
  let valuesCollectionReversed = ReversedKeyedCollection.valuesCollection;
  let valuesReversed = ReversedKeyedCollection.values;
  let valuesSequenceReversed = ReversedKeyedCollection.valuesSequence;

  let firstKeyOrRaise collection => firstOrRaise Functions.getKey collection;
  let lastKeyOrRaise collection => lastOrRaise Functions.getKey collection;
  let firstValueOrRaise collection => firstOrRaise Functions.getValue collection;
  let lastValueOrRaise collection => lastOrRaise Functions.getValue collection;

  let keysSequentialCollectionBase: SequentialCollection.s (t 'v) k = {
    count,
    firstOrRaise: firstKeyOrRaise,
    reduce: Base.reduceKeys,
    toSequence: keysSequence,
  };

  let keysSequentialCollectionReversedBase: SequentialCollection.s (t 'v) k = {
    count,
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
    firstOrRaise: firstValueOrRaise,
    reduce: Base.reduceValues,
    toSequence: valuesSequence,
  };

  let valuesSequentialCollectionReversedBase: SequentialCollection.s (t 'v) 'v = {
    count,
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
    firstOrRaise,
    reduce: Base.reduce,
    reduceKeys: Base.reduceKeys,
    reduceValues: Base.reduceValues,
    toSequence,
  };

  let sequentialKeyedCollectionReversedBase: s (t 'v) k 'v  = {
    containsKey,
    count,
    firstOrRaise: lastOrRaise,
    reduce: Base.reduceReversed,
    reduceKeys: Base.reduceKeysReversed,
    reduceValues: Base.reduceValuesReversed,
    toSequence: toSequenceReversed,
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
  let firstOrRaise: ('k => 'v => 'c) => (t 'k 'v) => 'c;
  let lastOrRaise: ('k => 'v => 'c) => (t 'k 'v) => 'c;
  let reduce: while_::('acc => 'k => 'v => bool) => ('acc => 'k => 'v => 'acc) => 'acc => t 'k 'v => 'acc;
  let reduceReversed: while_::('acc => 'k => 'v => bool) => ('acc => 'k => 'v => 'acc) => 'acc => t 'k 'v => 'acc;
  let reduceKeys: while_::('acc => 'k => bool) => ('acc => 'k => 'acc) => 'acc => t 'k 'v => 'acc;
  let reduceKeysReversed: while_::('acc => 'k => bool) => ('acc => 'k => 'acc) => 'acc => t 'k 'v => 'acc;
  let reduceValues: while_::('acc => 'v => bool) => ('acc => 'v => 'acc) => 'acc => t 'k 'v => 'acc;
  let reduceValuesReversed: while_::('acc => 'v => bool) => ('acc => 'v => 'acc) => 'acc => t 'k 'v => 'acc;
  let toSequence: ('k => 'v => 'c) => (t 'k 'v) => Sequence.t 'c;
  let toSequenceReversed: ('k => 'v => 'c) => (t 'k 'v) => Sequence.t 'c;
}) => ({
  include Base;

  include (KeyedCollection.Make2 Base: KeyedCollection.S2 with type t 'k 'v := t 'k 'v);

  let first (selector: 'k => 'v => 'c) (keyed: t 'k 'v): (option 'c) =>
    if (isEmpty keyed) None
    else Some (firstOrRaise selector keyed);

  let last (selector: 'k => 'v => 'c) (keyed: t 'k 'v): (option 'c) =>
    if (isEmpty keyed) None
    else Some (lastOrRaise selector keyed);

  let module ReversedKeyedCollection = KeyedCollection.Make2 {
    type nonrec t 'k 'v = t 'k 'v;

    let containsKey = containsKey;
    let count = count;
    let reduce = Base.reduceReversed;
    let reduceKeys = Base.reduceKeysReversed;
    let reduceValues = Base.reduceValuesReversed;
    let toSequence = toSequenceReversed;
  };

  let keysCollectionReversed = ReversedKeyedCollection.keysCollection;
  let keysReversed = ReversedKeyedCollection.keys;
  let keysSequenceReversed = ReversedKeyedCollection.keysSequence;
  let reduceReversed = ReversedKeyedCollection.reduce;
  let reduceKeysReversed = ReversedKeyedCollection.reduceKeys;
  let reduceValuesReversed = ReversedKeyedCollection.reduceValues;
  let toIterableReversed = ReversedKeyedCollection.toIterable;
  let toKeyedCollectionReversed = ReversedKeyedCollection.toKeyedCollection;
  let toKeyedIterableReversed = ReversedKeyedCollection.toKeyedIterable;
  let toSequenceReversed = ReversedKeyedCollection.toSequence;
  let valuesCollectionReversed = ReversedKeyedCollection.valuesCollection;
  let valuesReversed = ReversedKeyedCollection.values;
  let valuesSequenceReversed = ReversedKeyedCollection.valuesSequence;

  let firstKeyOrRaise collection => firstOrRaise Functions.getKey collection;
  let lastKeyOrRaise collection => lastOrRaise Functions.getKey collection;
  let firstValueOrRaise collection => firstOrRaise Functions.getValue collection;
  let lastValueOrRaise collection => lastOrRaise Functions.getValue collection;

  let keysSequentialCollectionBase: SequentialCollection.s (t 'k 'v) 'k = {
    count,
    firstOrRaise: firstKeyOrRaise,
    reduce: Base.reduceKeys,
    toSequence: keysSequence,
  };

  let keysSequentialCollectionReversedBase: SequentialCollection.s (t 'k 'v) 'k = {
    count,
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
    firstOrRaise: firstValueOrRaise,
    reduce: Base.reduceValues,
    toSequence: valuesSequence,
  };

  let valuesSequentialCollectionReversedBase: SequentialCollection.s (t 'k 'v) 'v = {
    count,
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
    firstOrRaise,
    reduce: Base.reduce,
    reduceKeys: Base.reduceKeys,
    reduceValues: Base.reduceValues,
    toSequence,
  };

  let sequentialKeyedCollectionReversedBase: s (t 'k 'v) 'k 'v  = {
    containsKey,
    count,
    firstOrRaise: lastOrRaise,
    reduce: Base.reduceReversed,
    reduceKeys: Base.reduceKeysReversed,
    reduceValues: Base.reduceValuesReversed,
    toSequence: toSequenceReversed,
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

  let firstOrRaise (selector: 'k => 'v => 'c) (keyed: t 'k 'v): 'c => switch keyed {
    | Empty => failwith "empty"
    | Instance keyed { firstOrRaise } _ => firstOrRaise selector keyed
  };

  let lastOrRaise (selector: 'k => 'v => 'c) (keyed: t 'k 'v): 'c => switch keyed {
    | Empty => failwith "empty"
    | Instance keyed _ { firstOrRaise } => firstOrRaise selector keyed
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

  let toSequence (selector: 'k => 'v => 'c) (keyed: t 'k 'v): Sequence.t 'c => switch keyed {
    | Empty => Sequence.empty ()
    | Instance keyed { toSequence } _ => toSequence selector keyed
  };

  let toSequenceReversed (selector: 'k => 'v => 'c) (keyed: t 'k 'v): Sequence.t 'c => switch keyed {
    | Empty => Sequence.empty ()
    | Instance keyed _ { toSequence } => toSequence selector keyed
  };
}: S2 with type t 'k 'v := t 'k 'v);

let empty (): (t 'k 'v) => Empty;

let toNavigableKeyedCollection (keyed: t 'k 'v): t 'k 'v => keyed;
