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
  toSequence: 'c . ('k => 'v => 'c) => 'keyed => Sequence.t 'c,
};

type t 'k 'v =
  | Empty
  | Instance 'keyed (s 'keyed 'k 'v) (s 'keyed 'k 'v): t 'k 'v;

type navigableKeyedCollection 'k 'v = t 'k 'v;

module type SGeneric = {
  type t 'k 'v;
  type k 'k;
  type v 'v;

  include KeyedCollection.SGeneric with type t 'k 'v := t 'k 'v and type k 'k := k 'k and type v 'v := v 'v;

  let first: (k 'k => v 'v => 'c) => (t 'k 'v) => (option 'c);
  let firstOrRaise: (k 'k => v 'v => 'c) => (t 'k 'v) => 'c;
  let keysCollectionReversed: (t 'k 'v) => (Collection.t (k 'k));
  let keysNavigableCollection: (t 'k 'v) => (NavigableCollection.t (k 'k));
  let keysNavigableCollectionReversed: (t 'k 'v) => (NavigableCollection.t (k 'k));
  let keysReversed: (t 'k 'v) => (Iterable.t (k 'k));
  let keysSequentialCollection: (t 'k 'v) => (SequentialCollection.t (k 'k));
  let keysSequentialCollectionReversed: (t 'k 'v) => (SequentialCollection.t (k 'k));
  let keysSequenceReversed: (t 'k 'v) => Sequence.t (k 'k);
  let last: (k 'k => v 'v => 'c) => (t 'k 'v) => (option 'c);
  let lastOrRaise: (k 'k => v 'v => 'c) => (t 'k 'v) => 'c;
  let reduceReversed: while_::('acc => k 'k => v 'v => bool)? => ('acc => k 'k => v 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceKeysReversed: while_::('acc => k 'k => bool)? => ('acc => k 'k => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceValuesReversed: while_::('acc => v 'v => bool)? => ('acc => v 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let toIterableReversed: (k 'k => v 'v => 'c) => t 'k 'v => Iterable.t 'c;
  let toKeyedCollectionReversed: t 'k 'v => KeyedCollection.t (k 'k) (v 'v);
  let toKeyedIterableReversed: t 'k 'v => KeyedIterable.t (k 'k) (v 'v);
  let toNavigableKeyedCollection: t 'k 'v => navigableKeyedCollection (k 'k) (v 'v);
  let toNavigableKeyedCollectionReversed: t 'k 'v => navigableKeyedCollection (k 'k) (v 'v);
  let toSequenceReversed: (k 'k => v 'v => 'c) => (t 'k 'v) => (Sequence.t 'c);
  let valuesCollectionReversed: (t 'k 'v) => (Collection.t (v 'v));
  let valuesNavigableCollection: (t 'k 'v) => (NavigableCollection.t (v 'v));
  let valuesNavigableCollectionReversed: (t 'k 'v) => (NavigableCollection.t (v 'v));
  let valuesReversed: (t 'k 'v) => (Iterable.t (v 'v));
  let valuesSequentialCollection: (t 'k 'v) => (SequentialCollection.t (v 'v));
  let valuesSequentialCollectionReversed: (t 'k 'v) => (SequentialCollection.t (v 'v));
  let valuesSequenceReversed: (t 'k 'v) => (Sequence.t (v 'v));
};

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

let module MakeGeneric = fun (Base: {
  type t 'k 'v;
  type k 'k;
  type v 'v;

  let containsKey: k 'k => t 'k 'v => bool;
  let count: t 'k 'v => int;
  let firstOrRaise: (k 'k => v 'v => 'c) => (t 'k 'v) => 'c;
  let lastOrRaise: (k 'k => v 'v => 'c) => (t 'k 'v) => 'c;
  let reduce: while_::('acc => k 'k => v 'v => bool) => ('acc => k 'k => v 'v => 'acc) => 'acc => t 'k 'v => 'acc;
  let reduceReversed: while_::('acc => k 'k => v 'v => bool) => ('acc => k 'k => v 'v => 'acc) => 'acc => t 'k 'v => 'acc;
  let reduceKeys: while_::('acc => k 'k => bool) => ('acc => k 'k => 'acc) => 'acc => t 'k 'v => 'acc;
  let reduceKeysReversed: while_::('acc => k 'k => bool) => ('acc => k 'k => 'acc) => 'acc => t 'k 'v => 'acc;
  let reduceValues: while_::('acc => v 'v => bool) => ('acc => v 'v => 'acc) => 'acc => t 'k 'v => 'acc;
  let reduceValuesReversed: while_::('acc => v 'v => bool) => ('acc => v 'v => 'acc) => 'acc => t 'k 'v => 'acc;
  let toSequence: (k 'k => v 'v => 'c) => (t 'k 'v) => Sequence.t 'c;
  let toSequenceReversed: (k 'k => v 'v => 'c) => (t 'k 'v) => Sequence.t 'c;
}) => ({
  include Base;

  include (KeyedCollection.MakeGeneric Base: KeyedCollection.SGeneric with type t 'k 'v := t 'k 'v  and type k 'k := k 'k and type v 'v := v 'v);

  let first (selector: k 'k => v 'v => 'c) (keyed: t 'k 'v): (option 'c) =>
    if (isEmpty keyed) None
    else Some (firstOrRaise selector keyed);

  let last (selector: k 'k => v 'v => 'c) (keyed: t 'k 'v): (option 'c) =>
    if (isEmpty keyed) None
    else Some (lastOrRaise selector keyed);

  let module ReversedKeyedCollection = KeyedCollection.MakeGeneric {
    type nonrec t 'k 'v = t 'k 'v;
    type nonrec k 'k = k 'k;
    type nonrec v 'v = v 'v;

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

  let keysSequentialCollectionBase: SequentialCollection.s (t 'k 'v) (k 'k) = {
    count,
    firstOrRaise: firstKeyOrRaise,
    reduce: Base.reduceKeys,
    toSequence: keysSequence,
  };

  let keysSequentialCollectionReversedBase: SequentialCollection.s (t 'k 'v) (k 'k) = {
    count,
    firstOrRaise: lastKeyOrRaise,
    reduce: Base.reduceKeysReversed,
    toSequence: keysSequenceReversed,
  };

  let keysSequentialCollection (collection: t 'k 'v): (SequentialCollection.t (k 'k)) =>
    if (isEmpty collection) (SequentialCollection.empty ())
    else SequentialCollection.Instance collection keysSequentialCollectionBase;

  let keysSequentialCollectionReversed (collection: t 'k 'v): (SequentialCollection.t (k 'k)) =>
    if (isEmpty collection) (SequentialCollection.empty ())
    else SequentialCollection.Instance collection keysSequentialCollectionReversedBase;

  let keysNavigableCollection (collection: t 'k 'v): (NavigableCollection.t (k 'k)) =>
    if (isEmpty collection) (NavigableCollection.empty ())
    else NavigableCollection.Instance collection keysSequentialCollectionBase keysSequentialCollectionReversedBase;

  let keysNavigableCollectionReversed (collection: t 'k 'v): (NavigableCollection.t (k 'k)) =>
    if (isEmpty collection) (NavigableCollection.empty ())
    else NavigableCollection.Instance collection keysSequentialCollectionReversedBase keysSequentialCollectionBase;

  let valuesSequentialCollectionBase: SequentialCollection.s (t 'k 'v) (v 'v) = {
    count,
    firstOrRaise: firstValueOrRaise,
    reduce: Base.reduceValues,
    toSequence: valuesSequence,
  };

  let valuesSequentialCollectionReversedBase: SequentialCollection.s (t 'k 'v) (v 'v) = {
    count,
    firstOrRaise: lastValueOrRaise,
    reduce: Base.reduceValuesReversed,
    toSequence: valuesSequenceReversed,
  };

  let valuesSequentialCollection (collection: t 'k 'v): (SequentialCollection.t (v 'v)) =>
    if (isEmpty collection) (SequentialCollection.empty ())
    else SequentialCollection.Instance collection valuesSequentialCollectionBase;

  let valuesSequentialCollectionReversed (collection: t 'k 'v): (SequentialCollection.t (v 'v)) =>
    if (isEmpty collection) (SequentialCollection.empty ())
    else SequentialCollection.Instance collection valuesSequentialCollectionReversedBase;

  let valuesNavigableCollection (collection: t 'k 'v): (NavigableCollection.t (v 'v)) =>
    if (isEmpty collection) (NavigableCollection.empty ())
    else NavigableCollection.Instance collection valuesSequentialCollectionBase valuesSequentialCollectionReversedBase;

  let valuesNavigableCollectionReversed (collection: t 'k 'v): (NavigableCollection.t (v 'v)) =>
    if (isEmpty collection) (NavigableCollection.empty ())
    else NavigableCollection.Instance collection valuesSequentialCollectionReversedBase valuesSequentialCollectionBase;

  let sequentialKeyedCollectionBase: s (t 'k 'v) (k 'k) (v 'v)  = {
    containsKey,
    count,
    firstOrRaise,
    reduce: Base.reduce,
    toSequence,
  };

  let sequentialKeyedCollectionReversedBase: s (t 'k 'v) (k 'k) (v 'v)  = {
    containsKey,
    count,
    firstOrRaise: lastOrRaise,
    reduce: Base.reduceReversed,
    toSequence: toSequenceReversed,
  };

  let toNavigableKeyedCollection (keyed: t 'k 'v): navigableKeyedCollection (k 'k) (v 'v) =>
    if (isEmpty keyed) Empty
    else Instance keyed sequentialKeyedCollectionBase sequentialKeyedCollectionReversedBase;

  let toNavigableKeyedCollectionReversed (keyed: t 'k 'v): navigableKeyedCollection (k 'k) (v 'v) =>
    if (isEmpty keyed) Empty
    else Instance keyed sequentialKeyedCollectionReversedBase sequentialKeyedCollectionBase;

}: SGeneric with type t 'k 'v := Base.t 'k 'v and type k 'k := Base.k 'k and type v 'v := Base.v 'v);

include (MakeGeneric {
  type nonrec t 'k 'v = t 'k 'v;
  type k 'k = 'k;
  type v 'v = 'v;

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

  include (KeyedReducer.MakeGeneric {
    type nonrec t 'k 'v = t 'k 'v;
    type k 'k = 'k;
    type v 'v = 'v;

    let reduce
        while_::(predicate:'acc => 'k => 'v => bool)
        (f: 'acc => 'k => 'v => 'acc)
        (acc: 'acc)
        (iter: t 'k 'v): 'acc => switch iter {
      | Empty => acc
      | Instance iter { reduce } _ => iter |> reduce while_::predicate f acc;
    };
  }: KeyedReducer.S2 with type t 'k 'v:= t 'k 'v);

  let module KeyedReducerReversed = (KeyedReducer.MakeGeneric {
    type nonrec t 'k 'v = t 'k 'v;
    type k 'k = 'k;
    type v 'v = 'v;

    let reduce
        while_::(predicate:'acc => 'k => 'v => bool)
        (f: 'acc => 'k => 'v => 'acc)
        (acc: 'acc)
        (iter: t 'k 'v): 'acc => switch iter {
      | Empty => acc
      | Instance iter _ { reduce } => iter |> reduce while_::predicate f acc;
    };
  }: KeyedReducer.S2 with type t 'k 'v:= t 'k 'v);

  let reduceReversed = KeyedReducerReversed.reduce;

  let reduceKeysReversed = KeyedReducerReversed.reduceKeys;

  let reduceValuesReversed = KeyedReducerReversed.reduceValues;

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
