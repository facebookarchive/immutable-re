/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Functions.Operators;

 /** AVL tree based Map. */
module type S1 = {
  type k;
  type t +'v;

  let first: t 'v => option (k, 'v);
  let firstOrRaise: t 'v => (k, 'v);
  let firstKey: t 'v => option k;
  let firstKeyOrRaise: t 'v => k;
  let firstValue: t 'v => option 'v;
  let firstValueOrRaise: t 'v => 'v;
  let keysReversed: t 'v => Iterable.t k;
  let last: t 'v => option (k, 'v);
  let lastOrRaise: t 'v => (k, 'v);
  let lastKey: t 'v => option k;
  let lastKeyOrRaise: t 'v => k;
  let lastValue: t 'v => option 'v;
  let lastValueOrRaise: t 'v => 'v;
  let reduceReversed:
    while_::('acc => k => 'v => bool)? =>
    ('acc => k => 'v => 'acc) => 'acc => t 'v => 'acc;
  let toIterableReversed: t 'v => Iterable.t (k, 'v);
  let toKeyedIterableReversed: t 'v => KeyedIterable.t k 'v;
  let toNavigableKeyedCollection: t 'v => NavigableKeyedCollection.t k 'v;
  let toSequenceReversed: t 'v => Sequence.t (k, 'v);
  let valuesReversed: t 'v => Iterable.t 'v;
  let navigableKeySet: t 'v => NavigableSet.t k;
  let toNavigableMap: t 'v => NavigableMap.t k 'v;
  let remove: k => t 'v => t 'v;
  let removeAll: t 'v => t 'v;
  let keys: t 'v => Iterable.t k;
  let reduce:
    while_::('acc => k => 'v => bool)? =>
    ('acc => k => 'v => 'acc) => 'acc => t 'v => 'acc;
  let toIterable: t 'v => Iterable.t (k, 'v);
  let toKeyedIterable: t 'v => KeyedIterable.t k 'v;
  let values: t 'v => Iterable.t 'v;
  let containsKey: k => t 'v => bool;
  let count: t 'v => int;
  let isEmpty: t 'v => bool;
  let isNotEmpty: t 'v => bool;
  let toKeyedCollection: t 'v => KeyedCollection.t k 'v;
  let toSequence: t 'v => Sequence.t (k, 'v);
  let get: k => t 'v => option 'v;
  let getOrRaise: k => t 'v => 'v;
  let keySet: t 'v => ImmSet.t k;
  let toMap: t 'v => ImmMap.t k 'v;
  let alter: k => (option 'v => option 'v) => t 'v => t 'v;
  let empty: unit => t 'v;
  let from: KeyedIterable.t k 'v => t 'v;
  let fromEntries: Iterable.t (k, 'v) => t 'v;
  let merge:
    (k => option 'vAcc => option 'v => option 'vAcc) =>
    t 'vAcc => t 'v => t 'vAcc;
  let put: k => 'v => t 'v => t 'v;
  let putAll: KeyedIterable.t k 'v => t 'v => t 'v;
  let putAllEntries: Iterable.t (k, 'v) => t 'v => t 'v;
  let removeFirstOrRaise: t 'v => t 'v;
  let removeLastOrRaise: t 'v => t 'v;
};

let module Make1 = fun (Comparable: Comparable.S) => {
  type k = Comparable.t;

  let comparator = Comparable.compare;

  type t 'v = {
    count: int,
    tree: AVLTreeMap.t k 'v,
  };

  let empty (): (t 'v) => {
    count: 0,
    tree: AVLTreeMap.Empty,
  };

  let alter
      (key: k)
      (f: option 'v => option 'v)
      ({ count, tree } as map: t 'v): (t 'v) => {
    let alterResult = ref AlterResult.NoChange;
    let newTree = tree |> AVLTreeMap.alter comparator alterResult key f;
    switch !alterResult {
      | AlterResult.Added => { count: count + 1, tree: newTree }
      | AlterResult.NoChange => map
      | AlterResult.Replace => { count, tree: newTree }
      | AlterResult.Removed => { count: count - 1, tree: newTree }
    };
  };

  let containsKey (key: k) ({ tree }: t 'v): bool =>
    tree |> AVLTreeMap.containsKey comparator key;

  let count ({ count }: t 'v): int => count;

  let first ({ tree }: t 'v): (option (k, 'v)) =>
    tree |> AVLTreeMap.first;

  let firstOrRaise ({ tree }: t 'v): (k, 'v) =>
    tree |> AVLTreeMap.firstOrRaise;

  let firstKey ({ tree }: t 'v): (option k) =>
    tree |> AVLTreeMap.firstKey;

  let firstKeyOrRaise ({ tree }: t 'v): k =>
    tree |> AVLTreeMap.firstKeyOrRaise;

  let firstValue ({ tree }: t 'v): (option 'v) =>
    tree |> AVLTreeMap.firstValue;

  let firstValueOrRaise ({ tree }: t 'v): 'v =>
    tree |> AVLTreeMap.firstValueOrRaise;

  let get (key: k) ({ tree }: t 'v): (option 'v) =>
    tree |> AVLTreeMap.get comparator key;

  let getOrRaise (key: k) ({ tree }: t 'v): 'v =>
    tree |> AVLTreeMap.getOrRaise comparator key;

  let isEmpty ({ count }: t 'v): bool =>
    count === 0;

  let isNotEmpty ({ count }: t 'v): bool =>
    count !== 0;

  let last ({ tree }: t 'v): (option (k, 'v)) =>
    tree |> AVLTreeMap.last;

  let lastOrRaise ({ tree }: t 'v): (k, 'v) =>
    tree |> AVLTreeMap.lastOrRaise;

  let lastKey ({ tree }: t 'v): (option k) =>
    tree |> AVLTreeMap.lastKey;

  let lastKeyOrRaise ({ tree }: t 'v): k =>
    tree |> AVLTreeMap.lastKeyOrRaise;

  let lastValue ({ tree }: t 'v): (option 'v) =>
    tree |> AVLTreeMap.lastValue;

  let lastValueOrRaise ({ tree }: t 'v): 'v =>
    tree |> AVLTreeMap.lastValueOrRaise;

  let put (key: k) (value: 'v) ({ count, tree } as map: t 'v): (t 'v) => {
    let alterResult = ref AlterResult.NoChange;
    let newTree = tree |> AVLTreeMap.putWithResult comparator alterResult key value;
    switch !alterResult {
      | AlterResult.Added => { count: count + 1, tree: newTree }
      | AlterResult.NoChange => map
      | AlterResult.Replace => { count, tree: newTree }
      | AlterResult.Removed => failwith "invalid state"
    };
  };

  let putAll (iter: KeyedIterable.t k 'v) (map: t 'v): (t 'v) =>
    iter |> KeyedIterable.reduce (fun acc k v => acc |> put k v) map;

  let putAllEntries (iter: Iterable.t (k, 'v)) (map: t 'v): (t 'v) =>
    iter |> Iterable.reduce (fun acc (k, v) => acc |> put k v) map;

  let from (iter: KeyedIterable.t k 'v): (t 'v) =>
    empty () |> putAll iter;

  let fromEntries (iter: Iterable.t (k, 'v)): (t 'v) =>
    empty () |> putAllEntries iter;

  let reduceImpl
      while_::(predicate: 'acc => k => 'v => bool)
      (f: 'acc => k => 'v => 'acc)
      (acc: 'acc)
      ({ tree }: t 'v): 'acc =>
    if (predicate === Functions.alwaysTrue3) (AVLTreeMap.reduce f acc tree)
    else (AVLTreeMap.reduceWhile predicate f acc tree);

  let reduce
      while_::(predicate: 'acc => k => 'v => bool)=Functions.alwaysTrue3
      (f: 'acc => k => 'v => 'acc)
      (acc: 'acc)
      (map: t 'v): 'acc =>
    reduceImpl while_::predicate f acc map;

  let reduceReversedImpl
      while_::(predicate: 'acc => k => 'v => bool)
      (f: 'acc => k => 'v => 'acc)
      (acc: 'acc)
      ({ tree }: t 'v): 'acc =>
    if (predicate === Functions.alwaysTrue3) (AVLTreeMap.reduceReversed f acc tree)
    else (AVLTreeMap.reduceReversedWhile predicate f acc tree);

  let reduceReversed
      while_::(predicate: 'acc => k => 'v => bool)=Functions.alwaysTrue3
      (f: 'acc => k => 'v => 'acc)
      (acc: 'acc)
      (map: t 'v): 'acc =>
    reduceReversedImpl while_::predicate f acc map;

  let remove (key: k) (map: t 'v): (t 'v) =>
    map |> alter key Functions.alwaysNone;

  let removeAll (_: t 'v): (t 'v) =>
    empty ();

  let removeFirstOrRaise ({ count, tree }: t 'v): (t 'v) => {
    let newTree = tree |> AVLTreeMap.removeFirstOrRaise;
    { count: count - 1, tree: newTree }
  };

  let removeLastOrRaise ({ count, tree }: t 'v): (t 'v) => {
    let newTree = tree |> AVLTreeMap.removeLastOrRaise;
    { count: count - 1, tree: newTree }
  };

  let toKeySequence ({ tree }: t 'v): (Sequence.t k) =>
    tree |> AVLTreeMap.toKeySequence;

  let toKeySequenceReversed ({ tree }: t 'v): (Sequence.t k) =>
    tree |> AVLTreeMap.toKeySequenceReversed;

  let toSequence ({ tree }: t 'v): (Sequence.t (k, 'v)) =>
    tree |> AVLTreeMap.toSequence;

  let toSequenceReversed ({ tree }: t 'v): (Sequence.t (k, 'v)) =>
    tree |> AVLTreeMap.toSequenceReversed;

  let iterator: Iterable.Iterator.t (k, 'v) (t 'v) = {
    reduce: fun while_::predicate f acc map => map |> reduce
      while_::(fun acc k v => predicate acc (k, v))
      (fun acc k v => f acc (k, v))
      acc
  };

  let toIterable (map: t 'v): (Iterable.t (k, 'v)) =>
    if (isEmpty map) (Iterable.empty ())
    else Iterable.Iterable map iterator;

  let iteratorReversed: Iterable.Iterator.t (k, 'v) (t 'v) = {
    reduce: fun while_::predicate f acc map => map |> reduceReversed
      while_::(fun acc k v => predicate acc (k, v))
      (fun acc k v => f acc (k, v))
      acc
  };

  let toIterableReversed (map: t 'v): (Iterable.t (k, 'v)) =>
    if (isEmpty map) (Iterable.empty ())
    else Iterable.Iterable map iteratorReversed;

  let keyedIterator: KeyedIterable.KeyedIterator.t k 'v (t 'v) = { reduce: reduceImpl };

  let toKeyedIterable (map: t 'v): (KeyedIterable.t k 'v) =>
    if (isEmpty map) (KeyedIterable.empty ())
    else KeyedIterable.KeyedIterable map keyedIterator;

  let keyedIteratorReversed: KeyedIterable.KeyedIterator.t k 'v (t 'v) = { reduce: reduceReversedImpl };

  let toKeyedIterableReversed (map: t 'v): (KeyedIterable.t k 'v) =>
    if (isEmpty map) (KeyedIterable.empty ())
    else KeyedIterable.KeyedIterable map keyedIteratorReversed;

  let keys (map: t 'v): (Iterable.t k) =>
    map |> toKeyedIterable |> KeyedIterable.keys;

  let keysReversed (map: t 'v): (Iterable.t k) =>
    map |> toKeyedIterableReversed |> KeyedIterable.keys;

  let values (map: t 'v): (Iterable.t 'v) =>
    map |> toKeyedIterable |> KeyedIterable.values;

  let valuesReversed (map: t 'v): (Iterable.t 'v) =>
    map |> toKeyedIterableReversed |> KeyedIterable.values;

  let keyCollectionOps (): Collection.Ops.t 'k (t 'v) => {
    count: count,
    toIterable: toKeyedIterable >> KeyedIterable.keys,
    toSequence: toKeySequence,
  };

  let toKeyCollection (map: t _): Collection.t k =>
    if (isEmpty map) (Collection.empty ())
    else (Collection.Collection map (keyCollectionOps ()));

  let keySetOps (): ImmSet.Ops.t k (t 'v) => {
    contains: containsKey,
    count,
    toCollection: toKeyCollection,
    toIterable: toKeyedIterable >> KeyedIterable.keys,
    toSequence: toKeySequence,
  };

  let keySet (map: t 'v): (ImmSet.t k) =>
    if (isEmpty map) (ImmSet.empty ())
    else ImmSet.Set map (keySetOps ());

  let sequentialKeyCollectionOps (): SequentialCollection.Ops.t k (t 'v) => {
    count,
    first: firstKey,
    firstOrRaise: firstKeyOrRaise,
    toCollection: toKeyCollection,
    toIterable: toKeyedIterable >> KeyedIterable.keys,
    toSequence: toKeySequence,
  };

  let toSequentialKeyCollection (map: t 'v): (SequentialCollection.t k) =>
    if (isEmpty map) (SequentialCollection.empty ())
    else SequentialCollection.SequentialCollection map (sequentialKeyCollectionOps ());

  let navigableKeyCollectionOps (): NavigableCollection.Ops.t k (t 'v) => {
    count,
    first: firstKey,
    firstOrRaise: firstKeyOrRaise,
    last: lastKey,
    lastOrRaise: lastKeyOrRaise,
    toCollection: toKeyCollection,
    toSequentialCollection: toSequentialKeyCollection,
    toIterable: toKeyedIterable >> KeyedIterable.keys,
    toIterableReversed: toKeyedIterable >> KeyedIterable.keys,
    toSequence: toKeySequence,
    toSequenceReversed: toKeySequenceReversed,
  };

  let toNavigableKeyCollection (map: t 'v): (NavigableCollection.t k) =>
    if (isEmpty map) (NavigableCollection.empty ())
    else NavigableCollection.NavigableCollection map (navigableKeyCollectionOps ());

  let navigableKeySetOps (): NavigableSet.Ops.t k (t 'v) => {
    contains: containsKey,
    count,
    first: firstKey,
    firstOrRaise: firstKeyOrRaise,
    last: lastKey,
    lastOrRaise: lastKeyOrRaise,
    toCollection: toKeyCollection,
    toIterable: toKeyedIterable >> KeyedIterable.keys,
    toIterableReversed: toKeyedIterable >> KeyedIterable.keys,
    toNavigableCollection: toNavigableKeyCollection,
    toSequence: toKeySequence,
    toSequenceReversed: toKeySequenceReversed,
    toSequentialCollection: toSequentialKeyCollection,
    toSet: keySet,
  };

  let navigableKeySet (map: t 'v): (NavigableSet.t k) =>
    if (isEmpty map) (NavigableSet.empty ())
    else NavigableSet.NavigableSet map (navigableKeySetOps ());

  let keyedCollectionOps: KeyedCollection.Ops.t k 'v (t 'v) = {
    containsKey,
    count,
    keys,
    toIterable,
    toKeyedIterable,
    toSequence,
    values,
  };

  let toKeyedCollection (map: t 'v): (KeyedCollection.t k 'v) =>
    if (isEmpty map) (KeyedCollection.empty ())
    else KeyedCollection.KeyedCollection map keyedCollectionOps;

  let mapOps: ImmMap.Ops.t k 'v (t 'v) = {
    containsKey,
    count,
    get,
    getOrRaise,
    keys,
    keySet,
    toIterable,
    toKeyedCollection,
    toKeyedIterable,
    toSequence,
    values,
  };

  let toMap (map: t 'v): (ImmMap.t k 'v) =>
    if (isEmpty map) (ImmMap.empty ())
    else ImmMap.Map map mapOps;

  let navigableKeyedCollectionOps: NavigableKeyedCollection.Ops.t k 'v (t 'v) = {
    containsKey,
    count,
    first,
    firstOrRaise,
    firstKey,
    firstKeyOrRaise,
    firstValue,
    firstValueOrRaise,
    keys,
    keysReversed,
    last,
    lastOrRaise,
    lastKey,
    lastKeyOrRaise,
    lastValue,
    lastValueOrRaise,
    toIterable,
    toIterableReversed,
    toKeyedCollection,
    toKeyedIterable,
    toKeyedIterableReversed,
    toSequence,
    toSequenceReversed,
    values,
    valuesReversed,
  };

  let toNavigableKeyedCollection (map: t 'v): (NavigableKeyedCollection.t k 'v) =>
    if (isEmpty map) (NavigableKeyedCollection.empty ())
    else NavigableKeyedCollection.NavigableKeyedCollection map navigableKeyedCollectionOps;

  let navigableMap: NavigableMap.Ops.t k 'v (t 'v) = {
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
    keysReversed,
    keySet,
    last,
    lastOrRaise,
    lastKey,
    lastKeyOrRaise,
    lastValue,
    lastValueOrRaise,
    navigableKeySet,
    toIterable,
    toIterableReversed,
    toKeyedCollection,
    toKeyedIterable,
    toKeyedIterableReversed,
    toMap,
    toNavigableKeyedCollection,
    toSequence,
    toSequenceReversed,
    values,
    valuesReversed
  };

  let toNavigableMap (map: t 'v): (NavigableMap.t k 'v) =>
    if (isEmpty map) (NavigableMap.empty ())
    else NavigableMap.NavigableMap map navigableMap;

  let merge
      (f: k => (option 'vAcc) => (option 'v) => (option 'vAcc))
      (acc: t 'vAcc)
      (next: t 'v): (t 'vAcc) =>  ImmSet.union (keySet next) (keySet acc)
    |> Iterable.reduce (
        fun acc key => {
          let result = f key (acc |> get key) (next |> get key);
          switch result {
            | None => acc |> remove key
            | Some value => acc |> put key value
          }
        }
      ) acc;
};
