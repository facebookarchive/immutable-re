/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

 /** AVL tree based Map. */
module type S1 = {
  type k;
  type t +'v;

  let first: (k => 'v => 'c) => t 'v => option 'c;
  let firstOrRaise: (k => 'v => 'c) => t 'v => 'c;
  let keysCollectionReversed: t 'v => Collection.t k;
  let keysNavigableCollection: t 'v => NavigableCollection.t k;
  let keysNavigableCollectionReversed: t 'v => NavigableCollection.t k;
  let keysReversed: t 'v => Iterable.t k;
  let keysSequentialCollection: t 'v => SequentialCollection.t k;
  let keysSequentialCollectionReversed: t 'v => SequentialCollection.t k;
  let keysSequenceReversed: t 'v => Sequence.t k;
  let last: (k => 'v => 'c) => t 'v => option 'c;
  let lastOrRaise: (k => 'v => 'c) => t 'v => 'c;
  let reduceReversed:
    while_::('acc => k => 'v => bool)? =>
    ('acc => k => 'v => 'acc) => 'acc => t 'v => 'acc;
  let reduceKeysReversed:
    while_::('acc => k => bool)? =>
    ('acc => k => 'acc) => 'acc => t 'v => 'acc;
  let reduceValuesReversed:
    while_::('acc => 'v => bool)? =>
    ('acc => 'v => 'acc) => 'acc => t 'v => 'acc;
  let toIterableReversed: (k => 'v => 'c) => t 'v => Iterable.t 'c;
  let toKeyedCollectionReversed: t 'v => KeyedCollection.t k 'v;
  let toKeyedIterableReversed: t 'v => KeyedIterable.t k 'v;
  let toNavigableKeyedCollection: t 'v => NavigableKeyedCollection.t k 'v;
  let toNavigableKeyedCollectionReversed:
    t 'v => NavigableKeyedCollection.t k 'v;
  let toSequenceReversed: (k => 'v => 'c) => t 'v => Sequence.t 'c;
  let valuesCollectionReversed: t 'v => Collection.t 'v;
  let valuesNavigableCollection: t 'v => NavigableCollection.t 'v;
  let valuesNavigableCollectionReversed: t 'v => NavigableCollection.t 'v;
  let valuesReversed: t 'v => Iterable.t 'v;
  let valuesSequentialCollection: t 'v => SequentialCollection.t 'v;
  let valuesSequentialCollectionReversed: t 'v => SequentialCollection.t 'v;
  let valuesSequenceReversed: t 'v => Sequence.t 'v;
  let keysNavigableSet: t 'v => NavigableSet.t k;
  let keysNavigableSetReversed: t 'v => NavigableSet.t k;
  let toMapReversed: t 'v => ImmMap.t k 'v;
  let toNavigableMap: t 'v => NavigableMap.t k 'v;
  let toNavigableMapReversed: t 'v => NavigableMap.t k 'v;
  let remove: k => t 'v => t 'v;
  let removeAll: t 'v => t 'v;
  let every: (k => 'v => bool) => t 'v => bool;
  let find: (k => 'v => 'c) => (k => 'v => bool) => t 'v => option 'c;
  let findOrRaise: (k => 'v => 'c) => (k => 'v => bool) => t 'v => 'c;
  let forEach:
    while_::(k => 'v => bool)? => (k => 'v => unit) => t 'v => unit;
  let keys: t 'v => Iterable.t k;
  let none: (k => 'v => bool) => t 'v => bool;
  let reduce:
    while_::('acc => k => 'v => bool)? =>
    ('acc => k => 'v => 'acc) => 'acc => t 'v => 'acc;
  let reduceKeys:
    while_::('acc => k => bool)? =>
    ('acc => k => 'acc) => 'acc => t 'v => 'acc;
  let reduceValues:
    while_::('acc => 'v => bool)? =>
    ('acc => 'v => 'acc) => 'acc => t 'v => 'acc;
  let some: (k => 'v => bool) => t 'v => bool;
  let toIterable: (k => 'v => 'c) => t 'v => Iterable.t 'c;
  let toKeyedIterable: t 'v => KeyedIterable.t k 'v;
  let values: t 'v => Iterable.t 'v;
  let containsKey: k => t 'v => bool;
  let count: t 'v => int;
  let isEmpty: t 'v => bool;
  let isNotEmpty: t 'v => bool;
  let keysCollection: t 'v => Collection.t k;
  let keysSequence: t 'v => Sequence.t k;
  let toKeyedCollection: t 'v => KeyedCollection.t k 'v;
  let toSequence: (k => 'v => 'c) => t 'v => Sequence.t 'c;
  let valuesCollection: t 'v => Collection.t 'v;
  let valuesSequence: t 'v => Sequence.t 'v;
  let get: k => t 'v => option 'v;
  let getOrDefault: default::'v => k => (t 'v) => 'v;
  let getOrRaise: k => t 'v => 'v;
  let keysSet: t 'v => ImmSet.t k;
  let toMap: t 'v => ImmMap.t k 'v;
  let alter: k => (option 'v => option 'v) => t 'v => t 'v;
  let merge:
    (k => option 'vAcc => option 'v => option 'vAcc) =>
    t 'vAcc => t 'v => t 'vAcc;
  let put: k => 'v => t 'v => t 'v;
  let putAll: KeyedIterable.t k 'v => t 'v => t 'v;
  let putAllEntries: Iterable.t (k, 'v) => t 'v => t 'v;
  let removeFirstOrRaise: t 'v => t 'v;
  let removeLastOrRaise: t 'v => t 'v;
  let empty: unit => t 'v;
  let from: KeyedIterable.t k 'v => t 'v;
  let fromEntries: Iterable.t (k, 'v) => t 'v;
};

let module Make1 = fun (Comparable: Comparable.S) => {
  type k = Comparable.t;

  let comparator = Comparable.compare;

  type t 'v = {
    count: int,
    tree: AVLTreeMap.t k 'v,
  };

  include (NavigableMap.Make1 {
    type nonrec k = k;
    type nonrec t 'v = t 'v;

    let containsKey (key: k) ({ tree }: t 'v): bool =>
      tree |> AVLTreeMap.containsKey comparator key;

    let count ({ count }: t 'v): int => count;

    let firstOrRaise (selector: k => 'v => 'c) ({ tree }: t 'v): 'c =>
      tree |> AVLTreeMap.firstOrRaise selector;

    let get (key: k) ({ tree }: t 'v): (option 'v) =>
      tree |> AVLTreeMap.get comparator key;

    let getOrDefault default::(default: 'v) (key: k) ({ tree }: t 'v): 'v =>
      tree |> AVLTreeMap.getOrDefault comparator ::default key;

    let getOrRaise (key: k) ({ tree }: t 'v): 'v =>
      tree |> AVLTreeMap.getOrRaise comparator key;

    let lastOrRaise (selector: k => 'v => 'c) ({ tree }: t 'v): 'c =>
      tree |> AVLTreeMap.lastOrRaise selector;

    let reduce
        while_::(predicate: 'acc => k => 'v => bool)
        (f: 'acc => k => 'v => 'acc)
        (acc: 'acc)
        ({ tree }: t 'v): 'acc =>
      if (predicate === Functions.alwaysTrue3) (AVLTreeMap.reduce f acc tree)
      else (AVLTreeMap.reduceWhile predicate f acc tree);

    let reduceReversed
        while_::(predicate: 'acc => k => 'v => bool)
        (f: 'acc => k => 'v => 'acc)
        (acc: 'acc)
        ({ tree }: t 'v): 'acc =>
      if (predicate === Functions.alwaysTrue3) (AVLTreeMap.reduceReversed f acc tree)
      else (AVLTreeMap.reduceReversedWhile predicate f acc tree);

    let reduceKeys
        while_::(predicate: 'acc => k => bool)
        (f: 'acc => k => 'acc)
        (acc: 'acc)
        ({ tree }: t 'v): 'acc =>
      if (predicate === Functions.alwaysTrue2) (AVLTreeMap.reduceKeys f acc tree)
      else (AVLTreeMap.reduceKeysWhile predicate f acc tree);

    let reduceKeysReversed
        while_::(predicate: 'acc => k => bool)
        (f: 'acc => k => 'acc)
        (acc: 'acc)
        ({ tree }: t 'v): 'acc =>
      if (predicate === Functions.alwaysTrue2) (AVLTreeMap.reduceKeysReversed f acc tree)
      else (AVLTreeMap.reduceKeysReversedWhile predicate f acc tree);

    let reduceValues
        while_::(predicate: 'acc => 'v => bool)
        (f: 'acc => 'v => 'acc)
        (acc: 'acc)
        ({ tree }: t 'v): 'acc =>
      if (predicate === Functions.alwaysTrue2) (AVLTreeMap.reduceValues f acc tree)
      else (AVLTreeMap.reduceValuesWhile predicate f acc tree);

    let reduceValuesReversed
        while_::(predicate: 'acc => 'v => bool)
        (f: 'acc => 'v => 'acc)
        (acc: 'acc)
        ({ tree }: t 'v): 'acc =>
      if (predicate === Functions.alwaysTrue2) (AVLTreeMap.reduceValuesReversed f acc tree)
      else (AVLTreeMap.reduceValuesReversedWhile predicate f acc tree);

    let toSequence (selector: k => 'v => 'c) ({ tree }: t 'v): (Sequence.t 'c) =>
      tree |> AVLTreeMap.toSequence selector;

    let toSequenceReversed (selector: k => 'v => 'c) ({ tree }: t 'v): (Sequence.t 'c) =>
      tree |> AVLTreeMap.toSequenceReversed selector;
  }: NavigableMap.S1 with type t 'v:= t 'v and type k:= k);

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

  let empty (): (t 'v) => {
    count: 0,
    tree: AVLTreeMap.Empty,
  };

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

  let merge
      (f: k => (option 'vAcc) => (option 'v) => (option 'vAcc))
      (acc: t 'vAcc)
      (next: t 'v): (t 'vAcc) =>  ImmSet.union (keysSet next) (keysSet acc)
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
