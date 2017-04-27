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

  let first: t 'v => option (k, 'v);
  let firstOrRaise: t 'v => (k, 'v);
  let firstKey: t 'v => option k;
  let firstKeyOrRaise: t 'v => k;
  let firstValue: t 'v => option 'v;
  let firstValueOrRaise: t 'v => 'v;
  let keysCollectionReversed: t 'v => Collection.t k;
  let keysNavigableCollection: t 'v => NavigableCollection.t k;
  let keysNavigableCollectionReversed: t 'v => NavigableCollection.t k;
  let keysSequentialCollection: t 'v => SequentialCollection.t k;
  let keysSequentialCollectionReversed: t 'v => SequentialCollection.t k;
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
  let reduceKeysReversed:
    while_::('acc => k => bool)? =>
    ('acc => k => 'acc) => 'acc => t 'v => 'acc;
  let reduceValuesReversed:
    while_::('acc => 'v => bool)? =>
    ('acc => 'v => 'acc) => 'acc => t 'v => 'acc;
  let toIterableReversed: t 'v => Iterable.t (k, 'v);
  let toKeyedCollectionReversed: t 'v => KeyedCollection.t k 'v;
  let toKeyedIterableReversed: t 'v => KeyedIterable.t k 'v;
  let toNavigableKeyedCollection: t 'v => NavigableKeyedCollection.t k 'v;
  let toNavigableKeyedCollectionReversed:
    t 'v => NavigableKeyedCollection.t k 'v;
  let toSequenceReversed: t 'v => Sequence.t (k, 'v);
  let valuesCollectionReversed: t 'v => Collection.t 'v;
  let valuesNavigableCollection: t 'v => NavigableCollection.t 'v;
  let valuesNavigableCollectionReversed: t 'v => NavigableCollection.t 'v;
  let valuesSequentialCollection: t 'v => SequentialCollection.t 'v;
  let valuesSequentialCollectionReversed: t 'v => SequentialCollection.t 'v;
  let valuesReversed: t 'v => Iterable.t 'v;
  let keysNavigableSet: t 'v => NavigableSet.t k;
  let keysNavigableSetReversed: t 'v => NavigableSet.t k;
  let toMapReversed: t 'v => ImmMap.t k 'v;
  let toNavigableMap: t 'v => NavigableMap.t k 'v;
  let toNavigableMapReversed: t 'v => NavigableMap.t k 'v;
  let remove: k => t 'v => t 'v;
  let removeAll: t 'v => t 'v;
  let every: (k => 'v => bool) => (t 'v) => bool;
  let find: (k => 'v => bool) => (t 'v) => (option (k, 'v));
  let findOrRaise: (k => 'v => bool) => (t 'v) => (k, 'v);
  let findKey: (k => 'v => bool) => (t 'v) => (option k);
  let findKeyOrRaise: (k => 'v => bool) => (t 'v) => k;
  let findValue: (k => 'v => bool) => (t 'v) => (option 'v);
  let findValueOrRaise: (k => 'v => bool) => (t 'v) => 'v;
  let forEach: while_::(k => 'v => bool)? => (k => 'v => unit) => (t 'v) => unit;
  let keys: (t 'v) => (Iterable.t k);
  let none: (k => 'v => bool) => (t 'v) => bool;
  let reduce: while_::('acc => k => 'v => bool)? => ('acc => k => 'v => 'acc) => 'acc => (t 'v) => 'acc;
  let reduceKeys: while_::('acc => k => bool)? => ('acc => k => 'acc) => 'acc => (t 'v) => 'acc;
  let reduceValues: while_::('acc => 'v => bool)? => ('acc => 'v => 'acc) => 'acc => (t 'v) => 'acc;
  let some: (k => 'v => bool) => (t 'v) => bool;
  let toIterable: t 'v => Iterable.t (k, 'v);
  let toKeyedIterable: t 'v => KeyedIterable.t k 'v;
  let values: (t 'v) => Iterable.t 'v;
  let containsKey: k => t 'v => bool;
  let count: t 'v => int;
  let isEmpty: t 'v => bool;
  let isNotEmpty: t 'v => bool;
  let keysCollection: t 'v => Collection.t k;
  let keysSequence: t 'v => Sequence.t k;
  let toKeyedCollection: t 'v => KeyedCollection.t k 'v;
  let toSequence: t 'v => Sequence.t (k, 'v);
  let valuesCollection: t 'v => Collection.t 'v;
  let valuesSequence: t 'v => Sequence.t 'v;
  let get: k => t 'v => option 'v;
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

    let keysSequence ({ tree }: t 'v): (Sequence.t k) =>
      tree |> AVLTreeMap.keysSequence;

    let keysSequenceReversed ({ tree }: t 'v): (Sequence.t k) =>
      tree |> AVLTreeMap.keysSequenceReversed;

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

    let toSequence ({ tree }: t 'v): (Sequence.t (k, 'v)) =>
      tree |> AVLTreeMap.toSequence;

    let toSequenceReversed ({ tree }: t 'v): (Sequence.t (k, 'v)) =>
      tree |> AVLTreeMap.toSequenceReversed;

    let valuesSequence ({ tree }: t 'v): (Sequence.t 'v) =>
      tree |> AVLTreeMap.valuesSequence;

    let valuesSequenceReversed ({ tree }: t 'v): (Sequence.t 'v) =>
      tree |> AVLTreeMap.valuesSequenceReversed;
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
