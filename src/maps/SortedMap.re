/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

 /** AVL tree based Map. */
let module Make1 = fun (Comparable: Comparable.S) => {
  type k = Comparable.t;

  let comparator = Comparable.compare;

  type t 'v = {
    count: int,
    tree: AVLTreeMap.t k 'v,
  };

  let containsKey (key: 'k) ({ tree }: t 'v): bool =>
    tree |> AVLTreeMap.containsKey comparator key;

  let count ({ count }: t 'v): int => count;

  let firstOrRaise (selector: 'k => 'v => 'c) ({ tree }: t 'v): 'c =>
    tree |> AVLTreeMap.firstOrRaise selector;

  let get (key: 'k) ({ tree }: t 'v): (option 'v) =>
    tree |> AVLTreeMap.get comparator key;

  let getOrDefault default::(default: 'v) (key: 'k) ({ tree }: t 'v): 'v =>
    tree |> AVLTreeMap.getOrDefault comparator ::default key;

  let getOrRaise (key: 'k) ({ tree }: t 'v): 'v =>
    tree |> AVLTreeMap.getOrRaise comparator key;

  let lastOrRaise (selector: 'k => 'v => 'c) ({ tree }: t 'v): 'c =>
    tree |> AVLTreeMap.lastOrRaise selector;

  include (KeyedReducer.MakeGeneric {
    type nonrec t 'k 'v = t 'v;
    type nonrec k 'k = k;
    type nonrec v 'v = 'v;

    let reduce
        while_::(predicate: 'acc => k 'k => v 'v => bool)
        (f: 'acc => k 'k => v 'v => 'acc)
        (acc: 'acc)
        ({ tree }: t 'k 'v): 'acc =>
      AVLTreeMap.reduce while_::predicate f acc tree;
  }: KeyedReducer.S1 with type t 'v := t 'v and type k := k);

  let module KeyedReducerReversed = (KeyedReducer.MakeGeneric {
    type nonrec t 'k 'v = t 'v;
    type nonrec k 'k = k;
    type nonrec v 'v = 'v;

    let reduce
        while_::(predicate: 'acc => 'k => 'v => bool)
        (f: 'acc => 'k => 'v => 'acc)
        (acc: 'acc)
        ({ tree }: t 'k 'v): 'acc =>
      AVLTreeMap.reduceReversed while_::predicate f acc tree;
  }: KeyedReducer.S1 with type t 'v := t 'v and type k := k);

  let reduceReversed = KeyedReducerReversed.reduce;
  let reduceKeysReversed = KeyedReducerReversed.reduceKeys;
  let reduceValuesReversed = KeyedReducerReversed.reduceValues;

  let toSequence (selector: 'k => 'v => 'c) ({ tree }: t 'v): (Sequence.t 'c) =>
    tree |> AVLTreeMap.toSequence selector;

  let toSequenceReversed (selector: 'k => 'v => 'c) ({ tree }: t 'v): (Sequence.t 'c) =>
    tree |> AVLTreeMap.toSequenceReversed selector;

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
};
