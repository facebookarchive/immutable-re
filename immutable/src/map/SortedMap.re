/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

 /** AVL tree based Map. */
module type S = {
  type k;
  type t +'v;

  let reduceRight: ('acc => k => 'v => 'acc) => 'acc => t 'v => 'acc;
  let reduceRightWhile:
    ('acc => k => 'v => bool) =>
    ('acc => k => 'v => 'acc) => 'acc => t 'v => 'acc;
  let first: t 'v => option (k, 'v);
  let firstOrRaise: t 'v => (k, 'v);
  let last: t 'v => option (k, 'v);
  let lastOrRaise: t 'v => (k, 'v);
  let toIteratorRight: t 'v => Iterator.t (k, 'v);
  let toKeyedIteratorRight: t 'v => KeyedIterator.t k 'v;
  let toSequenceRight: t 'v => Sequence.t (k, 'v);
  let reduce: ('acc => k => 'v => 'acc) => 'acc => t 'v => 'acc;
  let reduceWhile:
    ('acc => k => 'v => bool) =>
    ('acc => k => 'v => 'acc) => 'acc => t 'v => 'acc;
  let toIterator: t 'v => Iterator.t (k, 'v);
  let toKeyedIterator: t 'v => KeyedIterator.t k 'v;
  let count: t 'v => int;
  let isEmpty: t 'v => bool;
  let isNotEmpty: t 'v => bool;
  let toSequence: t 'v => Sequence.t (k, 'v);
  let containsKey: k => t 'v => bool;
  let get: k => t 'v => option 'v;
  let getOrRaise: k => t 'v => 'v;
  let keys: t 'v => ImmSet.t k;
  let values: t 'v => Iterator.t 'v;
  let toMap: t 'v => ImmMap.t k 'v;
  let map: (k => 'a => 'b) => t 'a => t 'b;
  let alter: k => (option 'v => option 'v) => t 'v => t 'v;
  let merge:
    (k => option 'vAcc => option 'v => option 'vAcc) =>
    t 'v => t 'vAcc => t 'vAcc;
  let put: k => 'v => t 'v => t 'v;
  let putAll: KeyedIterator.t k 'v => t 'v => t 'v;
  let remove: k => t 'v => t 'v;
  let removeAll: t 'v => t 'v;
  let removeFirstOrRaise: t 'v => t 'v;
  let removeLastOrRaise: t 'v => t 'v;
  let empty: t 'v;
  let from: KeyedIterator.t k 'v => t 'v;
};

let module Make = fun (Comparable: Comparable.S) => {
  type k = Comparable.t;

  let comparator = Comparable.compare;

  type t 'v = {
    count: int,
    tree: AVLTreeMap.t k 'v,
  };

  let empty: (t 'v) = {
    count: 0,
    tree: AVLTreeMap.Empty,
  };

  let alter
      (key: k)
      (f: option 'v => option 'v)
      ({ count, tree } as map: t 'v): (t 'v) => {
    let alterResult = ref AVLTreeMap.NoChange;
    let newTree = tree |> AVLTreeMap.alter comparator alterResult key f;
    switch !alterResult {
      | AVLTreeMap.Added => { count: count + 1, tree: newTree }
      | AVLTreeMap.NoChange => map
      | AVLTreeMap.Replace => { count, tree: newTree }
      | AVLTreeMap.Removed => { count: count - 1, tree: newTree }
    };
  };

  let containsKey (key: k) ({ tree }: t 'v): bool =>
    tree |> AVLTreeMap.containsKey comparator key;

  let count ({ count }: t 'v): int => count;

  let first ({ tree }: t 'v): (option (k, 'v)) =>
    tree |> AVLTreeMap.first;

  let firstOrRaise ({ tree }: t 'v): (k, 'v) =>
    tree |> AVLTreeMap.firstOrRaise;

  let get (key: k) ({ tree }: t 'v): (option 'v) =>
    tree |> AVLTreeMap.get comparator key;

  let getOrRaise (key: k) ({ tree }: t 'v): 'v =>
    tree |> AVLTreeMap.getOrRaise comparator key;

  let isEmpty ({ count }: t 'v): bool =>
    count == 0;

  let isNotEmpty ({ count }: t 'v): bool =>
    count != 0;

  let last ({ tree }: t 'v): (option (k, 'v)) =>
    tree |> AVLTreeMap.last;

  let lastOrRaise ({ tree }: t 'v): (k, 'v) =>
    tree |> AVLTreeMap.lastOrRaise;

  let put (key: k) (value: 'v) ({ count, tree } as map: t 'v): (t 'v) => {
    let alterResult = ref AVLTreeMap.NoChange;
    let newTree = tree |> AVLTreeMap.putWithResult comparator alterResult key value;
    switch !alterResult {
      | AVLTreeMap.Added => { count: count + 1, tree: newTree }
      | AVLTreeMap.NoChange => map
      | AVLTreeMap.Replace => { count, tree: newTree }
      | AVLTreeMap.Removed => failwith "invalid state"
    };
  };

  let putAll (iter: KeyedIterator.t k 'v) (map: t 'v): (t 'v) =>
    iter |> KeyedIterator.reduce (fun acc k v => acc |> put k v) map;

  let from (iter: KeyedIterator.t k 'v): (t 'v) =>
    empty |> putAll iter;

  let reduce (f: 'acc => k => 'v => 'acc) (acc: 'acc) ({ tree }: t 'v): 'acc =>
    tree |> AVLTreeMap.reduce f acc;

  let reduceWhile
      (predicate: 'acc => k => 'v => bool)
      (f: 'acc => k => 'v => 'acc)
      (acc: 'acc)
      ({ tree }: t 'v): 'acc =>
    tree |> AVLTreeMap.reduceWhile predicate f acc;

  let reduceRight (f: 'acc => k => 'v => 'acc) (acc: 'acc) ({ tree }: t 'v): 'acc =>
    tree |> AVLTreeMap.reduceRight f acc;

  let reduceRightWhile
      (predicate: 'acc => k => 'v => bool)
      (f: 'acc => k => 'v => 'acc)
      (acc: 'acc)
      ({ tree }: t 'v): 'acc =>
    tree |> AVLTreeMap.reduceRightWhile predicate f acc;

  let map (f: k => 'a => 'b) (map: t 'a): (t 'b) =>
    map |> reduce (fun acc k v => acc |> put k (f k v)) empty;

  let remove (key: k) (map: t 'v): (t 'v) =>
    map |> alter key Functions.alwaysNone;

  let removeAll (_: t 'v): (t 'v) =>
    empty;

  let removeFirstOrRaise ({ count, tree }: t 'v): (t 'v) => {
    let newTree = tree |> AVLTreeMap.removeFirstOrRaise;
    { count: count - 1, tree: newTree }
  };

  let removeLastOrRaise ({ count, tree }: t 'v): (t 'v) => {
    let newTree = tree |> AVLTreeMap.removeLastOrRaise;
    { count: count - 1, tree: newTree }
  };

  let toSequence ({ tree }: t 'v): (Sequence.t (k, 'v)) =>
    tree |> AVLTreeMap.toSequence;

  let toSequenceRight ({ tree }: t 'v): (Sequence.t (k, 'v)) =>
    tree |> AVLTreeMap.toSequenceRight;

  let values ({ tree }: t 'v): (Iterator.t 'v) =>
    tree |> AVLTreeMap.values;

  let toIterator (map: t 'v): (Iterator.t (k, 'v)) =>
    if (isEmpty map) Iterator.empty
    else {
      reduceWhile: fun predicate f acc => map |> reduceWhile
        (fun acc k v => predicate acc (k, v))
        (fun acc k v => f acc (k, v))
        acc
    };

  let toIteratorRight (map: t 'v): (Iterator.t (k, 'v)) =>
    if (isEmpty map) Iterator.empty
    else {
      reduceWhile: fun predicate f acc => map |> reduceRightWhile
        (fun acc k v => predicate acc (k, v))
        (fun acc k v => f acc (k, v))
        acc
    };

  let toKeyedIterator (map: t 'v): (KeyedIterator.t k 'v) =>
    if (isEmpty map) KeyedIterator.empty
    else {
      reduceWhile: fun predicate f acc => map |> reduceWhile predicate f acc
    };

  let toKeyedIteratorRight (map: t 'v): (KeyedIterator.t k 'v) =>
    if (isEmpty map) KeyedIterator.empty
    else {
      reduceWhile: fun predicate f acc => map |> reduceRightWhile predicate f acc
    };

  let toMap (map: t 'v): (ImmMap.t k 'v) => {
    containsKey: fun k => containsKey k map,
    count: (count map),
    get: fun i => get i map,
    getOrRaise: fun i => getOrRaise i map,
    keyedIterator: toKeyedIterator map,
    sequence: toSequence map,
  };

  let keys (map: t 'v): (ImmSet.t k) =>
    map |> toMap |> ImmMap.keys;

  let merge
      (f: k => (option 'vAcc) => (option 'v) => (option 'vAcc))
      (next: t 'v)
      (map: t 'vAcc): (t 'vAcc) =>
    ImmSet.union (keys map) (keys next) |> Iterator.reduce (
      fun acc key => {
        let result = f key (map |> get key) (next |> get key);
        switch result {
          | None => acc |> remove key
          | Some value => acc |> put key value
        }
      }
    )
    map;
};
