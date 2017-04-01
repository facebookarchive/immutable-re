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
  let last: t 'v => option (k, 'v);
  let lastOrRaise: t 'v => (k, 'v);
  let reduceRight: while_::('acc => k => 'v => bool)? => ('acc => k => 'v => 'acc) => 'acc => t 'v => 'acc;
  let toIterableRight: t 'v => Iterable.t (k, 'v);
  let toKeyedIterableRight: t 'v => KeyedIterable.t k 'v;
  let toSequenceRight: t 'v => Sequence.t (k, 'v);
  let remove: k => t 'v => t 'v;
  let removeAll: t 'v => t 'v;
  let reduce: while_::('acc => k => 'v => bool)? => ('acc => k => 'v => 'acc) => 'acc => t 'v => 'acc;
  let toIterable: t 'v => Iterable.t (k, 'v);
  let toKeyedIterable: t 'v => KeyedIterable.t k 'v;
  let containsKey: k => t 'v => bool;
  let count: t 'v => int;
  let isEmpty: t 'v => bool;
  let isNotEmpty: t 'v => bool;
  let keys: t 'v => ImmSet.t k;
  let toSequence: t 'v => Sequence.t (k, 'v);
  let get: k => t 'v => option 'v;
  let getOrRaise: k => t 'v => 'v;
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
  let module KeyedReducerRight: KeyedIterable.KeyedReducer.S1 with type k:= k and type t 'v:= t 'v;
  let module KeyedReducer: KeyedIterable.KeyedReducer.S1 with type k:= k and type t 'v:= t 'v;
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

  let reduceRightImpl
      while_::(predicate: 'acc => k => 'v => bool)
      (f: 'acc => k => 'v => 'acc)
      (acc: 'acc)
      ({ tree }: t 'v): 'acc =>
    if (predicate === Functions.alwaysTrue3) (AVLTreeMap.reduceRight f acc tree)
    else (AVLTreeMap.reduceRightWhile predicate f acc tree);

  let reduceRight
      while_::(predicate: 'acc => k => 'v => bool)=Functions.alwaysTrue3
      (f: 'acc => k => 'v => 'acc)
      (acc: 'acc)
      (map: t 'v): 'acc =>
    reduceRightImpl while_::predicate f acc map;

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

  let toSequence ({ tree }: t 'v): (Sequence.t (k, 'v)) =>
    tree |> AVLTreeMap.toSequence;

  let toSequenceRight ({ tree }: t 'v): (Sequence.t (k, 'v)) =>
    tree |> AVLTreeMap.toSequenceRight;

  let iterator: Iterable.Iterator.t (k, 'v) (t 'v) = {
    reduce: fun while_::predicate f acc map => map |> reduce
      while_::(fun acc k v => predicate acc (k, v))
      (fun acc k v => f acc (k, v))
      acc
  };

  let toIterable (map: t 'v): (Iterable.t (k, 'v)) =>
    if (isEmpty map) (Iterable.empty ())
    else Iterable.Iterable map iterator;

  let iteratorRight: Iterable.Iterator.t (k, 'v) (t 'v) = {
    reduce: fun while_::predicate f acc map => map |> reduceRight
      while_::(fun acc k v => predicate acc (k, v))
      (fun acc k v => f acc (k, v))
      acc
  };

  let toIterableRight (map: t 'v): (Iterable.t (k, 'v)) =>
    if (isEmpty map) (Iterable.empty ())
    else Iterable.Iterable map iteratorRight;

  let keyedIterator: KeyedIterable.KeyedIterator.t k 'v (t 'v) = { reduce: reduceImpl };

  let toKeyedIterable (map: t 'v): (KeyedIterable.t k 'v) =>
    if (isEmpty map) (KeyedIterable.empty ())
    else KeyedIterable.KeyedIterable map keyedIterator;

  let keyedIteratorRight: KeyedIterable.KeyedIterator.t k 'v (t 'v) = { reduce: reduceRightImpl };

  let toKeyedIterableRight (map: t 'v): (KeyedIterable.t k 'v) =>
    if (isEmpty map) (KeyedIterable.empty ())
    else KeyedIterable.KeyedIterable map keyedIteratorRight;

  let mapOps: ImmMap.Ops.t k 'v (t 'v) = {
    containsKey,
    count,
    get,
    getOrRaise,
    toKeyedIterable,
    toSequence,
  };

  let toMap (map: t 'v): (ImmMap.t k 'v) =>
    if (isEmpty map) (ImmMap.empty ())
    else ImmMap.Map map mapOps;

  let keys (map: t 'v): (ImmSet.t k) =>
    map |> toMap |> ImmMap.keys;

  let merge
      (f: k => (option 'vAcc) => (option 'v) => (option 'vAcc))
      (acc: t 'vAcc)
      (next: t 'v): (t 'vAcc) =>  ImmSet.union (keys next) (keys acc)
    |> Iterable.reduce (
        fun acc key => {
          let result = f key (acc |> get key) (next |> get key);
          switch result {
            | None => acc |> remove key
            | Some value => acc |> put key value
          }
        }
      ) acc;

  let module KeyedReducerRight = KeyedIterable.KeyedReducer.Make1 {
    type nonrec k = k;
    type nonrec t 'v = t 'v;

    let reduce = reduceRight;
    let toIterable = toIterableRight;
    let toKeyedIterable = toKeyedIterableRight;
  };

  let module KeyedReducer = KeyedIterable.KeyedReducer.Make1 {
    type nonrec k = k;
    type nonrec t 'v = t 'v;

    let reduce = reduce;
    let toIterable = toIterable;
    let toKeyedIterable = toKeyedIterable;
  };
};
