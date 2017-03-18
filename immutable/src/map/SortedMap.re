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

  let first: t 'v => (k, 'v);
  let last: t 'v => (k, 'v);
  let reduceRight: ('acc => k => 'v => 'acc) => 'acc => t 'v => 'acc;
  let toIteratorRight: t 'v => Iterator.t (k, 'v);
  let toKeyedIteratorRight: t 'v => KeyedIterator.t k 'v;
  let toSequenceRight: t 'v => Sequence.t (k, 'v);
  let tryFirst: t 'v => option (k, 'v);
  let tryLast: t 'v => option (k, 'v);
  let forEach: (k => 'v => unit) => t 'v => unit;
  let reduce: ('acc => k => 'v => 'acc) => 'acc => t 'v => 'acc;
  let every: (k => 'v => bool) => t 'v => bool;
  let find: (k => 'v => bool) => t 'v => (k, 'v);
  let isEmpty: t 'v => bool;
  let isNotEmpty: t 'v => bool;
  let none: (k => 'v => bool) => t 'v => bool;
  let some: (k => 'v => bool) => t 'v => bool;
  let toIterator: t 'v => Iterator.t (k, 'v);
  let toKeyedIterator: t 'v => KeyedIterator.t k 'v;
  let tryFind: (k => 'v => bool) => t 'v => option (k, 'v);
  let containsKey: k => t 'v => bool;
  let count: t 'v => int;
  let toSequence: t 'v => Sequence.t (k, 'v);
  let get: k => t 'v => 'v;
  let keys: t 'v => ImmSet.t k;
  let tryGet: k => t 'v => option 'v;
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
  let removeFirst: t 'v => t 'v;
  let removeLast: t 'v => t 'v;
  let compare: Comparator.t (t 'v);
  let compareWith: Comparator.t 'v => Comparator.t (t 'v);
  let contains: k => 'v => t 'v => bool;
  let containsWith: Equality.t 'v => k => 'v => t 'v => bool;
  let empty: t 'v;
  let equals: t 'v => t 'v => bool;
  let equalsWith: Equality.t 'v => t 'v => t 'v => bool;
  let from: KeyedIterator.t k 'v => t 'v;
  let hash: Hash.t (t 'v);
  let hashWith: Hash.t k => Hash.t 'v => Hash.t (t 'v);
  let toSet: t 'v => ImmSet.t (k, 'v);
  let toSetWith: Equality.t 'v => t 'v => ImmSet.t (k, 'v);
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

  let containsWith
      (valueEquals: Equality.t 'v )
      (key: k)
      (value: 'v)
      ({ tree }: t 'v): bool =>
    tree |> AVLTreeMap.contains comparator valueEquals key value;

  let contains (key: k) (value: 'v) (map: t 'v): bool =>
    map |> containsWith Equality.structural key value;

  let containsKey (key: k) ({ tree }: t 'v): bool =>
    tree |> AVLTreeMap.containsKey comparator key;

  let count ({ count }: t 'v): int => count;

  let every (f: k => 'v => bool) ({ tree }: t 'v): bool =>
    tree |> AVLTreeMap.every f;

  let first ({ tree }: t 'v): (k, 'v) =>
    tree |> AVLTreeMap.first;

  let forEach (f: k => 'v => unit) ({ tree }: t 'v): unit =>
    tree |> AVLTreeMap.forEach f;

  let isEmpty ({ count }: t 'v): bool =>
    count == 0;

  let isNotEmpty ({ count }: t 'v): bool =>
    count != 0;

  let last ({ tree }: t 'v): (k, 'v) =>
    tree |> AVLTreeMap.last;

  let none (f: k => 'v => bool) ({ tree }: t 'v): bool =>
    tree |> AVLTreeMap.none f;

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

  let get (key: k) ({ tree }: t 'v): 'v =>
    tree |> AVLTreeMap.get comparator key;

  let reduce (f: 'acc => k => 'v => 'acc) (acc: 'acc) ({ tree }: t 'v): 'acc =>
    tree |> AVLTreeMap.reduce f acc;

  let reduceRight (f: 'acc => k => 'v => 'acc) (acc: 'acc) ({ tree }: t 'v): 'acc =>
    tree |> AVLTreeMap.reduceRight f acc;

  let map (f: k => 'a => 'b) (map: t 'a): (t 'b) =>
    map |> reduce (fun acc k v => acc |> put k (f k v)) empty;

  let remove (key: k) (map: t 'v): (t 'v) =>
    map |> alter key Functions.alwaysNone;

  let removeAll (_: t 'v): (t 'v) =>
    empty;

  let removeFirst ({ count, tree } as map: t 'v): (t 'v) => {
    let newTree = tree |> AVLTreeMap.removeFirst;

    if (tree === newTree) map
    else { count: count - 1, tree: newTree }
  };

  let removeLast ({ count, tree } as map: t 'v): (t 'v) => {
    let newTree = tree |> AVLTreeMap.removeLast;

    if (tree === newTree) map
    else { count: count - 1, tree: newTree }
  };

  let some (f: k => 'v => bool) ({ tree }: t 'v): bool =>
    tree |> AVLTreeMap.none f;

  let toSequence ({ tree }: t 'v): (Sequence.t (k, 'v)) =>
    tree |> AVLTreeMap.toSequence;

  let toSequenceRight ({ tree }: t 'v): (Sequence.t (k, 'v)) =>
    tree |> AVLTreeMap.toSequenceRight;

  let tryFind (f: k => 'v => bool) ({ tree }: t 'v): (option (k, 'v)) =>
    tree |> AVLTreeMap.tryFind f;

  let find (f: k => 'v => bool) ({ tree }: t 'v): (k, 'v) =>
    tree |> AVLTreeMap.tryFind f |> Option.first;

  let tryFirst ({ tree }: t 'v): (option (k, 'v)) =>
    tree |> AVLTreeMap.tryFirst;

  let tryGet (key: k) ({ tree }: t 'v): (option 'v) =>
    tree |> AVLTreeMap.tryGet comparator key;

  let tryLast ({ tree }: t 'v): (option (k, 'v)) =>
    tree |> AVLTreeMap.tryLast;

  let values ({ tree }: t 'v): (Iterator.t 'v) =>
    tree |> AVLTreeMap.values;

  let toIterator (map: t 'v): (Iterator.t (k, 'v)) =>
    if (isEmpty map) Iterator.empty
    else {
      reduce: fun f acc => map |> reduce
        (fun acc k v => f acc (k, v))
        acc
    };

  let toIteratorRight (map: t 'v): (Iterator.t (k, 'v)) =>
    if (isEmpty map) Iterator.empty
    else {
      reduce: fun f acc => map |> reduceRight
        (fun acc k v => f acc (k, v))
        acc
    };

  let toKeyedIterator (map: t 'v): (KeyedIterator.t k 'v) =>
    if (isEmpty map) KeyedIterator.empty
    else {
      reduce: fun f acc => map |> reduce f acc
    };

  let toKeyedIteratorRight (map: t 'v): (KeyedIterator.t k 'v) =>
    if (isEmpty map) KeyedIterator.empty
    else {
      reduce: fun f acc => map |> reduceRight f acc
    };

  let toMap (map: t 'v): (ImmMap.t k 'v) => {
    containsWith: fun eq k v => map |> containsWith eq k v,
    containsKey: fun k => containsKey k map,
    count: (count map),
    every: fun f => every f map,
    find: fun f => find f map,
    forEach: fun f => forEach f map,
    get: fun i => get i map,
    none: fun f => none f map,
    reduce: fun f acc => map |> reduce f acc,
    some: fun f => map |> some f,
    toSequence: (toSequence map),
    tryFind: fun f => tryFind f map,
    tryGet: fun i => tryGet i map,
    values: (values map),
  };

  let compareWith
      (compareValue: Comparator.t 'v)
      (this: t 'v)
      (that: t 'v): Ordering.t =>
    Sequence.compareWith (fun (k1, v1) (k2, v2) => {
      let cmp = comparator k1 k2;
      if (cmp === Ordering.equal) (compareValue v1 v2)
      else cmp
    }) (toSequence this) (toSequence that);

  let compare (this: t 'v) (that: t 'v): Ordering.t =>
    compareWith Comparator.structural this that;

  let equalsWith
      (valueEquals: Equality.t 'v)
      (this: t 'v)
      (that: t 'v): bool =>
    Sequence.equalsWith (fun (k1, v1) (k2, v2) =>
      if (k1 === k2) true
      else if (comparator k1 k2 === Ordering.equal) (valueEquals v1 v2)
      else false
    ) (toSequence this) (toSequence that);

  let equals (this: t 'v) (that: t 'v): bool =>
    equalsWith Equality.structural this that;

  let hash (map: t 'v): int =>
    map |> toMap |> ImmMap.hash;

  let hashWith (keyHash: Hash.t k) (valueHash: Hash.t 'v) (map: t 'v): int =>
    map |> toMap |> ImmMap.hashWith keyHash valueHash;

  let keys (map: t 'v): (ImmSet.t k) =>
    map |> toMap |> ImmMap.keys;

  let merge
      (f: k => (option 'vAcc) => (option 'v) => (option 'vAcc))
      (next: t 'v)
      (map: t 'vAcc): (t 'vAcc) =>
    ImmSet.union (keys map) (keys next) |> Iterator.reduce (
      fun acc key => {
        let result = f key (map |> tryGet key) (next |> tryGet key);
        switch result {
          | None => acc |> remove key
          | Some value => acc |> put key value
        }
      }
    )
    map;

  let toSetWith (equality: Equality.t 'v) (map: t 'v): (ImmSet.t (k, 'v)) =>
    map |> toMap |> ImmMap.toSetWith equality;

  let toSet (map: t 'v): (ImmSet.t (k, 'v)) =>
    map |> toMap |> ImmMap.toSet;
};
