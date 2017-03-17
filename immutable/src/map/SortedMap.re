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
   type key;
   type t +'v;

   let alter: key => (option 'v => option 'v) => (t 'v) => (t 'v);
   let compare: (Comparator.t (t 'v));
   let compareWith: (Comparator.t 'v) => (Comparator.t (t 'v));
   let contains: key => 'v => (t 'v) => bool;
   let containsWith: (Equality.t 'v) => key => 'v => (t 'v) => bool;
   let containsKey: key => (t 'v) => bool;
   let count: (t 'v) => int;
   let empty: (t 'v);
   let equals: (t 'v) => (t 'v) => bool;
   let equalsWith: (Equality.t 'v) => (t 'v) => (t 'v) => bool;
   let every: (key => 'v => bool) => (t 'v) => bool;
   let find: (key => 'v => bool) => (t 'v) => (key, 'v);
   let first: (t 'v) => (key, 'v);
   let forEach: (key => 'v => unit) => (t 'v) => unit;
   let from: (KeyedIterator.t key 'v) => (t 'v);
   let get: key => (t 'v) => 'v;
   let hash: (Hash.t (t 'v));
   let hashWith: (Hash.t key) => (Hash.t 'v) => (Hash.t (t 'v));
   let isEmpty: t 'v => bool;
   let isNotEmpty: t 'v => bool;
   let keys: (t 'v) => (ImmSet.t key);
   let last: (t 'v) => (key, 'v);
   let map: (key => 'a => 'b) => (t 'a) => (t 'b);
   let merge: (key => (option 'vAcc) => (option 'v) => (option 'vAcc)) => (t 'v) => (t 'vAcc)  => (t 'vAcc);
   let none: (key => 'v => bool) => (t 'v) => bool;
   let put: key => 'v => (t 'v) => (t 'v);
   let putAll: (KeyedIterator.t key 'v) => (t 'v) => (t 'v);
   let reduce: ('acc => key => 'v => 'acc) => 'acc => (t 'v) => 'acc;
   let reduceRight: ('acc => key => 'v => 'acc) => 'acc => (t 'v) => 'acc;
   let remove: key => (t 'v) => (t 'v);
   let removeAll: (t 'v) => (t 'v);
   let removeFirst: (t 'v) => (t 'v);
   let removeLast: (t 'v) => (t 'v);
   let some: (key => 'v => bool) => (t 'v) => bool;
   let toIterator: (t 'v) => (Iterator.t (key, 'v));
   let toIteratorReversed: (t 'v) => (Iterator.t (key, 'v));
   let toKeyedIterator: (t 'v) => (KeyedIterator.t key 'v);
   let toKeyedIteratorReversed: (t 'v) => (KeyedIterator.t key 'v);
   let toMap: (t 'v) => (ImmMap.t key 'v);
   let toSequence: (t 'v) => (Sequence.t (key, 'v));
   let toSequenceReversed: (t 'v) => (Sequence.t (key, 'v));
   let toSet: (t 'v) => (ImmSet.t (key, 'v));
   let toSetWith: (Equality.t 'v) => (t 'v) => (ImmSet.t (key, 'v));
   let tryFind: (key => 'v => bool) => (t 'v) => (option (key, 'v));
   let tryFirst: (t 'v) => (option (key, 'v));
   let tryLast: (t 'v) => (option (key, 'v));
   let tryGet: key => (t 'v) => (option 'v);
   let values: (t 'v) => (Iterator.t 'v);
 };

let module Make = fun (Comparable: Comparable.S) => {
  type key = Comparable.t;

  let comparator = Comparable.compare;

  type t 'v = {
    count: int,
    tree: AVLTreeMap.t key 'v,
  };

  let empty: (t 'v) = {
    count: 0,
    tree: AVLTreeMap.Empty,
  };

  let alter
      (key: key)
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
      (key: key)
      (value: 'v)
      ({ tree }: t 'v): bool =>
    tree |> AVLTreeMap.contains comparator valueEquals key value;

  let contains (key: key) (value: 'v) (map: t 'v): bool =>
    map |> containsWith Equality.structural key value;

  let containsKey (key: key) ({ tree }: t 'v): bool =>
    tree |> AVLTreeMap.containsKey comparator key;

  let count ({ count }: t 'v): int => count;

  let every (f: key => 'v => bool) ({ tree }: t 'v): bool =>
    tree |> AVLTreeMap.every f;

  let first ({ tree }: t 'v): (key, 'v) =>
    tree |> AVLTreeMap.first;

  let forEach (f: key => 'v => unit) ({ tree }: t 'v): unit =>
    tree |> AVLTreeMap.forEach f;

  let isEmpty ({ count }: t 'v): bool =>
    count == 0;

  let isNotEmpty ({ count }: t 'v): bool =>
    count != 0;

  let last ({ tree }: t 'v): (key, 'v) =>
    tree |> AVLTreeMap.last;

  let none (f: key => 'v => bool) ({ tree }: t 'v): bool =>
    tree |> AVLTreeMap.none f;

  let put (key: key) (value: 'v) ({ count, tree } as map: t 'v): (t 'v) => {
    let alterResult = ref AVLTreeMap.NoChange;
    let newTree = tree |> AVLTreeMap.putWithResult comparator alterResult key value;
    switch !alterResult {
      | AVLTreeMap.Added => { count: count + 1, tree: newTree }
      | AVLTreeMap.NoChange => map
      | AVLTreeMap.Replace => { count, tree: newTree }
      | AVLTreeMap.Removed => failwith "invalid state"
    };
  };

  let putAll (iter: KeyedIterator.t key 'v) (map: t 'v): (t 'v) =>
    iter |> KeyedIterator.reduce (fun acc k v => acc |> put k v) map;

  let from (iter: KeyedIterator.t key 'v): (t 'v) =>
    empty |> putAll iter;

  let get (key: key) ({ tree }: t 'v): 'v =>
    tree |> AVLTreeMap.get comparator key;

  let reduce (f: 'acc => key => 'v => 'acc) (acc: 'acc) ({ tree }: t 'v): 'acc =>
    tree |> AVLTreeMap.reduce f acc;

  let reduceRight (f: 'acc => key => 'v => 'acc) (acc: 'acc) ({ tree }: t 'v): 'acc =>
    tree |> AVLTreeMap.reduceRight f acc;

  let map (f: key => 'a => 'b) (map: t 'a): (t 'b) =>
    map |> reduce (fun acc k v => acc |> put k (f k v)) empty;

  let remove (key: key) (map: t 'v): (t 'v) =>
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

  let some (f: key => 'v => bool) ({ tree }: t 'v): bool =>
    tree |> AVLTreeMap.none f;

  let toSequence ({ tree }: t 'v): (Sequence.t (key, 'v)) =>
    tree |> AVLTreeMap.toSequence;

  let toSequenceReversed ({ tree }: t 'v): (Sequence.t (key, 'v)) =>
    tree |> AVLTreeMap.toSequenceReversed;

  let tryFind (f: key => 'v => bool) ({ tree }: t 'v): (option (key, 'v)) =>
    tree |> AVLTreeMap.tryFind f;

  let find (f: key => 'v => bool) ({ tree }: t 'v): (key, 'v) =>
    tree |> AVLTreeMap.tryFind f |> Option.first;

  let tryFirst ({ tree }: t 'v): (option (key, 'v)) =>
    tree |> AVLTreeMap.tryFirst;

  let tryGet (key: key) ({ tree }: t 'v): (option 'v) =>
    tree |> AVLTreeMap.tryGet comparator key;

  let tryLast ({ tree }: t 'v): (option (key, 'v)) =>
    tree |> AVLTreeMap.tryLast;

  let values ({ tree }: t 'v): (Iterator.t 'v) =>
    tree |> AVLTreeMap.values;

  let toIterator (map: t 'v): (Iterator.t (key, 'v)) =>
    if (isEmpty map) Iterator.empty
    else {
      reduce: fun f acc => map |> reduce
        (fun acc k v => f acc (k, v))
        acc
    };

  let toIteratorReversed (map: t 'v): (Iterator.t (key, 'v)) =>
    if (isEmpty map) Iterator.empty
    else {
      reduce: fun f acc => map |> reduceRight
        (fun acc k v => f acc (k, v))
        acc
    };

  let toKeyedIterator (map: t 'v): (KeyedIterator.t key 'v) =>
    if (isEmpty map) KeyedIterator.empty
    else {
      reduce: fun f acc => map |> reduce f acc
    };

  let toKeyedIteratorReversed (map: t 'v): (KeyedIterator.t key 'v) =>
    if (isEmpty map) KeyedIterator.empty
    else {
      reduce: fun f acc => map |> reduceRight f acc
    };

  let toMap (map: t 'v): (ImmMap.t key 'v) => {
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

  let hashWith (keyHash: Hash.t key) (valueHash: Hash.t 'v) (map: t 'v): int =>
    map |> toMap |> ImmMap.hashWith keyHash valueHash;

  let keys (map: t 'v): (ImmSet.t key) =>
    map |> toMap |> ImmMap.keys;

  let merge
      (f: key => (option 'vAcc) => (option 'v) => (option 'vAcc))
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

  let toSetWith (equality: Equality.t 'v) (map: t 'v): (ImmSet.t (key, 'v)) =>
    map |> toMap |> ImmMap.toSetWith equality;

  let toSet (map: t 'v): (ImmSet.t (key, 'v)) =>
    map |> toMap |> ImmMap.toSet;
};
