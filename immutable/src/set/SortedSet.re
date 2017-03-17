/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Option.Operators;

type t 'a = {
  comparator: Comparator.t 'a,
  count: int,
  tree: AVLTreeSet.t 'a,
};

let add (x: 'a) ({ comparator, count, tree } as sortedSet: t 'a): (t 'a) => {
  let newTree = tree |> AVLTreeSet.add comparator x;
  if (newTree === tree) sortedSet else { comparator, count: count + 1, tree: newTree }
};

let addAll (iter: Iterator.t 'a) (sortedSet: t 'a): (t 'a) => iter
  |> Iterator.reduce (fun acc next => acc |> add next) sortedSet;

let contains (x: 'a) ({ comparator, tree }: t 'a): bool =>
  AVLTreeSet.contains comparator x tree;

let count ({ count }: t 'a): int => count;

let empty: t 'a = { comparator: Comparator.structural, count: 0, tree: AVLTreeSet.Empty };

let emptyWith (comparator: Comparator.t 'a): (t 'a) => ({ comparator, count: 0, tree: AVLTreeSet.Empty });

let isEmpty ({ count }: t 'a): bool => count == 0;

let isNotEmpty ({ count }: t 'a): bool => count != 0;

let from (iter: Iterator.t 'a): (t 'a) =>
  empty |> addAll iter;

let fromWith (comparator: Comparator.t 'a) (iter: Iterator.t 'a): (t 'a) =>
  emptyWith comparator |> addAll iter;

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) ({ tree }: t 'a): 'acc =>
  tree |> AVLTreeSet.reduce f acc;

let reduceRight (f: 'acc => 'a => 'acc) (acc: 'acc) ({ tree }: t 'a): 'acc =>
  tree |> AVLTreeSet.reduceRight f acc;

let remove (x: 'a) ({ comparator, count, tree } as sortedSet: t 'a): (t 'a) => {
  let newTree = AVLTreeSet.remove comparator x tree;
  if (newTree === tree) sortedSet else { comparator, count: count - 1, tree: newTree }
};

let removeAll ({ comparator }: t 'a): (t 'a) =>
  emptyWith comparator;

let removeFirst ({ comparator, count, tree } as sortedSet: t 'a): (t 'a) => {
  let newTree = AVLTreeSet.removeFirst tree;
  if (newTree === tree) sortedSet else { comparator, count: count - 1, tree: newTree }
};

let removeLast ({ comparator, count, tree } as sortedSet: t 'a): (t 'a) => {
  let newTree = AVLTreeSet.removeLast tree;
  if (newTree === tree) sortedSet else { comparator, count: count - 1, tree: newTree }
};

let toSequence ({ tree }: t 'a): (Sequence.t 'a) =>
  tree |> AVLTreeSet.toSequence;

let toSequenceReversed ({ tree }: t 'a): (Sequence.t 'a) =>
  tree |> AVLTreeSet.toSequenceReversed;

let compare
    ({ comparator: thisCompare } as this: t 'a)
    ({ comparator: thatCompare } as that: t 'a): Ordering.t =>
  if (thisCompare !== thatCompare) { failwith "Sets must use the same comparator" }
  /* FIXME: Should be possible to make this more efficient
   * by recursively walking the tree.
   */
  else Sequence.compareWith thisCompare (toSequence this) (toSequence that);

let equals
    ({ comparator: thisCompare } as this: t 'a)
    ({ comparator: thatCompare } as that: t 'a): bool =>
  (this === that) || (
    (thisCompare === thatCompare) &&

    /* FIXME: Should be possible to make this more efficient
     * by recursively walking the tree.
     */
    Sequence.equalsWith (fun a b => (thisCompare a b) === Ordering.equal) (toSequence this) (toSequence that)
  );

let every (f: 'a => bool) (set: t 'a): bool =>
  set |> toSequence |> Sequence.every f;

let find (f: 'a => bool) (set: t 'a): 'a =>
  set |> toSequence |> Sequence.find f;

let first ({ tree }: t 'a): 'a =>
  AVLTreeSet.first tree;

let forEach (f: 'a => unit) ({ tree }: t 'a) =>
  tree |> AVLTreeSet.forEach f;

let hashWith (hash: (Hash.t 'a)) (set: t 'a): int => set
  |> reduce (Hash.reducer hash) Hash.initialValue;

let hash (set: t 'a): int =>
  hashWith Hash.structural set;

let last ({ tree }: t 'a): 'a =>
  AVLTreeSet.last tree;

let none (f: 'a => bool) (set: t 'a): bool =>
  set |> toSequence |> Sequence.none f;

let some (f: 'a => bool) (set: t 'a): bool =>
  set |> toSequence |> Sequence.some f;

let toIterator (set: t 'a): (Iterator.t 'a) =>
  if (isEmpty set) Iterator.empty
  else { reduce: fun f acc => reduce f acc set };

let toIteratorReversed (set: t 'a): (Iterator.t 'a) =>
  if (isEmpty set) Iterator.empty
  else { reduce: fun f acc => reduceRight f acc set };

let toKeyedIterator (set: t 'a): (KeyedIterator.t 'a 'a) =>
  if (isEmpty set) KeyedIterator.empty
  else {
    reduce: fun f acc => set |> reduce
      (fun acc next => f acc next next)
      acc
  };

let toKeyedIteratorReversed (set: t 'a): (KeyedIterator.t 'a 'a) =>
  if (isEmpty set) KeyedIterator.empty
  else {
    reduce: fun f acc => set |> reduceRight
      (fun acc next => f acc next next)
      acc
  };

let tryFind (f: 'a => bool) (set: t 'a): (option 'a) =>
  set |> toSequence |> Sequence.tryFind f;

let tryFirst ({ tree }: t 'a): (option 'a) =>
  AVLTreeSet.tryFirst tree;

let tryLast ({ tree }: t 'a): (option 'a) =>
  AVLTreeSet.tryLast tree;

let toSet (set: t 'a): (ImmSet.t 'a) => {
  contains: fun a => contains a set,
  count: count set,
  every: fun f => every f set,
  find: fun f => find f set,
  forEach: fun f => forEach f set,
  none: fun f => none f set,
  reduce: fun f acc => reduce f acc set,
  some: fun f => some f set,
  toSequence: toSequence set,
  tryFind: fun f => tryFind f set,
};

let toMap (set: t 'a): (ImmMap.t 'a 'a) => {
  containsWith: fun equals k v =>
    if (set |> contains k) (equals k v)
    else false,
  containsKey: fun k => set |> contains k,
  count: count set,
  every: fun f => set |> every (fun k => f k k),
  find: fun f => {
    let k = set |> find (fun k => f k k);
    (k, k)
  },
  forEach: fun f => set |> forEach (fun k => f k k),
  get: fun k =>
    if (set |> contains k) k
    else failwith "not found",
  none: fun f => set |> none (fun k => f k k),
  reduce: fun f acc => set |> reduce (fun acc k => f acc k k) acc,
  some: fun f => set |> some (fun k => f k k),
  toSequence: toSequence set |> Sequence.map (fun k => (k, k)),
  tryFind: fun f => set |> tryFind (fun k => f k k) >>| (fun k => (k, k)),
  tryGet: fun k =>
    if (set |> contains k) (Some k)
    else None,
  values: toIterator set,
};

let intersect ({ comparator } as this: t 'a) (that: t 'a): (t 'a) =>
  /* FIXME: Improve this implementation to be O(log N) */
  ImmSet.intersect (toSet this) (toSet that) |> fromWith comparator;

let subtract ({ comparator } as this: t 'a) (that: t 'a): (t 'a) =>
  /* FIXME: Improve this implementation to be O(log N) */
  ImmSet.subtract (toSet this) (toSet that) |> fromWith comparator;

let union ({ comparator } as this: t 'a) (that: t 'a): (t 'a) =>
  /* FIXME: Improve this implementation to be O(log N) */
  ImmSet.union (toSet this) (toSet that) |> fromWith comparator;
