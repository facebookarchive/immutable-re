/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Option.Operators;

module type S = {
  type elt;
  type t;

  let add: elt => t => t;
  let addAll: (Iterator.t elt) => t => t;
  let compare: (Comparator.t t);
  let contains: elt => t => bool;
  let count: t => int;
  let empty: t;
  let equals: t => t => bool;
  let every: (elt => bool) => t => bool;
  let find: (elt => bool) => t => elt;
  let first: t => elt;
  let forEach: (elt => unit) => t => unit;
  let from: (Iterator.t elt) => t;
  let hash: (Hash.t t);
  let hashWith: (Hash.t elt) => (Hash.t t);
  let intersect: t => t => t;
  let isEmpty: t => bool;
  let isNotEmpty: t => bool;
  let last: t => elt;
  let none: (elt => bool) => t => bool;
  let reduce: ('acc => elt => 'acc) => 'acc => t => 'acc;
  let reduceRight: ('acc => elt => 'acc) => 'acc => t => 'acc;
  let remove: elt => t => t;
  let removeAll: t => t;
  let removeFirst: t => t;
  let removeLast: t => t;
  let some: (elt => bool) => t => bool;
  let subtract: t => t => t;
  let toIterator: t => (Iterator.t elt);
  let toIteratorReversed: t => (Iterator.t elt);
  let toKeyedIterator: t => (KeyedIterator.t elt elt);
  let toKeyedIteratorReversed: t => (KeyedIterator.t elt elt);
  let toMap: t => (ImmMap.t elt elt);
  let toSequence: t => (Sequence.t elt);
  let toSequenceReversed: t => (Sequence.t elt);
  let toSet: t => (ImmSet.t elt);
  let tryFind: (elt => bool) => t => (option elt);
  let tryFirst: t => (option elt);
  let tryLast: t => (option elt);
  let union: t => t => t;
};

let module Make = fun (Comparable: Comparable.S) => {
  type elt = Comparable.t;

  type t = {
    count: int,
    tree: AVLTreeSet.t elt,
  };

  let comparator = Comparable.compare;

  let add (x: elt) ({ count, tree } as sortedSet: t): t => {
    let newTree = tree |> AVLTreeSet.add comparator x;
    if (newTree === tree) sortedSet else { count: count + 1, tree: newTree }
  };

  let addAll (iter: Iterator.t elt) (sortedSet: t): t => iter
    |> Iterator.reduce (fun acc next => acc |> add next) sortedSet;

  let contains (x: elt) ({ tree }: t): bool =>
    AVLTreeSet.contains comparator x tree;

  let count ({ count }: t): int => count;

  let empty: t = { count: 0, tree: AVLTreeSet.Empty };

  let isEmpty ({ count }: t): bool => count == 0;

  let isNotEmpty ({ count }: t): bool => count != 0;

  let from (iter: Iterator.t elt): t =>
    empty |> addAll iter;

  let reduce (f: 'acc => elt => 'acc) (acc: 'acc) ({ tree }: t): 'acc =>
    tree |> AVLTreeSet.reduce f acc;

  let reduceRight (f: 'acc => elt => 'acc) (acc: 'acc) ({ tree }: t): 'acc =>
    tree |> AVLTreeSet.reduceRight f acc;

  let remove (x: elt) ({ count, tree } as sortedSet: t): t => {
    let newTree = AVLTreeSet.remove comparator x tree;
    if (newTree === tree) sortedSet else { count: count - 1, tree: newTree }
  };

  let removeAll (_: t): t =>
    empty;

  let removeFirst ({ count, tree } as sortedSet: t): t => {
    let newTree = AVLTreeSet.removeFirst tree;
    if (newTree === tree) sortedSet else { count: count - 1, tree: newTree }
  };

  let removeLast ({ count, tree } as sortedSet: t): t => {
    let newTree = AVLTreeSet.removeLast tree;
    if (newTree === tree) sortedSet else { count: count - 1, tree: newTree }
  };

  let toSequence ({ tree }: t): (Sequence.t elt) =>
    tree |> AVLTreeSet.toSequence;

  let toSequenceReversed ({ tree }: t): (Sequence.t elt) =>
    tree |> AVLTreeSet.toSequenceReversed;

  let compare
      (this: t)
      (that: t): Ordering.t =>
    /* FIXME: Should be possible to make this more efficient
     * by recursively walking the tree.
     */
    Sequence.compareWith comparator (toSequence this) (toSequence that);

  let equals
      (this: t)
      (that: t): bool =>
    (this === that) || (
      /* FIXME: Should be possible to make this more efficient
       * by recursively walking the tree.
       */
      Sequence.equalsWith
        (fun a b => (comparator a b) === Ordering.equal)
        (toSequence this)
        (toSequence that)
    );

  let every (f: elt => bool) (set: t): bool =>
    set |> toSequence |> Sequence.every f;

  let find (f: elt => bool) (set: t): elt =>
    set |> toSequence |> Sequence.find f;

  let first ({ tree }: t): elt =>
    AVLTreeSet.first tree;

  let forEach (f: elt => unit) ({ tree }: t) =>
    tree |> AVLTreeSet.forEach f;

  let hashWith (hash: (Hash.t elt)) (set: t): int => set
    |> reduce (Hash.reducer hash) Hash.initialValue;

  let hash (set: t): int =>
    hashWith Hash.structural set;

  let last ({ tree }: t): elt =>
    AVLTreeSet.last tree;

  let none (f: elt => bool) (set: t): bool =>
    set |> toSequence |> Sequence.none f;

  let some (f: elt => bool) (set: t): bool =>
    set |> toSequence |> Sequence.some f;

  let toIterator (set: t): (Iterator.t elt) =>
    if (isEmpty set) Iterator.empty
    else { reduce: fun f acc => reduce f acc set };

  let toIteratorReversed (set: t): (Iterator.t elt) =>
    if (isEmpty set) Iterator.empty
    else { reduce: fun f acc => reduceRight f acc set };

  let toKeyedIterator (set: t): (KeyedIterator.t elt elt)  =>
    if (isEmpty set) KeyedIterator.empty
    else {
      reduce: fun f acc => set |> reduce
        (fun acc next => f acc next next)
        acc
    };

  let toKeyedIteratorReversed (set: t): (KeyedIterator.t elt elt) =>
    if (isEmpty set) KeyedIterator.empty
    else {
      reduce: fun f acc => set |> reduceRight
        (fun acc next => f acc next next)
        acc
    };

  let tryFind (f: elt => bool) (set: t): (option elt) =>
    set |> toSequence |> Sequence.tryFind f;

  let tryFirst ({ tree }: t): (option elt) =>
    AVLTreeSet.tryFirst tree;

  let tryLast ({ tree }: t): (option elt) =>
    AVLTreeSet.tryLast tree;

  let toSet (set: t): (ImmSet.t elt) => {
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

  let toMap (set: t): (ImmMap.t elt elt) => {
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

  let intersect (this: t) (that: t): t =>
    /* FIXME: Improve this implementation to be O(log N) */
    ImmSet.intersect (toSet this) (toSet that) |> from;

  let subtract (this: t) (that: t): t =>
    /* FIXME: Improve this implementation to be O(log N) */
    ImmSet.subtract (toSet this) (toSet that) |> from;

  let union (this: t) (that: t): t =>
    /* FIXME: Improve this implementation to be O(log N) */
    ImmSet.union (toSet this) (toSet that) |> from;
};
