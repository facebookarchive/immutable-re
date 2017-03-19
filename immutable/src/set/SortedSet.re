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
  type a;
  type t;

  let compare: t => t => Ordering.t;
  let first: t => option a;
  let firstOrRaise: t => a;
  let forEachRight: (a => unit) => t => unit;
  let reduceRight: ('acc => a => 'acc) => 'acc => t => 'acc;
  let last: t => option a;
  let lastOrRaise: t => a;
  let toIteratorRight: t => Iterator.t a;
  let toSequenceRight: t => Sequence.t a;
  let toKeyedIteratorRight: t => KeyedIterator.t a a;
  let forEach: (a => unit) => t => unit;
  let reduce: ('acc => a => 'acc) => 'acc => t => 'acc;
  let every: (a => bool) => t => bool;
  let find: (a => bool) => t => option a;
  let findOrRaise: (a => bool) => t => a;
  let isEmpty: t => bool;
  let isNotEmpty: t => bool;
  let none: (a => bool) => t => bool;
  let some: (a => bool) => t => bool;
  let toIterator: t => Iterator.t a;
  let count: t => int;
  let toSequence: t => Sequence.t a;
  let contains: a => t => bool;
  let equals: Equality.t t;
  let toKeyedIterator: t => KeyedIterator.t a a;
  let toMap: t => ImmMap.t a a;
  let toSet: t => ImmSet.t a;
  let add: a => t => t;
  let addAll: Iterator.t a => t => t;
  let intersect: t => t => t;
  let remove: a => t => t;
  let removeAll: t => t;
  let subtract: t => t => t;
  let union: t => t => t;
  let removeFirst: t => t;
  let removeLast: t => t;
  let empty: t;
  let from: (Iterator.t a) => t;
  let hash: (Hash.t t);
  let hashWith: (Hash.t a) => (Hash.t t);
};

let module Make = fun (Comparable: Comparable.S) => {
  type a = Comparable.t;

  type t = {
    count: int,
    tree: AVLTreeSet.t a,
  };

  let comparator = Comparable.compare;

  let add (x: a) ({ count, tree } as sortedSet: t): t => {
    let newTree = tree |> AVLTreeSet.add comparator x;
    if (newTree === tree) sortedSet else { count: count + 1, tree: newTree }
  };

  let addAll (iter: Iterator.t a) (sortedSet: t): t => iter
    |> Iterator.reduce (fun acc next => acc |> add next) sortedSet;

  let contains (x: a) ({ tree }: t): bool =>
    AVLTreeSet.contains comparator x tree;

  let count ({ count }: t): int => count;

  let empty: t = { count: 0, tree: AVLTreeSet.Empty };

  let isEmpty ({ count }: t): bool => count == 0;

  let isNotEmpty ({ count }: t): bool => count != 0;

  let from (iter: Iterator.t a): t =>
    empty |> addAll iter;

  let reduce (f: 'acc => a => 'acc) (acc: 'acc) ({ tree }: t): 'acc =>
    tree |> AVLTreeSet.reduce f acc;

  let reduceRight (f: 'acc => a => 'acc) (acc: 'acc) ({ tree }: t): 'acc =>
    tree |> AVLTreeSet.reduceRight f acc;

  let remove (x: a) ({ count, tree } as sortedSet: t): t => {
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

  let toSequence ({ tree }: t): (Sequence.t a) =>
    tree |> AVLTreeSet.toSequence;

  let toSequenceRight ({ tree }: t): (Sequence.t a) =>
    tree |> AVLTreeSet.toSequenceRight;

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

  let every (f: a => bool) (set: t): bool =>
    set |> toSequence |> Sequence.every f;

  let find (f: a => bool) (set: t): (option a) =>
    set |> toSequence |> Sequence.find f;

  let findOrRaise (f: a => bool) (set: t): a =>
    set |> toSequence |> Sequence.findOrRaise f;

  let first ({ tree }: t): (option a) =>
    AVLTreeSet.first tree;

  let firstOrRaise ({ tree }: t): a =>
    AVLTreeSet.firstOrRaise tree;

  let forEach (f: a => unit) ({ tree }: t) =>
    tree |> AVLTreeSet.forEach f;

  let forEachRight (f: a => unit) ({ tree }: t) =>
    tree |> AVLTreeSet.forEach f;

  let hashWith (hash: (Hash.t a)) (set: t): int => set
    |> reduce (Hash.reducer hash) Hash.initialValue;

  let hash (set: t): int =>
    hashWith Hash.structural set;

  let last ({ tree }: t): (option a) =>
    AVLTreeSet.last tree;

  let lastOrRaise ({ tree }: t): a =>
    AVLTreeSet.lastOrRaise tree;

  let none (f: a => bool) (set: t): bool =>
    set |> toSequence |> Sequence.none f;

  let some (f: a => bool) (set: t): bool =>
    set |> toSequence |> Sequence.some f;

  let toIterator (set: t): (Iterator.t a) =>
    if (isEmpty set) Iterator.empty
    else { reduce: fun f acc => reduce f acc set };

  let toIteratorRight (set: t): (Iterator.t a) =>
    if (isEmpty set) Iterator.empty
    else { reduce: fun f acc => reduceRight f acc set };

  let toKeyedIterator (set: t): (KeyedIterator.t a a)  =>
    if (isEmpty set) KeyedIterator.empty
    else {
      reduce: fun f acc => set |> reduce
        (fun acc next => f acc next next)
        acc
    };

  let toKeyedIteratorRight (set: t): (KeyedIterator.t a a) =>
    if (isEmpty set) KeyedIterator.empty
    else {
      reduce: fun f acc => set |> reduceRight
        (fun acc next => f acc next next)
        acc
    };

  let toSet (set: t): (ImmSet.t a) => {
    contains: fun a => contains a set,
    count: count set,
    every: fun f => every f set,
    find: fun f => find f set,
    findOrRaise: fun f => findOrRaise f set,
    forEach: fun f => forEach f set,
    none: fun f => none f set,
    reduce: fun f acc => reduce f acc set,
    some: fun f => some f set,
    toSequence: toSequence set,
  };

  let toMap (set: t): (ImmMap.t a a) => {
    containsWith: fun equals k v =>
      if (set |> contains k) (equals k v)
      else false,
    containsKey: fun k => set |> contains k,
    count: count set,
    every: fun f => set |> every (fun k => f k k),
    find: fun f => set |> find (fun k => f k k) >>| (fun k => (k, k)),
    findOrRaise: fun f => {
      let k = set |> findOrRaise (fun k => f k k);
      (k, k)
    },
    forEach: fun f => set |> forEach (fun k => f k k),
    get: fun k =>
      if (set |> contains k) (Some k)
      else None,
    getOrRaise: fun k =>
      if (set |> contains k) k
      else failwith "not found",
    none: fun f => set |> none (fun k => f k k),
    reduce: fun f acc => set |> reduce (fun acc k => f acc k k) acc,
    some: fun f => set |> some (fun k => f k k),
    toSequence: toSequence set |> Sequence.map (fun k => (k, k)),
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
