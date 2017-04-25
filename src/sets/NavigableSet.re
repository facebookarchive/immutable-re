/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Functions;
open Functions.Operators;

type s 'set 'a = {
  contains: 'a => 'set => bool,
  count: 'set => int,
  first: 'set => (option 'a),
  firstOrRaise: 'set => 'a,
  reduce: 'acc . while_::('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => 'set => 'acc,
  toCollection: 'set => Collection.t 'a,
  toIterable: 'set => Iterable.t 'a,
  toNavigableCollection: 'set => NavigableCollection.t 'a,
  toSequence: 'set => Sequence.t 'a,
  toSequentialCollection: 'set => SequentialCollection.t 'a,
  toSet: 'set => ImmSet.t 'a,
};

type t 'a =
  | Empty
  | Instance 'set (s 'set 'a) (s 'set 'a): t 'a;

let contains (value: 'a) (set: t 'a): bool => switch set {
  | Empty => false
  | Instance set { contains } _ => contains value set
};

let count (set: t 'a): int => switch set {
  | Empty => 0
  | Instance set { count } _ => count set
};

let empty (): (t 'a) => Empty;

let first (set: t 'a): (option 'a) => switch set {
  | Empty => None
  | Instance set { first } _ => first set
};

let firstOrRaise (set: t 'a): 'a => switch set {
  | Empty => failwith "empty"
  | Instance set { firstOrRaise } _ => firstOrRaise set
};

let isEmpty (set: t 'a): bool =>
  (count set) === 0;

let isNotEmpty (set: t 'a): bool =>
  (count set) !== 0;

let last (set: t 'a): (option 'a) => switch set {
  | Empty => None
  | Instance set _ { first } => first set
};

let lastOrRaise (set: t 'a): 'a => switch set {
  | Empty => failwith "empty"
  | Instance set _ { firstOrRaise } => firstOrRaise set
};

let reduce
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (set: t 'a): 'acc => switch set {
  | Empty => acc
  | Instance set { reduce } _ =>
      set |> reduce while_::predicate f acc;
};

let reduceReversed
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (set: t 'a): 'acc => switch set {
  | Empty => acc
  | Instance set _ { reduce } =>
      set |> reduce while_::predicate f acc;
};

let toCollection (set: t 'a): (Collection.t 'a) => switch set {
  | Empty => Collection.empty ()
  | Instance set { toCollection } _ => toCollection set
};

let toCollectionReversed (set: t 'a): (Collection.t 'a) => switch set {
  | Empty => Collection.empty ()
  | Instance set _ { toCollection } => toCollection set
};

let toIterable (set: t 'a): (Iterable.t 'a) => switch set {
  | Empty => Iterable.empty ()
  | Instance set { toIterable } _ => toIterable set
};

let toIterableReversed (set: t 'a): (Iterable.t 'a) => switch set {
  | Empty => Iterable.empty ()
  | Instance set _ { toIterable } => toIterable set
};

let toNavigableCollection (set: t 'a): (NavigableCollection.t 'a) => switch set {
  | Empty => NavigableCollection.empty ()
  | Instance set { toNavigableCollection } _ => toNavigableCollection set
};

let toNavigableCollectionReversed (set: t 'a): (NavigableCollection.t 'a) => switch set {
  | Empty => NavigableCollection.empty ()
  | Instance set _ { toNavigableCollection } => toNavigableCollection set
};

let toNavigableSet (set: t 'a): (t 'a) => set;

let toNavigableSetReversed (set: t 'a): (t 'a) => switch set {
  | Empty => Empty
  | Instance set impl implReversed => Instance set implReversed impl
};

let toSequence (set: t 'a): (Sequence.t 'a) => switch set {
  | Empty => Sequence.empty ()
  | Instance set { toSequence } _ => toSequence set
};

let toSequenceReversed (set: t 'a): (Sequence.t 'a) => switch set {
  | Empty => Sequence.empty ()
  | Instance set _ { toSequence } => toSequence set
};

let toSequentialCollection (set: t 'a): (SequentialCollection.t 'a) => switch set {
  | Empty => SequentialCollection.empty ()
  | Instance set { toSequentialCollection } _ => toSequentialCollection set
};

let toSequentialCollectionReversed (set: t 'a): (SequentialCollection.t 'a) => switch set {
  | Empty => SequentialCollection.empty ()
  | Instance set _ { toSequentialCollection } => toSequentialCollection set
};

let toSet (set: t 'a): (ImmSet.t 'a) => switch set {
  | Empty => ImmSet.empty ()
  | Instance set { toSet } _ => toSet set
};

let toSetReversed (set: t 'a): (ImmSet.t 'a) => switch set {
  | Empty => ImmSet.empty ()
  | Instance set _ { toSet } => toSet set
};

let equals (this: t 'a) (that: t 'a): bool => switch (this, that) {
  | (Instance _ _ _, Instance _ _ _) =>
      if (this === that) true
      else if ((count this) !== (count that)) false
      else this |> toIterable |> Iterable.every (flip contains that)
  | _ => false
};

let intersect (this: t 'a) (that: t 'a): (Iterable.t 'a) =>
  this |> toIterable |> Iterable.filter (flip contains that);

let subtract (this: t 'a) (that: t 'a): (Iterable.t 'a) =>
  this |> toIterable |> Iterable.filter (flip contains that >> not);

let union (this: t 'a) (that: t 'a): (Iterable.t 'a) => Iterable.concat [
  this |> toIterable,
  subtract that this,
];

type navigableSet 'a = t 'a;

module type S = {
  type a;
  type t;

  include ImmSet.S with type a := a and type t := t;
  include NavigableCollection.S with type a := a and type t := t;

  let toNavigableSet: t => navigableSet a;
  let toNavigableSetReversed: t => navigableSet a;
  let toSetReversed: t => ImmSet.t a;
};

module type S1 = {
  type t 'a;

  include ImmSet.S1 with type t 'a := t 'a;
  include NavigableCollection.S1 with type t 'a := t 'a;

  let toNavigableSet: (t 'a) => navigableSet 'a;
  let toNavigableSetReversed: (t 'a) => navigableSet 'a;
  let toSetReversed: (t 'a) => ImmSet.t 'a;
};

let module Make = fun (Base: {
  type a;
  type t;

  let contains: a => t => bool;
  let count: t => int;
  let equals: t => t => bool;
  let first: t => (option a);
  let firstOrRaise: t => a;
  let last: t => (option a);
  let lastOrRaise: t => a;
  let reduce: while_::('acc => a => bool) => ('acc => a => 'acc) => 'acc => t => 'acc;
  let reduceReversed: while_::('acc => a => bool) => ('acc => a => 'acc) => 'acc => t => 'acc;
  let toSequence: t => Sequence.t a;
  let toSequenceReversed: t => Sequence.t a;
}) => ({
  include Base;

  include (ImmSet.Make Base: ImmSet.S with type t := t and type a := a);
  include (NavigableCollection.Make Base: NavigableCollection.S with type t := t and type a := a);

  let module ReversedImmSet = ImmSet.Make {
    type nonrec a = a;
    type nonrec t = t;

    let contains = contains;
    let count = count;
    let equals = equals;
    let reduce = Base.reduceReversed;
    let toSequence = toSequenceReversed;
  };

  let toSetReversed = ReversedImmSet.toSet;

  let navigableSetBase: s t a = {
    contains,
    count,
    first,
    firstOrRaise,
    reduce: Base.reduce,
    toCollection,
    toIterable,
    toNavigableCollection,
    toSequence,
    toSequentialCollection,
    toSet,
  };

  let navigableSetReversedBase: s t a = {
    contains,
    count,
    first: last,
    firstOrRaise: lastOrRaise,
    reduce: Base.reduceReversed,
    toCollection: toCollectionReversed,
    toIterable: toIterableReversed,
    toNavigableCollection: toNavigableCollectionReversed,
    toSequence: toSequenceReversed,
    toSequentialCollection: toSequentialCollectionReversed,
    toSet: toSetReversed,
  };

  let toNavigableSet (set: t): (navigableSet a) =>
    if (isEmpty set) (empty ())
    else Instance set navigableSetBase navigableSetReversedBase;

  let toNavigableSetReversed (set: t): (navigableSet a) =>
    if (isEmpty set) (empty ())
    else Instance set navigableSetReversedBase navigableSetBase;

}: S with type t := Base.t and type a := Base.a);
