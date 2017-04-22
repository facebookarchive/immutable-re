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
  last: 'set => (option 'a),
  lastOrRaise: 'set => 'a,
  reduce: 'acc . while_::('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => 'set => 'acc,
  reduceReversed: 'acc . while_::('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => 'set => 'acc,
  toCollection: 'set => Collection.t 'a,
  toCollectionReversed: 'set => Collection.t 'a,
  toSequentialCollection: 'set => SequentialCollection.t 'a,
  toSequentialCollectionReversed: 'set => SequentialCollection.t 'a,
  toIterable: 'set => Iterable.t 'a,
  toIterableReversed: 'set => Iterable.t 'a,
  toNavigableCollection: 'set => NavigableCollection.t 'a,
  toNavigableCollectionReversed: 'set => NavigableCollection.t 'a,
  toNavigableSetReversed: 'set => t 'a,
  toSequence: 'set => Sequence.t 'a,
  toSequenceReversed: 'set => Sequence.t 'a,
  toSet: 'set => ImmSet.t 'a,
  toSetReversed: 'set => ImmSet.t 'a,
}

and t 'a =
  | Empty
  | Instance 'set (s 'set 'a): t 'a;

let contains (value: 'a) (set: t 'a): bool => switch set {
  | Empty => false
  | Instance set { contains } => contains value set
};

let count (set: t 'a): int => switch set {
  | Empty => 0
  | Instance set { count } => count set
};

let empty (): (t 'a) => Empty;

let first (set: t 'a): (option 'a) => switch set {
  | Empty => None
  | Instance set { first } => first set
};

let firstOrRaise (set: t 'a): 'a => switch set {
  | Empty => failwith "empty"
  | Instance set { firstOrRaise } => firstOrRaise set
};

let isEmpty (set: t 'a): bool =>
  (count set) === 0;

let isNotEmpty (set: t 'a): bool =>
  (count set) !== 0;

let last (set: t 'a): (option 'a) => switch set {
  | Empty => None
  | Instance set { last } => last set
};

let lastOrRaise (set: t 'a): 'a => switch set {
  | Empty => failwith "empty"
  | Instance set { lastOrRaise } => lastOrRaise set
};

let reduce
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (set: t 'a): 'acc => switch set {
  | Empty => acc
  | Instance set { reduce } =>
      set |> reduce while_::predicate f acc;
};

let reduceReversed
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (set: t 'a): 'acc => switch set {
  | Empty => acc
  | Instance set { reduceReversed } =>
      set |> reduceReversed while_::predicate f acc;
};

let toCollection (set: t 'a): (Collection.t 'a) => switch set {
  | Empty => Collection.empty ()
  | Instance set { toCollection } => toCollection set
};

let toCollectionReversed (set: t 'a): (Collection.t 'a) => switch set {
  | Empty => Collection.empty ()
  | Instance set { toCollectionReversed } => toCollectionReversed set
};

let toIterable (set: t 'a): (Iterable.t 'a) => switch set {
  | Empty => Iterable.empty ()
  | Instance set { toIterable } => toIterable set
};

let toIterableReversed (set: t 'a): (Iterable.t 'a) => switch set {
  | Empty => Iterable.empty ()
  | Instance set { toIterableReversed } => toIterableReversed set
};

let toNavigableCollection (set: t 'a): (NavigableCollection.t 'a) => switch set {
  | Empty => NavigableCollection.empty ()
  | Instance set { toNavigableCollection } => toNavigableCollection set
};

let toNavigableCollectionReversed (set: t 'a): (NavigableCollection.t 'a) => switch set {
  | Empty => NavigableCollection.empty ()
  | Instance set { toNavigableCollectionReversed } => toNavigableCollectionReversed set
};

let toNavigableSet (set: t 'a): (t 'a) => set;

let toNavigableSetReversed (set: t 'a): (t 'a) => switch set {
  | Empty => Empty
  | Instance set { toNavigableSetReversed } => toNavigableSetReversed set
};

let toSequence (set: t 'a): (Sequence.t 'a) => switch set {
  | Empty => Sequence.empty ()
  | Instance set { toSequence } => toSequence set
};

let toSequenceReversed (set: t 'a): (Sequence.t 'a) => switch set {
  | Empty => Sequence.empty ()
  | Instance set { toSequenceReversed } => toSequenceReversed set
};

let toSequentialCollection (set: t 'a): (SequentialCollection.t 'a) => switch set {
  | Empty => SequentialCollection.empty ()
  | Instance set { toSequentialCollection } => toSequentialCollection set
};

let toSequentialCollectionReversed (set: t 'a): (SequentialCollection.t 'a) => switch set {
  | Empty => SequentialCollection.empty ()
  | Instance set { toSequentialCollectionReversed } => toSequentialCollectionReversed set
};

let toSet (set: t 'a): (ImmSet.t 'a) => switch set {
  | Empty => ImmSet.empty ()
  | Instance set { toSet } => toSet set
};

let toSetReversed (set: t 'a): (ImmSet.t 'a) => switch set {
  | Empty => ImmSet.empty ()
  | Instance set { toSetReversed } => toSetReversed set
};

let equals (this: t 'a) (that: t 'a): bool => switch (this, that) {
  | (Instance _ _, Instance _ _) =>
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

  let setReversedBase: ImmSet.s t a = {
    contains,
    count,
    reduce: Base.reduceReversed,
    toCollection: toCollectionReversed,
    toIterable: toIterableReversed,
    toSequence: toSequenceReversed,
  };

  let toSetReversed (set: t): (ImmSet.t a) =>
    if (isEmpty set) (ImmSet.empty ())
    else ImmSet.Instance set setReversedBase;

  let rec navigableSetBase: s t a = {
    contains,
    count,
    first,
    firstOrRaise,
    last,
    lastOrRaise,
    reduce: Base.reduce,
    reduceReversed: Base.reduceReversed,
    toCollection,
    toCollectionReversed,
    toSequentialCollection,
    toSequentialCollectionReversed,
    toIterable,
    toIterableReversed,
    toNavigableCollection,
    toNavigableCollectionReversed,
    toNavigableSetReversed,
    toSequence,
    toSequenceReversed,
    toSet,
    toSetReversed,
  }

  and navigableSetReversedBase: s t a = {
    contains,
    count,
    first: last,
    firstOrRaise: lastOrRaise,
    last: first,
    lastOrRaise: firstOrRaise,
    reduce: Base.reduceReversed,
    reduceReversed: Base.reduce,
    toCollection: toCollectionReversed,
    toCollectionReversed: toCollection,
    toSequentialCollection: toSequentialCollectionReversed,
    toSequentialCollectionReversed: toSequentialCollection,
    toIterable: toIterableReversed,
    toIterableReversed: toIterable,
    toNavigableCollection: toNavigableCollectionReversed,
    toNavigableCollectionReversed: toNavigableCollection,
    toNavigableSetReversed: toNavigableSet,
    toSequence: toSequenceReversed,
    toSequenceReversed: toSequence,
    toSet: toSetReversed,
    toSetReversed: toSet,
  }

  and toNavigableSet (set: t): (navigableSet a) =>
    if (isEmpty set) (empty ())
    else Instance set navigableSetBase

  and toNavigableSetReversed (set: t): (navigableSet a) =>
    if (isEmpty set) (empty ())
    else Instance set navigableSetReversedBase;

}: S with type t := Base.t and type a := Base.a);

let module Make1 = fun (Base: {
  type t 'a;

  let contains: 'a => t 'a => bool;
  let count: t 'a => int;
  let equals: t 'a => t 'a => bool;
  let first: t 'a => (option 'a);
  let firstOrRaise: t 'a => 'a;
  let last: t 'a => (option 'a);
  let lastOrRaise: t 'a => 'a;
  let reduce: while_::('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => t 'a => 'acc;
  let reduceReversed: while_::('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => t 'a => 'acc;
  let toSequence: t 'a => Sequence.t 'a;
  let toSequenceReversed: t 'a => Sequence.t 'a;
}) => ({
  include Base;

  include (ImmSet.Make1 Base: ImmSet.S1 with type t 'a := t 'a);
  include (NavigableCollection.Make1 Base: NavigableCollection.S1 with type t 'a := t 'a);

  let setReversedBase: ImmSet.s (t 'a) 'a = {
    contains,
    count,
    reduce: Base.reduceReversed,
    toCollection: toCollectionReversed,
    toIterable: toIterableReversed,
    toSequence: toSequenceReversed,
  };

  let toSetReversed (set: t 'a): (ImmSet.t 'a) =>
    if (isEmpty set) (ImmSet.empty ())
    else ImmSet.Instance set setReversedBase;

  let rec navigableSetBase: s (t 'a) 'a = {
    contains,
    count,
    first,
    firstOrRaise,
    last,
    lastOrRaise,
    reduce: Base.reduce,
    reduceReversed: Base.reduceReversed,
    toCollection,
    toCollectionReversed,
    toSequentialCollection,
    toSequentialCollectionReversed,
    toIterable,
    toIterableReversed,
    toNavigableCollection,
    toNavigableCollectionReversed,
    toNavigableSetReversed,
    toSequence,
    toSequenceReversed,
    toSet,
    toSetReversed,
  }

  and navigableSetReversedBase: s (t 'a) 'a = {
    contains,
    count,
    first: last,
    firstOrRaise: lastOrRaise,
    last: first,
    lastOrRaise: firstOrRaise,
    reduce: Base.reduceReversed,
    reduceReversed: Base.reduce,
    toCollection: toCollectionReversed,
    toCollectionReversed: toCollection,
    toSequentialCollection: toSequentialCollectionReversed,
    toSequentialCollectionReversed: toSequentialCollection,
    toIterable: toIterableReversed,
    toIterableReversed: toIterable,
    toNavigableCollection: toNavigableCollectionReversed,
    toNavigableCollectionReversed: toNavigableCollection,
    toNavigableSetReversed: toNavigableSet,
    toSequence: toSequenceReversed,
    toSequenceReversed: toSequence,
    toSet: toSetReversed,
    toSetReversed: toSet,
  }

  and toNavigableSet (set: t 'a): (navigableSet 'a) =>
    if (isEmpty set) (empty ())
    else Instance set navigableSetBase

  and toNavigableSetReversed (set: t 'a): (navigableSet 'a) =>
    if (isEmpty set) (empty ())
    else Instance set navigableSetReversedBase;
}: S1 with type t 'a := Base.t 'a);
