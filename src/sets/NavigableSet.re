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
  firstOrRaise: 'set => 'a,
  reduce: 'acc . while_::('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => 'set => 'acc,
  toSequence: 'set => Sequence.t 'a,
};

type t 'a =
  | Empty
  | Instance 'set (s 'set 'a) (s 'set 'a): t 'a;

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
  let firstOrRaise: t => a;
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
    let reduce = Base.reduceReversed;
    let toSequence = toSequenceReversed;
  };

  let toSetReversed = ReversedImmSet.toSet;

  let navigableSetBase: s t a = {
    contains,
    count,
    firstOrRaise,
    reduce: Base.reduce,
    toSequence,
  };

  let navigableSetReversedBase: s t a = {
    contains,
    count,
    firstOrRaise: lastOrRaise,
    reduce: Base.reduceReversed,
    toSequence: toSequenceReversed,
  };

  let toNavigableSet (set: t): (navigableSet a) =>
    if (isEmpty set) Empty
    else Instance set navigableSetBase navigableSetReversedBase;

  let toNavigableSetReversed (set: t): (navigableSet a) =>
    if (isEmpty set) Empty
    else Instance set navigableSetReversedBase navigableSetBase;

}: S with type t := Base.t and type a := Base.a);

let module Make1 = fun (Base: {
  type t 'a;

  let contains: 'a => t 'a => bool;
  let count: t 'a => int;
  let firstOrRaise: t 'a => 'a;
  let lastOrRaise: t 'a => 'a;
  let reduce: while_::('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => t 'a => 'acc;
  let reduceReversed: while_::('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => t 'a => 'acc;
  let toSequence: t 'a => Sequence.t 'a;
  let toSequenceReversed: t 'a => Sequence.t 'a;
}) => ({
  include Base;

  include (ImmSet.Make1 Base: ImmSet.S1 with type t 'a := t 'a);
  include (NavigableCollection.Make1 Base: NavigableCollection.S1 with type t 'a := t 'a);

  let module ReversedImmSet = ImmSet.Make1 {
    type nonrec t 'a = t 'a;

    let contains = contains;
    let count = count;
    let reduce = Base.reduceReversed;
    let toSequence = toSequenceReversed;
  };

  let toSetReversed = ReversedImmSet.toSet;

  let navigableSetBase: s (t 'a) 'a = {
    contains,
    count,
    firstOrRaise,
    reduce: Base.reduce,
    toSequence,
  };

  let navigableSetReversedBase: s (t 'a) 'a = {
    contains,
    count,
    firstOrRaise: lastOrRaise,
    reduce: Base.reduceReversed,
    toSequence: toSequenceReversed,
  };

  let toNavigableSet (set: t 'a): (navigableSet 'a) =>
    if (isEmpty set) Empty
    else Instance set navigableSetBase navigableSetReversedBase;

  let toNavigableSetReversed (set: t 'a): (navigableSet 'a) =>
    if (isEmpty set) Empty
    else Instance set navigableSetReversedBase navigableSetBase;

}: S1 with type t 'a := Base.t 'a);

include(Make1 {
  type nonrec t 'a = t 'a;

  let contains (value: 'a) (set: t 'a): bool => switch set {
    | Empty => false
    | Instance set { contains } _ => contains value set
  };

  let count (set: t 'a): int => switch set {
    | Empty => 0
    | Instance set { count } _ => count set
  };

  let firstOrRaise (set: t 'a): 'a => switch set {
    | Empty => failwith "empty"
    | Instance set { firstOrRaise } _ => firstOrRaise set
  };

  let lastOrRaise (set: t 'a): 'a => switch set {
    | Empty => failwith "empty"
    | Instance set _ { firstOrRaise } => firstOrRaise set
  };

  let reduce
      while_::(predicate: 'acc => 'a => bool)
      (f: 'acc => 'a => 'acc)
      (acc: 'acc)
      (set: t 'a): 'acc => switch set {
    | Empty => acc
    | Instance set { reduce } _ =>
        set |> reduce while_::predicate f acc;
  };

  let reduceReversed
      while_::(predicate: 'acc => 'a => bool)
      (f: 'acc => 'a => 'acc)
      (acc: 'acc)
      (set: t 'a): 'acc => switch set {
    | Empty => acc
    | Instance set _ { reduce } =>
        set |> reduce while_::predicate f acc;
  };

  let toSequence (set: t 'a): (Sequence.t 'a) => switch set {
    | Empty => Sequence.empty ()
    | Instance set { toSequence } _ => toSequence set
  };

  let toSequenceReversed (set: t 'a): (Sequence.t 'a) => switch set {
    | Empty => Sequence.empty ()
    | Instance set _ { toSequence } => toSequence set
  };
}: S1 with type t 'a := t 'a);

let empty (): (t 'a) => Empty;

let toNavigableSet (set: t 'a): (t 'a) => set;

let intersect (this: t 'a) (that: t 'a): (Iterable.t 'a) =>
  this |> toIterable |> Iterable.filter (flip contains that);

let subtract (this: t 'a) (that: t 'a): (Iterable.t 'a) =>
  this |> toIterable |> Iterable.filter (flip contains that >> not);

let union (this: t 'a) (that: t 'a): (Iterable.t 'a) => Iterable.concat [
  this |> toIterable,
  subtract that this,
];
