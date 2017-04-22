/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type s 'collection 'a = {
  count: 'collection => int,
  first: 'collection => (option 'a),
  firstOrRaise: 'collection => 'a,
  last: 'collection => (option 'a),
  lastOrRaise: 'collection => 'a,
  reduce: 'acc . while_::('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => 'collection => 'acc,
  reduceReversed: 'acc . while_::('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => 'collection => 'acc,
  toCollection: 'collection => Collection.t 'a,
  toCollectionReversed: 'collection => Collection.t 'a,
  toIterable: 'collection => Iterable.t 'a,
  toIterableReversed: 'collection => Iterable.t 'a,
  toNavigableCollectionReversed: 'collection => t 'a,
  toSequence: 'collection => Sequence.t 'a,
  toSequenceReversed: 'collection => Sequence.t 'a,
  toSequentialCollection: 'collection => SequentialCollection.t 'a,
  toSequentialCollectionReversed: 'collection => SequentialCollection.t 'a,
}

and t 'a =
  | Empty
  | Instance 'collection (s 'collection 'a): t 'a;

let count (collection: t 'a): int => switch collection {
  | Empty => 0
  | Instance collection { count } => count collection
};

let empty (): (t 'a) => Empty;

let first (collection: t 'a): (option 'a) => switch collection {
  | Empty => None
  | Instance collection { first } => first collection
};

let firstOrRaise (collection: t 'a): 'a => switch collection {
  | Empty => failwith "empty"
  | Instance collection { firstOrRaise } => firstOrRaise collection
};

let last (collection: t 'a): (option 'a) => switch collection {
  | Empty => None
  | Instance collection { last } => last collection
};

let lastOrRaise (collection: t 'a): 'a => switch collection {
  | Empty => failwith "empty"
  | Instance collection { lastOrRaise } => lastOrRaise collection
};

let isEmpty (collection: t 'a): bool =>
  (count collection) === 0;

let isNotEmpty (collection: t 'a): bool =>
  (count collection) !== 0;

let reduce
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (collection: t 'a): 'acc => switch collection {
  | Empty => acc
  | Instance collection { reduce } =>
      collection |> reduce while_::predicate f acc;
};

let reduceReversed
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (collection: t 'a): 'acc => switch collection {
  | Empty => acc
  | Instance collection { reduceReversed } =>
      collection |> reduceReversed while_::predicate f acc;
};

let toCollection (collection: t 'a): (Collection.t 'a) => switch collection {
  | Empty => Collection.empty ()
  | Instance collection { toCollection } => toCollection collection
};

let toCollectionReversed (collection: t 'a): (Collection.t 'a) => switch collection {
  | Empty => Collection.empty ()
  | Instance collection { toCollectionReversed } => toCollectionReversed collection
};

let toIterable (collection: t 'a): (Iterable.t 'a) => switch collection {
  | Empty => Iterable.empty ()
  | Instance collection { toIterable } => toIterable collection
};

let toIterableReversed (collection: t 'a): (Iterable.t 'a) => switch collection {
  | Empty => Iterable.empty ()
  | Instance collection { toIterableReversed } => toIterableReversed collection
};

let toNavigableCollection (collection: t 'a): (t 'a) =>
  collection;

let toNavigableCollectionReversed (collection: t 'a): (t 'a) => switch collection {
  | Empty => Empty
  | Instance collection { toNavigableCollectionReversed } =>
      toNavigableCollectionReversed collection
};

let toSequence (collection: t 'a): (Sequence.t 'a) => switch collection {
  | Empty => Sequence.empty ()
  | Instance collection { toSequence } => toSequence collection
};

let toSequenceReversed (collection: t 'a): (Sequence.t 'a) => switch collection {
  | Empty => Sequence.empty ()
  | Instance collection { toSequenceReversed } => toSequenceReversed collection
};

let toSequentialCollection (collection: t 'a): (SequentialCollection.t 'a) => switch collection {
  | Empty => SequentialCollection.empty ()
  | Instance collection { toSequentialCollection } => toSequentialCollection collection
};

let toSequentialCollectionReversed (collection: t 'a): (SequentialCollection.t 'a) => switch collection {
  | Empty => SequentialCollection.empty ()
  | Instance collection { toSequentialCollectionReversed } => toSequentialCollectionReversed collection
};

type navigableCollection 'a = t 'a;

module type S = {
  type a;
  type t;

  include SequentialCollection.S with type a := a and type t := t;

  let last: t => (option a);
  let lastOrRaise: t => a;
  let reduceReversed: while_::('acc => a => bool)? => ('acc => a => 'acc) => 'acc => t => 'acc;
  let toCollectionReversed: t => (Collection.t a);
  let toIterableReversed: t => (Iterable.t a);
  let toNavigableCollection: t => (navigableCollection a);
  let toNavigableCollectionReversed: t => (navigableCollection a);
  let toSequenceReversed: t => (Sequence.t a);
  let toSequentialCollectionReversed: t => (SequentialCollection.t a);
};

module type S1 = {
  type t 'a;

  include SequentialCollection.S1 with type t 'a := t 'a;

  let last: (t 'a) => (option 'a);
  let lastOrRaise: (t 'a) => 'a;
  let reduceReversed: while_::('acc => 'a => bool)? => ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let toCollectionReversed: t 'a => (Collection.t 'a);
  let toIterableReversed: t 'a => (Iterable.t 'a);
  let toNavigableCollection: (t 'a) => (navigableCollection 'a);
  let toNavigableCollectionReversed: (t 'a) => (navigableCollection 'a);
  let toSequenceReversed: (t 'a) => (Sequence.t 'a);
  let toSequentialCollectionReversed: t 'a => (SequentialCollection.t 'a);
};

let module Make = fun (Base: {
  type a;
  type t;

  let count: t => int;
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

  include (SequentialCollection.Make Base: SequentialCollection.S with type t := t and type a := a);

  let reduceReversed
      while_::(predicate: 'acc => a => bool)=Functions.alwaysTrue2
      (f: 'acc => a => 'acc)
      (acc: 'acc)
      (collection: t): 'acc =>
    reduceReversed while_::predicate f acc collection;

  let module ReversedSequentialCollection = SequentialCollection.Make {
    type nonrec t = t;
    type nonrec a = a;

    let count = count;
    let first = last;
    let firstOrRaise = lastOrRaise;
    let reduce = Base.reduceReversed;
    let toSequence = toSequenceReversed;
  };

  let toIterableReversed = ReversedSequentialCollection.toIterable;

  let toSequenceReversed = ReversedSequentialCollection.toSequence;

  let toCollectionReversed = ReversedSequentialCollection.toCollection;

  let toSequentialCollectionReversed = ReversedSequentialCollection.toSequentialCollection;

  let rec navigableCollectionBase: s t a = {
    count,
    first,
    firstOrRaise,
    last,
    lastOrRaise,
    reduce: Base.reduce,
    reduceReversed: Base.reduceReversed,
    toCollection,
    toCollectionReversed,
    toIterable,
    toIterableReversed,
    toNavigableCollectionReversed,
    toSequence,
    toSequenceReversed,
    toSequentialCollection,
    toSequentialCollectionReversed,
  }

  and navigableCollectionReversedBase: s t a = {
    count,
    first: last,
    firstOrRaise: lastOrRaise,
    last: first,
    lastOrRaise: firstOrRaise,
    reduce: Base.reduceReversed,
    reduceReversed: Base.reduce,
    toCollection: toCollectionReversed,
    toCollectionReversed: toCollection,
    toIterable: toIterableReversed,
    toIterableReversed: toIterable,
    toNavigableCollectionReversed: toNavigableCollection,
    toSequence: toSequenceReversed,
    toSequenceReversed: toSequence,
    toSequentialCollection: toSequentialCollectionReversed,
    toSequentialCollectionReversed: toSequentialCollection,
  }

  and toNavigableCollection (collection: t): (navigableCollection a) =>
    if (isEmpty collection) (empty ())
    else Instance collection navigableCollectionBase

  and toNavigableCollectionReversed  (collection: t): (navigableCollection a) =>
    if (isEmpty collection) (empty ())
    else Instance collection navigableCollectionReversedBase;

}: S with type t := Base.t and type a := Base.a);

let module Make1 = fun (Base: {
  type t 'a;

  let count: t 'a => int;
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

  include (SequentialCollection.Make1 Base: SequentialCollection.S1 with type t 'a := t 'a);

  let reduceReversed
      while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
      (f: 'acc => 'a => 'acc)
      (acc: 'acc)
      (collection: t 'a): 'acc =>
    reduceReversed while_::predicate f acc collection;

  let module ReversedSequentialCollection = SequentialCollection.Make1 {
    type nonrec t 'a = t 'a;

    let count = count;
    let first = last;
    let firstOrRaise = lastOrRaise;
    let reduce = Base.reduceReversed;
    let toSequence = toSequenceReversed;
  };

  let toIterableReversed = ReversedSequentialCollection.toIterable;

  let toSequenceReversed = ReversedSequentialCollection.toSequence;

  let toCollectionReversed = ReversedSequentialCollection.toCollection;

  let toSequentialCollectionReversed = ReversedSequentialCollection.toSequentialCollection;

  let rec navigableCollectionBase: s (t 'a) 'a = {
    count,
    first,
    firstOrRaise,
    last,
    lastOrRaise,
    reduce: Base.reduce,
    reduceReversed: Base.reduceReversed,
    toCollection,
    toCollectionReversed,
    toIterable,
    toIterableReversed,
    toNavigableCollectionReversed,
    toSequence,
    toSequenceReversed,
    toSequentialCollection,
    toSequentialCollectionReversed,
  }

  and navigableCollectionReversedBase: s (t 'a) 'a = {
    count,
    first: last,
    firstOrRaise: lastOrRaise,
    last: first,
    lastOrRaise: firstOrRaise,
    reduce: Base.reduceReversed,
    reduceReversed: Base.reduce,
    toCollection: toCollectionReversed,
    toCollectionReversed: toCollection,
    toIterable: toIterableReversed,
    toIterableReversed: toIterable,
    toNavigableCollectionReversed: toNavigableCollection,
    toSequence: toSequenceReversed,
    toSequenceReversed: toSequence,
    toSequentialCollection: toSequentialCollectionReversed,
    toSequentialCollectionReversed: toSequentialCollection,
  }

  and toNavigableCollection (collection: t 'a): (navigableCollection 'a) =>
    if (isEmpty collection) (empty ())
    else Instance collection navigableCollectionBase

  and toNavigableCollectionReversed (collection: t 'a): (navigableCollection 'a) =>
    if (isEmpty collection) (empty ())
    else Instance collection navigableCollectionReversedBase

}: S1 with type t 'a := Base.t 'a);
