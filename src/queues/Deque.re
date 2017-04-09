/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'a =
  | Ascending (Vector.t 'a)
  | Descending (Vector.t 'a);

let empty (): (t 'a) => Ascending (Vector.empty ());

let addFirst (value: 'a) (deque: t 'a): (t 'a) => switch deque {
  | Ascending vector =>
      Ascending (vector |> Vector.addFirst value)
  | Descending vector =>
      Descending (vector |> Vector.addLast value);
};

let addLast (value: 'a) (deque: t 'a): (t 'a) => switch deque {
  | Ascending vector =>
      Ascending (vector |> Vector.addLast value)
  | Descending vector =>
      Descending (vector |> Vector.addFirst value);
};

let count (deque: t 'a): int => switch deque {
  | Ascending vector
  | Descending vector => Vector.count vector
};

let first (deque: t 'a): (option 'a) => switch deque {
  | Ascending vector => vector |> Vector.first
  | Descending vector => vector |> Vector.last
};

let firstOrRaise (deque: t 'a): 'a => switch deque {
  | Ascending vector => Vector.firstOrRaise vector
  | Descending vector => Vector.lastOrRaise vector
};

let isEmpty (deque: t 'a): bool =>
  deque |> count === 0;

let isNotEmpty (deque: t 'a): bool =>
  deque |> count !== 0;

let last (deque: t 'a): (option 'a) => switch deque {
  | Ascending vector => vector |> Vector.last
  | Descending vector => vector |> Vector.first
};

let lastOrRaise (deque: t 'a): 'a => switch deque {
  | Ascending vector => Vector.lastOrRaise vector
  | Descending vector => Vector.firstOrRaise vector
};

let removeFirstOrRaise (deque: t 'a): (t 'a) => switch deque {
  | Ascending vector =>
      Ascending (Vector.removeFirstOrRaise vector)
  | Descending vector =>
      Descending (Vector.removeLastOrRaise vector)
};

let removeLastOrRaise (deque: t 'a): (t 'a) => switch deque {
  | Ascending vector =>
      Ascending (Vector.removeLastOrRaise vector)
  | Descending vector =>
      Descending (Vector.removeFirstOrRaise vector)
};

let return (value: 'a): (t 'a) =>
  Ascending (Vector.return value);

let reverse (deque: t 'a): (t 'a) => switch deque {
  | Ascending vector => Descending vector
  | Descending vector => Ascending vector
};

let reduceImpl
    while_::(predicate: 'acc => 'a => bool)
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (deque: t 'a): 'acc => switch deque {
  | Ascending vector => vector |> Vector.reduce while_::predicate f acc;
  | Descending vector => vector |> Vector.reduceReversed while_::predicate f acc;
};

let reduce
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (deque: t 'a): 'acc =>
  reduceImpl while_::predicate f acc deque;

let reduceReversedImpl
    while_::(predicate: 'acc => 'a => bool)
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (deque: t 'a): 'acc => switch deque {
  | Ascending vector => vector |> Vector.reduceReversed while_::predicate f acc;
  | Descending vector => vector |> Vector.reduce while_::predicate f acc;
};

let reduceReversed
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (deque: t 'a): 'acc =>
  reduceReversedImpl while_::predicate f acc deque;

let removeAll (_: t 'a): (t 'a) => empty ();

let iterator: Iterable.Iterator.t 'a (t 'a) = { reduce: reduceImpl };

let toIterable (deque: t 'a): (Iterable.t 'a) =>
 if (isEmpty deque) (Iterable.empty ())
 else Iterable.Iterable deque iterator;

let iteratorReversed: Iterable.Iterator.t 'a (t 'a) = { reduce: reduceReversedImpl };

let toIterableReversed (deque: t 'a): (Iterable.t 'a) =>
  if (isEmpty deque) (Iterable.empty ())
  else Iterable.Iterable deque iteratorReversed;

let toSequence (deque: t 'a): (Sequence.t 'a) => switch deque {
  | Ascending vector => vector |> Vector.toSequence
  | Descending vector => vector |> Vector.toSequenceReversed;
};

let toSequenceReversed (deque: t 'a): (Sequence.t 'a) => switch deque {
  | Ascending vector => vector |> Vector.toSequenceReversed
  | Descending vector => vector |> Vector.toSequence;
};

let collectionOps: Collection.Ops.t 'a (t 'a) = {
  count,
  toIterable,
  toSequence,
};

let toCollection (deque: t 'a): (Collection.t 'a) =>
  if (isEmpty deque) (Collection.empty ())
  else Collection.Collection deque collectionOps;

let seqCollectionOps: SequentialCollection.Ops.t 'a (t 'a) = {
  count,
  first,
  firstOrRaise,
  toCollection,
  toIterable,
  toSequence,
};

let toSequentialCollection (deque: t 'a): (SequentialCollection.t 'a) =>
  if (isEmpty deque) (SequentialCollection.empty ())
  else SequentialCollection.SequentialCollection deque seqCollectionOps;

let navCollectionOps: NavigableCollection.Ops.t 'a (t 'a) = {
  count,
  first,
  firstOrRaise,
  last,
  lastOrRaise,
  toCollection,
  toSequentialCollection,
  toIterable,
  toIterableReversed,
  toSequence,
  toSequenceReversed,
};

let toNavigableCollection (deque: t 'a): (NavigableCollection.t 'a) =>
  if (isEmpty deque) (NavigableCollection.empty ())
  else NavigableCollection.NavigableCollection deque navCollectionOps;

let module Transient = {
  type deque 'a = t 'a;

  type transientDequeImpl 'a =
    | Ascending (Vector.Transient.t 'a)
    | Descending (Vector.Transient.t 'a);

  type t 'a = Transient.t (transientDequeImpl 'a);

  let mutate (deque: deque 'a): (t 'a) => switch deque {
    | Ascending vector =>
        Transient.create (Ascending (Vector.mutate vector))
    | Descending vector  =>
        Transient.create (Descending (Vector.mutate vector))
  };

  let addFirst
      (value: 'a)
      (transient: t 'a): (t 'a) => switch (Transient.get transient) {
    | Ascending vector =>
        Vector.Transient.addFirst value vector |> ignore;
        transient;
    | Descending vector =>
        Vector.Transient.addLast value vector |> ignore;
        transient;
  };

  let addFirstAll
      (iter: Iterable.t 'a)
      (transient: t 'a): (t 'a) => switch (Transient.get transient) {
    | Ascending vector =>
        vector |> Vector.Transient.addFirstAll iter |> ignore;
        transient;
    | Descending vector =>
        vector |> Vector.Transient.addLastAll iter |> ignore;
        transient;
  };

  let addLast
      (value: 'a)
      (transient: t 'a): (t 'a) => switch (Transient.get transient) {
    | Ascending vector =>
        Vector.Transient.addLast value vector |> ignore;
        transient;
    | Descending vector =>
        Vector.Transient.addFirst value vector |> ignore;
        transient;
  };

  let addLastAll
      (iter: Iterable.t 'a)
      (transient: t 'a): (t 'a) => switch (Transient.get transient) {
    | Ascending vector =>
        vector |> Vector.Transient.addLastAll iter |> ignore;
        transient;
    | Descending vector =>
        vector |> Vector.Transient.addFirstAll iter |> ignore;
        transient;
  };

  let count (transient: t 'a): int => switch (Transient.get transient) {
    | Ascending vector
    | Descending vector => Vector.Transient.count vector
  };

  let empty (): (t 'a) =>
    empty () |> mutate;

  let first (transient: t 'a): (option 'a) => switch (Transient.get transient) {
    | Ascending vector => vector |> Vector.Transient.first
    | Descending vector => vector |> Vector.Transient.last
  };

  let firstOrRaise (transient: t 'a): 'a => switch (Transient.get transient) {
    | Ascending vector => Vector.Transient.firstOrRaise vector
    | Descending vector => Vector.Transient.lastOrRaise vector
  };

  let isEmpty (transient: t 'a): bool =>
    transient |> count === 0;

  let isNotEmpty (transient: t 'a): bool =>
    transient |> count !== 0;

  let last (transient: t 'a): (option 'a) => switch (Transient.get transient) {
    | Ascending vector => vector |> Vector.Transient.last
    | Descending vector => vector |> Vector.Transient.first
  };

  let lastOrRaise (transient: t 'a): 'a => switch (Transient.get transient)  {
    | Ascending vector => Vector.Transient.lastOrRaise vector
    | Descending vector => Vector.Transient.firstOrRaise vector
  };

  let persist (transient: t 'a): (deque 'a) => switch (Transient.persist transient) {
    | Ascending vector => Ascending (Vector.Transient.persist vector)
    | Descending vector => Descending (Vector.Transient.persist vector)
  };

  let removeAllImpl
      (_: Transient.Owner.t)
      (_: transientDequeImpl 'a): (transientDequeImpl 'a) =>
    Ascending (Vector.Transient.empty ());

  let removeAll (transient: t 'a): (t 'a) =>
    transient |> Transient.update removeAllImpl;

  let removeFirstOrRaise (transient: t 'a): (t 'a) => switch (Transient.get transient) {
    | Ascending vector =>
        Vector.Transient.removeFirstOrRaise vector |> ignore;
        transient;
    | Descending vector =>
        Vector.Transient.removeLastOrRaise vector |> ignore;
        transient;
  };

  let removeLastOrRaise (transient: t 'a): (t 'a) => switch (Transient.get transient) {
    | Ascending vector =>
        Vector.Transient.removeLastOrRaise vector |> ignore;
        transient;
    | Descending vector =>
        Vector.Transient.removeFirstOrRaise vector |> ignore;
        transient;
  };

  let reverseImpl
      (_: Transient.Owner.t)
      (vector: transientDequeImpl 'a) => switch vector {
    | Ascending vector => Descending vector;
    | Descending vector => Ascending vector;
  };

  let reverse (transient: t 'a): (t 'a) =>
    transient |> Transient.update reverseImpl;
};

let mutate = Transient.mutate;

let addFirstAll (iter: Iterable.t 'a) (deque: t 'a): (t 'a) => deque
  |> mutate
  |> Transient.addFirstAll iter
  |> Transient.persist;

let addLastAll (iter: Iterable.t 'a) (deque: t 'a): (t 'a) => deque
  |> mutate
  |> Transient.addLastAll iter
  |> Transient.persist;

let from (iter: Iterable.t 'a): (t 'a) =>
  empty () |> addLastAll iter;

let fromReverse (iter: Iterable.t 'a): (t 'a) =>
  empty () |> addFirstAll iter;
