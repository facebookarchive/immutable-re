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

let empty: (t 'a) = Ascending Vector.empty;

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

let equalsWith (valueEquals: Equality.t 'a) (this: t 'a) (that: t 'a): bool =>
  if (this === that) true
  else if ((count this) != (count that)) false
  else switch (this, that) {
    | (Ascending this, Ascending that)
    | (Descending this, Descending that) =>
        Vector.equalsWith valueEquals this that
    | (Ascending this, Descending that)
    | (Descending this, Ascending that) =>
      let countThis = Vector.count this;
      this |> Vector.everyWithIndex
        (fun i => that |> Vector.getOrRaise (countThis - i - 1) |> valueEquals);
  };

let equals (this: t 'a) (that: t 'a): bool =>
  equalsWith Equality.structural this that;

let find (f: 'a => bool) (deque: t 'a): (option 'a) => switch deque {
  | Ascending vector
  | Descending vector => Vector.find f vector
};

let findOrRaise (f: 'a => bool) (deque: t 'a): 'a => switch deque {
  | Ascending vector
  | Descending vector => Vector.findOrRaise f vector
};

let first (deque: t 'a): (option 'a) => switch deque {
  | Ascending vector => vector |> Vector.first
  | Descending vector => vector |> Vector.last
};

let firstOrRaise (deque: t 'a): 'a => switch deque {
  | Ascending vector => Vector.firstOrRaise vector
  | Descending vector => Vector.lastOrRaise vector
};

let hashWith (hash: Hash.t 'a) (deque: t 'a): int => switch deque {
  | Ascending vector
  | Descending vector => Vector.hashWith hash vector
};

let hash (deque: t 'a): int => switch deque {
  | Ascending vector
  | Descending vector => Vector.hash vector
};

let isEmpty (deque: t 'a): bool =>
  deque |> count == 0;

let isNotEmpty (deque: t 'a): bool =>
  deque |> count != 0;

let last (deque: t 'a): (option 'a) => switch deque {
  | Ascending vector => vector |> Vector.last
  | Descending vector => vector |> Vector.first
};

let lastOrRaise (deque: t 'a): 'a => switch deque {
  | Ascending vector => Vector.lastOrRaise vector
  | Descending vector => Vector.firstOrRaise vector
};

let removeFirst (deque: t 'a): (t 'a) => switch deque {
  | Ascending vector =>
      Ascending (Vector.removeFirst vector)
  | Descending vector =>
      Descending (Vector.removeLast vector)
};

let removeLast (deque: t 'a): (t 'a) => switch deque {
  | Ascending vector =>
      Ascending (Vector.removeLast vector)
  | Descending vector =>
      Descending (Vector.removeFirst vector)
};

let return (value: 'a): (t 'a) =>
  Ascending (Vector.return value);

let reverse (deque: t 'a): (t 'a) => switch deque {
  | Ascending vector => Descending vector
  | Descending vector => Ascending vector
};

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) (deque: t 'a): 'acc => switch deque {
  | Ascending vector => vector |> Vector.reduce f acc;
  | Descending vector => vector |> Vector.reduceRight f acc;
};

let forEach (f: 'a => unit) (deque: t 'a): unit =>
  deque |> reduce (fun _ next => f next) ();

let reduceRight (f: 'acc => 'a => 'acc) (acc: 'acc) (deque: t 'a): 'acc => switch deque {
  | Ascending vector => vector |> Vector.reduceRight f acc;
  | Descending vector => vector |> Vector.reduce f acc;
};

let forEachRight (f: 'a => unit) (deque: t 'a): unit =>
  deque |> reduceRight (fun _ next => f next) ();

let removeAll (_: t 'a): (t 'a) => empty;

let toIterator (deque: t 'a): (Iterator.t 'a) =>
 if (isEmpty deque) Iterator.empty
 else { reduce: fun f acc => reduce f acc deque };

let toIteratorRight (deque: t 'a): (Iterator.t 'a) =>
  if (isEmpty deque) Iterator.empty
  else { reduce: fun f acc => reduceRight f acc deque };

let toSequence (deque: t 'a): (Sequence.t 'a) => switch deque {
  | Ascending vector => vector |> Vector.toSequence
  | Descending vector => vector |> Vector.toSequenceRight;
};

let toSequenceRight (deque: t 'a): (Sequence.t 'a) => switch deque {
  | Ascending vector => vector |> Vector.toSequenceRight
  | Descending vector => vector |> Vector.toSequence;
};

let compareWith (valueCompare: Comparator.t 'a) (this: t 'a) (that: t 'a): Ordering.t =>
  if (this === that) Ordering.equal
  else Sequence.compareWith valueCompare (toSequence this) (toSequence that);

let compare (this: t 'a) (that: t 'a): Ordering.t =>
  compareWith Comparator.structural this that;

let containsWith (valueEquals: Equality.t 'a) (value: 'a) (deque: t 'a): bool => switch deque {
  | Ascending vector
  | Descending vector =>
      vector |> Vector.containsWith valueEquals value;
};

let contains (value: 'a) (deque: t 'a): bool =>
  containsWith Equality.structural value deque;

let module TransientDeque = {
  let module TransientVector = Vector.TransientVector;

  type deque 'a = t 'a;

  type transientDequeImpl 'a =
    | Ascending (TransientVector.t 'a)
    | Descending (TransientVector.t 'a);

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
        TransientVector.addFirst value vector |> ignore;
        transient;
    | Descending vector =>
        TransientVector.addLast value vector |> ignore;
        transient;
  };

  let addFirstAll
      (iter: Iterator.t 'a)
      (transient: t 'a): (t 'a) => switch (Transient.get transient) {
    | Ascending vector =>
        vector |> TransientVector.addFirstAll iter |> ignore;
        transient;
    | Descending vector =>
        vector |> TransientVector.addLastAll iter |> ignore;
        transient;
  };

  let addLast
      (value: 'a)
      (transient: t 'a): (t 'a) => switch (Transient.get transient) {
    | Ascending vector =>
        TransientVector.addLast value vector |> ignore;
        transient;
    | Descending vector =>
        TransientVector.addFirst value vector |> ignore;
        transient;
  };

  let addLastAll
      (iter: Iterator.t 'a)
      (transient: t 'a): (t 'a) => switch (Transient.get transient) {
    | Ascending vector =>
        vector |> TransientVector.addLastAll iter |> ignore;
        transient;
    | Descending vector =>
        vector |> TransientVector.addFirstAll iter |> ignore;
        transient;
  };

  let count (transient: t 'a): int => switch (Transient.get transient) {
    | Ascending vector
    | Descending vector => TransientVector.count vector
  };

  let empty (): (t 'a) =>
    empty |> mutate;

  let first (transient: t 'a): (option 'a) => switch (Transient.get transient) {
    | Ascending vector => vector |> TransientVector.first
    | Descending vector => vector |> TransientVector.last
  };

  let firstOrRaise (transient: t 'a): 'a => switch (Transient.get transient) {
    | Ascending vector => TransientVector.firstOrRaise vector
    | Descending vector => TransientVector.lastOrRaise vector
  };

  let isEmpty (transient: t 'a): bool =>
    transient |> count == 0;

  let isNotEmpty (transient: t 'a): bool =>
    transient |> count != 0;

  let last (transient: t 'a): (option 'a) => switch (Transient.get transient) {
    | Ascending vector => vector |> TransientVector.last
    | Descending vector => vector |> TransientVector.first
  };

  let lastOrRaise (transient: t 'a): 'a => switch (Transient.get transient)  {
    | Ascending vector => TransientVector.lastOrRaise vector
    | Descending vector => TransientVector.firstOrRaise vector
  };

  let persist (transient: t 'a): (deque 'a) => switch (Transient.persist transient) {
    | Ascending vector => Ascending (TransientVector.persist vector)
    | Descending vector => Descending (TransientVector.persist vector)
  };

  let removeAllImpl
      (_: Transient.Owner.t)
      (_: transientDequeImpl 'a): (transientDequeImpl 'a) =>
    Ascending (Vector.empty |> Vector.mutate);

  let removeAll (transient: t 'a): (t 'a) =>
    transient |> Transient.update removeAllImpl;

  let removeFirst (transient: t 'a): (t 'a) => switch (Transient.get transient) {
    | Ascending vector =>
        TransientVector.removeFirst vector |> ignore;
        transient;
    | Descending vector =>
        TransientVector.removeLast vector |> ignore;
        transient;
  };

  let removeLast (transient: t 'a): (t 'a) => switch (Transient.get transient) {
    | Ascending vector =>
        TransientVector.removeLast vector |> ignore;
        transient;
    | Descending vector =>
        TransientVector.removeFirst vector |> ignore;
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

let mutate = TransientDeque.mutate;

let addFirstAll (iter: Iterator.t 'a) (deque: t 'a): (t 'a) => deque
  |> mutate
  |> TransientDeque.addFirstAll iter
  |> TransientDeque.persist;

let addLastAll (iter: Iterator.t 'a) (deque: t 'a): (t 'a) => deque
  |> mutate
  |> TransientDeque.addLastAll iter
  |> TransientDeque.persist;

let every (f: 'a => bool) (deque: t 'a): bool => switch deque {
  | Ascending vector
  | Descending vector =>
      Vector.every f vector
};

let from (iter: Iterator.t 'a): (t 'a) =>
  empty |> addLastAll iter;

let fromReverse (iter: Iterator.t 'a): (t 'a) =>
  empty|> addFirstAll iter;

let map (f: 'a => 'b) (deque: t 'a): (t 'b) => switch deque {
  | Ascending vector => Ascending (Vector.map f vector)
  | Descending vector => Descending (Vector.map f vector)
};

let mapReverse (f: 'a => 'b) (deque: t 'a): (t 'b) => switch deque {
  | Ascending vector => Ascending (Vector.mapReverse f vector)
  | Descending vector => Descending (Vector.mapReverse f vector)
};

let none (f: 'a => bool) (deque: t 'a): bool => switch deque {
  | Ascending vector
  | Descending vector =>
      Vector.none f vector
};

let some (f: 'a => bool) (deque: t 'a): bool => switch deque {
  | Ascending vector
  | Descending vector =>
      Vector.some f vector
};
