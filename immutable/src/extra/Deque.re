/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Comparator;
open CopyOnWriteArray;
open Equality;
open Functions;
open Hash;
open Indexed;
open Option.Operators;
open Ordering;
open Preconditions;
open Seq;
open Transient;
open Vector;

type deque 'a =
  | Ascending (vector 'a)
  | Descending (vector 'a);

let empty: (deque 'a) = Ascending Vector.empty;

type transientDequeImpl 'a =
  | Ascending (transientVector 'a)
  | Descending (transientVector 'a);

type transientDeque 'a = transient (transientDequeImpl 'a);

let mutate (deque: deque 'a): (transientDeque 'a) => switch deque {
  | Ascending vector =>
      Transient.create (Ascending (Vector.mutate vector))
  | Descending vector  =>
      Transient.create (Descending (Vector.mutate vector))
};

let addFirst (value: 'a) (deque: deque 'a): (deque 'a) => switch deque {
  | Ascending vector =>
      Ascending (vector |> Vector.addFirst value)
  | Descending vector =>
      Descending (vector |> Vector.addLast value);
};

let addLast (value: 'a) (deque: deque 'a): (deque 'a) => switch deque {
  | Ascending vector =>
      Ascending (vector |> Vector.addLast value)
  | Descending vector =>
      Descending (vector |> Vector.addFirst value);
};

let add = addLast;

let count (deque: deque 'a): int => switch deque {
  | Ascending vector
  | Descending vector => Vector.count vector
};

let equalsWith (valueEquals: equality 'a) (this: deque 'a) (that: deque 'a): bool =>
  this === that ? true :
  (count this) != (count that) ? false :
  switch (this, that) {
    | (Ascending this, Ascending that)
    | (Descending this, Descending that) =>
        Vector.equalsWith valueEquals this that
    | (Ascending this, Descending that)
    | (Descending this, Ascending that) =>
        Seq.equalsWith valueEquals (Vector.toSeq this) (Vector.toSeqReversed that)
  };

let equals (this: deque 'a) (that: deque 'a): bool =>
  equalsWith Equality.structural this that;

let find (f: 'a => bool) (deque: deque 'a): 'a => switch deque {
  | Ascending vector
  | Descending vector => Vector.find f vector
};

let first (deque: deque 'a): 'a => switch deque {
  | Ascending vector => Vector.first vector
  | Descending vector => Vector.last vector
};

let hashWith (hash: hash 'a) (deque: deque 'a): int => switch deque {
  | Ascending vector
  | Descending vector => Vector.hashWith hash vector
};

let hash (deque: deque 'a): int => switch deque {
  | Ascending vector
  | Descending vector => Vector.hash vector
};

let last (deque: deque 'a): 'a => switch deque {
  | Ascending vector => Vector.last vector
  | Descending vector => Vector.first vector
};

let removeFirst (deque: deque 'a): (deque 'a) => switch deque {
  | Ascending vector =>
      Ascending (Vector.removeFirst vector)
  | Descending vector =>
      Descending (Vector.removeLast vector)
};

let removeLast (deque: deque 'a): (deque 'a) => switch deque {
  | Ascending vector =>
      Ascending (Vector.removeLast vector)
  | Descending vector =>
      Descending (Vector.removeFirst vector)
};

let reverse (deque: deque 'a): (deque 'a) => switch deque {
  | Ascending vector => Descending vector
  | Descending vector => Ascending vector
};

let tryFind (f: 'a => bool) (deque: deque 'a): (option 'a) => switch deque {
  | Ascending vector
  | Descending vector => Vector.tryFind f vector
};

let tryFirst (deque: deque 'a): (option 'a) => switch deque {
  | Ascending vector => vector |> Vector.tryFirst
  | Descending vector => vector |> Vector.tryLast
};

let tryLast (deque: deque 'a): (option 'a) => switch deque {
  | Ascending vector => vector |> Vector.tryLast
  | Descending vector => vector |> Vector.tryFirst
};

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) (deque: deque 'a): 'acc => switch deque {
  | Ascending vector => vector |> Vector.reduce f acc;
  | Descending vector => vector |> Vector.reduceRight f acc;
};

let reduceRight (f: 'acc => 'a => 'acc) (acc: 'acc) (deque: deque 'a): 'acc => switch deque {
  | Ascending vector => vector |> Vector.reduceRight f acc;
  | Descending vector => vector |> Vector.reduce f acc;
};

let removeAll (_: deque 'a): (deque 'a) => empty;

let toSeq (deque: deque 'a): (seq 'a) => switch deque {
  | Ascending vector => vector |> Vector.toSeq
  | Descending vector => vector |> Vector.toSeqReversed;
};

let toSeqReversed (deque: deque 'a): (seq 'a) => switch deque {
  | Ascending vector => vector |> Vector.toSeqReversed
  | Descending vector => vector |> Vector.toSeq;
};

let compareWith (valueCompare: comparator 'a) (this: deque 'a) (that: deque 'a): ordering =>
  this === that ? Ordering.equal : Seq.compareWith valueCompare (toSeq this) (toSeq that);

let compare (this: deque 'a) (that: deque 'a): ordering =>
  compareWith Comparator.structural this that;

let module TransientDeque = {
  let addFirst
      (value: 'a)
      (transient: transientDeque 'a): (transientDeque 'a) => switch (Transient.get transient) {
    | Ascending vector =>
        TransientVector.addFirst value vector |> ignore;
        transient;
    | Descending vector =>
        TransientVector.addLast value vector |> ignore;
        transient;
  };

  let addLast
      (value: 'a)
      (transient: transientDeque 'a): (transientDeque 'a) => switch (Transient.get transient) {
    | Ascending vector =>
        TransientVector.addLast value vector |> ignore;
        transient;
    | Descending vector =>
        TransientVector.addFirst value vector |> ignore;
        transient;
  };

  let add = addLast;

  let count (transient: transientDeque 'a): int => switch (Transient.get transient) {
    | Ascending vector
    | Descending vector => TransientVector.count vector
  };

  let empty () => empty |> mutate;

  let first (transient: transientDeque 'a) => switch (Transient.get transient) {
    | Ascending vector => TransientVector.first vector
    | Descending vector => TransientVector.last vector
  };

  let last (transient: transientDeque 'a): 'a => switch (Transient.get transient)  {
    | Ascending vector => TransientVector.last vector
    | Descending vector => TransientVector.first vector
  };

  let removeFirst (transient: transientDeque 'a): (transientDeque 'a) => switch (Transient.get transient) {
    | Ascending vector =>
        TransientVector.removeFirst vector |> ignore;
        transient;
    | Descending vector =>
        TransientVector.removeLast vector |> ignore;
        transient;
  };

  let removeLast (transient: transientDeque 'a): (transientDeque 'a) => switch (Transient.get transient) {
    | Ascending vector =>
        TransientVector.removeLast vector |> ignore;
        transient;
    | Descending vector =>
        TransientVector.removeFirst vector |> ignore;
        transient;
  };

  let reverse (transient: transientDeque 'a): (transientDeque 'a) =>
    transient |> Transient.update(fun _ vector => switch vector {
      | Ascending vector => Descending vector;
      | Descending vector => Ascending vector;
    });

  let tryFirst (transient: transientDeque 'a): (option 'a) => switch (Transient.get transient) {
    | Ascending vector => vector |> TransientVector.tryFirst
    | Descending vector => vector |> TransientVector.tryLast
  };

  let tryLast (transient: transientDeque 'a): (option 'a) => switch (Transient.get transient) {
    | Ascending vector => vector |> TransientVector.tryLast
    | Descending vector => vector |> TransientVector.tryFirst
  };

  let addAll
      (values: seq 'a)
      (transient: transientDeque 'a): (transientDeque 'a) => switch (Transient.get transient) {
    | Ascending vector =>
        vector |> TransientVector.addAll values |> ignore;
        transient;
    | Descending vector =>
        values |> Seq.reduce (fun acc next => acc |> TransientVector.addFirst next) vector |> ignore;
        transient;
  };

  let persist (transient: transientDeque 'a): (deque 'a) => switch (Transient.persist transient) {
    | Ascending vector => Ascending (TransientVector.persist vector)
    | Descending vector => Descending (TransientVector.persist vector)
  };

  let removeAll (transient: transientDeque 'a): (transientDeque 'a) =>
    transient |> Transient.update (fun owner _ => {
      Ascending (Vector.empty |> Vector.mutate)
    });
};

let addAll (values: seq 'a) (deque: deque 'a): (deque 'a) => deque
  |> mutate
  |> TransientDeque.addAll values
  |> TransientDeque.persist;

let every (f: 'a => bool) (deque: deque 'a): bool => switch deque {
  | Ascending vector
  | Descending vector =>
      Vector.every f vector
};

let fromSeq (src: seq 'a): (deque 'a) =>
  empty |> addAll src;

let map (f: 'a => 'b) (deque: deque 'a): (deque 'b) => switch deque {
  | Ascending vector => Ascending (Vector.map f vector)
  | Descending vector => Descending (Vector.map f vector)
};

let mapReverse (f: 'a => 'b) (deque: deque 'a): (deque 'b) => switch deque {
  | Ascending vector => Ascending (Vector.mapReverse f vector)
  | Descending vector => Descending (Vector.mapReverse f vector)
};

let none (f: 'a => bool) (deque: deque 'a): bool => switch deque {
  | Ascending vector
  | Descending vector =>
      Vector.none f vector
};

let some (f: 'a => bool) (deque: deque 'a): bool => switch deque {
  | Ascending vector
  | Descending vector =>
      Vector.some f vector
};
