open Comparator;
open CopyOnWriteArray;
open Equality;
open Functions;
open Hash;
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

let isEmpty (deque: deque 'a): bool =>
  deque |> count == 0;

let isNotEmpty (deque: deque 'a): bool =>
  deque |> count != 0;

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

let return (value: 'a): (deque 'a) =>
  Ascending (Vector.return value);

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

let forEach (f: 'a => unit) (deque: deque 'a): unit =>
  deque |> reduce (fun _ next => f next) ();

let reduceRight (f: 'acc => 'a => 'acc) (acc: 'acc) (deque: deque 'a): 'acc => switch deque {
  | Ascending vector => vector |> Vector.reduceRight f acc;
  | Descending vector => vector |> Vector.reduce f acc;
};

let forEachReverse (f: 'a => unit) (deque: deque 'a): unit =>
  deque |> reduceRight (fun _ next => f next) ();

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

let containsWith (valueEquals: equality 'a) (value: 'a) (deque: deque 'a): bool => switch deque {
  | Ascending vector
  | Descending vector =>
      vector |> Vector.containsWith valueEquals value;
};

let contains (value: 'a) (deque: deque 'a): bool =>
  containsWith Equality.structural value deque;

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

  let addFirstAll
      (values: seq 'a)
      (transient: transientDeque 'a): (transientDeque 'a) => switch (Transient.get transient) {
    | Ascending vector =>
        vector |> TransientVector.addFirstAll values |> ignore;
        transient;
    | Descending vector =>
        vector |> TransientVector.addLastAll values |> ignore;
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

  let addLastAll
      (values: seq 'a)
      (transient: transientDeque 'a): (transientDeque 'a) => switch (Transient.get transient) {
    | Ascending vector =>
        vector |> TransientVector.addLastAll values |> ignore;
        transient;
    | Descending vector =>
        vector |> TransientVector.addFirstAll values |> ignore;
        transient;
  };

  let count (transient: transientDeque 'a): int => switch (Transient.get transient) {
    | Ascending vector
    | Descending vector => TransientVector.count vector
  };

  let empty (): (transientDeque 'a) =>
    empty |> mutate;

  let first (transient: transientDeque 'a) => switch (Transient.get transient) {
    | Ascending vector => TransientVector.first vector
    | Descending vector => TransientVector.last vector
  };

  let isEmpty (transient: transientDeque 'a): bool =>
    transient |> count == 0;

  let isNotEmpty (transient: transientDeque 'a): bool =>
    transient |> count != 0;

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

  let persist (transient: transientDeque 'a): (deque 'a) => switch (Transient.persist transient) {
    | Ascending vector => Ascending (TransientVector.persist vector)
    | Descending vector => Descending (TransientVector.persist vector)
  };

  let removeAll (transient: transientDeque 'a): (transientDeque 'a) =>
    transient |> Transient.update (fun owner _ => {
      Ascending (Vector.empty |> Vector.mutate)
    });
};

let addFirstAll (values: seq 'a) (deque: deque 'a): (deque 'a) => deque
  |> mutate
  |> TransientDeque.addFirstAll values
  |> TransientDeque.persist;

let addLastAll (values: seq 'a) (deque: deque 'a): (deque 'a) => deque
  |> mutate
  |> TransientDeque.addLastAll values
  |> TransientDeque.persist;

let every (f: 'a => bool) (deque: deque 'a): bool => switch deque {
  | Ascending vector
  | Descending vector =>
      Vector.every f vector
};

let fromSeq (seq: seq 'a): (deque 'a) =>
  empty |> addLastAll seq;

let fromSeqReversed (seq: seq 'a): (deque 'a) =>
  empty|> addFirstAll seq;

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
