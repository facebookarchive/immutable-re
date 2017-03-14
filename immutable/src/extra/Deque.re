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
        Seq.equalsWith valueEquals (Vector.toSeq this) (Vector.toSeqReversed that)
  };

let equals (this: t 'a) (that: t 'a): bool =>
  equalsWith Equality.structural this that;

let find (f: 'a => bool) (deque: t 'a): 'a => switch deque {
  | Ascending vector
  | Descending vector => Vector.find f vector
};

let first (deque: t 'a): 'a => switch deque {
  | Ascending vector => Vector.first vector
  | Descending vector => Vector.last vector
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

let last (deque: t 'a): 'a => switch deque {
  | Ascending vector => Vector.last vector
  | Descending vector => Vector.first vector
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

let tryFind (f: 'a => bool) (deque: t 'a): (option 'a) => switch deque {
  | Ascending vector
  | Descending vector => Vector.tryFind f vector
};

let tryFirst (deque: t 'a): (option 'a) => switch deque {
  | Ascending vector => vector |> Vector.tryFirst
  | Descending vector => vector |> Vector.tryLast
};

let tryLast (deque: t 'a): (option 'a) => switch deque {
  | Ascending vector => vector |> Vector.tryLast
  | Descending vector => vector |> Vector.tryFirst
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

let forEachReverse (f: 'a => unit) (deque: t 'a): unit =>
  deque |> reduceRight (fun _ next => f next) ();

let removeAll (_: t 'a): (t 'a) => empty;

let toSeq (deque: t 'a): (Seq.t 'a) => switch deque {
  | Ascending vector => vector |> Vector.toSeq
  | Descending vector => vector |> Vector.toSeqReversed;
};

let toSeqReversed (deque: t 'a): (Seq.t 'a) => switch deque {
  | Ascending vector => vector |> Vector.toSeqReversed
  | Descending vector => vector |> Vector.toSeq;
};

let compareWith (valueCompare: Comparator.t 'a) (this: t 'a) (that: t 'a): Ordering.t =>
  if (this === that) Ordering.equal
  else Seq.compareWith valueCompare (toSeq this) (toSeq that);

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
      (values: Seq.t 'a)
      (transient: t 'a): (t 'a) => switch (Transient.get transient) {
    | Ascending vector =>
        vector |> TransientVector.addFirstAll values |> ignore;
        transient;
    | Descending vector =>
        vector |> TransientVector.addLastAll values |> ignore;
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
      (values: Seq.t 'a)
      (transient: t 'a): (t 'a) => switch (Transient.get transient) {
    | Ascending vector =>
        vector |> TransientVector.addLastAll values |> ignore;
        transient;
    | Descending vector =>
        vector |> TransientVector.addFirstAll values |> ignore;
        transient;
  };

  let count (transient: t 'a): int => switch (Transient.get transient) {
    | Ascending vector
    | Descending vector => TransientVector.count vector
  };

  let empty (): (t 'a) =>
    empty |> mutate;

  let first (transient: t 'a) => switch (Transient.get transient) {
    | Ascending vector => TransientVector.first vector
    | Descending vector => TransientVector.last vector
  };

  let isEmpty (transient: t 'a): bool =>
    transient |> count == 0;

  let isNotEmpty (transient: t 'a): bool =>
    transient |> count != 0;

  let last (transient: t 'a): 'a => switch (Transient.get transient)  {
    | Ascending vector => TransientVector.last vector
    | Descending vector => TransientVector.first vector
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

  let tryFirst (transient: t 'a): (option 'a) => switch (Transient.get transient) {
    | Ascending vector => vector |> TransientVector.tryFirst
    | Descending vector => vector |> TransientVector.tryLast
  };

  let tryLast (transient: t 'a): (option 'a) => switch (Transient.get transient) {
    | Ascending vector => vector |> TransientVector.tryLast
    | Descending vector => vector |> TransientVector.tryFirst
  };
};

let mutate = TransientDeque.mutate;

let addFirstAll (values: Seq.t 'a) (deque: t 'a): (t 'a) => deque
  |> mutate
  |> TransientDeque.addFirstAll values
  |> TransientDeque.persist;

let addLastAll (values: Seq.t 'a) (deque: t 'a): (t 'a) => deque
  |> mutate
  |> TransientDeque.addLastAll values
  |> TransientDeque.persist;

let every (f: 'a => bool) (deque: t 'a): bool => switch deque {
  | Ascending vector
  | Descending vector =>
      Vector.every f vector
};

let fromSeq (seq: Seq.t 'a): (t 'a) =>
  empty |> addLastAll seq;

let fromSeqReversed (seq: Seq.t 'a): (t 'a) =>
  empty|> addFirstAll seq;

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
