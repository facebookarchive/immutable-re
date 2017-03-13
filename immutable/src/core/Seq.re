open Functions.Operators;
open Option.Operators;

type iterator 'a =
  | Next 'a (t 'a)
  | Completed

and t 'a = unit => iterator 'a;

let empty: (t 'a) = fun () => Completed;

let return (value: 'a): (t 'a) => fun () =>
  Next value empty;

let rec ofList (list: list 'a): (t 'a) => fun () => switch list {
  | [value] => Next value empty
  | [value, ...tail] => Next value (ofList tail)
  | [] => Completed
};

let rec compareWith (valueCompare: Comparator.t 'a) (this: t 'a) (that: t 'a): Ordering.t =>
  if (this === that) Ordering.equal
  else switch (this (), that ()) {
    | (Next thisValue thisNext, Next thatValue thatNext) =>
        let cmp = valueCompare thisValue thatValue;

        if (cmp === Ordering.equal) (compareWith valueCompare thisNext thatNext)
        else cmp
    | (Completed, Completed) => Ordering.equal
    | (Next _ _, Completed) => Ordering.greaterThan
    | (Completed, Next _ _) => Ordering.lessThan
  };

let compare (this: t 'a) (that: t 'a): Ordering.t =>
  compareWith Comparator.structural this that;

let concatAll (seq: t (t 'a)): (t 'a) => {
  let rec continuedWith (continuation: t (t 'a)) (iter: (iterator 'a)): (iterator 'a) => switch (iter) {
    | Next value next =>
        Next value (next >> continuedWith continuation);
    | Completed => continuation () |> flattenIter
  }

  and flattenIter (iter: iterator (t 'a)): (iterator 'a) => switch iter {
    | Next value next => value () |> continuedWith next
    | Completed => Completed
  };

  fun () => seq () |> flattenIter;
};

let concat (seqs: list (t 'a)): (t 'a) =>
  seqs |> ofList |> concatAll;

let rec containsWith (valueEquals: Equality.t 'a) (value: 'a) (seq: t 'a): bool => switch (seq ()) {
  | Next next _ when valueEquals next value => true
  | Next _ nextSeq => containsWith valueEquals value nextSeq
  | Completed => false
};

let contains (value: 'a) (seq: t 'a): bool =>
  containsWith Equality.structural value seq;

let defer (f: unit => (t 'a)): (t 'a) => fun () => f () ();

let rec equalsWith (equality: Equality.t 'a) (this: t 'a) (that: t 'a): bool =>
  (that === this) ||
  switch (that (), this ()) {
    | (Next thisValue thisNext, Next thatValue thatNext) =>
        if (equality thisValue thatValue) (equalsWith equality thisNext thatNext)
        else false
    | (Completed, Completed) => true
    | _ => false
  };

let equals (that: t 'a) (this: t 'a): bool =>
  equalsWith Equality.structural that this;

let rec every (f: 'a => bool) (seq: t 'a): bool => switch ( seq () ) {
  | Next value next =>
      if (f value) (every f next)
      else false
  | Completed => true
};

let rec filter (f: 'a => bool) (seq: t 'a): (t 'a) => {
  let rec filterIter (f: 'a => bool) (iter: iterator 'a): iterator 'b => switch iter {
    | Next value next =>
        if (f value) (Next value (filter f next))
        else next () |> filterIter f
    | Completed => Completed
  };

  fun () => seq () |> filterIter f
};

let rec find (predicate: 'a => bool) (seq: t 'a): 'a => switch (seq ()) {
  | Next value next =>
      if (predicate value) value
      else (find predicate next)
  | Completed => failwith "not found"
};

let first (seq: t 'a): 'a => switch (seq ()) {
  | Next value _ => value
  | Completed => failwith "empty"
};

let flatten (seq: t (t 'a)): (t 'a) =>
  concatAll seq;

let rec forEach (f: 'a => unit) (seq: t 'a) => switch (seq ()) {
  | Next value next =>
      f value;
      forEach f next;
  | Completed => ()
};

let inRange (start: int) (count: option int) (step: int): (t int) => switch count {
  | Some count when count < 0 => failwith "Count must be great than 0"
  | _ =>
      let rec recurse (start: int) (count: option int) (step: int): (t int) => fun () => switch count {
        | Some count when count == 0 => Completed
        | Some count => Next start (recurse (start + step) (Some (count - 1)) step)
        | None => Next start (recurse (start + step) None step)
      };

      recurse start count step
};

let isEmpty (seq: t 'a): bool => switch (seq ()) {
  | Next _ => false
  | Completed => true
};

let isNotEmpty (seq: t 'a): bool => switch (seq ()) {
  | Next _ => true
  | Completed => false
};

let last (seq: t 'a): 'a => {
  let rec loop acc seq => switch (seq ()) {
    | Next v next => loop v next
    | Completed => acc
  };

  switch (seq ()) {
    | Next v next => loop v next
    | Completed => failwith "not found"
  }
};

let rec map (f: 'a => 'b) (seq: t 'a): (t 'b) => fun () => switch (seq ()) {
  | Next value next =>
      Next (f value) (map f next)
  | Completed => Completed
};

let concatMap (f: 'a => (t 'b)) (seq: t 'a): (t 'b) =>
  seq |> map f |> concatAll;

let doOnNext (f: 'a => unit) (seq: t 'a): (t 'a) =>
  seq |> map (fun next => { f next; next });

let flatMap (f: 'a => (t 'b)) (seq: t 'a): (t 'b) =>
  seq |> map f |> flatten;

let rec none (f: 'a => bool) (seq: t 'a): bool => switch (seq ()) {
  | Next value next =>
      if (f value) false
      else none f next
  | Completed => true
};

let ofOption (opt: option 'a): (t 'a) => switch opt {
  | Some value => return value
  | None => empty
};

let rec reduce
    (reducer: 'acc => 'a => 'acc)
    (acc: 'acc)
    (seq: t 'a): 'acc => switch (seq ()) {
  | Next value next =>
      let acc = reducer acc value;
      reduce reducer acc next
  | Completed => acc
};

let count (seq: t 'a): int => seq
  |> reduce (fun acc _ => succ acc) 0;

let hashWith (hash: (Hash.t 'a)) (seq: t 'a): int => seq
  |> reduce (Hash.reducer hash) Hash.initialValue;

let hash (seq: t 'a): int =>
  hashWith Hash.structural seq;

let listAddFirstAll (seq: t 'a) (list: list 'a): (list 'a) =>
  seq |> reduce (fun acc next => acc |> ImmList.addFirst next) list;

let listFromSeqReverse (seq: t 'a): (list 'a) =>
  [] |> listAddFirstAll seq;

let rec repeat (value: 'a) (count: option int): (t 'a) => switch count {
  | Some count when count > 0 => fun () => Next value (repeat value (Some (count - 1)))
  | Some count when count < 0 => failwith "count must be greater or equal to 0"
  | Some _ => empty
  | None =>
      let rec repeatForever () => Next value repeatForever;
      repeatForever
};

let rec scan
    (reducer: 'acc => 'a => 'acc)
    (acc: 'acc)
    (seq: t 'a): (t 'acc) => fun () => switch (seq ()) {
  | Next value next => {
      let acc = reducer acc value;
      Next acc (scan reducer acc next)
    }
  | Completed => Completed
};

let buffer
    (count: int)
    (skip: int)
    (seq: t 'a): (t (list 'a)) =>
  if (count <= 0 || skip <= 0) (failwith "out of range")
  else seq
    |> scan (
      fun (lst, counted, skipped) next =>
        if (counted < count && skipped < skip) ([next, ...lst], counted + 1, skipped + 1)
        else if (skipped < skip) (lst, counted, skipped + 1)
        else if (counted < count) ([next, ...lst], counted + 1, skipped)
        else if (skip < count) ([next, ...(ImmList.take (count - skip) lst)], counted, skipped)
        else ([next], 1, 1)
      ) ([], 0, 0)
    |> filter (fun (_, counted, skipped) => counted == count && skipped == skip)
    |> map (fun (lst, _, _) => lst);

let distinctUntilChangedWith
    (equality: Equality.t 'a)
    (seq: t 'a): (t 'a) => seq
  |> scan
    (fun (_, accNext) next => (accNext, Some next))
    (None, None)
  |> map (fun v => switch v {
      | (Some prev, Some next) when (not @@ equality prev @@ next) => Some next
      | (None, Some next) => Some next
      | _ => None
    })
  |> filter Option.isNotEmpty
  |> map Option.first;

let distinctUntilChanged (seq: t 'a): (t 'a) =>
  seq |> distinctUntilChangedWith Equality.structural;

let skip (count: int) (seq: t 'a): (t 'a) => fun () => {
  let rec skipIter (count: int) (iter: iterator 'a): (iterator 'a) => switch iter {
    | Next _ next =>
        if (count > 0) { skipIter (count - 1) (next ()) }
        else iter
    | Completed => Completed
  };

  skipIter count (seq ())
};

let get (index: int) (seq: t 'a): 'a =>
  if (index < 0) (failwith "index < 0")
  else seq |> skip index |> first;

let skipWhile (f: 'a => bool) (seq: t 'a): (t 'a) => seq
  |> scan
    (fun acc next => switch acc {
      | Some _ => Some next
      | _ =>
        if (f next) None
        else Some next
    }) None
  |> filter Option.isNotEmpty
  |> map Option.first;

let rec some (f: 'a => bool) (seq: t 'a): bool => switch (seq ()) {
  | Next value next => (f value) || (some f next)
  | Completed => false
};

let startWith (value: 'a) (seq: t 'a): (t 'a) =>
  concat [return value, seq];

let rec takeWhile (f: 'a => bool) (seq: t 'a): (t 'a) => fun () => switch (seq ()) {
  | Next value next =>
      if (f value) (Next value (takeWhile f next))
      else Completed;
  | Completed => Completed
};

let take (count: int) (seq: t 'a): (t 'a) =>
  if (count > 0) (seq
    |> scan (fun (count, _) next =>
        if (count > 0) (count - 1, Some next)
        else (count, None)
      ) (count, None)
    |> takeWhile (snd >> Option.isNotEmpty)
    |> map (snd >> Option.first)
  )
  else if (count == 0) empty
  else failwith "count must be greater or equal to 0";

let rec tryFind (predicate: 'a => bool) (seq: t 'a): (option 'a) => switch (seq ()) {
  | Next value next =>
      if (predicate value) (Some value)
      else (tryFind predicate next)
  | Completed => None
};

let tryFirst (seq: t 'a): (option 'a) => switch (seq ()) {
  | Next value _ => Some value
  | Completed => None
};

let tryGet (index: int) (seq: t 'a): (option 'a) =>
  if (index < 0) None
  else seq |> skip index |> tryFirst;

let tryLast (seq: t 'a): (option 'a) => {
  let rec loop acc seq => switch (seq ()) {
    | Next v next => loop v next
    | Completed => Some acc
  };

  switch (seq ()) {
    | Next v next => loop v next
    | Completed => None
  }
};

let rec zip (seqs: list (t 'a)): (t (list 'a)) => fun () => {
  let iters = seqs |> ImmList.mapReverse Functions.call;

  let nextSeq: (t (list 'a)) = fun () =>
    (iters |> ImmList.mapReverse (fun next => switch next {
      | Next _ next => next
      | Completed => empty
    }) |> zip) ();

  iters |> ImmList.reduce (
    fun acc next  => switch (acc, next) {
      | (Some Completed      , _              ) => acc
      | (_                   , Completed      ) => Some Completed
      | (Some (Next values _), Next value _) =>
          Next [value, ...values] nextSeq |> Option.return
      | (None                , Next value _) =>
          Next [value] nextSeq |> Option.return
    }
  ) None |? Completed;
};

let rec zip2 (a: t 'a) (b: t 'b): (t ('a, 'b)) => fun () => switch (a (), b ()) {
  | (Next aValue aNext, Next bValue bNext) =>
      Next (aValue, bValue) (zip2 aNext bNext)
  | _ => Completed
};

let rec zip3 (a: t 'a) (b: t 'b) (c: t 'c): (t ('a, 'b, 'c)) => fun () => switch (a (), b (), c ()) {
  | (Next aValue aNext, Next bValue bNext, Next cValue cNext) =>
      Next (aValue, bValue, cValue) (zip3 aNext bNext cNext)
  | _ => Completed
};

let zipLongest (seqs: list (t 'a)): (t (list (option 'a))) => seqs
  |> ImmList.mapReverse (fun seq => concat [seq |> map Option.return, repeat None None ])
  |> ImmList.reverse
  |> zip
  |> takeWhile (ImmList.some Option.isNotEmpty);

let rec zipLongest2 (a: t 'a) (b: t 'b): (t (option 'a, option 'b)) => fun () => switch (a (), b ()) {
  | (Next aValue aNext, Next bValue bNext) =>
      Next (Some aValue, Some bValue) (zipLongest2 aNext bNext)
  | (Next aValue aNext, Completed) =>
      Next (Some aValue, None) (zipLongest2 aNext empty)
  | (Completed, Next bValue bNext) =>
      Next (None, Some bValue) (zipLongest2 empty bNext)
  | _ => Completed
};

let rec zipLongest3
    (a: t 'a)
    (b: t 'b)
    (c: t 'c): (t (option 'a, option 'b, option 'c)) => fun () => switch (a (), b (), c ()) {
  | (Next aValue aNext, Next bValue bNext, Next cValue cNext) =>
      Next (Some aValue, Some bValue, Some cValue) (zipLongest3 aNext bNext cNext)
  | (Next aValue aNext, Next bValue bNext, Completed) =>
      Next (Some aValue, Some bValue, None) (zipLongest3 aNext bNext empty)
  | (Next aValue aNext, Completed, Next cValue cNext) =>
      Next (Some aValue, None, Some cValue) (zipLongest3 aNext empty cNext)
  | (Completed, Next bValue bNext, Next cValue cNext) =>
      Next (None, Some bValue, Some cValue) (zipLongest3 empty bNext cNext)
  | _ => Completed
};
