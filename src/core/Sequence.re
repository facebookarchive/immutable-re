/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Functions.Operators;
open Option.Operators;

type iterator 'a =
  | Next 'a (t 'a)
  | Completed

and t 'a = unit => iterator 'a;

let emptySeq () => Completed;

let empty (): (t 'a) => emptySeq;

let return (value: 'a): (t 'a) => fun () =>
  Next value (empty ());

let rec ofList (list: list 'a): (t 'a) => fun () => switch list {
  | [value] => Next value (empty ())
  | [value, ...tail] => Next value (ofList tail)
  | [] => Completed
};

let flatten (seq: t (t 'a)): (t 'a) => {
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
  seqs |> ofList |> flatten;

let defer (f: unit => (t 'a)): (t 'a) => fun () => f () ();

let rec filter (f: 'a => bool) (seq: t 'a): (t 'a) => {
  let rec filterIter (f: 'a => bool) (iter: iterator 'a): iterator 'b => switch iter {
    | Next value next =>
        if (f value) (Next value (filter f next))
        else next () |> filterIter f
    | Completed => Completed
  };

  fun () => seq () |> filterIter f
};

let rec generate (f: 'acc => 'acc) (acc: 'acc): (t 'acc) => fun () =>
  Next acc (generate f (f acc));

let rec map (f: 'a => 'b) (seq: t 'a): (t 'b) => fun () => switch (seq ()) {
  | Next value next =>
      Next (f value) (map f next)
  | Completed => Completed
};

let doOnNext (f: 'a => unit) (seq: t 'a): (t 'a) =>
  seq |> map (fun next => { f next; next });

let flatMap (f: 'a => (t 'b)) (seq: t 'a): (t 'b) =>
  seq |> map f |> flatten;

let ofOption (opt: option 'a): (t 'a) => switch opt {
  | Some value => return value
  | None => (empty ())
};

let rec reduceImpl
    while_::(predicate: 'acc => 'a => bool)
    (reducer: 'acc => 'a => 'acc)
    (acc: 'acc)
    (seq: t 'a): 'acc => switch (seq ()) {
  | Next value next when predicate acc value =>
      let acc = reducer acc value;
      reduceImpl while_::predicate reducer acc next
  | _ => acc
};

let rec reduce
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (reducer: 'acc => 'a => 'acc)
    (acc: 'acc)
    (seq: t 'a): 'acc => reduceImpl while_::predicate reducer acc seq;

let scan
    (reducer: 'acc => 'a => 'acc)
    (acc: 'acc)
    (seq: t 'a): (t 'acc) => {
  let rec recurse reducer acc seq => fun () => switch (seq ()) {
    | Next value next =>
        let acc = reducer acc value;
        Next acc (recurse reducer acc next)
    | Completed => Completed
  };

  fun () => Next acc (recurse reducer acc seq);
};

let sequenceIterator: Iterable.Iterator.t 'acc (t 'a) = {
  reduce: reduceImpl
};

let toIterable (seq: t 'a): (Iterable.t 'a) =>
  Iterable.Iterable seq sequenceIterator;

let buffer
    count::(count: int)
    skip::(skip: int)
    (seq: t 'a): (t (list 'a)) => {
  if (count <= 0 || skip <= 0) (failwith "out of range");

  let rec recurse (lst: list 'a) (counted: int) (skipped: int) (seq: t 'a) => fun () => switch (seq ()) {
    | Next value next =>
        let nextSequence =
          if (counted < count && skipped < skip) (recurse [value, ...lst] (counted + 1) (skipped + 1) next)
          else if (skipped < skip) (recurse lst counted (skipped + 1) next)
          else if (counted < count) (recurse [value, ...lst] (counted + 1) skipped next)
          else if (skip < count) (recurse [value, ...(ImmList.take (count - skip) lst)] counted skipped next)
          else (recurse [value] 1 1 next);

        if (counted === count && skipped === skip) (Next lst nextSequence)
        else (nextSequence ())
    | Completed =>
        if (counted === count && skipped === skip) (Next lst (empty ()))
        else Completed
  };

  recurse [] 0 0 seq;
};

let distinctUntilChangedWith
    (equality: Equality.t 'a)
    (seq: t 'a): (t 'a) => fun () => {
  let rec iter
      (equality: Equality.t 'a)
      (prevValue: 'a)
      (next: t 'a): (t 'a) => fun () => switch (next ()) {
    | Next value next =>
        if (equality prevValue value) (iter equality prevValue next ())
        else Next value (iter equality value next)
    | Completed => Completed
  };

  switch (seq ()) {
    | Next value next => Next value (iter equality value next)
    | Completed => Completed
  }
};

let rec seek (count: int) (seq: t 'a): (t 'a) => {
  Preconditions.failIf "count must be >= 0" (count < 0);

  if (count === 0) seq
  else switch (seq ()) {
    | Next _ next => seek (count - 1) next
    | Completed => seq
  };
};

let rec seekWhile (f: 'a => bool) (seq: t 'a): (t 'a) => switch (seq ()) {
  | Next value next =>
      if (f value) (seekWhile f next)
      else seq
  | Completed => seq
};

let skip (count: int) (seq: t 'a): (t 'a) => {
  Preconditions.failIf "count must be >= 0" (count < 0);

  fun () => {
    let rec skipIter (count: int) (iter: iterator 'a): (iterator 'a) => switch iter {
      | Next _ next =>
          if (count > 0) { skipIter (count - 1) (next ()) }
          else iter
      | Completed => Completed
    };

    skipIter count (seq ())
  };
};

let skipWhile (f: 'a => bool) (seq: t 'a): (t 'a) => fun () => {
  let rec skipIter f (iter: iterator 'a): (iterator 'a) => switch iter {
    | Next value next =>
        if (f value) { skipIter f (next ()) }
        else iter
    | Completed => Completed
  };

  skipIter f (seq ())
};

let startWith (value: 'a) (seq: t 'a): (t 'a) =>
  concat [return value, seq];

let rec takeWhile (f: 'a => bool) (seq: t 'a): (t 'a) => fun () => switch (seq ()) {
  | Next value next =>
      if (f value) (Next value (takeWhile f next))
      else Completed;
  | Completed => Completed
};

let rec take (count: int) (seq: t 'a): (t 'a) => {
  Preconditions.failIf "count must be >= 0" (count < 0);

  fun () => if (count > 0) (switch (seq ()) {
    | Next value next =>
        Next value (take (count - 1) next)
    | Completed => Completed
  })
  else Completed;
};

let rec mapReverseImpl (f: 'a => 'b) (src: list 'a) (dst: list 'b): (list 'b) => switch src {
  | [head, ...tail] => mapReverseImpl f tail [f head, ...dst]
  | [] => dst
};

let mapReverse (f: 'a => 'b) (list: list 'a): (list 'b) =>
  mapReverseImpl f list [];

let rec zip (seqs: list (t 'a)): (t (list 'a)) => fun () => {
  let iters = seqs |> mapReverse Functions.call;

  let nextSequence: (t (list 'a)) = fun () =>
    (iters |> mapReverse (fun next => switch next {
      | Next _ next => next
      | Completed => (empty ())
    }) |> zip) ();

  iters |> ImmList.reduce (
    fun acc next  => switch (acc, next) {
      | (Some Completed      , _              ) => acc
      | (_                   , Completed      ) => Some Completed
      | (Some (Next values _), Next value _) =>
          Next [value, ...values] nextSequence |> Option.return
      | (None                , Next value _) =>
          Next [value] nextSequence |> Option.return
    }
  ) None |? Completed;
};

let rec zip2With
    (f: 'a => 'b => 'c)
    (a: t 'a)
    (b: t 'b): (t 'c) => fun () => switch (a (), b ()) {
  | (Next aValue aNext, Next bValue bNext) =>
      Next (f aValue bValue) (zip2With f aNext bNext)
  | _ => Completed
};

let rec zip3With
    (f: 'a => 'b => 'c => 'd)
    (a: t 'a)
    (b: t 'b)
    (c: t 'c): (t 'd) => fun () => switch (a (), b (), c ()) {
  | (Next aValue aNext, Next bValue bNext, Next cValue cNext) =>
      Next (f aValue bValue cValue) (zip3With f aNext bNext cNext)
  | _ => Completed
};

let rec reverseImpl (src: list 'a) (dst: list 'a): (list 'a) => switch src {
  | [head, ...tail] => reverseImpl tail [head, ...dst]
  | [] => dst
};

let reverse (list: list 'a): (list 'a) =>
  reverseImpl list [];

let zipLongest (seqs: list (t 'a)): (t (list (option 'a))) => seqs
  |> mapReverse (fun seq => concat [seq |> map Option.return, generate Functions.identity None])
  |> reverse
  |> zip
  |> takeWhile (ImmList.some Option.isNotEmpty);

let rec zipLongest2With
    (f: option 'a => option 'b => 'c)
    (a: t 'a) (b: t 'b): (t 'c) => fun () => switch (a (), b ()) {
  | (Next aValue aNext, Next bValue bNext) =>
      Next (f (Some aValue) (Some bValue)) (zipLongest2With f aNext bNext)
  | (Next aValue aNext, Completed) =>
      Next (f (Some aValue) None) (zipLongest2With f aNext (empty ()))
  | (Completed, Next bValue bNext) =>
      Next (f None (Some bValue)) (zipLongest2With f (empty ()) bNext)
  | _ => Completed
};

let rec zipLongest3With
    (f: option 'a => option 'b => option 'c => 'd)
    (a: t 'a)
    (b: t 'b)
    (c: t 'c): (t 'd) => fun () => switch (a (), b (), c ()) {
  | (Next aValue aNext, Next bValue bNext, Next cValue cNext) =>
      Next (f (Some aValue) (Some bValue) (Some cValue)) (zipLongest3With f aNext bNext cNext)

  | (Next aValue aNext, Next bValue bNext, Completed) =>
      Next (f (Some aValue) (Some bValue) None) (zipLongest3With f aNext bNext (empty ()))
  | (Next aValue aNext, Completed, Next cValue cNext) =>
      Next (f (Some aValue) None (Some cValue)) (zipLongest3With f aNext (empty ()) cNext)
  | (Completed, Next bValue bNext, Next cValue cNext) =>
      Next (f None (Some bValue) (Some cValue)) (zipLongest3With f (empty ()) bNext cNext)

  | (Completed, Next bValue bNext, Completed) =>
      Next (f None (Some bValue) None) (zipLongest3With f (empty ()) bNext (empty ()))
  | (Next aValue aNext, Completed, Completed) =>
      Next (f (Some aValue) None None) (zipLongest3With f aNext (empty ()) (empty ()))
  | (Completed, Completed, Next cValue cNext) =>
      Next (f None None (Some cValue)) (zipLongest3With f (empty ()) (empty ()) cNext)
  | _ => Completed
};
