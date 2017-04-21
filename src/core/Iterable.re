/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Functions.Operators;

type s 'iterable 'a = {
  reduce: 'acc . while_::('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => 'iterable => 'acc
};

type t 'a =
  | Empty
  | Instance 'iterable (s 'iterable 'a): t 'a;

let create (impl: s 'iterable 'a) (instance: 'iterable): (t 'a) =>
  Instance instance impl;

let empty (): t 'a => Empty;

let reduce
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (iter: t 'a): 'acc => switch iter {
  | Empty => acc
  | Instance iter { reduce } =>
      iter |> reduce while_::predicate f acc;
};

let concatImpl: s 'iterable 'a  = {
  reduce: fun while_::predicate f acc iters => {
    let shouldContinue = ref true;

    let predicate acc next => {
      let result = predicate acc next;
      shouldContinue := result;
      result;
    };

    iters |> ImmList.reduce
      while_::(fun _ _ => !shouldContinue)
      (fun acc next => next |> reduce while_::predicate f acc)
      acc
  }
};

let concat (iters: list (t 'a)): (t 'a) => switch iters {
  | [] => empty ()
  | _ => create concatImpl iters
};

let increment acc _ => acc + 1;
let count (iterable: t 'a): int =>
  iterable |> reduce increment 0;

let deferImpl: s 'iterable 'a = {
  reduce: fun while_::predicate f acc provider =>
    (provider ()) |> reduce while_::predicate f acc
};

let defer (provider: unit => (t 'a)): (t 'a) =>
  create deferImpl provider;

let distinctUntilChangedWith
    (equals: Equality.t 'a)
    (iterable: t 'a): (t 'a) => switch iterable {
  | Empty => Empty
  | Instance iter { reduce } => Instance iter {
      reduce: fun while_::predicate f acc iter => {
        let previous = MutableOption.empty ();

        let shouldNotSkip next =>
          MutableOption.isEmpty previous ||
          previous |> MutableOption.firstOrRaise |> equals next |> not;

        let predicate acc next =>
          if (shouldNotSkip next) (predicate acc next)
          else true;

        let f acc next =>
          if (shouldNotSkip next) {
            previous |> MutableOption.set next;
            f acc next
          }
          else acc;

        iter |> reduce while_::predicate f acc
      }
    }
};

let doOnNext (sideEffect: 'a => unit) (iterable: t 'a): (t 'a) => switch iterable {
  | Empty => Empty
  | Instance iter { reduce } => Instance iter {
      reduce: fun while_::predicate f acc iter => iter |> reduce
        while_::predicate (fun acc next => {
          sideEffect next; (f acc next)
        }) acc
    }
};

let every (f: 'a => bool) (iterable: t 'a): bool =>
  iterable |> reduce while_::(fun acc _ => acc) (fun _ => f) true;

let filter (filter: 'a => bool) (iterable: t 'a): (t 'a) => switch iterable {
  | Empty => Empty
  | Instance iter { reduce } => Instance iter {
      reduce: fun while_::predicate f acc iter => {
        let predicate acc next =>
          if (filter next) (predicate acc next)
          else true;

        iter |> reduce
          while_::predicate (fun acc next =>
            if (filter next) (f acc next) else acc
          ) acc;
      }
    }
};

let find (f: 'a => bool) (iterable: t 'a): (option 'a) =>
  iterable |> reduce while_::(fun acc _ => Option.isEmpty acc) (
    fun _ next => if (f next) (Some next) else None
  ) None;

let findOrRaise (f: 'a => bool) (iterable: t 'a): 'a =>
  find f iterable |> Option.firstOrRaise;

let first (iterable: t 'a): (option 'a) =>
  iterable |> reduce
    while_::(fun acc _ => Option.isEmpty acc)
    (fun _ => Option.return)
    None;

let firstOrRaise (iterable: t 'a): 'a =>
  iterable |> first |> Option.firstOrRaise;

let flattenImpl: s 'iterable 'a = {
  reduce: fun while_::predicate f acc iters => {
    let shouldContinue = ref true;

    let predicate acc next => {
      let result = predicate acc next;
      shouldContinue := result;
      result;
    };

    iters |> reduce while_::(fun _ _ => !shouldContinue) (fun acc next =>
      next |> reduce while_::predicate f acc
    ) acc
  }
};

let flatten (iters: t (t 'a)): (t 'a) => switch iters {
  | Empty => Empty
  | Instance _ _ => create flattenImpl iters
};

let forEach while_::(predicate: 'a => bool)=Functions.alwaysTrue (f: 'a => unit) (iterable: t 'a) =>
  if (predicate === Functions.alwaysTrue ) {
    iterable |> reduce (fun _ => f) ();
  }
  else iterable |> reduce while_::(fun _ => predicate) (fun _ => f) ();

let generateImpl: s ('acc => 'acc, 'acc) 'acc  = {
  reduce: fun while_::predicate f acc (gen, initialValue) => {
    let rec recurse gen value predicate f acc => {
      let nextValue = gen value;

      if (predicate acc nextValue) {
        let acc = f acc nextValue;
        recurse gen nextValue predicate f acc;
      }
      else acc;
    };

    if (predicate acc initialValue) {
      let acc = f acc initialValue;
      recurse gen initialValue predicate f acc;
    } else acc;
  }
};

let generate (gen: 'acc => 'acc) (initialValue: 'acc): (t 'acc) =>
  create generateImpl (gen, initialValue);

let listAddFirstAll (iter: t 'a) (list: list 'a): (list 'a) =>
  iter |> reduce (fun acc next => acc |> ImmList.addFirst next) list;

let listFromReverse (iter: t 'a): (list 'a) =>
  [] |> listAddFirstAll iter;

let map
    (mapper: 'a => 'b)
    (iter: t 'a): (t 'b) => switch iter {
  | Empty => Empty
  | Instance iter { reduce } => Instance iter {
      reduce: fun while_::predicate f acc iter => {
        let memoize = MutableOption.empty ();

        let predicate acc next => {
          let next = mapper next;
          memoize |> MutableOption.set next;
          predicate acc next
        };

        let f acc _ => f acc (MutableOption.firstOrRaise memoize);

        iter |> reduce while_::predicate f acc
      }
    }
};

let flatMap (mapper: 'a => t 'b) (iter: t 'a): (t 'b) =>
  iter |> map mapper |> flatten;

let none (f: 'a => bool) (iterable: t 'a): bool =>
  iterable |> reduce while_::(fun acc _ => acc) (fun _ => f >> not) true;

let listImpl: s (list 'a) 'a = { reduce: ImmList.reduceImpl };

let ofList (list: list 'a): (t 'a) => switch list {
  | [] => Empty
  | _ => create listImpl list
};

let returnImpl: s 'a 'a = {
  reduce: fun while_::predicate f acc value =>
    if (predicate acc value) (f acc value)
    else acc
};

let return (value: 'a): (t 'a) => create returnImpl value;

let scan
    (reducer: 'acc => 'a => 'acc)
    (initialValue: 'acc)
    (iter: t 'a): (t 'acc) => switch iter {
  | Empty => Empty
  | Instance iter { reduce } => Instance iter {
      reduce: fun while_::predicate f acc iter =>
        if (predicate acc initialValue) {
          let result = ref (f acc initialValue);
          let memoized = ref initialValue;

          let predicate acc next => {
            let nextValue = reducer acc next;
            memoized := nextValue;
            predicate !result nextValue;
          };

          let f _ _ => {
            let acc = !memoized;
            result := f !result acc;
            acc
          };

          iter |> reduce while_::predicate f initialValue |> ignore;

          !result
      } else acc
    }
};

let skip (count: int) (iter: t 'a): (t 'a) => if (count === 0) iter else switch iter {
  | Empty => Empty
  | Instance iter { reduce } => Instance iter {
      reduce: fun while_::predicate f acc iter => {
        let count = ref count;

        let predicate acc next => {
          if (!count > 0) true
          else (predicate acc next);
        };

        let f acc next => {
          count := !count - 1;

          if (!count >= 0) acc
          else f acc next;
        };

        iter |> reduce while_::predicate f acc;
      }
    }
};

let skipWhile (keepSkipping: 'a => bool) (iter: t 'a): (t 'a) => switch iter {
  | Empty => Empty
  | Instance iter { reduce } => Instance iter {
      reduce: fun while_::predicate f acc iter => {
        let doneSkipping = ref false;

        let predicate acc next =>
          if (!doneSkipping) (predicate acc next)
          else true;

        let f acc next =>
          if (!doneSkipping) (f acc next)
          else if (keepSkipping next) acc
          else {
            doneSkipping := true;
            f acc next;
          };

        iter |> reduce while_::predicate f acc
      }
    }
};

let some (f: 'a => bool) (iterable: t 'a): bool =>
  iterable |> reduce while_::(fun acc _ => not acc) (fun _ => f) false;

let startWith (value: 'a) (iter: t 'a): (t 'a) =>
  concat [return value, iter];

let take (count: int) (iter: t 'a): (t 'a) => if (count === 0) Empty else switch iter {
  | Empty => Empty
  | Instance iter { reduce } => Instance iter {
      reduce: fun while_::predicate f acc iter => {
        let count = ref count;

        let predicate acc next => {
          if (!count > 0) (predicate acc next)
          else false;
        };

        let f acc next => {
          count := !count - 1;
          f acc next;
        };

        iter |> reduce while_::predicate f acc;
      }
    }
};

let takeWhile (keepTaking: 'a => bool) (iter: t 'a): (t 'a) => switch iter {
  | Empty => Empty
  | Instance iter { reduce } => Instance iter {
      reduce: fun while_::predicate f acc iter => {
        let predicate acc next =>
          if (keepTaking next) (predicate acc next)
          else false;

        iter |> reduce while_::predicate f acc
      }
    }
};

let toIterable (iter: t 'a): (t 'a) => iter;

let buffer
    count::(count: int)
    skip::(skip: int)
    (iter: t 'a): (t (list 'a)) =>
  if (count <= 0 || skip <= 0) (failwith "out of range")
  else iter |> scan (
      fun (lst, counted, skipped) next =>
        if (counted < count && skipped < skip) ([next, ...lst], counted + 1, skipped + 1)
        else if (skipped < skip) (lst, counted, skipped + 1)
        else if (counted < count) ([next, ...lst], counted + 1, skipped)
        else if (skip < count) ([next, ...(ImmList.take (count - skip) lst)], counted, skipped)
        else ([next], 1, 1)
      ) ([], 0, 0)
    |> filter (fun (_, counted, skipped) => counted === count && skipped === skip)
    |> map (fun (lst, _, _) => lst);

type iterable 'a = t 'a;

module type S = {
  type a;
  type t;

  let reduce: while_::('acc => a => bool)? => ('acc => a => 'acc) => 'acc => t => 'acc;
  let toIterable: t => (iterable a);
};

module type S1 = {
  type t 'a;

  let reduce: while_::('acc => 'a => bool)? => ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let toIterable: t 'a => (iterable 'a);
};

let module Make = fun (Base: {
  type a;
  type t;

  let isEmpty: t => bool;
  let reduce: while_::('acc => a => bool) => ('acc => a => 'acc) => 'acc => t => 'acc;
}) => ({
  include Base;

  let reduce
      while_::(predicate: 'acc => a => bool)=Functions.alwaysTrue2
      (f: 'acc => a => 'acc)
      (acc: 'acc)
      (iterable: t): 'acc =>
    reduce while_::predicate f acc iterable;

  let iterableBase: s t a = { reduce: Base.reduce };

  let toIterable (iterable: t): (iterable 'a) =>
    if (isEmpty iterable) (empty ())
    else create iterableBase iterable;
}: S with type t := Base.t and type a := Base.a);

let module Make1 = fun (Base: {
  type t 'a;

  let isEmpty: (t 'a) => bool;
  let reduce: while_::('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => t 'a => 'acc;
}) => ({
  include Base;

  let reduce
      while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
      (f: 'acc => 'a => 'acc)
      (acc: 'acc)
      (iterable: t 'a): 'acc =>
    reduce while_::predicate f acc iterable;

  let iterableBase: s (t 'a) 'a = { reduce: Base.reduce };

  let toIterable (iterable: t 'a): (iterable 'a) =>
    if (isEmpty iterable) (empty ())
    else create iterableBase iterable;
}: S1 with type t 'a := Base.t 'a);
