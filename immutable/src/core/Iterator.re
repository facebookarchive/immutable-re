/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'a = {
  reduce: 'acc . ('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => 'acc,
};

let reduce
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (iter: t 'a): 'acc =>
  iter.reduce predicate f acc;

let empty: t 'a = {
  reduce: fun _ _ acc => acc
};

let concat (iters: list (t 'a)): (t 'a) => switch iters {
  | [] => empty
  | _ => {
    reduce: fun predicate f acc => {
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
  }
};

let doOnNext (sideEffect: 'a => unit) (iter: t 'a): (t 'a) =>
  if (iter === empty) empty
  else {
    reduce: fun predicate f acc => iter |> reduce
      while_::predicate (fun acc next => {
        sideEffect next; (f acc next)
      }) acc
  };

let every (f: 'a => bool) (iter: t 'a): bool =>
  if (iter === empty) true
  else iter |> reduce
    while_::(fun acc _ => acc) (fun _ =>
      f
    ) true;

let find (f: 'a => bool) (iter: t 'a): (option 'a) =>
  if (iter === empty) None
  else iter |> reduce while_::(fun acc _ => Option.isEmpty acc) (
    fun _ next => if (f next) (Some next) else None
  ) None;

let findOrRaise (f: 'a => bool) (iter: t 'a): 'a =>
  find f iter |> Option.firstOrRaise;

let filter (filter: 'a => bool) (iter: t 'a): (t 'a) =>
  if (iter === empty) empty
  else {
    reduce: fun predicate f acc => iter |> reduce
      while_::predicate (fun acc next =>
        if (filter next) (f acc next) else acc
      ) acc
  };

let flatMap (mapper: 'a => t 'b) (iter: t 'a): (t 'b) =>
  if (iter === empty) empty
  else {
    reduce: fun predicate f acc => {
      let shouldContinue = ref true;

      let predicate acc next => {
        let result = predicate acc next;
        shouldContinue := result;
        result;
      };

      iter |> reduce while_::(fun _ _ => !shouldContinue) (
        fun acc next => next |> mapper |> reduce while_::predicate f acc
      ) acc
    }
  };

let flatten (iters: t (t 'a)): (t 'a) =>
  if (iters === empty) empty
  else {
    reduce: fun predicate f acc => {
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

let forEach while_::(predicate: 'a => bool)=Functions.alwaysTrue (f: 'a => unit) (iter: t 'a) =>
  iter |> reduce while_::(fun _ => predicate) (fun _ => f) ();

let listAddFirstAll (iter: t 'a) (list: list 'a): (list 'a) =>
  iter |> reduce (fun acc next => acc |> ImmList.addFirst next) list;

let listFromReverse (iter: t 'a): (list 'a) =>
  [] |> listAddFirstAll iter;

let map (mapper: 'a => 'b) (iter: t 'a): (t 'b) =>
  if (iter === empty) empty
  else {
    reduce: fun predicate f acc => iter |> reduce
      /* FIXME: Memoize the mapper result so that we don't compute it twice */
      while_::(fun acc next => predicate acc (mapper next)) (fun acc next =>
        f acc (mapper next)
      ) acc
  };

let none (f: 'a => bool) (iter: t 'a): bool =>
  if (iter === empty) true
  else iter |> reduce
    while_::(fun acc _ => acc)
    (fun _ => f)
    true;

let ofList (list: list 'a): (t 'a) =>
  if (ImmList.isEmpty list) empty
  else {
    reduce: fun predicate f acc =>
      ImmList.reduce while_::predicate f acc list
  };

let ofOption (opt: Option.t 'a): (t 'a) =>
  if (Option.isEmpty opt) empty
  else {
    reduce: fun predicate f acc =>
      Option.reduce while_::predicate f acc opt
  };

let return (value: 'a): (t 'a) => {
  reduce: fun predicate f acc =>
    if (predicate acc value) (f acc value)
    else acc
};

let skip (count: int) (iter: t 'a): (t 'a) =>
  if (iter === empty) empty
  else {
    reduce: fun predicate f acc => {
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
  };

let skipWhile (keepSkipping: 'a => bool) (iter: t 'a): (t 'a) =>
  if (iter === empty) empty
  else {
    reduce: fun predicate f acc => {
      let doneSkipping = ref false;

      let f acc next =>
        if (!doneSkipping) (f acc next)
        else if (keepSkipping next) acc
        else {
          doneSkipping := true;
          f acc next;
        };

      iter |> reduce while_::predicate f acc
    }
  };

let some (f: 'a => bool) (iter: t 'a): bool =>
  if (iter === empty) false
  else iter |> reduce
    while_::(fun acc _ => not acc)
    (fun _ => f)
    false;

let take (count: int) (iter: t 'a): (t 'a) =>
  if (iter === empty) empty
  else {
    reduce: fun predicate f acc => {
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
  };

let takeWhile (keepTaking: 'a => bool) (iter: t 'a): (t 'a) =>
  if (iter === empty) empty
  else {
    reduce: fun predicate f acc => {
      let predicate acc next =>
        if (keepTaking next) (predicate acc next)
        else false;

      iter |> reduce while_::predicate f acc
    }
  };
