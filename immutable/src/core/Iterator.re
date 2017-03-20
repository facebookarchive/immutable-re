/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'a = {
  reduceWhile: 'acc . ('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => 'acc,
};

let reduceWhile
    (predicate: 'acc => 'a => bool)
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (iter: t 'a): 'acc =>
  iter.reduceWhile predicate f acc;

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) (iter: t 'a): 'acc =>
  iter.reduceWhile Functions.alwaysTrue2 f acc;

let empty: t 'a = {
  reduceWhile: fun _ _ acc => acc
};

let concat (iters: list (t 'a)): (t 'a) => switch iters {
  | [] => empty
  | _ => {
    reduceWhile: fun predicate f acc => {
      let shouldContinue = ref true;

      let predicate acc next => {
        let result = predicate acc next;
        shouldContinue := result;
        result;
      };

      iters |> ImmList.reduceWhile
        (fun _ _ => !shouldContinue)
        (fun acc next => next |> reduceWhile predicate f acc)
        acc
    }
  }
};

let doOnNext (sideEffect: 'a => unit) (iter: t 'a): (t 'a) =>
  if (iter === empty) empty
  else {
    reduceWhile: fun predicate f acc => iter |> reduceWhile
      predicate
      (fun acc next => { sideEffect next; (f acc next) })
      acc
  };

let every (f: 'a => bool) (iter: t 'a): bool =>
  if (iter === empty) true
  else iter |> reduceWhile
    (fun acc _ => acc)
    (fun _ => f)
    true;

let find (f: 'a => bool) (iter: t 'a): (option 'a) =>
  if (iter === empty) None
  else iter |> reduceWhile
    (fun acc _ => Option.isEmpty acc)
    (fun _ next => if (f next) (Some next) else None)
    None;

let findOrRaise (f: 'a => bool) (iter: t 'a): 'a =>
  find f iter |> Option.firstOrRaise;

let filter (filter: 'a => bool) (iter: t 'a): (t 'a) =>
  if (iter === empty) empty
  else {
    reduceWhile: fun predicate f acc => iter |> reduceWhile
      predicate
      (fun acc next => if (filter next) (f acc next) else acc)
      acc
  };

let flatMap (mapper: 'a => t 'b) (iter: t 'a): (t 'b) =>
  if (iter === empty) empty
  else {
    reduceWhile: fun predicate f acc => {
      let shouldContinue = ref true;

      let predicate acc next => {
        let result = predicate acc next;
        shouldContinue := result;
        result;
      };

      iter |> reduceWhile
        (fun _ _ => !shouldContinue)
        (fun acc next => next |> mapper |> reduceWhile predicate f acc)
        acc
    }
  };

let flatten (iters: t (t 'a)): (t 'a) =>
  if (iters === empty) empty
  else {
    reduceWhile: fun predicate f acc => {
      let shouldContinue = ref true;

      let predicate acc next => {
        let result = predicate acc next;
        shouldContinue := result;
        result;
      };

      iters |> reduceWhile
        (fun _ _ => !shouldContinue)
        (fun acc next => next |> reduceWhile predicate f acc)
        acc
    }
  };

let forEach (f: 'a => unit) (iter: t 'a) =>
  iter |> reduce (fun _ => f) ();

let forEachWhile (predicate: 'a => bool) (f: 'a => unit) (iter: t 'a) =>
  iter |> reduceWhile (fun _ => predicate) (fun _ => f) ();

let listAddFirstAll (iter: t 'a) (list: list 'a): (list 'a) =>
  iter |> reduce (fun acc next => acc |> ImmList.addFirst next) list;

let listFromReverse (iter: t 'a): (list 'a) =>
  [] |> listAddFirstAll iter;

let map (mapper: 'a => 'b) (iter: t 'a): (t 'b) =>
  if (iter === empty) empty
  else {
    reduceWhile: fun predicate f acc => {
      iter |> reduceWhile
        /* FIXME: Memoize the mapper result so that we don't compute it twice */
        (fun acc next => predicate acc (mapper next))
        (fun acc next => f acc (mapper next))
        acc
    }
  };

let none (f: 'a => bool) (iter: t 'a): bool =>
  if (iter === empty) true
  else iter |> reduceWhile
    (fun acc _ => acc)
    (fun _ => f)
    true;

let ofList (list: list 'a): (t 'a) =>
  if (ImmList.isEmpty list) empty
  else {
    reduceWhile: fun predicate f acc => ImmList.reduceWhile predicate f acc list
  };

let ofOption (opt: Option.t 'a): (t 'a) =>
  if (Option.isEmpty opt) empty
  else {
    reduceWhile: fun predicate f acc => Option.reduceWhile predicate f acc opt
  };

let return (value: 'a): (t 'a) => {
  reduceWhile: fun predicate f acc =>
    if (predicate acc value) (f acc value)
    else acc
};

let some (f: 'a => bool) (iter: t 'a): bool =>
  if (iter === empty) false
  else iter |> reduceWhile
    (fun acc _ => not acc)
    (fun _ => f)
    false;
