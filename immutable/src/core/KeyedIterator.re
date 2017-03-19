/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'k 'v = {
  reduceWhile: 'acc . ('acc => 'k => 'v => bool) => ('acc => 'k => 'v => 'acc) => 'acc => 'acc,
};

let reduceWhile
    (predicate:'acc => 'k => 'v => bool)
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    (iter: t 'k 'v): 'acc =>
  iter.reduceWhile predicate f acc;

let reduce (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) (iter: t 'k 'v): 'acc =>
  iter |> reduceWhile Functions.alwaysTrue3 f acc;

let empty: t 'k 'v = {
  reduceWhile: fun _ _ acc => acc
};

let concat (iters: list (t 'k 'v)): (t 'k 'v) => switch iters {
  | [] => empty
  | _ => {
    reduceWhile: fun predicate f acc => {
      let shouldContinue = ref true;

      let predicate acc key value => {
        let result = predicate acc key value;
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

let doOnNext (sideEffect: 'k => 'v => unit) (iter: t 'k 'v): (t 'k 'v) =>
  if (iter === empty) empty
  else {
    reduceWhile: fun predicate f acc => iter |> reduceWhile
      predicate
      (fun acc k v => { sideEffect k v; (f acc k v) })
      acc
  };

let every (f: 'k => 'v => bool) (iter: t 'k 'v): bool =>
  if (iter === empty) true
  else iter |> reduceWhile
    (fun acc _ _ => acc)
    (fun _ => f)
    true;

let find (f: 'k => 'v => bool) (iter: t 'k 'v): (option ('k, 'v)) =>
  if (iter === empty) None
  else iter |> reduceWhile
    (fun acc _ _ => Option.isEmpty acc)
    (fun _ k v => if (f k v) (Some (k, v)) else None)
    None;

let findOrRaise (f: 'k => 'v => bool) (iter: t 'k 'v): 'a =>
  find f iter |> Option.firstOrRaise;

let filter (filter: 'k => 'v => bool) (iter: t 'k 'v): (t 'k 'v) =>
  if (iter === empty) empty
  else {
    reduceWhile: fun predicate f acc => iter |> reduceWhile
      predicate
      (fun acc k v => if (filter k v) (f acc k v) else acc)
      acc
  };

let flatMap (mapper: 'kA => 'vA => t 'kB 'vB) (iter: t 'kA 'vA): (t 'kB 'vB) =>
  if (iter === empty) empty
  else {
    reduceWhile: fun predicate f acc => {
      let shouldContinue = ref true;

      let predicate acc key value => {
        let result = predicate acc key value;
        shouldContinue := result;
        result;
      };

      iter |> reduceWhile
        (fun _ _ _ => !shouldContinue)
        (fun acc k v => (mapper k v) |> reduceWhile predicate f acc)
        acc
    }
  };

let forEach (f: 'k => 'v => unit) (iter: t 'k 'v) =>
  iter |> reduce (fun _ => f) ();

let forEachWhile (predicate: 'k => 'v => bool) (f: 'k => 'v => unit) (iter: t 'k 'v) =>
  iter |> reduceWhile (fun _ => predicate) (fun _ => f) ();

let isEmpty (iter: t 'k 'v): bool =>
  if (iter === empty) true
  else iter |> reduceWhile
    (fun acc  _ _ => acc)
    (fun _ _ _ => false)
    true;

let isNotEmpty (iter: t 'k 'v): bool =>
  if (iter === empty) false
  else iter |> reduceWhile
    (fun acc _ _ => not acc)
    (fun _ _ _ => true)
    false;

let keys (iter: t 'k 'v): (Iterator.t 'k) =>
  if (iter === empty) Iterator.empty
  else {
    reduceWhile: fun predicate f acc => iter |> reduceWhile
      (fun acc k v => predicate acc k)
      (fun acc k v => f acc k)
      acc
  };

let map (mapper: 'k => 'a => 'b) (iter: t 'k 'a): (t 'k 'b) =>
  if (iter === empty) empty
  else {
    reduceWhile: fun predicate f acc => {
      iter |> reduceWhile
        /* FIXME: Memoize the mapper result so that we don't compute it twice */
        (fun acc k v => predicate acc k (mapper k v))
        (fun acc k v => f acc k (mapper k v))
        acc
    }
  };

let none (f: 'k => 'v => bool) (iter: t 'k 'v): bool =>
  if (iter === empty) true
  else iter |> reduceWhile
    (fun acc _ _ => acc)
    (fun acc => f)
    true;

let some (f: 'k => 'v => bool) (iter: t 'k 'v): bool =>
  if (iter === empty) false
  else iter |> reduceWhile
    (fun acc _ _ => not acc)
    (fun acc => f)
    false;

let values (iter: t 'k 'v): (Iterator.t 'v) =>
  if (iter === empty ) Iterator.empty
  else {
    reduceWhile: fun predicate f acc => iter |> reduceWhile
      (fun acc k v => predicate acc v)
      (fun acc k v => f acc v)
      acc
  };

let toIterator (iter: t 'k 'v): (Iterator.t ('k, 'v)) =>
  if (iter === empty ) Iterator.empty
  else {
    reduceWhile: fun predicate f acc => iter |> reduceWhile
      (fun acc k v => predicate acc (k, v))
      (fun acc k v => f acc (k, v))
      acc
  };