/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'k 'v = {
  reduce: 'acc . ('acc => 'k => 'v => bool) => ('acc => 'k => 'v => 'acc) => 'acc => 'acc,
};

let reduce
    while_::(predicate:'acc => 'k => 'v => bool)=Functions.alwaysTrue3
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    (iter: t 'k 'v): 'acc =>
  iter.reduce predicate f acc;

let empty: t 'k 'v = {
  reduce: fun _ _ acc => acc
};

let concat (iters: list (t 'k 'v)): (t 'k 'v) => switch iters {
  | [] => empty
  | _ => {
    reduce: fun predicate f acc => {
      let shouldContinue = ref true;

      let predicate acc key value => {
        let result = predicate acc key value;
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

let doOnNext (sideEffect: 'k => 'v => unit) (iter: t 'k 'v): (t 'k 'v) =>
  if (iter === empty) empty
  else {
    reduce: fun predicate f acc => iter |> reduce
      while_::predicate
      (fun acc k v => { sideEffect k v; (f acc k v) })
      acc
  };

let filter (filter: 'k => 'v => bool) (iter: t 'k 'v): (t 'k 'v) =>
  if (iter === empty) empty
  else {
    reduce: fun predicate f acc => {
      let predicate acc key value =>
        if (filter key value) (predicate acc key value)
        else true;

      iter |> reduce
        while_::predicate
        (fun acc k v => if (filter k v) (f acc k v) else acc)
        acc;
    }
  };

let flatMap (mapper: 'kA => 'vA => t 'kB 'vB) (iter: t 'kA 'vA): (t 'kB 'vB) =>
  if (iter === empty) empty
  else {
    reduce: fun predicate f acc => {
      let shouldContinue = ref true;

      let predicate acc key value => {
        let result = predicate acc key value;
        shouldContinue := result;
        result;
      };

      iter |> reduce while_::(fun _ _ _ => !shouldContinue) (
        fun acc k v => (mapper k v) |> reduce while_::predicate f acc
      ) acc
    }
  };

let keys (iter: t 'k 'v): (Iterator.t 'k) =>
  if (iter === empty) Iterator.empty
  else {
    reduce: fun predicate f acc => iter |> reduce
      while_::(fun acc k _ => predicate acc k)
      (fun acc k _ => f acc k)
      acc
  };

let map (mapper: 'k => 'a => 'b) (iter: t 'k 'a): (t 'k 'b) =>
  if (iter === empty) empty
  else {
    reduce: fun predicate f acc => {
      iter |> reduce
        /* FIXME: Memoize the mapper result so that we don't compute it twice */
        while_::(fun acc k v => predicate acc k (mapper k v))
        (fun acc k v => f acc k (mapper k v))
        acc
    }
  };

let return (key: 'k) (value: 'v): (t 'k 'v) => {
  reduce: fun predicate f acc =>
    if (predicate acc key value) (f acc key value)
    else acc
};

let skip (count: int) (iter: t 'k 'v): (t 'k 'v) =>
  if (iter === empty) empty
  else {
    reduce: fun predicate f acc => {
      let count = ref count;

      let predicate acc key value => {
        if (!count > 0) true
        else (predicate acc key value);
      };

      let f acc key value => {
        count := !count - 1;

        if (!count >= 0) acc
        else f acc key value;
      };

      iter |> reduce while_::predicate f acc;
    }
  };

let skipWhile (keepSkipping: 'k => 'v => bool) (iter: t 'k 'v): (t 'k 'v) =>
  if (iter === empty) empty
  else {
    reduce: fun predicate f acc => {
      let doneSkipping = ref false;

      let predicate acc key value =>
        if (!doneSkipping) (predicate acc key value)
        else true;

      let f acc key value =>
        if (!doneSkipping) (f acc key value)
        else if (keepSkipping key value) acc
        else {
          doneSkipping := true;
          f acc key value;
        };

      iter |> reduce while_::predicate f acc
    }
  };

let take (count: int) (iter: t 'k 'v): (t 'k 'v) =>
  if (iter === empty) empty
  else {
    reduce: fun predicate f acc => {
      let count = ref count;

      let predicate acc key value => {
        if (!count > 0) (predicate acc key value)
        else false;
      };

      let f acc key value => {
        count := !count - 1;
        f acc key value;
      };

      iter |> reduce while_::predicate f acc;
    }
  };

let takeWhile (keepTaking: 'k => 'v => bool) (iter: t 'k 'v): (t 'k 'v) =>
  if (iter === empty) empty
  else {
    reduce: fun predicate f acc => {
      let predicate acc key value =>
        if (keepTaking key value) (predicate acc key value)
        else false;

      iter |> reduce while_::predicate f acc
    }
  };

let values (iter: t 'k 'v): (Iterator.t 'v) =>
  if (iter === empty ) Iterator.empty
  else {
    reduce: fun predicate f acc => iter |> reduce
      while_::(fun acc _ v => predicate acc v)
      (fun acc _ v => f acc v)
      acc
  };

let toIterator (iter: t 'k 'v): (Iterator.t ('k, 'v)) =>
  if (iter === empty ) Iterator.empty
  else {
    reduce: fun predicate f acc => iter |> reduce
      while_::(fun acc k v => predicate acc (k, v))
      (fun acc k v => f acc (k, v))
      acc
  };

let module KeyedReducer = KeyedReducer.Make2 {
  type nonrec t 'k 'v = t 'k 'v;
  let reduce = reduce;
};
