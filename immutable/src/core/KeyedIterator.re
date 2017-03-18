/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'k 'v = {
  reduce: 'acc . ('acc => 'k => 'v => 'acc) => 'acc => 'acc,
};

let reduce (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) (iter: t 'k 'v): 'acc =>
  iter.reduce f acc;

let empty: t 'k 'v = {
  reduce: fun _ acc => acc
};

let concat (iters: list (t 'k 'v)): (t 'k 'v) => switch iters {
  | [] => empty
  | _ => {
    reduce: fun f acc => iters |> ImmList.reduce
      (fun acc next => next |> reduce f acc)
      acc
    }
};

let counter acc _ _ => acc + 1;

let count (iter: t 'k 'v): int =>
  iter |> reduce counter 0;

let doOnNext (sideEffect: 'k => 'v => unit) (iter: t 'k 'v): (t 'k 'v) =>
  if (iter === empty) empty
  else {
    reduce: fun f acc => iter |> reduce
      (fun acc k v => { sideEffect k v; (f acc k v) })
      acc
  };

let filter (filter: 'k => 'v => bool) (iter: t 'k 'v): (t 'k 'v) =>
  if (iter === empty) empty
  else {
    reduce: fun f acc => iter |> reduce
      (fun acc k v => if (filter k v) (f acc k v) else acc)
      acc
  };

let flatMap (mapper: 'kA => 'vA => t 'kB 'vB) (iter: t 'kA 'vA): (t 'kB 'vB) =>
  if (iter === empty) empty
  else {
    reduce: fun f acc => iter |> reduce
      (fun acc k v => (mapper k v) |> reduce f acc)
      acc
  };

let forEach (f: 'k => 'v => unit) (iter: t 'k 'v) =>
  iter |> reduce (fun _ => f) ();

let hashWith (keyHash: Hash.t 'k) (valueHash: Hash.t 'v) (iter: t 'k 'v): int => iter
  |> reduce (MapEntry.hashReducer keyHash valueHash) Hash.initialValue;

let hash (iter: t 'k 'v): int =>
  hashWith Hash.structural Hash.structural iter;

let keys (iter: t 'k 'v): (Iterator.t 'k) =>
  if (iter === empty) Iterator.empty
  else {
    reduce: fun f acc => iter |> reduce
      (fun acc k v => f acc k)
      acc
  };

let map (mapper: 'k => 'a => 'b) (iter: t 'k 'a): (t 'k 'b) =>
  if (iter === empty) empty
  else {
    reduce: fun f acc => iter |> reduce
      (fun acc k v => f acc k (mapper k v))
      acc
  };

let values (iter: t 'k 'v): (Iterator.t 'v) =>
  if (iter === empty ) Iterator.empty
  else {
    reduce: fun f acc => iter |> reduce
      (fun acc k v => f acc v)
      acc
  };

let toIterator (iter: t 'k 'v): (Iterator.t ('k, 'v)) =>
  if (iter === empty ) Iterator.empty
  else {
    reduce: fun f acc => iter |> reduce
      (fun acc k v => f acc (k, v))
      acc
  };
