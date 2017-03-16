/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'a = {
  reduce: 'acc . ('acc => 'a => 'acc) => 'acc => 'acc,
};

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) (iter: t 'a): 'acc =>
  iter.reduce f acc;

let empty: t 'a = {
  reduce: fun _ acc => acc
};

let concat (iters: list (t 'a)): (t 'a) => switch iters {
  | [] => empty
  | _ => {
    reduce: fun f acc => iters |> ImmList.reduce
      (fun acc next => next |> reduce f acc)
      acc
    }
};

let counter acc _ => acc + 1;

let count (iter: t 'a): int =>
  iter |> reduce counter 0;

let doOnNext (sideEffect: 'a => unit) (iter: t 'a): (t 'a) =>
  if (iter === empty) empty
  else {
    reduce: fun f acc => iter |> reduce
      (fun acc next => { sideEffect next; (f acc next) })
      acc
  };

let filter (filter: 'a => bool) (iter: t 'a): (t 'a) =>
  if (iter === empty) empty
  else {
    reduce: fun f acc => iter |> reduce
      (fun acc next => if (filter next) (f acc next) else acc)
      acc
  };

let flatMap (mapper: 'a => t 'b) (iter: t 'a): (t 'b) =>
  if (iter === empty) empty
  else {
    reduce: fun f acc => iter |> reduce
      (fun acc next => next |> mapper |> reduce f acc)
      acc
  };

let flatten (iters: t (t 'a)): (t 'a) =>
  if (iters === empty) empty
  else {
    reduce: fun f acc => iters |> reduce
      (fun acc next => next |> reduce f acc)
      acc
  };

let forEach (f: 'a => unit) (iter: t 'a) =>
  iter |> reduce (fun _ => f) ();

let hashWith (hash: (Hash.t 'a)) (iter: t 'a): int => iter
  |> reduce (Hash.reducer hash) Hash.initialValue;

let hash (iter: t 'a): int =>
  hashWith Hash.structural iter;

let map (mapper: 'a => 'b) (iter: t 'a): (t 'b) =>
  if (iter === empty) empty
  else {
    reduce: fun f acc => iter |> reduce
      (fun acc next => f acc (mapper next))
      acc
  };

let ofList (list: list 'a): (t 'a) =>
  if (ImmList.isEmpty list) empty
  else {
    reduce: fun f acc => ImmList.reduce f acc list
  };

let return (value: 'a): (t 'a) => {
  reduce: fun f acc => f acc value
};

let concatMap = flatMap;
let concatAll = flatten;

let listAddFirstAll (iter: t 'a) (list: list 'a): (list 'a) =>
  iter |> reduce (fun acc next => acc |> ImmList.addFirst next) list;

let listFromReverse (iter: t 'a): (list 'a) =>
  [] |> listAddFirstAll iter;
