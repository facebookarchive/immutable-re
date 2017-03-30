/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Functions.Operators;

type t 'a = {
  count: int,
  iterable: unit => (Iterable.t 'a),
  sequence: unit => (Sequence.t 'a),
};

let count ({ count }: t 'a): int => count;

let empty (): (t 'a) => {
  count: 0,
  iterable: Iterable.empty,
  sequence: Sequence.empty,
};

let isEmpty ({ count }: t 'a): bool =>
  count === 0;

let isNotEmpty ({ count }: t 'a): bool =>
  count !== 0;

let map (f: 'a => 'b) ({ count, iterable, sequence } as collection: t 'a): (t 'b) =>
  if (iterable === Iterable.empty) (empty ()) else {
    count,
    iterable: iterable >> Iterable.map f,
    sequence: sequence >> Sequence.map f,
  };

let reduce
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    ({ iterable }: t 'a): 'acc =>
  iterable () |> Iterable.reduce while_::predicate f acc;

let toCollection (collection: t 'a): (t 'a) =>
  collection;

let toIterable ({ iterable }: t 'a): (Iterable.t 'a) =>
  iterable ();

let toSequence ({ sequence }: t 'a): (Sequence.t 'a) =>
  sequence ();

let module Reducer = Iterable.Reducer.Make1 {
  type nonrec t 'a = t 'a;
  let reduce = reduce;
  let toIterable = toIterable;
};
