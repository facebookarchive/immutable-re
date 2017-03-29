/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'a = {
  count: int,
  iterator: unit => (Iterator.t 'a),
  sequence: unit => (Sequence.t 'a),
};

let count ({ count }: t 'a): int => count;

let empty (): (t 'a) => {
  count: 0,
  iterator: Iterator.empty,
  sequence: Sequence.empty,
};

let isEmpty ({ count }: t 'a): bool =>
  count === 0;

let isNotEmpty ({ count }: t 'a): bool =>
  count !== 0;

let reduce
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    ({ iterator }: t 'a): 'acc =>
  iterator () |> Iterator.reduce while_::predicate f acc;

let toCollection (collection: t 'a): (t 'a) =>
  collection;

let toIterator ({ iterator }: t 'a): (Iterator.t 'a) =>
  iterator ();

let toSequence ({ sequence }: t 'a): (Sequence.t 'a) =>
  sequence ();

let module Reducer = Reducer.Make1 {
  type nonrec t 'a = t 'a;
  let reduce = reduce;
};
