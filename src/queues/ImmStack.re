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
  list: list 'a,
};

let count ({ count }: t 'a): int => count;

let firstOrRaise ({ list }: t 'a): 'a => list |> ImmList.firstOrRaise;

let reduce
    while_::(predicate: 'acc => 'a => bool)
    (f: 'acc => 'a => 'acc )
    (acc: 'acc)
    ({ list }: t 'a): 'acc =>
  list |> ImmList.reduce while_::predicate f acc;

let toSequence ({ list }: t 'a): (Sequence.t 'a) => Sequence.ofList list;

let addFirst (value: 'a) ({ count, list }: t 'a): (t 'a) => ({
  count: count + 1,
  list: [value, ...list],
});

let empty (): t 'a => {
  count: 0,
  list: [],
};

let fromList (list: list 'a): (t 'a) =>
  { count: list |> ImmList.count, list };

let removeAll (_: t 'a): (t 'a) => empty ();

let removeFirstOrRaise ({ count, list }: t 'a): (t 'a) => ({
  count: count - 1,
  list: switch list {
    | [_, ...tail] => tail
    | [] => failwith "stack is empty"
  },
});

let return (value: 'a): (t 'a) => {
  count: 1,
  list: [value],
};

let toList ({ list }: t 'a): (list 'a) => list;
