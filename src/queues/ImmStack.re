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

let addFirst (value: 'a) ({ count, list }: t 'a): (t 'a) => ({
  count: count + 1,
  list: [value, ...list],
});

let addFirstAll (values: Iterable.t 'a) ({ count, list }: t 'a): (t 'a) => {
  let newCount = ref count;

  let newList = values |> Iterable.reduce
    (fun acc next => {
      newCount := !newCount + 1;
      [next, ...acc]
    })
    list;

  { count: !newCount, list: newList }
};

let count ({ count }: t 'a): int => count;

let empty (): t 'a => {
  count: 0,
  list: [],
};

let first ({ list }: t 'a): (option 'a) => list |> ImmList.first;

let firstOrRaise ({ list }: t 'a): 'a => list |> ImmList.firstOrRaise;

let fromList (list: list 'a): (t 'a) =>
  { count: list |> ImmList.count, list };

let fromReverse (iter: Iterable.t 'a): (t 'a) =>
  empty () |> addFirstAll iter;

include (Collection.Make1 {
  type nonrec t 'a = t 'a;

  let count ({ count }: t 'a): int => count;

  let reduce
      while_::(predicate: 'acc => 'a => bool)
      (f: 'acc => 'a => 'acc )
      (acc: 'acc)
      ({ list }: t 'a): 'acc =>
    list |> ImmList.reduce while_::predicate f acc;

  let toSequence ({ list }: t 'a): (Sequence.t 'a) => Sequence.ofList list;
}: Collection.S1 with type t 'a := t 'a);

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

let seqCollectionOps: SequentialCollection.Ops.t 'a (t 'a) = {
  count,
  first,
  firstOrRaise,
  toCollection,
  toIterable,
  toSequence,
};

let toSequentialCollection (set: t 'a): (SequentialCollection.t 'a) =>
  if (isEmpty set) (SequentialCollection.empty ())
  else SequentialCollection.SequentialCollection set seqCollectionOps;
