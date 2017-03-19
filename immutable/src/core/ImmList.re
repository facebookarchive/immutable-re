/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'a = list 'a;

let addFirst (value: 'a) (list: t 'a): (t 'a) =>
  [value, ...list];

let rec compareWith
    (valueCompare: Comparator.t 'a)
    (this: t 'a)
    (that: t 'a): Ordering.t =>
  if (this === that) Ordering.equal
  else switch (this, that) {
    | ([thisHead, ...thisTail], [thatHead, ...thatTail]) => switch (valueCompare thisHead thatHead) {
        | Ordering.Equal => compareWith valueCompare thisTail thatTail
        | x => x
      }
    | ([], []) => Ordering.equal
    | (_, []) => Ordering.greaterThan
    | ([], _) => Ordering.lessThan
  };

let compare (this: t 'a) (that: t 'a): Ordering.t =>
  compareWith Comparator.structural this that;

let containsWith (valueEquals: Equality.t 'a) (value: 'a) (list: t 'a): bool => {
  let rec loop list => switch list {
    | [head, ..._] when valueEquals head value => true
    | [_, ...tail] => loop tail
    | [] => false
  };

  loop list;
};

let contains (value: 'a) (list: t 'a): bool =>
  containsWith Equality.structural value list;

let rec countImpl (list: t 'a) (count: int): int => switch list {
  | [_, ...tail] => countImpl tail (count + 1)
  | [] => count
};

let count (list: t 'a): int => countImpl list 0;

let empty: (t 'a) = [];

let rec equalsWith (valueEquals: Equality.t 'a) (this: t 'a) (that: t 'a) => switch (this, that) {
  | ([thisHead, ...thisTail], [thatHead, ...thatTail]) =>
      if (valueEquals thisHead thatHead) (equalsWith valueEquals thisTail thatTail)
      else false
  | ([], []) => true
  | _ => false
};

let equals (this: t 'a) (that: t 'a) =>
  equalsWith Equality.structural this that;

let rec every (f: 'a => bool) (list: t 'a): bool => switch list {
  | [head, ...tail] =>
      if (f head) (every f tail)
      else false
  | [] => true
};

let forEach (f: 'a => unit) (list: t 'a): unit =>
  list |> List.iter f;

let isEmpty (list: t 'a): bool => switch list {
  | [] => true
  | _ => false;
};

let isNotEmpty (list: t 'a): bool => switch list {
  | [] => false
  | _ => true;
};

let rec find (f: 'a => bool) (list: t 'a): (option 'a) => switch list {
  | [head, ...tail] =>
      if (f head) (Some head)
      else find f tail
  | [] => None
};

let rec findOrRaise (f: 'a => bool) (list: t 'a): 'a => switch list {
  | [head, ...tail] =>
      if (f head) head
      else findOrRaise f tail
  | [] => failwith "not found"
};

let first (list: t 'a): (option 'a) => switch list {
  | [head, ..._] => Some head
  | [] => None
};

let firstOrRaise (list: t 'a): 'a => switch list {
  | [head, ..._] => head
  | [] => failwith "empty"
};

let rec mapReverseImpl (f: 'a => 'b) (src: t 'a) (dst: list 'b): (list 'b) => switch src {
  | [head, ...tail] => mapReverseImpl f tail [f head, ...dst]
  | [] => dst
};

let mapReverse (f: 'a => 'b) (list: t 'a): (list 'b) =>
  mapReverseImpl f list [];

let rec none (f: 'a => bool) (list: t 'a): bool => switch list {
  | [head, ...tail] =>
      if (f head) false
      else none f tail
  | [] => true
};

let rec reduce (f: 'acc => 'a => 'acc ) (acc: 'acc) (list: t 'a): 'acc => switch list {
  | [head, ...tail] =>
      let acc = f acc head;
      reduce f acc tail
  | [] => acc
};

let hashWith (hash: Hash.t 'a) (list: t 'a): int =>
  list |> reduce (Hash.reducer hash) Hash.initialValue;

let hash (list: t 'a): int => hashWith Hash.structural list;

let removeAll (_: t 'a): (t 'a) => [];

let removeFirst (list: t 'a): (t 'a) => switch list {
  | [_, ...tail] => tail
  | [] => failwith "List is empty"
};

let return (value: 'a): (t 'a) => [value];

let rec reverseImpl (src: t 'a) (dst: t 'a): (t 'a) => switch src {
  | [head, ...tail] => reverseImpl tail [head, ...dst]
  | [] => dst
};

let reverse (list: t 'a): (t 'a) =>
  reverseImpl list [];

let rec some (f: 'a => bool) (list: t 'a): bool => switch list {
  | [head, ...tail] => f head || some f tail
  | [] => false
};

let rec take (count: int) (list: t 'a): (t 'a) =>
  if (count == 0) []
  else switch list {
    | [head, ...tail] => [head, ...(tail |> take (count - 1))]
    | _ => failwith "list too short"
  };
