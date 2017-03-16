/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

let addFirst (value: 'a) (list: list 'a): (list 'a) =>
  [value, ...list];

let add = addFirst;

let rec compareWith
    (valueCompare: Comparator.t 'a)
    (this: list 'a)
    (that: list 'a): Ordering.t =>
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

let compare (this: list 'a) (that: list 'a): Ordering.t =>
  compareWith Comparator.structural this that;

let containsWith (valueEquals: Equality.t 'a) (value: 'a) (list: list 'a): bool => {
  let rec loop list => switch list {
    | [head, ..._] when valueEquals head value => true
    | [_, ...tail] => loop tail
    | [] => false
  };

  loop list;
};

let contains (value: 'a) (list: list 'a): bool =>
  containsWith Equality.structural value list;

let rec countImpl (list: list 'a) (count: int): int => switch list {
  | [_, ...tail] => countImpl tail (count + 1)
  | [] => count
};

let count (list: list 'a): int => countImpl list 0;

let empty: (list 'a) = [];

let rec equalsWith (valueEquals: Equality.t 'a) (this: list 'a) (that: list 'a) => switch (this, that) {
  | ([thisHead, ...thisTail], [thatHead, ...thatTail]) =>
      if (valueEquals thisHead thatHead) (equalsWith valueEquals thisTail thatTail)
      else false
  | ([], []) => true
  | _ => false
};

let equals (this: list 'a) (that: list 'a) =>
  equalsWith Equality.structural this that;

let rec every (f: 'a => bool) (list: list 'a): bool => switch list {
  | [head, ...tail] =>
      if (f head) (every f tail)
      else false
  | [] => true
};

let forEach (f: 'a => unit) (list: list 'a): unit =>
  list |> List.iter f;

let isEmpty (list: list 'a): bool => switch list {
  | [] => true
  | _ => false;
};

let isNotEmpty (list: list 'a): bool => switch list {
  | [] => false
  | _ => true;
};

let rec find (f: 'a => bool) (list: list 'a): 'a => switch list {
  | [head, ...tail] =>
      if (f head) head
      else find f tail
  | [] => failwith "not found"
};

let first (list: list 'a): 'a => switch list {
  | [head, ..._] => head
  | [] => failwith "empty"
};

let rec mapReverseImpl (f: 'a => 'b) (src: list 'a) (dst: list 'b): (list 'b) => switch src {
  | [head, ...tail] => mapReverseImpl f tail [f head, ...dst]
  | [] => dst
};

let mapReverse (f: 'a => 'b) (list: list 'a): (list 'b) =>
  mapReverseImpl f list [];

let rec none (f: 'a => bool) (list: list 'a): bool => switch list {
  | [head, ...tail] =>
      if (f head) false
      else none f tail
  | [] => true
};

let rec reduce (f: 'acc => 'a => 'acc ) (acc: 'acc) (list: list 'a): 'acc => switch list {
  | [head, ...tail] =>
      let acc = f acc head;
      reduce f acc tail
  | [] => acc
};

let hashWith (hash: Hash.t 'a) (list: list 'a): int =>
  list |> reduce (Hash.reducer hash) Hash.initialValue;

let hash (list: list 'a): int => hashWith Hash.structural list;

let removeAll (_: list 'a): (list 'a) => [];

let removeFirst (list: list 'a): (list 'a) => switch list {
  | [_, ...tail] => tail
  | [] => failwith "List is empty"
};

let return (value: 'a): (list 'a) => [value];

let rec reverseImpl (src: list 'a) (dst: list 'a): (list 'a) => switch src {
  | [head, ...tail] => reverseImpl tail [head, ...dst]
  | [] => dst
};

let reverse (list: list 'a): (list 'a) =>
  reverseImpl list [];

let rec some (f: 'a => bool) (list: list 'a): bool => switch list {
  | [head, ...tail] => f head || some f tail
  | [] => false
};

let rec take (count: int) (list: list 'a): (list 'a) =>
  if (count == 0) []
  else switch list {
    | [head, ...tail] => [head, ...(tail |> take (count - 1))]
    | _ => failwith "list too short"
  };

let rec tryFind (f: 'a => bool) (list: list 'a): (option 'a) => switch list {
  | [head, ...tail] =>
      if (f head) (Some head)
      else tryFind f tail
  | [] => None
};

let tryFirst (list: list 'a): (option 'a) => switch list {
  | [head, ..._] => Some head
  | [] => None
};
