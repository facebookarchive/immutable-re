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

let rec countImpl (list: t 'a) (count: int): int => switch list {
  | [_, ...tail] => countImpl tail (count + 1)
  | [] => count
};

let count (list: t 'a): int => countImpl list 0;

let empty (): (t 'a) => [];

let isEmpty (list: t 'a): bool => switch list {
  | [] => true
  | _ => false;
};

let isNotEmpty (list: t 'a): bool => switch list {
  | [] => false
  | _ => true;
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

let rec reduceImpl
    while_::(predicate: 'acc => 'a => bool)
    (f: 'acc => 'a => 'acc )
    (acc: 'acc)
    (list: t 'a): 'acc => switch list {
  | [head, ...tail] =>
      if (predicate acc head) {
        let acc = f acc head;
        reduceImpl while_::predicate f acc tail
      } else acc
  | [] => acc
};

let rec reduce
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc )
    (acc: 'acc)
    (list: t 'a): 'acc => reduceImpl while_::predicate f acc list;

let removeAll (_: t 'a): (t 'a) => [];

let removeFirstOrRaise (list: t 'a): (t 'a) => switch list {
  | [_, ...tail] => tail
  | [] => failwith "List is empty"
};

let return (value: 'a): (t 'a) => [value];

let rec some (f: 'a => bool) (list: t 'a): bool => switch list {
  | [head, ...tail] => f head || some f tail
  | [] => false
};

let rec take (count: int) (list: t 'a): (t 'a) =>
  if (count === 0) []
  else switch list {
    | [head, ...tail] => [head, ...(tail |> take (count - 1))]
    | _ => failwith "list too short"
  };
