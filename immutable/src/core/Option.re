/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'a = option 'a;

let compareWith
    (compare: Comparator.t 'a)
    (this: t 'a)
    (that: t 'a): Ordering.t => switch (this, that) {
  | (Some x, Some y) => compare x y
  | (Some _, _) => Ordering.greaterThan
  | (_, Some _) => Ordering.lessThan
  | (None, None) => Ordering.equal
};

let compare (this: t 'a) (that: t 'a): Ordering.t =>
  compareWith Comparator.structural this that;

let containsWith (equals: Equality.t 'a) (value: 'a) (opt: t 'a): bool => switch opt {
  | Some x => equals x value
  | None => false
};

let contains (value: 'a) (opt: t 'a): bool =>
  containsWith Equality.structural value opt;

let count (opt: t 'a): int => switch opt {
  | Some _ => 1
  | None => 0
};

let empty: (t 'a) = None;

let equalsWith (equals: Equality.t 'a) (this: t 'a) (that: t 'a): bool => switch (this, that) {
  | (Some x, Some y) => equals x y
  | (None, None) => true
  | _ => false
};

let equals (this: t 'a) (that: t 'a): bool =>
  equalsWith Equality.structural this that;

let every (f: 'a => bool) (opt: t 'a): bool => switch opt {
  | None => false
  | Some x => f x
};

let filter (f: 'a => bool) (opt: t 'a): (t 'a) => switch opt {
  | Some x =>
      if (f x) opt
      else None
  | _ => None
};

let find (f: 'a => bool) (opt: t 'a): 'a => switch opt {
  | None => failwith "empty"
  | Some a when f a => a
  | _ => failwith "not found"
};

let first (opt: t 'a): 'a => switch opt {
  | Some x => x
  | None => failwith "option is none"
};

let flatMap (f: 'a => option 'b) (opt: t 'a): option 'b => switch opt {
  | Some a => f a
  | _ => None
};

let flatten (opt: option (t 'a)): (t 'a) => switch opt {
  | Some (Some a) => Some a
  | _ => None
};

let forEach (f: 'a => unit) (opt: t 'a): unit => switch opt {
  | Some a => f a
  | _ => ()
};

let last = first;

let hashWith (hash: Hash.t 'a) (opt: t 'a): int => switch opt {
  | None => 0
  | Some x => hash x
};

let hash (opt: t 'a): int => hashWith Hash.structural opt;

let isEmpty (opt: option _): bool => switch opt {
  | Some _ => false
  | None => true
};

let isNotEmpty (opt: option _): bool => switch opt {
  | Some _ => true
  | None => false
};

let map (f: 'a => 'b) (opt: t 'a): option 'b => switch opt {
  | Some a => Some (f a)
  | _ => None
};

let none (f: 'a => bool) (opt: t 'a): bool => switch opt {
  | Some a => not @@ f @@ a
  | _ => true
};

let orCompute (compute: unit => 'a) (opt: t 'a): 'a => switch (opt) {
  | Some a => a
  | _ => compute ()
};

let orDefault (defaultValue: 'a) (opt: t 'a): 'a => switch (opt) {
  | Some a => a
  | _ => defaultValue
};

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) (opt: t 'a): 'acc => switch opt {
  | Some a => f acc a
  | _ => acc
};

let return (a: 'a): (t 'a) => Some a;

let some (f: 'a => bool) (opt: t 'a): bool => switch opt {
  | Some a => f a
  | _ => false
};

let toIterator (opt: t 'a): (Iterator.t 'a) =>
  if (isEmpty opt) Iterator.empty
  else {
    reduce: fun f acc => reduce f acc opt
  };

let tryFind (f: 'a => bool) (opt: t 'a): (t 'a) => switch opt {
  | None => None
  | Some a when f a => Some a
  | _ =>  None
};

let tryFirst = Functions.identity;
let tryLast = tryFirst;

let module Operators = {
  let (>>=) (opt: t 'a) (f: 'a => option 'b): option 'b => flatMap f opt;
  let (>>|) (opt: t 'a) (f:'a => 'b): option 'b => map f opt;
  let (|?) (opt: t 'a) (defaultValue: 'a): 'a => orDefault defaultValue opt;
};
