/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'a = option 'a;

let firstOrRaise (opt: t 'a): 'a => switch opt {
  | Some x => x
  | None => failwith "option is none"
};

let flatMap (f: 'a => option 'b) (opt: t 'a): option 'b => switch opt {
  | Some a => f a
  | _ => None
};

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

let orDefault (defaultValue: 'a) (opt: t 'a): 'a => switch (opt) {
  | Some a => a
  | _ => defaultValue
};

let return (a: 'a): (t 'a) => Some a;

let module Operators = {
  let (>>=) (opt: t 'a) (f: 'a => option 'b): option 'b => flatMap f opt;
  let (>>|) (opt: t 'a) (f:'a => 'b): option 'b => map f opt;
  let (|?) (opt: t 'a) (defaultValue: 'a): 'a => orDefault defaultValue opt;
};
