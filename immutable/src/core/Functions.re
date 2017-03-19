/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

let alwaysFailWith (message: string) _ => failwith message;
let alwaysFalse _: bool => false;
let alwaysNone _: option _ => None;
let alwaysTrue _: bool => true;
let alwaysTrue2 _ _: bool => true;
let alwaysTrue3 _ _ _: bool => true;
let alwaysUnit _ => ();
let call (f: unit => 'a): 'a => f ();
let compose (f1: 'a => 'b) (f2: 'b => 'c) (a: 'a): 'c => f2 (f1 a);
let flip (f: 'a => 'b => 'c): ('b => 'a => 'c) => fun b a => f a b;
let identity (a: 'a): 'a => a;
let isFalse = not;
let isTrue (a: bool): bool => a;
let return (a: 'a): (_ => 'a) => fun _ => a;

let module Operators = {
  let (>>) (f1: 'a => 'b) (f2: 'b => 'c): ('a => 'c) => compose f1 f2;
};
