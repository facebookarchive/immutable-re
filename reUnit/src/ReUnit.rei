/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

let module Test: {
  type t;

  let describe: string => (list t) => t;
  let it: string => (unit => unit) => t;
  let toList: t => (list (string, unit => unit));
};

let module Expect: {
  type t 'a;

  let expect: 'a => (t 'a);

  let toBeEqualToFalse: (t bool) => unit;
  let toBeEqualToInt: int => (t int) => unit;
  let toBeEqualToList:
    equals::('a => 'a => bool) =>
    toString::('a => string) =>
    (list 'a) =>
    (t (list 'a)) => unit;
  let toBeEqualToListOfInt: (list int) => (t (list int)) => unit;
  let toBeEqualToListOfString: (list string) => (t (list string)) => unit;
  let toBeEqualToNone:
    toString::('a => string) =>
    (t (option 'a)) =>
    unit;
  let toBeEqualToNoneOfInt: (t (option int)) => unit;
  let toBeEqualToNoneOfString: (t (option string)) => unit;
  let toBeEqualToSome:
    equals::('a => 'a => bool) =>
    toString::('a => string) =>
    'a =>
    (t (option 'a)) => unit;
  let toBeEqualToSomeOfInt: int => (t (option int)) => unit;
  let toBeEqualToSomeOfString: string => (t (option string)) => unit;
  let toBeEqualToString: string => (t string) => unit;
  let toBeEqualToTrue: (t bool) => unit;
  let toBeEqualToWith:
    equals::('a => 'a => bool) =>
    toString::('a => string) =>
    'a =>
    (t 'a) =>
    unit;

  let shouldRaise: (unit => 'a) => unit;
};

let run: Test.t => unit;
