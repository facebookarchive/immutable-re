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

  /* In ocaml 4.03 there is a result type that Expect maps to directly.
   * In that case we could move these monadic functions into Immutable.re directly
   */
  let defer: (unit => 'a) => (t 'a);
  let expect: 'a => (t 'a);
  let failwith: string => (t 'a);
  let flatMap: ('a => t 'b) => (t 'a) => (t 'b);
  let forEach: ('a => unit) => (t 'a) => unit;
  let get: (t 'a) => 'a;
  let map: ('a => 'b) => (t 'a) => (t 'b);
  let return: 'a => (t 'a);

  let toBeEqualTo: ('a => string) => 'a => (t 'a) => unit;
  let toBeEqualToFalse: (t bool) => unit;
  let toBeEqualToInt: int => (t int) => unit;
  let toBeEqualToNone: ('a => string) => (t (option 'a)) => unit;
  let toBeEqualToNoneOfInt: (t (option int)) => unit;
  let toBeEqualToNoneOfString: (t (option string)) => unit;
  let toBeEqualToSome: ('a => string) => 'a => (t (option 'a)) => unit;
  let toBeEqualToSomeOfInt: int => (t (option int)) => unit;
  let toBeEqualToSomeOfString: string => (t (option string)) => unit;
  let toBeEqualToString: string => (t string) => unit;
  let toBeEqualToTrue: (t bool) => unit;
  let toBeEqualToWith: (Equality.t 'a) => ('a => string) => 'a => (t 'a) => unit;
  let throws: (t 'a) => unit;
};

let run: Test.t => unit;
