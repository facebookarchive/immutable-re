/***
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
module Test: {
  type t;
  let describe: (string, list(t)) => t;
  let it: (string, unit => unit) => t;
  let toList: t => list((string, unit => unit));
};

module Expect: {
  let toBeEqualToFalse: bool => unit;
  let toBeEqualToInt: (int, int) => unit;
  let toBeEqualToListWith:
    (~equals: ('a, 'a) => bool, ~toString: 'a => string, list('a), list('a)) => unit;
  let toBeEqualToListOfInt: (list(int), list(int)) => unit;
  let toBeEqualToListOfString: (list(string), list(string)) => unit;
  let toBeEqualToNoneWith: (~toString: 'a => string, option('a)) => unit;
  let toBeEqualToNoneOfInt: option(int) => unit;
  let toBeEqualToNoneOfString: option(string) => unit;
  let toBeEqualToSomeWith:
    (~equals: ('a, 'a) => bool, ~toString: 'a => string, 'a, option('a)) => unit;
  let toBeEqualToSomeOfInt: (int, option(int)) => unit;
  let toBeEqualToSomeOfString: (string, option(string)) => unit;
  let toBeEqualToString: (string, string) => unit;
  let toBeEqualToTrue: bool => unit;
  let toBeEqualToWith: (~equals: ('a, 'a) => bool, ~toString: 'a => string, 'a, 'a) => unit;
  let shouldRaise: (unit => 'a) => unit;
};

let run: Test.t => unit;
