/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Immutable;
open ReUnit.Expect;
open ReUnit.Test;

let module Tester = VectorTester.Make (CopyOnWriteArray: Vector_1) ({
  let count = 100;
});

let test = describe "CopyOnWriteArray" Tester.tests;
