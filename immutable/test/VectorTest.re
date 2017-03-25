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

let module Tester10 = VectorTester.Make (Vector: Vector_1) ({
  let count = 10;
});

let module Tester50 = VectorTester.Make (Vector: Vector_1) ({
  let count = 50;
});

let module Tester200 = VectorTester.Make (Vector: Vector_1) ({
  let count = 200;
});

let module Tester2000 = VectorTester.Make (Vector: Vector_1) ({
  let count = 2000;
});

let module Tester50000 = VectorTester.Make (Vector: Vector_1) ({
  let count = 50000;
});

let test = [
  Tester10.tests,
  Tester50.tests,
  Tester200.tests,
  Tester2000.tests,
  Tester50000.tests,
] |> List.toIterator
  |> Iterator.flatMap List.toIterator
  |> List.fromReverse
  |> describe "Vector";
