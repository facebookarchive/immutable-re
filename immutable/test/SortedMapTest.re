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

let module SortedIntMap = SortedMap.Make {
  type t = int;

  let compare = Comparator.int;
  let equals = Equality.int;
};

let test = describe "SortedMap" [
  PersistentMapTester.test (module SortedIntMap: PersistentMap.S1  with type k = SortedIntMap.k) 100,
  PersistentMapTester.test (module SortedIntMap: PersistentMap.S1 with type k = SortedIntMap.k) 10000,
];
