/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

/*open Immutable;
open MapTester;
open ReUnit;
open ReUnit.Test;

let numOfElements = 10000;

let test = describe "HashMap" [
  describe "Comparator" (
    MapTester.test
      HashMap.empty
      HashMap.put
      HashMap.remove
      HashMap.tryGet
      numOfElements
  ),
  describe "Equality" (
    MapTester.test
      (fun () => HashMap.emptyWith @@ HashStrategy.structuralEquality @@ ())
      HashMap.put
      HashMap.remove
      HashMap.tryGet
      numOfElements
  ),

  describe "with hash collisions" (
    MapTester.test
      (fun () => HashMap.emptyWith (HashStrategy.createWithComparator (fun i => i mod 10) Comparator.structural))
      HashMap.put
      HashMap.remove
      HashMap.tryGet
      numOfElements
  ),

  describe "TransientHashMap" [
    describe "Comparator" (
      MapTester.test
        (fun () => HashMap.empty () |> HashMap.mutate)
        TransientHashMap.put
        TransientHashMap.remove
        TransientHashMap.tryGet
        numOfElements
    ),

    describe "Equality" (
      MapTester.test
        (fun () => (HashMap.emptyWith @@ HashStrategy.structuralEquality @@ ()) |> HashMap.mutate)
        TransientHashMap.put
        TransientHashMap.remove
        TransientHashMap.tryGet
        numOfElements
    ),
  ],
];*/
