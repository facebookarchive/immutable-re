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

let test = describe "IntMap" [
  describe "TransientIntMap" (
    MapTester.test
      (fun () => IntMap.empty |> IntMap.mutate)
      TransientIntMap.put
      TransientIntMap.remove
      TransientIntMap.tryGet
      numOfElements
  ),
  ...(
    MapTester.test
      (fun () => IntMap.empty)
      IntMap.put
      IntMap.remove
      IntMap.tryGet
      numOfElements
  )
];*/
