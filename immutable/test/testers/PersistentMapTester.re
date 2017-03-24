/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Printf;
open Immutable;
open ReUnit;
open ReUnit.Test;

let test (module PersistentMap: PersistentMap.S1 with type k = int) (count: int) => {
  let countDiv2 = count / 2;
  let countDiv4 = count / 4;

  describe (sprintf "count: %i" count) [
    it "alter" (fun () => ()),
    it "containsKey" (fun () => ()),
    it "count" (fun () => ()),
    it "get" (fun () => ()),
    it "getOrRaise" (fun () => ()),
    it "isEmpty" (fun () => ()),
    it "isNotEmpty" (fun () => ()),
    it "keys" (fun () => ()),
    it "map" (fun () => ()),
    it "merge" (fun () => ()),
    it "put" (fun () => ()),
    it "putAll" (fun () => ()),
    it "reduce" (fun () => ()),
    it "remove" (fun () => ()),
    it "removeAll" (fun () => ()),
    it "toIterator" (fun () => ()),
    it "toKeyedIterator" (fun () => ()),
    it "toMap" (fun () => ()),
    it "toSequence" (fun () => ()),
    it "values" (fun () => ()),
  ];
};
