/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Immutable;
open ReUnit;
open ReUnit.Test;

let test = describe "Map" [
  it "containsKey" (fun () => {
    let range = IntRange.create start::0 count::200
      |> IntRange.toSet
      |> Set.toMap;
    range |> Map.containsKey (-1) |> Expect.toBeEqualToFalse;
    range |> Map.containsKey 10 |> Expect.toBeEqualToTrue;
    range |> Map.containsKey 200 |> Expect.toBeEqualToFalse;
  }),
  it "count" (fun () => {
    IntRange.create start::0 count::200
      |> IntRange.toSet
      |> Set.toMap
      |> Map.count
      |> Expect.toBeEqualToInt 200;
  }),
  it "get" (fun () => {
    let map = IntRange.create start::0 count::200
      |> IntRange.toSet
      |> Set.toMap;

    map |> Map.get 10 |> Expect.toBeEqualToSomeOfInt 10;
    map |> Map.get (-10) |> Expect.toBeEqualToNoneOfInt;
  }),
  it "getOrRaise" (fun () => {
    let map = IntRange.create start::0 count::200
      |> IntRange.toSet
      |> Set.toMap;

    map |> Map.getOrRaise 10 |> Expect.toBeEqualToInt 10;
    (fun () => map |> Map.getOrRaise (-10)) |> Expect.shouldRaise;
  }),
  it "isEmpty" (fun () => {
    Map.empty |> Map.isEmpty |> Expect.toBeEqualToTrue;
    IntRange.create start::0 count::199
      |> IntRange.toSet
      |> Set.toMap
      |> Map.isEmpty
      |> Expect.toBeEqualToFalse;
  }),
  it "isNotEmpty" (fun () => {
    Map.empty |> Map.isNotEmpty |> Expect.toBeEqualToFalse;
    IntRange.create start::0 count::199
      |> IntRange.toSet
      |> Set.toMap
      |> Map.isNotEmpty
      |> Expect.toBeEqualToTrue;
  }),
  it "keys" (fun () => {
    IntRange.create start::0 count::200
      |> IntRange.toSet
      |> Set.toMap
      |> Map.keys
      |> Set.equals (
          IntRange.create start::0 count::200 |> IntRange.toSet
        )
      |> Expect.toBeEqualToTrue;
  }),
  it "reduce" (fun () => {
    IntRange.create start::0 count::200
      |> IntRange.toSet
      |> Set.toMap
      |> Map.reduce while_::(fun acc k v => k < 5) (fun acc k v => k + acc) 0
      |> Expect.toBeEqualToInt 10;

    IntRange.create start::0 count::200
      |> IntRange.toSet
      |> Set.toMap
      |> Map.reduce while_::(fun acc k v => v < 5) (fun acc k v => v + acc) 0
      |> Expect.toBeEqualToInt 10;
  }),
  it "toIterator" (fun () => ()),
  it "toKeyedIterator" (fun () => ()),
  it "map" (fun () => ()),
  it "toMap" (fun () => {
    let map = IntRange.create start::0 count::200
      |> IntRange.toSet
      |> Set.toMap;

    Pervasives.(===) map (Map.toMap map) |> Expect.toBeEqualToTrue;
  }),
  it "toSequence" (fun () => ()),
  it "values" (fun () => ()),
];
