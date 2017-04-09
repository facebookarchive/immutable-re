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

let module SortedIntMap = SortedMap.Make1 {
  type t = int;

  let compare = Comparator.int;
  let equals = Equality.int;
};

let test = describe "Map" [
  it "containsKey" (fun () => {
    let range = IntRange.create start::0 count::200
      |> IntRange.toIterable
      |> Iterable.map (fun i => (i, i))
      |> SortedIntMap.fromEntries
      |> SortedIntMap.toMap;
    range |> Map.containsKey (-1) |> Expect.toBeEqualToFalse;
    range |> Map.containsKey 10 |> Expect.toBeEqualToTrue;
    range |> Map.containsKey 200 |> Expect.toBeEqualToFalse;
  }),
  it "count" (fun () => {
    IntRange.create start::0 count::200
      |> IntRange.toIterable
      |> Iterable.map (fun i => (i, i))
      |> SortedIntMap.fromEntries
      |> SortedIntMap.toMap
      |> Map.count
      |> Expect.toBeEqualToInt 200;
  }),
  it "get" (fun () => {
    let map = IntRange.create start::0 count::200
      |> IntRange.toIterable
      |> Iterable.map (fun i => (i, i))
      |> SortedIntMap.fromEntries
      |> SortedIntMap.toMap;

    map |> Map.get 10 |> Expect.toBeEqualToSomeOfInt 10;
    map |> Map.get (-10) |> Expect.toBeEqualToNoneOfInt;
  }),
  it "getOrRaise" (fun () => {
    let map = IntRange.create start::0 count::200
      |> IntRange.toIterable
      |> Iterable.map (fun i => (i, i))
      |> SortedIntMap.fromEntries
      |> SortedIntMap.toMap;

    map |> Map.getOrRaise 10 |> Expect.toBeEqualToInt 10;
    (fun () => map |> Map.getOrRaise (-10)) |> Expect.shouldRaise;
  }),
  it "isEmpty" (fun () => {
    (Map.empty ()) |> Map.isEmpty |> Expect.toBeEqualToTrue;
    IntRange.create start::0 count::199
      |> IntRange.toIterable
      |> Iterable.map (fun i => (i, i))
      |> SortedIntMap.fromEntries
      |> SortedIntMap.toMap
      |> Map.isEmpty
      |> Expect.toBeEqualToFalse;
  }),
  it "isNotEmpty" (fun () => {
    (Map.empty ()) |> Map.isNotEmpty |> Expect.toBeEqualToFalse;
    IntRange.create start::0 count::199
      |> IntRange.toIterable
      |> Iterable.map (fun i => (i, i))
      |> SortedIntMap.fromEntries
      |> SortedIntMap.toMap
      |> Map.isNotEmpty
      |> Expect.toBeEqualToTrue;
  }),
  it "keySet" (fun () => {
    IntRange.create start::0 count::200
      |> IntRange.toIterable
      |> Iterable.map (fun i => (i, i))
      |> SortedIntMap.fromEntries
      |> SortedIntMap.toMap
      |> Map.keySet
      |> Set.equals (
          IntRange.create start::0 count::200 |> IntRange.toSet
        )
      |> Expect.toBeEqualToTrue;
  }),
  it "reduce" (fun () => {
    IntRange.create start::0 count::200
      |> IntRange.toIterable
      |> Iterable.map (fun i => (i, i))
      |> SortedIntMap.fromEntries
      |> SortedIntMap.toMap
      |> Map.reduce while_::(fun _ k _ => k < 5) (fun acc k _ => k + acc) 0
      |> Expect.toBeEqualToInt 10;

    IntRange.create start::0 count::200
      |> IntRange.toIterable
      |> Iterable.map (fun i => (i, i))
      |> SortedIntMap.fromEntries
      |> SortedIntMap.toMap
      |> Map.reduce while_::(fun _ _ v => v < 5) (fun acc _ v => v + acc) 0
      |> Expect.toBeEqualToInt 10;
  }),
  it "toIterable" (fun () => ()),
  it "toKeyedIterator" (fun () => ()),
  it "toSequence" (fun () => ()),
  it "values" (fun () => ()),
];
