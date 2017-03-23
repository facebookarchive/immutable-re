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

let test = describe "Set" [
  it "contains" (fun () => {
    let range = IntRange.create start::0 count::200 |> IntRange.toSet;
    range |> Set.contains (-1) |> expect |> toBeEqualToFalse;
    range |> Set.contains 10 |> expect |> toBeEqualToTrue;
    range |> Set.contains 200 |> expect |> toBeEqualToFalse;
  }),
  it "count" (fun () => {
    IntRange.create start::0 count::200
      |> IntRange.toSet
      |> Set.count
      |> expect
      |> toBeEqualToInt 200;
  }),
  it "equals" (fun () => {
    let set1 = IntRange.create start::0 count::200 |> IntRange.toSet;
    Set.equals set1 set1
      |> expect
      |> toBeEqualToTrue;

    let setEqualToSet1 = IntRange.create start::0 count::200 |> IntRange.toSet;
    Set.equals set1 setEqualToSet1
      |> expect
      |> toBeEqualToTrue;

    let setSameCountDifferentStartThanSet1 = IntRange.create start::(-10) count::200 |> IntRange.toSet;
    Set.equals set1 setSameCountDifferentStartThanSet1
      |> expect
      |> toBeEqualToFalse;

    let setDifferentCountSameStartAsSet1 = IntRange.create start::0 count::199 |> IntRange.toSet;
    Set.equals set1 setDifferentCountSameStartAsSet1
      |> expect
      |> toBeEqualToFalse;
  }),
  it "isEmpty" (fun () => {
    Set.empty |> Set.isEmpty |> expect |> toBeEqualToTrue;
    IntRange.create start::0 count::199
      |> IntRange.toSet
      |> Set.isEmpty
      |> expect
      |> toBeEqualToFalse;
  }),
  it "isNotEmpty" (fun () => {
    Set.empty |> Set.isNotEmpty |> expect |> toBeEqualToFalse;
    IntRange.create start::0 count::199
      |> IntRange.toSet
      |> Set.isNotEmpty
      |> expect
      |> toBeEqualToTrue;
  }),
  it "reduce" (fun () => {
    IntRange.create start::0 count::200
      |> IntRange.toSet
      |> Set.reduce while_::(fun acc i => i < 5) (fun acc i => i + acc) 0
      |> expect
      |> toBeEqualToInt 10;
  }),
  it "toIterator" (fun () => {
    IntRange.create start::0 count::200
      |> IntRange.toSet
      |> Set.toIterator
      |> Iterator.reduce while_::(fun acc i => i < 5) (fun acc i => i + acc) 0
      |> expect
      |> toBeEqualToInt 10;
  }),
  it "toSequence" (fun () => {
    IntRange.create start::0 count::200
      |> IntRange.toSet
      |> Set.toSequence
      |> Sequence.reduce while_::(fun acc i => i < 5) (fun acc i => i + acc) 0
      |> expect
      |> toBeEqualToInt 10;
  }),
  it "toSet" (fun () => {
    let set = IntRange.create start::0 count::200 |> IntRange.toSet;
    Pervasives.(===) set (Set.toSet set) |> expect |> toBeEqualToTrue;
  }),
  it "intersect" (fun () => {
    let set1 = IntRange.create start::0 count::200 |> IntRange.toSet;
    let set2 = IntRange.create start::50 count::300 |> IntRange.toSet;

    Set.intersect set1 set2
      |> IntSet.from
      |> IntSet.toSet
      |> Set.equals (IntRange.create start::50 count::150 |> IntRange.toSet)
      |> expect
      |> toBeEqualToTrue;
  }),
  it "subtract" (fun () => {
    let set1 = IntRange.create start::0 count::200 |> IntRange.toSet;
    let set2 = IntRange.create start::50 count::200 |> IntRange.toSet;

    Set.subtract set1 set2
      |> IntSet.from
      |> IntSet.toSet
      |> Set.equals (IntRange.create start::0 count::50 |> IntRange.toSet)
      |> expect
      |> toBeEqualToTrue;
  }),
  it "toMap" (fun () => {
    let map = IntRange.create start::0 count::200 |> IntRange.toSet |> Set.toMap;
    map |> Map.containsKey 10 |> expect |> toBeEqualToTrue;
    map |> Map.containsKey (-10) |> expect |> toBeEqualToFalse;
    map |> Map.containsKey 300 |> expect |> toBeEqualToFalse;

    map |> Map.get 10 |> expect |> toBeEqualToSomeOfInt 10;
    map |> Map.get (-10) |> expect |> toBeEqualToNoneOfInt;

    map |> Map.getOrRaise 10 |> expect |> toBeEqualToInt 10;
    (fun () => map |> Map.getOrRaise (-10)) |> shouldRaise;

    IntRange.create start::0 count::200
      |> IntRange.toSet
      |> Set.toMap
      |> Map.reduce while_::(fun acc k v => k < 5) (fun acc k v => v + acc) 0
      |> expect
      |> toBeEqualToInt 10;

    IntRange.create start::0 count::200
      |> IntRange.toSet
      |> Set.toMap
      |> Map.reduce while_::(fun acc k v => v < 5) (fun acc k v => k + acc) 0
      |> expect
      |> toBeEqualToInt 10;
  }),
  it "union" (fun () => {
    let set1 = IntRange.create start::0 count::200 |> IntRange.toSet;
    let set2 = IntRange.create start::50 count::200 |> IntRange.toSet;

    Set.union set1 set2
      |> IntSet.from
      |> IntSet.toSet
      |> Set.equals (IntRange.create start::0 count::250 |> IntRange.toSet)
      |> expect
      |> toBeEqualToTrue;
  }),
];
