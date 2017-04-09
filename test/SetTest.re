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

let test = describe "Set" [
  it "contains" (fun () => {
    let range = IntRange.create start::0 count::200 |> IntRange.toSet;
    range |> Set.contains (-1) |> Expect.toBeEqualToFalse;
    range |> Set.contains 10 |> Expect.toBeEqualToTrue;
    range |> Set.contains 200 |> Expect.toBeEqualToFalse;
  }),
  it "count" (fun () => {
    IntRange.create start::0 count::200
      |> IntRange.toSet
      |> Set.count
      |> Expect.toBeEqualToInt 200;
  }),
  it "equals" (fun () => {
    let set1 = IntRange.create start::0 count::200 |> IntRange.toSet;
    Set.equals set1 set1
      |> Expect.toBeEqualToTrue;

    let setEqualToSet1 = IntRange.create start::0 count::200 |> IntRange.toSet;
    Set.equals set1 setEqualToSet1
      |> Expect.toBeEqualToTrue;

    let setSameCountDifferentStartThanSet1 = IntRange.create start::(-10) count::200 |> IntRange.toSet;
    Set.equals set1 setSameCountDifferentStartThanSet1
      |> Expect.toBeEqualToFalse;

    let setDifferentCountSameStartAsSet1 = IntRange.create start::0 count::199 |> IntRange.toSet;
    Set.equals set1 setDifferentCountSameStartAsSet1
      |> Expect.toBeEqualToFalse;
  }),
  it "isEmpty" (fun () => {
    (Set.empty ()) |> Set.isEmpty |> Expect.toBeEqualToTrue;
    IntRange.create start::0 count::199
      |> IntRange.toSet
      |> Set.isEmpty
      |> Expect.toBeEqualToFalse;
  }),
  it "isNotEmpty" (fun () => {
    (Set.empty ()) |> Set.isNotEmpty |> Expect.toBeEqualToFalse;
    IntRange.create start::0 count::199
      |> IntRange.toSet
      |> Set.isNotEmpty
      |> Expect.toBeEqualToTrue;
  }),
  it "reduce" (fun () => {
    IntRange.create start::0 count::200
      |> IntRange.toSet
      |> Set.reduce while_::(fun _ i => i < 5) (fun acc i => i + acc) 0
      |> Expect.toBeEqualToInt 10;
  }),
  it "toIterable" (fun () => {
    IntRange.create start::0 count::200
      |> IntRange.toSet
      |> Set.toIterable
      |> Iterable.reduce while_::(fun _ i => i < 5) (fun acc i => i + acc) 0
      |> Expect.toBeEqualToInt 10;
  }),
  it "toSequence" (fun () => {
    IntRange.create start::0 count::200
      |> IntRange.toSet
      |> Set.toSequence
      |> Sequence.reduce while_::(fun _ i => i < 5) (fun acc i => i + acc) 0
      |> Expect.toBeEqualToInt 10;
  }),
  it "intersect" (fun () => {
    let set1 = IntRange.create start::0 count::200 |> IntRange.toSet;
    let set2 = IntRange.create start::50 count::300 |> IntRange.toSet;

    Set.intersect set1 set2
      |> IntSet.from
      |> IntSet.toSet
      |> Set.equals (IntRange.create start::50 count::150 |> IntRange.toSet)
      |> Expect.toBeEqualToTrue;
  }),
  it "subtract" (fun () => {
    let set1 = IntRange.create start::0 count::200 |> IntRange.toSet;
    let set2 = IntRange.create start::50 count::200 |> IntRange.toSet;

    Set.subtract set1 set2
      |> IntSet.from
      |> IntSet.toSet
      |> Set.equals (IntRange.create start::0 count::50 |> IntRange.toSet)
      |> Expect.toBeEqualToTrue;
  }),
  it "union" (fun () => {
    let set1 = IntRange.create start::0 count::200 |> IntRange.toSet;
    let set2 = IntRange.create start::50 count::200 |> IntRange.toSet;

    Set.union set1 set2
      |> IntSet.from
      |> IntSet.toSet
      |> Set.equals (IntRange.create start::0 count::250 |> IntRange.toSet)
      |> Expect.toBeEqualToTrue;
  }),
];
