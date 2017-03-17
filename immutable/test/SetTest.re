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
  describe "empty" [
    it "contains" (fun () => {
      expect (Set.empty |> Set.contains 0) |> toBeEqualToFalse;
    }),
    it "count" (fun () => {
      expect (Set.empty |> Set.count) |> toBeEqualToInt 0;
    }),
    it "every" (fun () => {
      expect (Set.empty |> Set.every Functions.alwaysTrue) |> toBeEqualToFalse;
    }),
    it "find" (fun () => {
      defer (fun () => Set.empty |> Set.find Functions.alwaysTrue) |> throws;
    }),
    it "forEach" (fun () => {
      Set.empty |> Set.forEach (Functions.alwaysFailWith "forEach had values");
    }),
    it "none" (fun () => {
      expect (Set.empty |> Set.none Functions.alwaysTrue) |> toBeEqualToTrue;
    }),
    it "reduce" (fun () => {
      Set.empty |> Set.reduce (fun _ => Functions.alwaysFailWith "forEach had values") ();
    }),
    it "some" (fun () => {
      expect (Set.empty |> Set.some Functions.alwaysTrue) |> toBeEqualToFalse;
    }),
    it "toSequence" (fun () => {
      expect (Set.empty |> Set.toSequence) |> toBeEqualToEmptySequenceOfString;
    }),
    it "tryFind" (fun () => {
      expect (Set.empty |> Set.tryFind Functions.alwaysTrue) |> toBeEqualToNoneOfInt;
    }),
  ],
  it "equals" (fun () => {
    let col = IntRange.create 0 20 |> IntRange.toSet;
    let dup = IntRange.create 0 20 |> IntRange.toSet;
    let rev = IntRange.create 0 20 |> IntRange.toSet;

    expect (Set.equals col dup) |> toBeEqualToTrue;
    expect (Set.equals col col) |> toBeEqualToTrue;
    expect (Set.equals col rev) |> toBeEqualToTrue;
    expect (Set.equals col Set.empty) |> toBeEqualToFalse;
    expect (
      Set.equals col (IntRange.create 1 20 |> IntRange.toSet)
    ) |> toBeEqualToFalse;
    expect (
      Set.equals (IntRange.empty |> IntRange.toSet) Set.empty
    ) |> toBeEqualToTrue;
  }),
  it "hash" (fun () => {
    let col = IntRange.create 0 20 |> IntRange.toSet;
    let dup = IntRange.create 0 20 |> IntRange.toSet;

    expect (Set.hash col) |> toBeEqualToInt (Set.hash col);
    expect (Set.hash col) |> toBeEqualToInt (Set.hash dup);
  }),
  it "intersect" (fun () => {
    let setA = ["a", "b", "c"] |> List.toIterator |> SortedSet.from |> SortedSet.toSet;
    let setB = ["b", "c", "d"] |> List.toIterator |> SortedSet.from |> SortedSet.toSet;

    let intersection = Set.intersect setA setB
      |> SortedSet.from
      |> SortedSet.toSet;

    let expected = ["b", "c"] |> List.toIterator |> SortedSet.from |> SortedSet.toSet;

    expect (Set.equals intersection expected) |> toBeEqualToTrue;
  }),
  it "isEmpty" (fun () => {
    expect (Set.empty |> Set.isEmpty) |> toBeEqualToTrue;
    expect (
      IntRange.create 0 1 |> IntRange.toSet|> Set.isEmpty
    ) |> toBeEqualToFalse;
  }),
  it "isNotEmpty" (fun () => {
    expect (Set.empty |> Set.isNotEmpty) |> toBeEqualToFalse;
    expect (
      IntRange.create 0 1 |> IntRange.toSet |> Set.isNotEmpty
    ) |> toBeEqualToTrue;
  }),
  it "subtract" (fun () => {
    let setA = ["a", "b", "c"] |> List.toIterator |> SortedSet.from |> SortedSet.toSet;
    let setB = ["b", "c", "d"] |> List.toIterator |> SortedSet.from |> SortedSet.toSet;

    let subtracted = Set.subtract setA setB
      |> SortedSet.from
      |> SortedSet.toSet;

    let expected = ["a"] |> List.toIterator |> SortedSet.from |> SortedSet.toSet;

    expect (Set.equals subtracted expected) |> toBeEqualToTrue;
  }),
  it "union" (fun () => {
    let setA = ["a", "b", "c"] |> List.toIterator |> SortedSet.from |> SortedSet.toSet;
    let setB = ["b", "c", "d"] |> List.toIterator |> SortedSet.from |> SortedSet.toSet;

    let union =  Set.union setA setB
      |> SortedSet.from
      |> SortedSet.toSet;

    let expected = ["a", "b", "c", "d"] |> List.toIterator |> SortedSet.from |> SortedSet.toSet;

    expect (Set.equals union expected) |> toBeEqualToTrue;
  }),
];
