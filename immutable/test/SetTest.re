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
    it "toSeq" (fun () => {
      expect (Set.empty |> Set.toSeq) |> toBeEqualToEmptySeqOfString;
    }),
    it "tryFind" (fun () => {
      expect (Set.empty |> Set.tryFind Functions.alwaysTrue) |> toBeEqualToNoneOfInt;
    }),
  ],
  it "equals" (fun () => {
    let col = ContiguousIntSet.create 0 20 |> ContiguousIntSet.toSet;
    let dup = ContiguousIntSet.create 0 20 |> ContiguousIntSet.toSet;
    let rev = ContiguousIntSet.create 0 20 |> ContiguousIntSet.toSet;

    expect (Set.equals col dup) |> toBeEqualToTrue;
    expect (Set.equals col col) |> toBeEqualToTrue;
    expect (Set.equals col rev) |> toBeEqualToTrue;
    expect (Set.equals col Set.empty) |> toBeEqualToFalse;
    expect (
      Set.equals col (ContiguousIntSet.create 1 20 |> ContiguousIntSet.toSet)
    ) |> toBeEqualToFalse;
    expect (
      Set.equals (ContiguousIntSet.empty |> ContiguousIntSet.toSet) Set.empty
    ) |> toBeEqualToTrue;
  }),
  it "hash" (fun () => {
    let col = ContiguousIntSet.create 0 20 |> ContiguousIntSet.toSet;
    let dup = ContiguousIntSet.create 0 20 |> ContiguousIntSet.toSet;

    expect (Set.hash col) |> toBeEqualToInt (Set.hash col);
    expect (Set.hash col) |> toBeEqualToInt (Set.hash dup);
  }),
  it "intersect" (fun () => {
    let setA = ["a", "b", "c"] |> List.toSeq |> SortedSet.fromSeq |> SortedSet.toSet;
    let setB = ["b", "c", "d"] |> List.toSeq |> SortedSet.fromSeq |> SortedSet.toSet;

    let intersection = Set.intersect setA setB
      |> SortedSet.fromSeq
      |> SortedSet.toSet;

    let expected = ["b", "c"] |> List.toSeq |> SortedSet.fromSeq |> SortedSet.toSet;

    expect (Set.equals intersection expected) |> toBeEqualToTrue;
  }),
  it "isEmpty" (fun () => {
    expect (Set.empty |> Set.isEmpty) |> toBeEqualToTrue;
    expect (
      ContiguousIntSet.create 0 1 |> ContiguousIntSet.toSet|> Set.isEmpty
    ) |> toBeEqualToFalse;
  }),
  it "isNotEmpty" (fun () => {
    expect (Set.empty |> Set.isNotEmpty) |> toBeEqualToFalse;
    expect (
      ContiguousIntSet.create 0 1 |> ContiguousIntSet.toSet |> Set.isNotEmpty
    ) |> toBeEqualToTrue;
  }),
  it "subtract" (fun () => {
    let setA = ["a", "b", "c"] |> List.toSeq |> SortedSet.fromSeq |> SortedSet.toSet;
    let setB = ["b", "c", "d"] |> List.toSeq |> SortedSet.fromSeq |> SortedSet.toSet;

    let subtracted = Set.subtract setA setB
      |> SortedSet.fromSeq
      |> SortedSet.toSet;

    let expected = ["a"] |> List.toSeq |> SortedSet.fromSeq |> SortedSet.toSet;

    expect (Set.equals subtracted expected) |> toBeEqualToTrue;
  }),
  it "union" (fun () => {
    let setA = ["a", "b", "c"] |> List.toSeq |> SortedSet.fromSeq |> SortedSet.toSet;
    let setB = ["b", "c", "d"] |> List.toSeq |> SortedSet.fromSeq |> SortedSet.toSet;

    let union =  Set.union setA setB
      |> SortedSet.fromSeq
      |> SortedSet.toSet;

    let expected = ["a", "b", "c", "d"] |> List.toSeq |> SortedSet.fromSeq |> SortedSet.toSet;

    expect (Set.equals union expected) |> toBeEqualToTrue;
  }),
];
