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
    let col = Set.inRange 0 20 1;
    let dup = Set.inRange 0 20 1;
    let rev = Set.inRange 19 20 (-1);

    expect (Set.equals col dup) |> toBeEqualToTrue;
    expect (Set.equals col col) |> toBeEqualToTrue;
    expect (Set.equals col rev) |> toBeEqualToTrue;
    expect (Set.equals col Set.empty) |> toBeEqualToFalse;
    expect (Set.equals col (Set.inRange 1 20 1)) |> toBeEqualToFalse;
    expect (Set.equals (Set.inRange 0 0 1) Set.empty) |> toBeEqualToTrue;
  }),
  it "hash" (fun () => {
    let col = Set.inRange 0 20 1;
    let dup = Set.inRange 19 20 (-1);

    expect (Set.hash col) |> toBeEqualToInt (Set.hash col);
    expect (Set.hash col) |> toBeEqualToInt (Set.hash dup);
  }),
  describe "inRange" [
    it "contains" (fun () => {
      let denseSet = Set.inRange (-5) 20 1;
      let denseSeq = Seq.inRange (-5) (Some 20) 1;
      denseSeq |> Seq.forEach (fun v => {
        expect (denseSet |> Set.contains v) |> toBeEqualToTrue;
      });

      let sparsePosSet = Set.inRange (-5) 20 3;
      let sparsePosSeq = Seq.inRange (-5) (Some 20) 3;
      sparsePosSeq |> Seq.forEach (fun v => {
        expect (sparsePosSet |> Set.contains v) |> toBeEqualToTrue;
      });

      let sparseNegSet = Set.inRange (-5) 20 (-3);
      let sparseNegSeq = Seq.inRange (-5) (Some 20) (-3);
      sparseNegSeq |> Seq.forEach (fun v => {
        expect (sparseNegSet |> Set.contains v) |> toBeEqualToTrue;
      });
    }),
    it "count" (fun () => {
      let emptySet = Set.inRange 0 0 1;
      expect (emptySet |> Set.count) |> toBeEqualToInt 0;

      let denseSet = Set.inRange (-5) 20 1;
      expect (denseSet |> Set.count) |> toBeEqualToInt 20;

      let sparsePosSet = Set.inRange (-5) 20 3;
      expect (sparsePosSet |> Set.count) |> toBeEqualToInt 20;

      let sparseNegSet = Set.inRange (-5) 20 (-3);
      expect (sparseNegSet |> Set.count) |> toBeEqualToInt 20;
    }),
    /* The other functions implemented by inRange are straight wrappers around the Seq apis. */
  ],
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
    expect (Set.inRange 0 1 1 |> Set.isEmpty) |> toBeEqualToFalse;
  }),
  it "isNotEmpty" (fun () => {
    expect (Set.empty |> Set.isNotEmpty) |> toBeEqualToFalse;
    expect (Set.inRange 0 1 1 |> Set.isNotEmpty) |> toBeEqualToTrue;
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
