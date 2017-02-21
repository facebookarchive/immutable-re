open Functions;
open Immutable;
open ReUnit;
open ReUnit.Expect;
open ReUnit.Test;

let test = describe "Collection" [
  describe "empty" [
    it "contains" (fun () => {
      expect (Collection.empty |> Collection.contains 0) |> toBeEqualToFalse;
    }),
    it "count" (fun () => {
      expect (Collection.empty |> Collection.count) |> toBeEqualToInt 0;
    }),
    it "every" (fun () => {
      expect (Collection.empty |> Collection.every Functions.alwaysTrue) |> toBeEqualToFalse;
    }),
    it "find" (fun () => {
      defer (fun () => Collection.empty |> Collection.find Functions.alwaysTrue) |> throws;
    }),
    it "forEach" (fun () => {
      Collection.empty |> Collection.forEach (Functions.alwaysFailWith "forEach had values");
    }),
    it "none" (fun () => {
      expect (Collection.empty |> Collection.none Functions.alwaysTrue) |> toBeEqualToTrue;
    }),
    it "reduce" (fun () => {
      Collection.empty |> Collection.reduce (fun _ => Functions.alwaysFailWith "forEach had values") ();
    }),
    it "some" (fun () => {
      expect (Collection.empty |> Collection.some Functions.alwaysTrue) |> toBeEqualToFalse;
    }),
    it "toSeq" (fun () => {
      expect (Collection.empty |> Collection.toSeq) |> toBeEqualToEmptySeqOfString;
    }),
    it "tryFind" (fun () => {
      expect (Collection.empty |> Collection.tryFind Functions.alwaysTrue) |> toBeEqualToNoneOfInt;
    }),
  ],
  it "equals" (fun () => {
    let col = Collection.inRange 0 20 1;
    let dup = Collection.inRange 0 20 1;
    let rev = Collection.inRange 19 20 (-1);

    expect (Collection.equals col dup) |> toBeEqualToTrue;
    expect (Collection.equals col col) |> toBeEqualToTrue;
    expect (Collection.equals col rev) |> toBeEqualToTrue;
    expect (Collection.equals col Collection.empty) |> toBeEqualToFalse;
    expect (Collection.equals col (Collection.inRange 1 20 1)) |> toBeEqualToFalse;
    expect (Collection.equals (Collection.inRange 0 0 1) Collection.empty) |> toBeEqualToTrue;
  }),
  it "hash" (fun () => {
    let col = Collection.inRange 0 20 1;
    let dup = Collection.inRange 19 20 (-1);

    expect (Collection.hash col) |> toBeEqualToInt (Collection.hash col);
    expect (Collection.hash col) |> toBeEqualToInt (Collection.hash dup);
  }),
  describe "inRange" [
    it "contains" (fun () => {
      let denseCollection = Collection.inRange (-5) 20 1;
      let denseSeq = Seq.inRange (-5) (Some 20) 1;
      denseSeq |> Seq.forEach (fun v => {
        expect (denseCollection |> Collection.contains v) |> toBeEqualToTrue;
      });

      let sparsePosCollection = Collection.inRange (-5) 20 3;
      let sparsePosSeq = Seq.inRange (-5) (Some 20) 3;
      sparsePosSeq |> Seq.forEach (fun v => {
        expect (sparsePosCollection |> Collection.contains v) |> toBeEqualToTrue;
      });

      let sparseNegCollection = Collection.inRange (-5) 20 (-3);
      let sparseNegSeq = Seq.inRange (-5) (Some 20) (-3);
      sparseNegSeq |> Seq.forEach (fun v => {
        expect (sparseNegCollection |> Collection.contains v) |> toBeEqualToTrue;
      });
    }),
    it "count" (fun () => {
      let emptyCollection = Collection.inRange 0 0 1;
      expect (emptyCollection |> Collection.count) |> toBeEqualToInt 0;

      let denseCollection = Collection.inRange (-5) 20 1;
      expect (denseCollection |> Collection.count) |> toBeEqualToInt 20;

      let sparsePosCollection = Collection.inRange (-5) 20 3;
      expect (sparsePosCollection |> Collection.count) |> toBeEqualToInt 20;

      let sparseNegCollection = Collection.inRange (-5) 20 (-3);
      expect (sparseNegCollection |> Collection.count) |> toBeEqualToInt 20;
    }),
    /* The other functions implemented by inRange are straight wrappers around the Seq apis. */
  ],
  it "intersect" (fun () => {
    let collectionA = ["a", "b", "c"] |> List.toSeq |> SortedSet.fromSeq |> SortedSet.toCollection;
    let collectionB = ["b", "c", "d"] |> List.toSeq |> SortedSet.fromSeq |> SortedSet.toCollection;

    let intersection = Collection.intersect collectionA collectionB
      |> SortedSet.fromSeq
      |> SortedSet.toCollection;

    let expected = ["b", "c"] |> List.toSeq |> SortedSet.fromSeq |> SortedSet.toCollection;

    expect (Collection.equals intersection expected) |> toBeEqualToTrue;
  }),
  it "isEmpty" (fun () => {
    expect (Collection.empty |> Collection.isEmpty) |> toBeEqualToTrue;
    expect (Collection.inRange 0 1 1 |> Collection.isEmpty) |> toBeEqualToFalse;
  }),
  it "isNotEmpty" (fun () => {
    expect (Collection.empty |> Collection.isNotEmpty) |> toBeEqualToFalse;
    expect (Collection.inRange 0 1 1 |> Collection.isNotEmpty) |> toBeEqualToTrue;
  }),
  it "subtract" (fun () => {
    let collectionA = ["a", "b", "c"] |> List.toSeq |> SortedSet.fromSeq |> SortedSet.toCollection;
    let collectionB = ["b", "c", "d"] |> List.toSeq |> SortedSet.fromSeq |> SortedSet.toCollection;

    let subtracted = Collection.subtract collectionA collectionB
      |> SortedSet.fromSeq
      |> SortedSet.toCollection;

    let expected = ["a"] |> List.toSeq |> SortedSet.fromSeq |> SortedSet.toCollection;

    expect (Collection.equals subtracted expected) |> toBeEqualToTrue;
  }),
  it "union" (fun () => {
    let collectionA = ["a", "b", "c"] |> List.toSeq |> SortedSet.fromSeq |> SortedSet.toCollection;
    let collectionB = ["b", "c", "d"] |> List.toSeq |> SortedSet.fromSeq |> SortedSet.toCollection;

    let union =  Collection.union collectionA collectionB
      |> SortedSet.fromSeq
      |> SortedSet.toCollection;

    let expected = ["a", "b", "c", "d"] |> List.toSeq |> SortedSet.fromSeq |> SortedSet.toCollection;

    expect (Collection.equals union expected) |> toBeEqualToTrue;
  }),
];
