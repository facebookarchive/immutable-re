open Immutable;
open ReUnit;
open ReUnit.Expect;
open ReUnit.Test;

let test = describe "Seq" [
  it "buffer" (fun () => {
    let test = [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] |> List.toSeq;

    let expected = [ [3, 2, 1], [6, 5, 4], [9, 8, 7] ] |> List.toSeq;
    expect (test |> Seq.buffer 3 3 |> Seq.equals expected) |> toBeEqualToTrue;

    let expected = [ [2, 1], [5, 4], [8, 7] ] |> List.toSeq;
    expect (test |> Seq.buffer 2 3 |> Seq.equals expected) |> toBeEqualToTrue;

    let expected = [ [2, 1], [3, 2], [4, 3], [5, 4], [6, 5], [7, 6], [8, 7], [9, 8] ] |> List.toSeq;
    expect (test |> Seq.buffer 2 1 |> Seq.equals expected) |> toBeEqualToTrue;

    let test = [] |> List.toSeq;
    expect (test |> Seq.buffer 3 3 |> Seq.equals Seq.empty) |> toBeEqualToTrue;
  }),
  it "concat" (fun () => {
    let test = Seq.concat [
      [3, 2, 1] |> List.toSeq,
      [6, 5, 4] |> List.toSeq,
      [9, 8, 7] |> List.toSeq,
    ];
    let expected = [3, 2, 1, 6, 5, 4, 9, 8, 7] |> List.toSeq;
    expect (test |> Seq.equals expected) |> toBeEqualToTrue;
  }),
  it "concatMap" (fun () => {
    let test = [1, 1, 1] |> List.toSeq |> Seq.concatMap (
      fun x => [3, 3, 3] |> List.toSeq
    );
    let expected = [3, 3, 3, 3, 3, 3, 3, 3, 3] |> List.toSeq;
    expect (test |> Seq.equals expected) |> toBeEqualToTrue;
  }),
  describe "defer" [],
  it "distinctUntilChanged" (fun () => {
    let test = [ "1", "1", "1", "2", "2", "2", "3", "3", "4", "4" ]
      |> List.toSeq
      |> Seq.distinctUntilChanged;
    expect test |> toBeEqualToSeqOfString (List.toSeq [ "1", "2", "3", "4" ])
  }),
  describe "every" [
    it "all items true" (fun () => {
      expect ([1, 1, 1] |> List.toSeq |> Seq.every (fun x => true )) |> toBeEqualToTrue;
    }),
    it "some items false" (fun () => {
      expect ([1, 2, 1] |> List.toSeq |> Seq.every (fun x => x != 2 )) |> toBeEqualToFalse;
    }),
  ],
  it "filter" (fun () => {
    let filterOdd = fun x => x mod 2 != 0;
    let test = [1, 2, 4, 3, 5, 6] |> List.toSeq |> Seq.filter filterOdd;
    let expected = [1, 3, 5] |> List.toSeq;

    expect (test |> Seq.equals expected) |> toBeEqualToTrue;
  }),
  it "flatMap" (fun () => {
    let test = [1, 1, 1] |> List.toSeq |> Seq.flatMap (
      fun x => [3, 3, 3] |> List.toSeq
    );
    let expected = [3, 3, 3, 3, 3, 3, 3, 3, 3] |> List.toSeq;
    expect (test |> Seq.equals expected) |> toBeEqualToTrue;
  }),
  it "flatten" (fun () => {
    let test = [ [ "0", "1", "2" ], [ "3", "4", "5" ], [ "6", "7", "8" ] ]
      |> List.toSeq
      |> Seq.map List.toSeq
      |> Seq.flatten;

    let expected = [ "0", "1", "2", "3", "4", "5", "6", "7", "8" ] |> List.toSeq;

    expect test |> toBeEqualToSeqOfString expected;
  }),
  describe "hash" [],
  describe "isEmpty" [],
  describe "map" [],
  describe "none" [],
  describe "ofValue" [],
  describe "range" [],
  describe "reduce" [],
  describe "repeat" [],
  describe "scan" [],
  describe "skip" [],
  describe "skipUntil" [],
  describe "skipWhile" [],
  describe "take" [],
  describe "takeUntil" [],
  describe "takeWhile" [],
  describe "toArray" [],
  describe "toReversedList" [],
  describe "tryFind" [],

  describe "tryFirst" [
    it "is none when seq is empty" (fun () => {
      expect (Seq.empty |> Seq.tryFirst) |> toBeEqualToNoneOfInt;
    }),
    it "is Some fst when seq has values" (fun () => {
      expect ([1, 2, 3] |> List.toSeq |> Seq.tryFirst) |> toBeEqualToSomeOfInt 1;
    }),
  ],
  describe "tryGet" [
    it "in range" (fun () =>
      expect (Seq.inRange 0 (Some 10) 1 |> Seq.tryGet 2) |> toBeEqualToSomeOfInt 2
    ),
    it "out of range" (fun () => {
      expect (Seq.inRange 0 (Some 10) 1 |> Seq.tryGet (-10)) |> toBeEqualToNoneOfInt;
      expect (Seq.inRange 0 (Some 10) 1 |> Seq.tryGet 20) |> toBeEqualToNoneOfInt;
    }),
  ],

  describe "zip2" [
    it "first is longer" (fun () => {
      let zipped = Seq.zip2
        ([1, 2, 3] |> List.toSeq)
        ([4, 5] |> List.toSeq);

      expect (zipped |> Seq.equals ([(1, 4), (2, 5)] |> List.toSeq)) |> toBeEqualToTrue;
    }),
    it "snd is longer" (fun () => {
      let zipped = Seq.zip2
        ([1, 2] |> List.toSeq)
        ([4, 5, 6] |> List.toSeq);

      expect (zipped |> Seq.equals ([(1, 4), (2, 5)] |> List.toSeq)) |> toBeEqualToTrue;
    }),
    it "same length" (fun () => {
      let zipped = Seq.zip2
        ([1, 2, 3] |> List.toSeq)
        ([4, 5, 6] |> List.toSeq);

      expect (zipped |> Seq.equals ([(1, 4), (2, 5), (3, 6)] |> List.toSeq)) |> toBeEqualToTrue;
    }),
  ],
  describe "zipLongest2" [
    it "first is longer" (fun () => {
      let zipped = Seq.zipLongest2
        ([1, 2, 3] |> List.toSeq)
        ([4, 5] |> List.toSeq);

      expect (
        zipped |> Seq.equals
          ([(Some 1, Some 4), (Some 2, Some 5), (Some 3, None)] |> List.toSeq)
      ) |> toBeEqualToTrue;
    }),
    it "snd is longer" (fun () => {
      let zipped = Seq.zipLongest2
        ([1, 2,] |> List.toSeq)
        ([4, 5, 6, 7, 8] |> List.toSeq);

      expect (
        zipped |> Seq.equals
          ([(Some 1, Some 4), (Some 2, Some 5), (None, Some 6), (None, Some 7), (None, Some 8)] |> List.toSeq)
      ) |> toBeEqualToTrue;
    }),
    it "same length" (fun () => {
      let zipped = Seq.zipLongest2
        ([1, 2, 3] |> List.toSeq)
        ([4, 5, 6] |> List.toSeq);

      expect (
        zipped |> Seq.equals
          ([(Some 1, Some 4), (Some 2, Some 5), (Some 3, Some 6)] |> List.toSeq)
      ) |> toBeEqualToTrue;
    }),
  ],
  describe "zip3" [],
];
