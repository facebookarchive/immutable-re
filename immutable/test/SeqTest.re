open Immutable;
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
  it "compare" (fun () => {
    ()
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
  it "concatAll" (fun () => {
    let test = [
      [3, 2, 1] |> List.toSeq,
      [6, 5, 4] |> List.toSeq,
      [9, 8, 7] |> List.toSeq,
    ] |> List.toSeq |> Seq.concatAll;

    let expected = [3, 2, 1, 6, 5, 4, 9, 8, 7] |> List.toSeq;
    expect (test |> Seq.equals expected) |> toBeEqualToTrue;
  }),
  it "concatMap" (fun () => {
    let test = [1, 1, 1] |> List.toSeq |> Seq.concatMap (
      fun _ => [3, 3, 3] |> List.toSeq
    );
    let expected = [3, 3, 3, 3, 3, 3, 3, 3, 3] |> List.toSeq;
    expect (test |> Seq.equals expected) |> toBeEqualToTrue;
  }),
  it "contains" (fun () => {
    let seq = ContiguousIntSet.create 0 10 |> ContiguousIntSet.toSeq;

    expect (seq |> Seq.contains (-1)) |> toBeEqualToFalse;
    expect (seq |> Seq.contains 1) |> toBeEqualToTrue;
    expect (seq |> Seq.contains 11) |> toBeEqualToFalse;
  }),
  it "count" (fun () => {
    let seq = ContiguousIntSet.create 0 10 |> ContiguousIntSet.toSeq;
    expect (seq |> Seq.count) |> toBeEqualToInt 10;
  }),
  it "defer" (fun () => {
    ()
  }),
  it "distinctUntilChanged" (fun () => {
    let test = [ "1", "1", "1", "2", "2", "2", "3", "3", "4", "4" ]
      |> List.toSeq
      |> Seq.distinctUntilChanged;
    expect test |> toBeEqualToSeqOfString (List.toSeq [ "1", "2", "3", "4" ])
  }),
  it "doOnNext" (fun () => {
    ()
  }),
  it "equals" (fun () => {
    ContiguousIntSet.create 0 10
      |> ContiguousIntSet.toSeq
      |> Seq.equals (ContiguousIntSet.create 0 10 |> ContiguousIntSet.toSeq)
      |> expect
      |> toBeEqualToTrue;

    ContiguousIntSet.create 0 9
      |> ContiguousIntSet.toSeq
      |> Seq.equals (ContiguousIntSet.create 0 10 |> ContiguousIntSet.toSeq)
      |> expect
      |> toBeEqualToFalse;

    ContiguousIntSet.create 0 11
      |> ContiguousIntSet.toSeq
      |> Seq.equals (ContiguousIntSet.create 0 10 |> ContiguousIntSet.toSeq)
      |> expect
      |> toBeEqualToFalse;

    ContiguousIntSet.create 1 10
      |> ContiguousIntSet.toSeq
      |> Seq.equals (ContiguousIntSet.create 0 10 |> ContiguousIntSet.toSeq)
      |> expect
      |> toBeEqualToFalse;
  }),
  it "every" (fun () => {
    expect ([1, 1, 1] |> List.toSeq |> Seq.every (fun _ => true )) |> toBeEqualToTrue;
    expect ([1, 2, 1] |> List.toSeq |> Seq.every (fun x => x != 2 )) |> toBeEqualToFalse;
    expect (Seq.empty |> Seq.every (fun _ => true )) |> toBeEqualToTrue;
  }),
  it "filter" (fun () => {
    let filterOdd = fun x => x mod 2 != 0;
    let test = [1, 2, 4, 3, 5, 6] |> List.toSeq |> Seq.filter filterOdd;
    let expected = [1, 3, 5] |> List.toSeq;

    expect (test |> Seq.equals expected) |> toBeEqualToTrue;
  }),
  it "find" (fun () => {
    let seq = ContiguousIntSet.create 0 10 |> ContiguousIntSet.toSeq;
    expect (Seq.find (fun a => a == 3) seq) |> toBeEqualToInt 3;
    defer (fun () => Seq.find (fun a => a == 11) seq) |> throws;
  }),
  it "first" (fun () => {
    let seq = ContiguousIntSet.create 0 10 |> ContiguousIntSet.toSeq;
    expect (Seq.first seq) |> toBeEqualToInt 0;
    defer (fun () => Seq.first Seq.empty) |> throws;
  }),
  it "flatMap" (fun () => {
    let test = [1, 1, 1] |> List.toSeq |> Seq.flatMap (
      fun _ => [3, 3, 3] |> List.toSeq
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
  it "forEach" (fun () => {
    ()
  }),
  it "generate" (fun () => {
    ()
  }),
  it "get" (fun () => {
    expect (
      ContiguousIntSet.create 0 10
      |> ContiguousIntSet.toSeq
      |> Seq.get 2
    ) |> toBeEqualToInt 2;
    defer (fun () => ContiguousIntSet.create 0 10 |> ContiguousIntSet.toSeq |> Seq.get (-10)) |> throws;
    defer (fun () => ContiguousIntSet.create 0 10 |> ContiguousIntSet.toSeq |> Seq.get 20) |> throws;
  }),
  it "hash" (fun () => {
    ()
  }),
  it "isEmpty" (fun () => {
    expect (Seq.isEmpty Seq.empty) |> toBeEqualToTrue;
    expect (Seq.isEmpty (List.toSeq [1])) |> toBeEqualToFalse;
  }),
  it "isNotEmpty" (fun () => {
    expect (Seq.isNotEmpty Seq.empty) |> toBeEqualToFalse;
    expect (Seq.isNotEmpty (List.toSeq [1])) |> toBeEqualToTrue;
  }),
  it "last" (fun () => {
    let seq = ContiguousIntSet.create 0 10 |> ContiguousIntSet.toSeq;
    expect (Seq.last seq) |> toBeEqualToInt 9;
    defer (fun () => Seq.last Seq.empty) |> throws;
  }),
  it "map" (fun () => {
    ContiguousIntSet.create 0 10
      |> ContiguousIntSet.toSeq
      |> Seq.map (fun i => i + 5)
      |> expect
      |> toBeEqualToSeqOfInt (ContiguousIntSet.create 5 10 |> ContiguousIntSet.toSeq);
  }),
  it "none" (fun () => {
    expect ([1, 1, 1] |> List.toSeq |> Seq.none (fun _ => false )) |> toBeEqualToTrue;
    expect ([1, 2, 1] |> List.toSeq |> Seq.none (fun x => x != 2 )) |> toBeEqualToFalse;
    expect (Seq.empty |> Seq.none (fun _ => true )) |> toBeEqualToTrue;
  }),
  it "reduce" (fun () => {
    ()
  }),
  it "repeat" (fun () => {
    let seq = Seq.repeat 1 (Some 4);
    expect seq |> toBeEqualToSeqOfInt ([1,1,1,1] |> List.toSeq);
  }),
  it "return" (fun () => {
    Seq.return 1 |> Seq.first |> expect |> toBeEqualToInt 1;
  }),
  it "scan" (fun () => {
    ()
  }),
  it "skip" (fun () => {
    let seq = ContiguousIntSet.create 0 10 |> ContiguousIntSet.toSeq;
    (Seq.skip 5 seq) |> expect |> toBeEqualToSeqOfInt (ContiguousIntSet.create 5 5 |> ContiguousIntSet.toSeq);
  }),
  it "skipWhile" (fun () => {
    let seq = ContiguousIntSet.create 0 10 |> ContiguousIntSet.toSeq;
    (Seq.skipWhile (fun x => x < 5) seq) |> expect |> toBeEqualToSeqOfInt (
      ContiguousIntSet.create 5 5 |> ContiguousIntSet.toSeq
    );
  }),
  it "some" (fun () => {
    expect ([1, 1, 1] |> List.toSeq |> Seq.some (fun x => x == 2 )) |> toBeEqualToFalse;
    expect ([1, 2, 1] |> List.toSeq |> Seq.some (fun x => x == 2 )) |> toBeEqualToTrue;
    expect (Seq.empty |> Seq.some (fun _ => true )) |> toBeEqualToFalse;
  }),
  it "startWith" (fun () => {
    let seq = Seq.startWith 0 (ContiguousIntSet.create 1 10 |> ContiguousIntSet.toSeqReversed);
    expect (Seq.first seq) |> toBeEqualToInt 0;
  }),
  it "take" (fun () => {
    let seq = ContiguousIntSet.create 0 10 |> ContiguousIntSet.toSeq;
    (Seq.take 5 seq) |> expect |> toBeEqualToSeqOfInt (
      ContiguousIntSet.create 0 5 |> ContiguousIntSet.toSeq
    );
  }),
  it "takeWhile" (fun () => {
    let seq = ContiguousIntSet.create 0 10 |> ContiguousIntSet.toSeq;
    (Seq.takeWhile (fun x => x < 5) seq) |> expect |> toBeEqualToSeqOfInt (
      ContiguousIntSet.create 0 5 |> ContiguousIntSet.toSeq
    );
  }),
  it "tryFind" (fun () => {
    let seq = ContiguousIntSet.create 0 10 |> ContiguousIntSet.toSeq;
    expect (Seq.tryFind (fun a => a == 3) seq) |> toBeEqualToSomeOfInt 3;
    expect (Seq.tryFind (fun a => a == 11) seq) |> toBeEqualToNoneOfInt;
  }),
  it "tryFirst" (fun () => {
    expect (Seq.empty |> Seq.tryFirst) |> toBeEqualToNoneOfInt;
    expect ([1, 2, 3] |> List.toSeq |> Seq.tryFirst) |> toBeEqualToSomeOfInt 1;
  }),
  it "tryGet" (fun () => {
    expect (ContiguousIntSet.create 0 10 |> ContiguousIntSet.toSeq |> Seq.tryGet 2) |> toBeEqualToSomeOfInt 2;
    expect (ContiguousIntSet.create 0 10 |> ContiguousIntSet.toSeq |> Seq.tryGet (-10)) |> toBeEqualToNoneOfInt;
    expect (ContiguousIntSet.create 0 10 |> ContiguousIntSet.toSeq |> Seq.tryGet 20) |> toBeEqualToNoneOfInt;
  }),
  it "tryLast" (fun () => {
    let seq = ContiguousIntSet.create 0 10 |> ContiguousIntSet.toSeq;
    expect (Seq.tryLast seq) |> toBeEqualToSomeOfInt 9;
    expect (Seq.tryLast Seq.empty) |> toBeEqualToNoneOfInt;
  }),

  describe "zip" [],

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
  describe "zip3" [],

  describe "zipLongest" [],

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
];
