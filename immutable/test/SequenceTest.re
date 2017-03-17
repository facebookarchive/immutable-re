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

let test = describe "Sequence" [
  it "buffer" (fun () => {
    let test = [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] |> List.toSequence;

    let expected = [ [3, 2, 1], [6, 5, 4], [9, 8, 7] ] |> List.toSequence;
    expect (test |> Sequence.buffer 3 3 |> Sequence.equals expected) |> toBeEqualToTrue;

    let expected = [ [2, 1], [5, 4], [8, 7] ] |> List.toSequence;
    expect (test |> Sequence.buffer 2 3 |> Sequence.equals expected) |> toBeEqualToTrue;

    let expected = [ [2, 1], [3, 2], [4, 3], [5, 4], [6, 5], [7, 6], [8, 7], [9, 8] ] |> List.toSequence;
    expect (test |> Sequence.buffer 2 1 |> Sequence.equals expected) |> toBeEqualToTrue;

    let test = [] |> List.toSequence;
    expect (test |> Sequence.buffer 3 3 |> Sequence.equals Sequence.empty) |> toBeEqualToTrue;
  }),
  it "compare" (fun () => {
    ()
  }),
  it "concat" (fun () => {
    let test = Sequence.concat [
      [3, 2, 1] |> List.toSequence,
      [6, 5, 4] |> List.toSequence,
      [9, 8, 7] |> List.toSequence,
    ];
    let expected = [3, 2, 1, 6, 5, 4, 9, 8, 7] |> List.toSequence;
    expect (test |> Sequence.equals expected) |> toBeEqualToTrue;
  }),
  it "concatAll" (fun () => {
    let test = [
      [3, 2, 1] |> List.toSequence,
      [6, 5, 4] |> List.toSequence,
      [9, 8, 7] |> List.toSequence,
    ] |> List.toSequence |> Sequence.concatAll;

    let expected = [3, 2, 1, 6, 5, 4, 9, 8, 7] |> List.toSequence;
    expect (test |> Sequence.equals expected) |> toBeEqualToTrue;
  }),
  it "concatMap" (fun () => {
    let test = [1, 1, 1] |> List.toSequence |> Sequence.concatMap (
      fun _ => [3, 3, 3] |> List.toSequence
    );
    let expected = [3, 3, 3, 3, 3, 3, 3, 3, 3] |> List.toSequence;
    expect (test |> Sequence.equals expected) |> toBeEqualToTrue;
  }),
  it "contains" (fun () => {
    let seq = IntRange.create 0 10 |> IntRange.toSequence;

    expect (seq |> Sequence.contains (-1)) |> toBeEqualToFalse;
    expect (seq |> Sequence.contains 1) |> toBeEqualToTrue;
    expect (seq |> Sequence.contains 11) |> toBeEqualToFalse;
  }),
  it "count" (fun () => {
    let seq = IntRange.create 0 10 |> IntRange.toSequence;
    expect (seq |> Sequence.count) |> toBeEqualToInt 10;
  }),
  it "defer" (fun () => {
    ()
  }),
  it "distinctUntilChanged" (fun () => {
    let test = [ "1", "1", "1", "2", "2", "2", "3", "3", "4", "4" ]
      |> List.toSequence
      |> Sequence.distinctUntilChanged;
    expect test |> toBeEqualToSequenceOfString (List.toSequence [ "1", "2", "3", "4" ])
  }),
  it "doOnNext" (fun () => {
    ()
  }),
  it "equals" (fun () => {
    IntRange.create 0 10
      |> IntRange.toSequence
      |> Sequence.equals (IntRange.create 0 10 |> IntRange.toSequence)
      |> expect
      |> toBeEqualToTrue;

    IntRange.create 0 9
      |> IntRange.toSequence
      |> Sequence.equals (IntRange.create 0 10 |> IntRange.toSequence)
      |> expect
      |> toBeEqualToFalse;

    IntRange.create 0 11
      |> IntRange.toSequence
      |> Sequence.equals (IntRange.create 0 10 |> IntRange.toSequence)
      |> expect
      |> toBeEqualToFalse;

    IntRange.create 1 10
      |> IntRange.toSequence
      |> Sequence.equals (IntRange.create 0 10 |> IntRange.toSequence)
      |> expect
      |> toBeEqualToFalse;
  }),
  it "every" (fun () => {
    expect ([1, 1, 1] |> List.toSequence |> Sequence.every (fun _ => true )) |> toBeEqualToTrue;
    expect ([1, 2, 1] |> List.toSequence |> Sequence.every (fun x => x != 2 )) |> toBeEqualToFalse;
    expect (Sequence.empty |> Sequence.every (fun _ => true )) |> toBeEqualToTrue;
  }),
  it "filter" (fun () => {
    let filterOdd = fun x => x mod 2 != 0;
    let test = [1, 2, 4, 3, 5, 6] |> List.toSequence |> Sequence.filter filterOdd;
    let expected = [1, 3, 5] |> List.toSequence;

    expect (test |> Sequence.equals expected) |> toBeEqualToTrue;
  }),
  it "find" (fun () => {
    let seq = IntRange.create 0 10 |> IntRange.toSequence;
    expect (Sequence.find (fun a => a == 3) seq) |> toBeEqualToInt 3;
    defer (fun () => Sequence.find (fun a => a == 11) seq) |> throws;
  }),
  it "first" (fun () => {
    let seq = IntRange.create 0 10 |> IntRange.toSequence;
    expect (Sequence.first seq) |> toBeEqualToInt 0;
    defer (fun () => Sequence.first Sequence.empty) |> throws;
  }),
  it "flatMap" (fun () => {
    let test = [1, 1, 1] |> List.toSequence |> Sequence.flatMap (
      fun _ => [3, 3, 3] |> List.toSequence
    );
    let expected = [3, 3, 3, 3, 3, 3, 3, 3, 3] |> List.toSequence;
    expect (test |> Sequence.equals expected) |> toBeEqualToTrue;
  }),
  it "flatten" (fun () => {
    let test = [ [ "0", "1", "2" ], [ "3", "4", "5" ], [ "6", "7", "8" ] ]
      |> List.toSequence
      |> Sequence.map List.toSequence
      |> Sequence.flatten;

    let expected = [ "0", "1", "2", "3", "4", "5", "6", "7", "8" ] |> List.toSequence;

    expect test |> toBeEqualToSequenceOfString expected;
  }),
  it "forEach" (fun () => {
    ()
  }),
  it "generate" (fun () => {
    ()
  }),
  it "get" (fun () => {
    expect (
      IntRange.create 0 10
      |> IntRange.toSequence
      |> Sequence.get 2
    ) |> toBeEqualToInt 2;
    defer (fun () => IntRange.create 0 10 |> IntRange.toSequence |> Sequence.get (-10)) |> throws;
    defer (fun () => IntRange.create 0 10 |> IntRange.toSequence |> Sequence.get 20) |> throws;
  }),
  it "hash" (fun () => {
    ()
  }),
  it "isEmpty" (fun () => {
    expect (Sequence.isEmpty Sequence.empty) |> toBeEqualToTrue;
    expect (Sequence.isEmpty (List.toSequence [1])) |> toBeEqualToFalse;
  }),
  it "isNotEmpty" (fun () => {
    expect (Sequence.isNotEmpty Sequence.empty) |> toBeEqualToFalse;
    expect (Sequence.isNotEmpty (List.toSequence [1])) |> toBeEqualToTrue;
  }),
  it "last" (fun () => {
    let seq = IntRange.create 0 10 |> IntRange.toSequence;
    expect (Sequence.last seq) |> toBeEqualToInt 9;
    defer (fun () => Sequence.last Sequence.empty) |> throws;
  }),
  it "map" (fun () => {
    IntRange.create 0 10
      |> IntRange.toSequence
      |> Sequence.map (fun i => i + 5)
      |> expect
      |> toBeEqualToSequenceOfInt (IntRange.create 5 10 |> IntRange.toSequence);
  }),
  it "none" (fun () => {
    expect ([1, 1, 1] |> List.toSequence |> Sequence.none (fun _ => false )) |> toBeEqualToTrue;
    expect ([1, 2, 1] |> List.toSequence |> Sequence.none (fun x => x != 2 )) |> toBeEqualToFalse;
    expect (Sequence.empty |> Sequence.none (fun _ => true )) |> toBeEqualToTrue;
  }),
  it "reduce" (fun () => {
    ()
  }),
  it "repeat" (fun () => {
    let seq = Sequence.repeat 1 |> Sequence.take  4;
    expect seq |> toBeEqualToSequenceOfInt ([1,1,1,1] |> List.toSequence);
  }),
  it "return" (fun () => {
    Sequence.return 1 |> Sequence.first |> expect |> toBeEqualToInt 1;
  }),
  it "scan" (fun () => {
    ()
  }),
  it "skip" (fun () => {
    let seq = IntRange.create 0 10 |> IntRange.toSequence;
    (Sequence.skip 5 seq) |> expect |> toBeEqualToSequenceOfInt (IntRange.create 5 5 |> IntRange.toSequence);
  }),
  it "skipWhile" (fun () => {
    let seq = IntRange.create 0 10 |> IntRange.toSequence;
    (Sequence.skipWhile (fun x => x < 5) seq) |> expect |> toBeEqualToSequenceOfInt (
      IntRange.create 5 5 |> IntRange.toSequence
    );
  }),
  it "some" (fun () => {
    expect ([1, 1, 1] |> List.toSequence |> Sequence.some (fun x => x == 2 )) |> toBeEqualToFalse;
    expect ([1, 2, 1] |> List.toSequence |> Sequence.some (fun x => x == 2 )) |> toBeEqualToTrue;
    expect (Sequence.empty |> Sequence.some (fun _ => true )) |> toBeEqualToFalse;
  }),
  it "startWith" (fun () => {
    let seq = Sequence.startWith 0 (IntRange.create 1 10 |> IntRange.toSequenceReversed);
    expect (Sequence.first seq) |> toBeEqualToInt 0;
  }),
  it "take" (fun () => {
    let seq = IntRange.create 0 10 |> IntRange.toSequence;
    (Sequence.take 5 seq) |> expect |> toBeEqualToSequenceOfInt (
      IntRange.create 0 5 |> IntRange.toSequence
    );
  }),
  it "takeWhile" (fun () => {
    let seq = IntRange.create 0 10 |> IntRange.toSequence;
    (Sequence.takeWhile (fun x => x < 5) seq) |> expect |> toBeEqualToSequenceOfInt (
      IntRange.create 0 5 |> IntRange.toSequence
    );
  }),
  it "tryFind" (fun () => {
    let seq = IntRange.create 0 10 |> IntRange.toSequence;
    expect (Sequence.tryFind (fun a => a == 3) seq) |> toBeEqualToSomeOfInt 3;
    expect (Sequence.tryFind (fun a => a == 11) seq) |> toBeEqualToNoneOfInt;
  }),
  it "tryFirst" (fun () => {
    expect (Sequence.empty |> Sequence.tryFirst) |> toBeEqualToNoneOfInt;
    expect ([1, 2, 3] |> List.toSequence |> Sequence.tryFirst) |> toBeEqualToSomeOfInt 1;
  }),
  it "tryGet" (fun () => {
    expect (IntRange.create 0 10 |> IntRange.toSequence |> Sequence.tryGet 2) |> toBeEqualToSomeOfInt 2;
    expect (IntRange.create 0 10 |> IntRange.toSequence |> Sequence.tryGet (-10)) |> toBeEqualToNoneOfInt;
    expect (IntRange.create 0 10 |> IntRange.toSequence |> Sequence.tryGet 20) |> toBeEqualToNoneOfInt;
  }),
  it "tryLast" (fun () => {
    let seq = IntRange.create 0 10 |> IntRange.toSequence;
    expect (Sequence.tryLast seq) |> toBeEqualToSomeOfInt 9;
    expect (Sequence.tryLast Sequence.empty) |> toBeEqualToNoneOfInt;
  }),

  describe "zip" [],

  describe "zip2" [
    it "first is longer" (fun () => {
      let zipped = Sequence.zip2
        ([1, 2, 3] |> List.toSequence)
        ([4, 5] |> List.toSequence);

      expect (zipped |> Sequence.equals ([(1, 4), (2, 5)] |> List.toSequence)) |> toBeEqualToTrue;
    }),
    it "snd is longer" (fun () => {
      let zipped = Sequence.zip2
        ([1, 2] |> List.toSequence)
        ([4, 5, 6] |> List.toSequence);

      expect (zipped |> Sequence.equals ([(1, 4), (2, 5)] |> List.toSequence)) |> toBeEqualToTrue;
    }),
    it "same length" (fun () => {
      let zipped = Sequence.zip2
        ([1, 2, 3] |> List.toSequence)
        ([4, 5, 6] |> List.toSequence);

      expect (zipped |> Sequence.equals ([(1, 4), (2, 5), (3, 6)] |> List.toSequence)) |> toBeEqualToTrue;
    }),
  ],
  describe "zip3" [],

  describe "zipLongest" [],

  describe "zipLongest2" [
    it "first is longer" (fun () => {
      let zipped = Sequence.zipLongest2
        ([1, 2, 3] |> List.toSequence)
        ([4, 5] |> List.toSequence);

      expect (
        zipped |> Sequence.equals
          ([(Some 1, Some 4), (Some 2, Some 5), (Some 3, None)] |> List.toSequence)
      ) |> toBeEqualToTrue;
    }),
    it "snd is longer" (fun () => {
      let zipped = Sequence.zipLongest2
        ([1, 2,] |> List.toSequence)
        ([4, 5, 6, 7, 8] |> List.toSequence);

      expect (
        zipped |> Sequence.equals
          ([(Some 1, Some 4), (Some 2, Some 5), (None, Some 6), (None, Some 7), (None, Some 8)] |> List.toSequence)
      ) |> toBeEqualToTrue;
    }),
    it "same length" (fun () => {
      let zipped = Sequence.zipLongest2
        ([1, 2, 3] |> List.toSequence)
        ([4, 5, 6] |> List.toSequence);

      expect (
        zipped |> Sequence.equals
          ([(Some 1, Some 4), (Some 2, Some 5), (Some 3, Some 6)] |> List.toSequence)
      ) |> toBeEqualToTrue;
    }),
  ],
];
