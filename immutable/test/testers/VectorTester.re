/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Printf;
open Immutable;
open ReUnit;
open ReUnit.Test;

let module Make = fun (Vector: Vector.S1) (Config: StackTester.Config) => {
  let module DequeTester = DequeTester.Make (Deque: Deque.S1) Config;

  let tests = [
    describe (sprintf "count: %i" Config.count) [
      describe "concat" [
        it "balanced segments" (fun () => {
          let segmentCount = Config.count / 2;

          let firstRange = IntRange.create start::0 count::segmentCount;
          let secondRange = IntRange.create start::segmentCount count::segmentCount;

          let firstVector = firstRange |> IntRange.toIterator |> Vector.from;
          let secondVector = secondRange |> IntRange.toIterator |> Vector.from;

          let concatted = Vector.concat [firstVector, secondVector];

          let expected = Sequence.concat [
            firstRange |> IntRange.toSequence,
            secondRange |> IntRange.toSequence,
          ];

          Sequence.zip2With
              (fun a b => a == b)
              (Vector.toSequence concatted)
              expected
            |> Sequence.toIterator
            |> Iterator.Reducer.every Functions.identity
            |> Expect.toBeEqualToTrue;
        }),
        it "left larger than right" (fun () => {
          let firstSegmentCount = Config.count / 4;
          let secondSegmentCount = Config.count - firstSegmentCount;

          let firstRange = IntRange.create start::0 count::firstSegmentCount;
          let secondRange = IntRange.create start::firstSegmentCount count::secondSegmentCount;

          let firstVector = firstRange |> IntRange.toIterator |> Vector.from;
          let secondVector = secondRange |> IntRange.toIterator |> Vector.from;

          let concatted = Vector.concat [firstVector, secondVector];

          let expected = Sequence.concat [
            firstRange |> IntRange.toSequence,
            secondRange |> IntRange.toSequence,
          ];

          Sequence.zip2With
              (fun a b => a == b)
              (Vector.toSequence concatted)
              expected
            |> Sequence.toIterator
            |> Iterator.Reducer.every Functions.identity
            |> Expect.toBeEqualToTrue;
        }),
        it "right larger than left" (fun () => {
          let secondSegmentCount = Config.count / 4;
          let firstSegmentCount = Config.count - secondSegmentCount;

          let firstRange = IntRange.create start::0 count::firstSegmentCount;
          let secondRange = IntRange.create start::firstSegmentCount count::secondSegmentCount;

          let firstVector = firstRange |> IntRange.toIterator |> Vector.from;
          let secondVector = secondRange |> IntRange.toIterator |> Vector.from;

          let concatted = Vector.concat [firstVector, secondVector];

          let expected = Sequence.concat [
            firstRange |> IntRange.toSequence,
            secondRange |> IntRange.toSequence,
          ];

          Sequence.zip2With
              (fun a b => a == b)
              (Vector.toSequence concatted)
              expected
            |> Sequence.toIterator
            |> Iterator.Reducer.every Functions.identity
            |> Expect.toBeEqualToTrue;
        }),
      ],
      it "get" (fun () => {
        let vector = IntRange.create start::0 count::Config.count
          |> IntRange.toIterator
          |> Vector.from;

        vector |> Vector.get (-1) |> Expect.toBeEqualToNoneOfInt;
        vector |> Vector.get Config.count |> Expect.toBeEqualToNoneOfInt;

        vector |> Vector.toIterator |> Iterator.Reducer.forEach (fun i => {
          vector |> Vector.get i |> Expect.toBeEqualToSomeOfInt i;
        });
      }),
      it "getOrRaise" (fun () => {
        let vector = IntRange.create start::0 count::Config.count
          |> IntRange.toIterator
          |> Vector.from;

        (fun () => vector |> Vector.getOrRaise (-1)) |> Expect.shouldRaise;
        (fun () => vector |> Vector.getOrRaise Config.count) |> Expect.shouldRaise;

        vector |> Vector.toIterator |> Iterator.Reducer.forEach (fun i => {
          vector |> Vector.getOrRaise i |> Expect.toBeEqualToInt i;
        });
      }),
      it "init" (fun () => {
        let vector = Vector.init Config.count (fun i => i + 1);
        vector
          |> Vector.toIterator
          |> Iterator.Reducer.forEach (fun i => {
              vector |> Vector.getOrRaise (i - 1) |> Expect.toBeEqualToInt i;
            });
      }),
      it "insertAt" (fun () => { () }),
      it "mapWithIndex" (fun () => { () }),
      it "mapReverseWithIndex" (fun () => { () }),
      it "range" (fun () => { () }),
      it "removeAt" (fun () => { () }),
      it "skip" (fun () => { () }),
      it "take" (fun () => { () }),
      it "toKeyedIterator" (fun () => { () }),
      it "toKeyedIteratorRight" (fun () => { () }),
      it "toMap" (fun () => { () }),
      it "update" (fun () => { () }),
      it "updateAll" (fun () => { () }),
      it "updateWith" (fun () => { () }),
    ],
    ...DequeTester.tests
  ];
};
