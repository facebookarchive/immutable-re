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

          Vector.concat [firstVector, secondVector]
            |> Vector.toIterator
            |> List.fromReverse
            |> Expect.toBeEqualToListOfInt (
                Iterator.concat [
                  firstRange |> IntRange.toIterator,
                  secondRange |> IntRange.toIterator,
                ] |> List.fromReverse
              );
        }),
        it "left larger than right" (fun () => {
          let firstSegmentCount = Config.count / 4;
          let secondSegmentCount = Config.count - firstSegmentCount;

          let firstRange = IntRange.create start::0 count::firstSegmentCount;
          let secondRange = IntRange.create start::firstSegmentCount count::secondSegmentCount;

          let firstVector = firstRange |> IntRange.toIterator |> Vector.from;
          let secondVector = secondRange |> IntRange.toIterator |> Vector.from;

          Vector.concat [firstVector, secondVector]
            |> Vector.toIterator
            |> List.fromReverse
            |> Expect.toBeEqualToListOfInt (
                Iterator.concat [
                  firstRange |> IntRange.toIterator,
                  secondRange |> IntRange.toIterator,
                ] |> List.fromReverse
              );
        }),
        it "right larger than left" (fun () => {
          let secondSegmentCount = Config.count / 4;
          let firstSegmentCount = Config.count - secondSegmentCount;

          let firstRange = IntRange.create start::0 count::firstSegmentCount;
          let secondRange = IntRange.create start::firstSegmentCount count::secondSegmentCount;

          let firstVector = firstRange |> IntRange.toIterator |> Vector.from;
          let secondVector = secondRange |> IntRange.toIterator |> Vector.from;

          Vector.concat [firstVector, secondVector]
            |> Vector.toIterator
            |> List.fromReverse
            |> Expect.toBeEqualToListOfInt (
                Iterator.concat [
                  firstRange |> IntRange.toIterator,
                  secondRange |> IntRange.toIterator,
                ] |> List.fromReverse
              );
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
      it "insertAt" (fun () => {
        let zeroedVectorOfSizeCount = Iterator.repeat 0
          |> Iterator.take Config.count
          |> Vector.from;

        let alternatingOneZeroVectorOfSizeTwoTimesCount = IntRange.create start::0 count::Config.count
          |> IntRange.toIterator
          |> Iterator.map (fun i => i * 2)
          |> Iterator.reduce (fun acc i => acc |> Vector.insertAt i 1) zeroedVectorOfSizeCount;

        alternatingOneZeroVectorOfSizeTwoTimesCount
          |> Vector.toIterator
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt (
              Iterator.repeat [1, 0]
                |> Iterator.take Config.count
                |> Iterator.flatMap List.toIterator
                |> List.fromReverse
            );
      }),
      it "mapWithIndex" (fun () => {
        Iterator.repeat 0
          |> Iterator.take Config.count
          |> Vector.from
          |> Vector.mapWithIndex (fun i v => if (i mod 2 === 0) 0 else 1)
          |> Vector.toIterator
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt (
              Iterator.repeat [0, 1]
                |> Iterator.take (Config.count / 2)
                |> Iterator.flatMap List.toIterator
                |> List.fromReverse
            );
      }),
      it "mapReverseWithIndex" (fun () => {
        Iterator.repeat 0
          |> Iterator.take Config.count
          |> Vector.from
          |> Vector.mapReverseWithIndex (fun i v => if (i mod 2 === 0) 0 else 1)
          |> Vector.toIterator
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt (
              Iterator.repeat [1, 0]
                |> Iterator.take (Config.count / 2)
                |> Iterator.flatMap List.toIterator
                |> List.fromReverse
            );
      }),
      it "removeAt" (fun () => {
        let vector = Iterator.repeat [0, 1]
          |> Iterator.take (Config.count / 2)
          |> Iterator.flatMap List.toIterator
          |> Vector.from;

        IntRange.create start::0 count::(Config.count / 2)
          |> IntRange.reduce (fun acc i => acc |> Vector.removeAt i) vector
          |> Vector.toIterator
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt (
              Iterator.repeat 1 |> Iterator.take (Config.count / 2) |> List.fromReverse
            );
      }),
      it "slice" (fun () => {
        let vector = IntRange.create start::0 count::Config.count
          |> IntRange.toIterator
          |> Vector.from;

        vector
          |> Vector.slice start::0 end_::Config.count
          |> Pervasives.(===) vector
          |> Expect.toBeEqualToTrue;

        vector
          |> Vector.slice
          |> Pervasives.(===) vector
          |> Expect.toBeEqualToTrue;

        let countDiv2 = Config.count / 2;
        let countDiv4 = Config.count / 4;

        vector
          |> Vector.slice start::countDiv4
          |> Vector.toIterator
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt (
              IntRange.create start::countDiv4 count::(Config.count - countDiv4)
                |> IntRange.toIterator
                |> List.fromReverse
            );

        vector
          |> Vector.slice start::(-countDiv4)
          |> Vector.toIterator
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt (
              IntRange.create start::(Config.count - countDiv4) count::countDiv4
                |> IntRange.toIterator
                |> List.fromReverse
            );

        vector
          |> Vector.slice end_::countDiv4
          |> Vector.toIterator
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt (
              IntRange.create start::0 count::countDiv4
                |> IntRange.toIterator
                |> List.fromReverse
            );

        vector
          |> Vector.slice end_::(-countDiv4)
          |> Vector.toIterator
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt (
              IntRange.create start::0 count::(Config.count - countDiv4)
                |> IntRange.toIterator
                |> List.fromReverse
            );

        vector
          |> Vector.slice start::countDiv4 end_::(-countDiv4)
          |> Vector.toIterator
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt (
              IntRange.create start::countDiv4 count::(Config.count - countDiv2)
                |> IntRange.toIterator
                |> List.fromReverse
            );

        vector
          |> Vector.slice start::(-Config.count) end_::Config.count
          |> Vector.toIterator
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt (
              IntRange.create start::0 count::Config.count
                |> IntRange.toIterator
                |> List.fromReverse
            );

        vector
          |> Vector.slice start::(-Config.count) end_::(Config.count - countDiv4)
          |> Vector.toIterator
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt (
              IntRange.create start::0 count::(Config.count - countDiv4)
                |> IntRange.toIterator
                |> List.fromReverse
            );

        vector
          |> Vector.slice start::(-Config.count + countDiv4) end_::(Config.count - countDiv4)
          |> Vector.toIterator
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt (
              IntRange.create start::countDiv4 count::(Config.count - countDiv4)
                |> IntRange.toIterator
                |> List.fromReverse
            );
      }),
      it "skip" (fun () => {
        let countDiv4 = Config.count / 4;
        let countDiv2 = Config.count / 2;

        let vector = IntRange.create start::0 count::Config.count
          |> IntRange.toIterator
          |> Vector.from;

        vector
          |> Vector.skip 0
          |> Pervasives.(===) vector
          |> Expect.toBeEqualToTrue;

        vector
          |> Vector.skip Config.count
          |> Vector.isEmpty
          |> Expect.toBeEqualToTrue;

        vector
          |> Vector.skip countDiv4
          |> Vector.toIterator
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt (
              IntRange.create start::countDiv4 count::(Config.count - countDiv4)
                |> IntRange.toIterator
                |> List.fromReverse
            );

        vector
          |> Vector.skip countDiv2
          |> Vector.toIterator
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt (
              IntRange.create start::countDiv2 count::(Config.count - countDiv2)
                |> IntRange.toIterator
                |> List.fromReverse
            );

        vector
          |> Vector.skip (Config.count - countDiv4)
          |> Vector.toIterator
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt (
              IntRange.create start::(Config.count - countDiv4) count::countDiv4
                |> IntRange.toIterator
                |> List.fromReverse
            );
      }),
      it "take" (fun () => {
        let countDiv4 = Config.count / 4;
        let countDiv2 = Config.count / 2;

        let vector = IntRange.create start::0 count::Config.count
          |> IntRange.toIterator
          |> Vector.from;

        vector
          |> Vector.take Config.count
          |> Pervasives.(===) vector
          |> Expect.toBeEqualToTrue;

        vector
          |> Vector.take 0
          |> Vector.isEmpty
          |> Expect.toBeEqualToTrue;

        vector
          |> Vector.take countDiv4
          |> Vector.toIterator
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt (
              IntRange.create start::0 count::countDiv4
                |> IntRange.toIterator
                |> List.fromReverse
            );

        vector
          |> Vector.take countDiv2
          |> Vector.toIterator
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt (
              IntRange.create start::0 count::(Config.count - countDiv2)
                |> IntRange.toIterator
                |> List.fromReverse
            );

        vector
          |> Vector.take (Config.count - countDiv4)
          |> Vector.toIterator
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt (
              IntRange.create start::0 count::(Config.count - countDiv4)
                |> IntRange.toIterator
                |> List.fromReverse
            );
      }),
      it "toKeyedIterator" (fun () => {
        IntRange.create start::0 count::Config.count
          |> IntRange.toIteratorRight
          |> Vector.from
          |> Vector.toKeyedIterator
          |> KeyedIterator.KeyedReducer.forEach (
              fun i v => Config.count - v - 1 |> Expect.toBeEqualToInt i
            );
      }),
      it "toKeyedIteratorRight" (fun () => { () }),
      it "toMap" (fun () => {
        let map = IntRange.create start::0 count::Config.count
          |> IntRange.toIterator
          |> Vector.from
          |> Vector.toMap;

        map |> Map.count |> Expect.toBeEqualToInt Config.count;

        let countDiv4 = Config.count / 4;
        let countDiv2 = Config.count / 2;

        map |> Map.containsKey 0 |> Expect.toBeEqualToTrue;
        map |> Map.containsKey countDiv4 |> Expect.toBeEqualToTrue;
        map |> Map.containsKey countDiv2 |> Expect.toBeEqualToTrue;
        map |> Map.containsKey (Config.count - countDiv4) |> Expect.toBeEqualToTrue;
        map |> Map.containsKey (Config.count - 1) |> Expect.toBeEqualToTrue;
        map |> Map.containsKey (-1) |> Expect.toBeEqualToFalse;
        map |> Map.containsKey Config.count |> Expect.toBeEqualToFalse;

        map |> Map.get 0 |> Expect.toBeEqualToSomeOfInt 0;
        map |> Map.get countDiv4 |> Expect.toBeEqualToSomeOfInt countDiv4;
        map |> Map.get countDiv2 |> Expect.toBeEqualToSomeOfInt countDiv2;
        map |> Map.get (Config.count - countDiv4) |> Expect.toBeEqualToSomeOfInt (Config.count - countDiv4);
        map |> Map.get (Config.count - 1) |> Expect.toBeEqualToSomeOfInt (Config.count - 1);
        map |> Map.get (-1) |> Expect.toBeEqualToNoneOfInt;
        map |> Map.get Config.count |> Expect.toBeEqualToNoneOfInt;

        map |> Map.getOrRaise 0 |> Expect.toBeEqualToInt 0;
        map |> Map.getOrRaise countDiv4 |> Expect.toBeEqualToInt countDiv4;
        map |> Map.getOrRaise countDiv2 |> Expect.toBeEqualToInt countDiv2;
        map |> Map.getOrRaise (Config.count - countDiv4) |> Expect.toBeEqualToInt (Config.count - countDiv4);
        map |> Map.getOrRaise (Config.count - 1) |> Expect.toBeEqualToInt (Config.count - 1);
        (fun () => map |> Map.getOrRaise (-1)) |> Expect.shouldRaise;
        (fun () => map |> Map.getOrRaise Config.count) |> Expect.shouldRaise;

        map
          |> Map.reduce while_::(fun acc k v => k < countDiv4 ) (fun acc k v => 1 + acc) 0
          |> Expect.toBeEqualToInt countDiv4;
      }),
      it "update" (fun () => {
        let vector = Iterator.repeat 0
          |> Iterator.take Config.count
          |> Vector.from;

        IntRange.create start::0 count::Config.count
          |> IntRange.reduce (
              fun vec i => vec  |> Vector.update i (if (i mod 2 === 0) 0 else 1)
            ) vector
          |> Vector.toIterator
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt (
              Iterator.repeat [0, 1]
                |> Iterator.take (Config.count / 2)
                |> Iterator.flatMap List.toIterator
                |> List.fromReverse
            );
      }),
      it "updateAll" (fun () => {
        let vector = Iterator.repeat 0
          |> Iterator.take Config.count
          |> Vector.from;

        vector
          |> Vector.updateAll (fun i v => if (i mod 2 === 0) 0 else 1)
          |> Vector.toIterator
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt (
              Iterator.repeat [0, 1]
                |> Iterator.take (Config.count / 2)
                |> Iterator.flatMap List.toIterator
                |> List.fromReverse
            );
      }),
      it "updateWith" (fun () => {
        let vector = Iterator.repeat [0, 1]
          |> Iterator.take (Config.count / 2)
          |> Iterator.flatMap List.toIterator
          |> Vector.from;

        IntRange.create start::0 count::Config.count
          |> IntRange.reduce (fun acc i =>
              acc |> Vector.updateWith i (fun v => if (v == 0) 1 else 0)
            ) vector
          |> Vector.toIterator
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt (
              Iterator.repeat [1, 0]
                |> Iterator.take (Config.count / 2)
                |> Iterator.flatMap List.toIterator
                |> List.fromReverse
            );
      }),
    ],
    ...DequeTester.tests
  ];
};
