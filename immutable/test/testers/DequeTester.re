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
open ReUnit.Expect;
open ReUnit.Test;

let module Make = fun (Deque: Deque.S1) (Config: StackTester.Config) => {
  let module StackTester = StackTester.Make (Deque: Stack.S1) Config;

  let tests = [
    describe (sprintf "count: %i" Config.count) [
      it "addLast" (fun () => {
        let testData = IntRange.create start::0 count::Config.count;

        testData
          |> IntRange.reduce
            (fun acc i => {
              let acc = Deque.addLast i acc;
              expect (Deque.last acc) |> toBeEqualToSomeOfInt i;
              expect (Deque.lastOrRaise acc) |> toBeEqualToInt i;
              expect (Deque.count acc) |> toBeEqualToInt (i + 1);
              expect (Deque.isEmpty acc) |> toBeEqualToFalse;
              expect (Deque.isNotEmpty acc) |> toBeEqualToTrue;
              acc
            })
            Deque.empty
          |> Deque.toIterator
          |> List.fromReverse
          |> expect
          |> toBeEqualToListOfInt (List.fromReverse (IntRange.toIterator testData));
      }),
      it "addLastAll" (fun () => {
        let testData = IntRange.create start::0 count::Config.count;

        Deque.empty
          |> Deque.addLastAll (IntRange.toIterator testData)
          |> Deque.toIterator
          |> List.fromReverse
          |> expect
          |> toBeEqualToListOfInt (List.fromReverse (IntRange.toIterator testData));
      }),
      it "from" (fun () => {
        let testData = IntRange.create start::0 count::Config.count;

        IntRange.toIterator testData
          |> Deque.from
          |> Deque.toIterator
          |> List.fromReverse
          |> expect
          |> toBeEqualToListOfInt (List.fromReverse (IntRange.toIterator testData));
      }),
      it "last" (fun () => {
        Deque.empty |> Deque.last |> expect |> toBeEqualToNoneOfInt;
      }),
      it "lastOrRaise" (fun () => {
        (fun () => Deque.empty |> Deque.lastOrRaise) |> shouldRaise;
      }),
      it "map" (fun () => {
        IntRange.create start::0 count::Config.count
          |> IntRange.toIterator
          |> Deque.from
          |> Deque.map (fun i => i + 1)
          |> Deque.toIterator
          |> List.fromReverse
          |> expect
          |> toBeEqualToListOfInt (List.fromReverse
            (IntRange.create start::1 count::Config.count |> IntRange.toIterator)
          );
      }),
      it "reduceRight" (fun () => {
        IntRange.create start::0 count::Config.count
          |> IntRange.toIterator
          |> Deque.from
          |> Deque.reduceRight
              while_::(fun acc i => i >= (Config.count / 2))
              (fun acc i => i)
              0
          |> expect
          |> toBeEqualToInt (Config.count / 2);
      }),
      it "removeLastOrRaise" (fun () => {
        let range = IntRange.create start::0 count::Config.count;

        let fullDeque = range |> IntRange.toIterator |> Deque.from;

        let emptyDeque = range |> IntRange.reduceRight
          (fun acc i => {
            expect (Deque.last acc) |> toBeEqualToSomeOfInt i;
            expect (Deque.lastOrRaise acc) |> toBeEqualToInt i;
            expect (Deque.count acc) |> toBeEqualToInt (i + 1);
            expect (Deque.isEmpty acc) |> toBeEqualToFalse;
            expect (Deque.isNotEmpty acc) |> toBeEqualToTrue;
            Deque.removeLastOrRaise acc;
          }) fullDeque;

        Deque.isEmpty emptyDeque |> expect |> toBeEqualToTrue;
      }),
      it "toIteratorRight" (fun () => {
        IntRange.create start::0 count::Config.count
          |> IntRange.toIterator
          |> Deque.from
          |> Deque.toIteratorRight
          |> Iterator.reduce
              while_::(fun acc i => i >= (Config.count / 2))
              (fun acc i => acc - 1)
              Config.count
          |> expect
          |> toBeEqualToInt (Config.count / 2);
      }),
      it "toSequenceRight" (fun () => {
        IntRange.create start::0 count::Config.count
          |> IntRange.toIterator
          |> Deque.from
          |> Deque.toSequenceRight
          |> Sequence.reduce
              while_::(fun acc i => i >= (Config.count / 2))
              (fun acc i => acc - 1)
              Config.count
          |> expect
          |> toBeEqualToInt (Config.count / 2);
      }),
      it "addLast, removeFirstOrRaise" (fun () => {
        let testData = IntRange.create start::0 count::Config.count;
        let deque = Deque.empty |> Deque.addLastAll (IntRange.toIterator testData);

        testData |> IntRange.reduce
          (fun acc i => {
            expect (Deque.first acc) |> toBeEqualToSomeOfInt i;
            expect (Deque.firstOrRaise acc) |> toBeEqualToInt i;
            expect (Deque.count acc) |> toBeEqualToInt (Config.count - i);
            expect (Deque.isEmpty acc) |> toBeEqualToFalse;
            expect (Deque.isNotEmpty acc) |> toBeEqualToTrue;
            Deque.removeFirstOrRaise acc;
          }) deque |> ignore
      }),

      it "addFirst, removeLastOrRaise" (fun () => {
        let testData = IntRange.create start::0 count::Config.count;
        let deque = Deque.empty |> Deque.addFirstAll (IntRange.toIteratorRight testData);

        testData |> IntRange.reduceRight
          (fun acc i => {
            expect (Deque.last acc) |> toBeEqualToSomeOfInt i;
            expect (Deque.lastOrRaise acc) |> toBeEqualToInt i;
            expect (Deque.count acc) |> toBeEqualToInt (i + 1);
            expect (Deque.isEmpty acc) |> toBeEqualToFalse;
            expect (Deque.isNotEmpty acc) |> toBeEqualToTrue;
            Deque.removeLastOrRaise acc;
          }) deque |> ignore;
      })
    ],
    ...StackTester.tests
  ]

};
