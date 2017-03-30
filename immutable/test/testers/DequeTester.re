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

let module Make = fun (Deque: PersistentNavigableCollection.S1) (Config: TesterConfig.S) => {
  let module StackTester = StackTester.Make (Deque: SequentialCollection.Persistent.S1) Config;

  let tests = [
    describe (sprintf "count: %i" Config.count) [
      it "addLast" (fun () => {
        let testData = IntRange.create start::0 count::Config.count;

        testData
          |> IntRange.reduce
            (fun acc i => {
              let acc = Deque.addLast i acc;
              Deque.last acc |> Expect.toBeEqualToSomeOfInt i;
              Deque.lastOrRaise acc |> Expect.toBeEqualToInt i;
              Deque.count acc |> Expect.toBeEqualToInt (i + 1);
              Deque.isEmpty acc |> Expect.toBeEqualToFalse;
              Deque.isNotEmpty acc |> Expect.toBeEqualToTrue;
              acc
            })
            (Deque.empty ())
          |> Deque.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt (List.fromReverse (IntRange.toIterable testData));
      }),
      it "addLastAll" (fun () => {
        let testData = IntRange.create start::0 count::Config.count;

        (Deque.empty ())
          |> Deque.addLastAll (IntRange.toIterable testData)
          |> Deque.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt (List.fromReverse (IntRange.toIterable testData));
      }),
      it "from" (fun () => {
        let testData = IntRange.create start::0 count::Config.count;

        IntRange.toIterable testData
          |> Deque.from
          |> Deque.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt (List.fromReverse (IntRange.toIterable testData));
      }),
      it "last" (fun () => {
        (Deque.empty ()) |> Deque.last |> Expect.toBeEqualToNoneOfInt;
      }),
      it "lastOrRaise" (fun () => {
        (fun () => (Deque.empty ()) |> Deque.lastOrRaise) |> Expect.shouldRaise;
      }),
      it "reduceRight" (fun () => {
        IntRange.create start::0 count::Config.count
          |> IntRange.toIterable
          |> Deque.from
          |> Deque.reduceRight
              while_::(fun _ i => i >= (Config.count / 2))
              (fun _ i => i)
              0
          |> Expect.toBeEqualToInt (Config.count / 2);
      }),
      it "removeLastOrRaise" (fun () => {
        let range = IntRange.create start::0 count::Config.count;

        let fullDeque = range |> IntRange.toIterable |> Deque.from;

        let emptyDeque = range |> IntRange.reduceRight
          (fun acc i => {
            Deque.last acc |> Expect.toBeEqualToSomeOfInt i;
            Deque.lastOrRaise acc |> Expect.toBeEqualToInt i;
            Deque.count acc |> Expect.toBeEqualToInt (i + 1);
            Deque.isEmpty acc |> Expect.toBeEqualToFalse;
            Deque.isNotEmpty acc |> Expect.toBeEqualToTrue;
            Deque.removeLastOrRaise acc;
          }) fullDeque;

        Deque.isEmpty emptyDeque |> Expect.toBeEqualToTrue;
      }),
      it "toIterableRight" (fun () => {
        IntRange.create start::0 count::Config.count
          |> IntRange.toIterable
          |> Deque.from
          |> Deque.toIterableRight
          |> Iterable.reduce
              while_::(fun _ i => i >= (Config.count / 2))
              (fun acc _ => acc - 1)
              Config.count
          |> Expect.toBeEqualToInt (Config.count / 2);
      }),
      it "toSequenceRight" (fun () => {
        IntRange.create start::0 count::Config.count
          |> IntRange.toIterable
          |> Deque.from
          |> Deque.toSequenceRight
          |> Sequence.reduce
              while_::(fun _ i => i >= (Config.count / 2))
              (fun acc _ => acc - 1)
              Config.count
          |> Expect.toBeEqualToInt (Config.count / 2);
      }),
      it "addLast, removeFirstOrRaise" (fun () => {
        let testData = IntRange.create start::0 count::Config.count;
        let deque = (Deque.empty ()) |> Deque.addLastAll (IntRange.toIterable testData);

        testData |> IntRange.reduce
          (fun acc i => {
            Deque.first acc |> Expect.toBeEqualToSomeOfInt i;
            Deque.firstOrRaise acc |> Expect.toBeEqualToInt i;
            Deque.count acc |> Expect.toBeEqualToInt (Config.count - i);
            Deque.isEmpty acc |> Expect.toBeEqualToFalse;
            Deque.isNotEmpty acc |> Expect.toBeEqualToTrue;
            Deque.removeFirstOrRaise acc;
          }) deque |> ignore
      }),

      it "addFirst, removeLastOrRaise" (fun () => {
        let testData = IntRange.create start::0 count::Config.count;
        let deque = (Deque.empty ()) |> Deque.addFirstAll (IntRange.toIterableRight testData);

        testData |> IntRange.reduceRight
          (fun acc i => {
            Deque.last acc |> Expect.toBeEqualToSomeOfInt i;
            Deque.lastOrRaise acc |> Expect.toBeEqualToInt i;
            Deque.count acc |> Expect.toBeEqualToInt (i + 1);
            Deque.isEmpty acc |> Expect.toBeEqualToFalse;
            Deque.isNotEmpty acc |> Expect.toBeEqualToTrue;
            Deque.removeLastOrRaise acc;
          }) deque |> ignore;
      })
    ],
    ...StackTester.tests
  ]
};
