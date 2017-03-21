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

module type Config = {
  let count: int;
};

let module Make = fun (Stack: Stack.S1) (Config: Config) => {
  let test = describe (sprintf "count: %i" Config.count) [
    it "addFirst" (fun () => {
      let testData = IntRange.create start::0 count::Config.count;

      testData
        |> IntRange.reduce
          (fun acc i => {
            let acc = Stack.addFirst i acc;
            expect (Stack.first acc) |> toBeEqualToSomeOfInt i;
            expect (Stack.firstOrRaise acc) |> toBeEqualToInt i;
            expect (Stack.count acc) |> toBeEqualToInt (i + 1);
            expect (Stack.isEmpty acc) |> toBeEqualToFalse;
            expect (Stack.isNotEmpty acc) |> toBeEqualToTrue;
            acc
          })
          Stack.empty
        |> Stack.toIterator
        |> List.fromReverse
        |> expect
        |> toBeEqualToListOfInt (List.fromReverse (IntRange.toIteratorRight testData));
    }),
    it "addFirstAll" (fun () => {
      let testData = IntRange.create start::0 count::Config.count;

      Stack.empty
        |> Stack.addFirstAll (IntRange.toIterator testData)
        |> Stack.toIterator
        |> List.fromReverse
        |> expect
        |> toBeEqualToListOfInt (List.fromReverse (IntRange.toIteratorRight testData));
    }),
    it "count" (fun () => {
      Stack.empty |> Stack.count |> expect |> toBeEqualToInt 0;
    }),
    it "first" (fun () => {
      Stack.empty |> Stack.first |> expect |> toBeEqualToNoneOfInt;
    }),
    it "firstOrRaise" (fun () => {
      (fun () => Stack.empty |> Stack.firstOrRaise) |> shouldRaise;
    }),
    it "isEmpty" (fun () => {
      Stack.empty |> Stack.isEmpty |> expect |> toBeEqualToTrue;
    }),
    it "isNotEmpty" (fun () => {
      Stack.empty |> Stack.isNotEmpty |> expect |> toBeEqualToFalse;
    }),
    it "mapReverse" (fun () => {
      IntRange.create start::0 count::Config.count
        |> IntRange.toIterator
        |> Stack.fromReverse
        |> Stack.mapReverse (fun i => i + 1)
        |> Stack.toIterator
        |> List.fromReverse
        |> expect
        |> toBeEqualToListOfInt (List.fromReverse
          (IntRange.create start::1 count::Config.count |> IntRange.toIterator)
        );
    }),
    it "reduce" (fun () => {
      IntRange.create start::0 count::Config.count
        |> IntRange.toIteratorRight
        |> Stack.fromReverse
        |> Stack.reduce
            while_::(fun acc i => i < (Config.count / 2))
            (fun acc i => acc + 1)
            0
        |> expect
        |> toBeEqualToInt (Config.count / 2);
    }),
    it "removeAll" (fun () => {
      IntRange.create start::0 count::Config.count
        |> IntRange.toIteratorRight
        |> Stack.fromReverse
        |> Stack.removeAll
        |> Stack.count
        |> expect
        |> toBeEqualToInt 0;
    }),
    it "removeFirstOrRaise" (fun () => {
      let range = IntRange.create start::0 count::Config.count;

      let fullStack = range |> IntRange.toIterator |> Stack.fromReverse;

      let emptyStack = range |> IntRange.reduceRight
        (fun acc i => {
          expect (Stack.first acc) |> toBeEqualToSomeOfInt i;
          expect (Stack.firstOrRaise acc) |> toBeEqualToInt i;
          expect (Stack.count acc) |> toBeEqualToInt (i + 1);
          expect (Stack.isEmpty acc) |> toBeEqualToFalse;
          expect (Stack.isNotEmpty acc) |> toBeEqualToTrue;
          Stack.removeFirstOrRaise acc;
        }) fullStack;

      Stack.isEmpty emptyStack |> expect |> toBeEqualToTrue;
    }),
    it "return" (fun () => {
      let stack = Stack.return 1;
      stack |> Stack.count |> expect |> toBeEqualToInt 1;
      stack |> Stack.firstOrRaise |> expect |> toBeEqualToInt 1;
    }),
    it "toIterator" (fun () => {
      IntRange.create start::0 count::Config.count
        |> IntRange.toIteratorRight
        |> Stack.fromReverse
        |> Stack.toIterator
        |> Iterator.reduce
            while_::(fun acc i => i < (Config.count / 2))
            (fun acc i => acc + 1)
            0
        |> expect
        |> toBeEqualToInt (Config.count / 2);
    }),
    it "toSequence" (fun () => {
      IntRange.create start::0 count::Config.count
        |> IntRange.toIteratorRight
        |> Stack.fromReverse
        |> Stack.toSequence
        |> Sequence.reduce
            while_::(fun acc i => i < (Config.count / 2))
            (fun acc i => acc + 1)
            0
        |> expect
        |> toBeEqualToInt (Config.count / 2);
    }),
  ];
};
