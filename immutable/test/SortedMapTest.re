/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Immutable;
open Printf;
open ReUnit;
open ReUnit.Test;

let module SortedIntMap = SortedMap.Make {
  type t = int;

  let compare = Comparator.int;
  let equals = Equality.int;
};

let navigationTests (count: int) => {
  let countDiv2 = count / 2;
  let countDiv4 = count / 4;

  describe (sprintf "count: %i" count) [
    it "first" (fun () => {
      SortedIntMap.empty () |> SortedIntMap.first |> Option.map fst |> Expect.toBeEqualToNoneOfInt;

      IntRange.create start::(-countDiv2) count::count
        |> IntRange.toIterator
        |> Iterator.map (fun i => (i, i))
        |> SortedIntMap.fromEntries
        |> SortedIntMap.first
        |> Option.map fst
        |> Expect.toBeEqualToSomeOfInt (-countDiv2);
    }),
    it "firstOrRaise" (fun () => {
      (fun () => SortedIntMap.empty () |> SortedIntMap.firstOrRaise) |> Expect.shouldRaise;
      IntRange.create start::(-countDiv2) count::count
        |> IntRange.toIterator
        |> Iterator.map (fun i => (i, i))
        |> SortedIntMap.fromEntries
        |> SortedIntMap.firstOrRaise
        |> fst
        |> Expect.toBeEqualToInt (-countDiv2);
    }),
    it "last" (fun () => {
      SortedIntMap.empty () |> SortedIntMap.last |> Option.map fst |> Expect.toBeEqualToNoneOfInt;

      IntRange.create start::(-countDiv2) count::count
        |> IntRange.toIterator
        |> Iterator.map (fun i => (i, i))
        |> SortedIntMap.fromEntries
        |> SortedIntMap.last
        |> Option.map fst
        |> Expect.toBeEqualToSomeOfInt (countDiv2 - 1);
    }),
    it "lastOrRaise" (fun () => {
      (fun () => SortedIntMap.empty () |> SortedIntMap.lastOrRaise) |> Expect.shouldRaise;
      IntRange.create start::(-countDiv2) count::count
        |> IntRange.toIterator
        |> Iterator.map (fun i => (i, i))
        |> SortedIntMap.fromEntries
        |> SortedIntMap.lastOrRaise
        |> fst
        |> Expect.toBeEqualToInt (countDiv2 - 1);
    }),/*
    it "reduceRight" (fun () => {
      IntRange.create start::0 count::count
        |> IntRange.toIterator
        |> SortedIntMap.from
        |> SortedIntMap.reduceRight while_::(fun acc i => acc < countDiv2) (fun acc i => 1 + acc) 0
        |> Expect.toBeEqualToInt countDiv2;
    }),*/
    it "removeFirstOrRaise" (fun () => {
      let set = IntRange.create start::0 count::count
        |> IntRange.toIterator
        |> Iterator.map (fun i => (i, i))
        |> SortedIntMap.fromEntries;

      IntRange.create start::0 count::count
        |> IntRange.reduce (fun acc i => {
            acc |> SortedIntMap.firstOrRaise |> fst |> Expect.toBeEqualToInt i;
            acc |> SortedIntMap.removeFirstOrRaise;
          }) set
        |> ignore;

      (fun () => SortedIntMap.empty () |> SortedIntMap.removeFirstOrRaise) |> Expect.shouldRaise;
    }),
    it "removeLastOrRaise" (fun () => {
      let set = IntRange.create start::0 count::count
        |> IntRange.toIterator
        |> Iterator.map (fun i => (i, i))
        |> SortedIntMap.fromEntries;

      IntRange.create start::0 count::count
        |> IntRange.reduceRight (fun acc i => {
            acc |> SortedIntMap.lastOrRaise |> fst |> Expect.toBeEqualToInt i;
            acc |> SortedIntMap.removeLastOrRaise;
          }) set
        |> ignore;

      (fun () => SortedIntMap.empty () |> SortedIntMap.removeLastOrRaise) |> Expect.shouldRaise;
    }),
    it "toIteratorRight" (fun () => {
      IntRange.create start::0 count::count
        |> IntRange.toIterator
        |> Iterator.map (fun i => (i, i))
        |> SortedIntMap.fromEntries
        |> SortedIntMap.toIteratorRight
        |> Iterator.reduce while_::(fun acc i => acc < countDiv4) (fun acc i => 1 + acc) 0
        |> Expect.toBeEqualToInt countDiv4;
    }),
    it "toSequenceRight" (fun () => {
      IntRange.create start::0 count::count
        |> IntRange.toIterator
        |> Iterator.map (fun i => (i, i))
        |> SortedIntMap.fromEntries
        |> SortedIntMap.toSequenceRight
        |> Sequence.reduce while_::(fun acc i => acc < countDiv4) (fun acc i => 1 + acc) 0
        |> Expect.toBeEqualToInt countDiv4;
    }),
  ]
};

let test = describe "SortedMap" [
  PersistentMapTester.test (module SortedIntMap: PersistentMap_1  with type k = SortedIntMap.k) 100,
  PersistentMapTester.test (module SortedIntMap: PersistentMap_1 with type k = SortedIntMap.k) 10000,
];
