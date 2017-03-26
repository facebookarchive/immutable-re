/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Immutable;
open ReUnit;
open ReUnit.Test;

let module SortedIntMap = SortedMap.Make {
  type t = int;
  let compare = Comparator.int;
  let equals = Equality.int;
};

let emptyHashIntMap () => HashMap.emptyWith
  hash::(fun i => i)
  comparator::Comparator.int;

let expectToBeEqualToIntPair = Expect.toBeEqualToWith
  equals::(fun (a, b) (c, d) => a === c && b === d)
  toString::(fun (a, b) => "(" ^ (string_of_int a) ^ ", " ^ (string_of_int b) ^ ")");

let expectToBeEqualToSomeOfIntPair = Expect.toBeEqualToSomeWith
  equals::(fun (a, b) (c, d) => a === c && b === d)
  toString::(fun (a, b) => "(" ^ (string_of_int a) ^ ", " ^ (string_of_int b) ^ ")");

let expectToBeEqualToNoneOfIntPair = Expect.toBeEqualToNoneWith
  toString::(fun (a, b) => "(" ^ (string_of_int a) ^ ", " ^ (string_of_int b) ^ ")");

let test = describe "KeyedReducer" [
  describe "S1" [
    it "every" (fun () => {
      SortedIntMap.KeyedReducer.every (fun _ _ => false) (SortedIntMap.empty ())
        |> Expect.toBeEqualToTrue;

      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> SortedIntMap.put i i) (SortedIntMap.empty ())
        |> SortedIntMap.KeyedReducer.every (fun k v => k >= 0 && v >= 0)
        |> Expect.toBeEqualToTrue;

      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> SortedIntMap.put i i) (SortedIntMap.empty ())
        |> SortedIntMap.KeyedReducer.every (fun k v => k < 3 && v < 3)
        |> Expect.toBeEqualToFalse;
    }),
    it "find" (fun () => {
      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> SortedIntMap.put i i) (SortedIntMap.empty ())
        |> SortedIntMap.KeyedReducer.find (fun k v => k === 2 && v === 2)
        |> expectToBeEqualToSomeOfIntPair (2, 2);

      (SortedIntMap.empty ())
        |> SortedIntMap.KeyedReducer.find (fun k v => k === 2 && v === 2)
        |> expectToBeEqualToNoneOfIntPair;

      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> SortedIntMap.put i i) (SortedIntMap.empty ())
        |> SortedIntMap.KeyedReducer.find (fun k v => k === 5 && v === 5)
        |> expectToBeEqualToNoneOfIntPair;
    }),
    it "findOrRaise" (fun () => {
      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> SortedIntMap.put i i) (SortedIntMap.empty ())
        |> SortedIntMap.KeyedReducer.findOrRaise (fun k v => k === 2 && v === 2)
        |> expectToBeEqualToIntPair (2, 2);

      (fun () => (SortedIntMap.empty ())
        |> SortedIntMap.KeyedReducer.findOrRaise (fun k v => k === 2 && v === 2)
      ) |> Expect.shouldRaise;

      (fun () => IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> SortedIntMap.put i i) (SortedIntMap.empty ())
        |> SortedIntMap.KeyedReducer.findOrRaise (fun k v => k === 5 && v === 5)
      ) |> Expect.shouldRaise;
    }),
    it "forEach" (fun () => {
      let last = ref 0;
      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> SortedIntMap.put i i) (SortedIntMap.empty ())
        |> SortedIntMap.KeyedReducer.forEach while_::(fun k _ => k < 3) (fun k _ => { last := k });
      !last |> Expect.toBeEqualToInt 2;
    }),
    it "none" (fun () => {
      SortedIntMap.KeyedReducer.none (fun _ _ => false) (SortedIntMap.empty ()) |> Expect.toBeEqualToTrue;

      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> SortedIntMap.put i i) (SortedIntMap.empty ())
        |> SortedIntMap.KeyedReducer.none (fun k v => k >= 5 && v >= 5)
        |> Expect.toBeEqualToTrue;

      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> SortedIntMap.put i i) (SortedIntMap.empty ())
        |> SortedIntMap.KeyedReducer.none (fun k v => k < 3 && v < 3)
        |> Expect.toBeEqualToFalse;
    }),
    it "some" (fun () => {
      SortedIntMap.KeyedReducer.some (fun _ _ => false) (SortedIntMap.empty ()) |> Expect.toBeEqualToFalse;

      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> SortedIntMap.put i i) (SortedIntMap.empty ())
        |> SortedIntMap.KeyedReducer.some (fun k v => k >= 5 && v >= 5)
        |> Expect.toBeEqualToFalse;

      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> SortedIntMap.put i i) (SortedIntMap.empty ())
        |> SortedIntMap.KeyedReducer.some (fun k v => k < 3 && k > 1 && v < 3 && v > 1)
        |> Expect.toBeEqualToTrue;
    }),
  ],
  describe "S2" [
    it "every" (fun () => {
      HashMap.KeyedReducer.every (fun _ _ => false) (emptyHashIntMap ()) |> Expect.toBeEqualToTrue;

      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> HashMap.put i i) (emptyHashIntMap ())
        |> HashMap.KeyedReducer.every (fun k v => k >= 0 && v >= 0)
        |> Expect.toBeEqualToTrue;

      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> HashMap.put i i) (emptyHashIntMap ())
        |> HashMap.KeyedReducer.every (fun k v => k < 3 && v < 3)
        |> Expect.toBeEqualToFalse;
    }),
    it "find" (fun () => {
      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> HashMap.put i i) (emptyHashIntMap ())
        |> HashMap.KeyedReducer.find (fun k v => k ===2 && v ===2)
        |> expectToBeEqualToSomeOfIntPair (2, 2);

      (emptyHashIntMap ())
        |> HashMap.KeyedReducer.find (fun k v => k ===2 && v ===2)
        |> expectToBeEqualToNoneOfIntPair;

      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> HashMap.put i i) (emptyHashIntMap ())
        |> HashMap.KeyedReducer.find (fun k v => k ===5 && v ===5)
        |> expectToBeEqualToNoneOfIntPair;
    }),
    it "findOrRaise" (fun () => {
      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> HashMap.put i i) (emptyHashIntMap ())
        |> HashMap.KeyedReducer.findOrRaise (fun k v => k ===2 && v ===2)
        |> expectToBeEqualToIntPair (2, 2);

      (fun () => (emptyHashIntMap ())
        |> HashMap.KeyedReducer.findOrRaise (fun k v => k ===2 && v ===2)
      ) |> Expect.shouldRaise;

      (fun () => IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> HashMap.put i i) (emptyHashIntMap ())
        |> HashMap.KeyedReducer.findOrRaise (fun k v => k ===5 && v ===5)
      ) |> Expect.shouldRaise;
    }),
    it "forEach" (fun () => {
      let last = ref 0;
      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> HashMap.put i i) (emptyHashIntMap ())
        |> HashMap.KeyedReducer.forEach while_::(fun k _ => k < 3) (fun k _ => { last := k });
      !last |> Expect.toBeEqualToInt 2;
    }),
    it "none" (fun () => {
      HashMap.KeyedReducer.none (fun _ _ => false) (emptyHashIntMap ()) |> Expect.toBeEqualToTrue;

      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> HashMap.put i i) (emptyHashIntMap ())
        |> HashMap.KeyedReducer.none (fun k v => k >= 5 && v >= 5)
        |> Expect.toBeEqualToTrue;

      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> HashMap.put i i) (emptyHashIntMap ())
        |> HashMap.KeyedReducer.none (fun k v => k < 3 && v < 3)
        |> Expect.toBeEqualToFalse;
    }),
    it "some" (fun () => {
      HashMap.KeyedReducer.some (fun _ _ => false) (emptyHashIntMap ()) |> Expect.toBeEqualToFalse;

      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> SortedIntMap.put i i) (SortedIntMap.empty ())
        |> SortedIntMap.KeyedReducer.some (fun k v => k >= 5 && v >= 5)
        |> Expect.toBeEqualToFalse;

      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> SortedIntMap.put i i) (SortedIntMap.empty ())
        |> SortedIntMap.KeyedReducer.some (fun k v => k < 3 && k > 1 && v < 3 && v > 1)
        |> Expect.toBeEqualToTrue;
    }),
  ],
];
