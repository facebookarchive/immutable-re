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

let module SortedIntMap = SortedMap.Make {
  type t = int;
  let compare = Comparator.int;
  let equals = Equality.int;
};

let emptyHashIntMap = HashMap.emptyWith
  hash::(fun i => i)
  comparator::Comparator.int;

let toBeEqualToIntPair = toBeEqualToWith
  equals::(fun (a, b) (c, d) => a === c && b === c)
  toString::(fun (a, b) => "(" ^ (string_of_int a) ^ ", " ^ (string_of_int b) ^ ")");

let toBeEqualToSomeOfIntPair = toBeEqualToSomeWith
  equals::(fun (a, b) (c, d) => a === c && b === c)
  toString::(fun (a, b) => "(" ^ (string_of_int a) ^ ", " ^ (string_of_int b) ^ ")");

let toBeEqualToNoneOfIntPair = toBeEqualToNoneWith
  toString::(fun (a, b) => "(" ^ (string_of_int a) ^ ", " ^ (string_of_int b) ^ ")");

let test = describe "KeyedReducer" [
  describe "S1" [
    it "every" (fun () => {
      expect (SortedIntMap.KeyedReducer.every (fun _ _ => false) SortedIntMap.empty) |> toBeEqualToTrue;

      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> SortedIntMap.put i i) SortedIntMap.empty
        |> SortedIntMap.KeyedReducer.every (fun k v => k >= 0 && v >= 0)
        |> expect
        |> toBeEqualToTrue;

      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> SortedIntMap.put i i) SortedIntMap.empty
        |> SortedIntMap.KeyedReducer.every (fun k v => k < 3 && v < 3)
        |> expect
        |> toBeEqualToFalse;
    }),
    it "find" (fun () => {
      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> SortedIntMap.put i i) SortedIntMap.empty
        |> SortedIntMap.KeyedReducer.find (fun k v => k == 2 && v == 2)
        |> expect
        |> toBeEqualToSomeOfIntPair (2, 2);

      SortedIntMap.empty
        |> SortedIntMap.KeyedReducer.find (fun k v => k == 2 && v == 2)
        |> expect
        |> toBeEqualToNoneOfIntPair;

      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> SortedIntMap.put i i) SortedIntMap.empty
        |> SortedIntMap.KeyedReducer.find (fun k v => k == 5 && v == 5)
        |> expect
        |> toBeEqualToNoneOfIntPair;
    }),
    it "findOrRaise" (fun () => {
      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> SortedIntMap.put i i) SortedIntMap.empty
        |> SortedIntMap.KeyedReducer.findOrRaise (fun k v => k == 2 && v == 2)
        |> expect
        |> toBeEqualToIntPair (2, 2);

      (fun () => SortedIntMap.empty
        |> SortedIntMap.KeyedReducer.findOrRaise (fun k v => k == 2 && v == 2)
      ) |> shouldRaise;

      (fun () => IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> SortedIntMap.put i i) SortedIntMap.empty
        |> SortedIntMap.KeyedReducer.findOrRaise (fun k v => k == 5 && v == 5)
      ) |> shouldRaise;
    }),
    it "forEach" (fun () => {
      let last = ref 0;
      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> SortedIntMap.put i i) SortedIntMap.empty
        |> SortedIntMap.KeyedReducer.forEach while_::(fun k v => k < 3) (fun k v => { last := k });
      expect !last |> toBeEqualToInt 2;
    }),
    it "none" (fun () => {
      expect (SortedIntMap.KeyedReducer.none (fun _ _ => false) SortedIntMap.empty) |> toBeEqualToTrue;

      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> SortedIntMap.put i i) SortedIntMap.empty
        |> SortedIntMap.KeyedReducer.none (fun k v => k >= 5 && v >= 5)
        |> expect
        |> toBeEqualToTrue;

      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> SortedIntMap.put i i) SortedIntMap.empty
        |> SortedIntMap.KeyedReducer.none (fun k v => k < 3 && v < 3)
        |> expect
        |> toBeEqualToFalse;
    }),
    it "some" (fun () => {
      expect (SortedIntMap.KeyedReducer.some (fun _ _ => false) SortedIntMap.empty) |> toBeEqualToFalse;

      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> SortedIntMap.put i i) SortedIntMap.empty
        |> SortedIntMap.KeyedReducer.some (fun k v => k >= 5 && v >= 5)
        |> expect
        |> toBeEqualToFalse;

      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> SortedIntMap.put i i) SortedIntMap.empty
        |> SortedIntMap.KeyedReducer.some (fun k v => k < 3 && k > 1 && v < 3 && v > 1)
        |> expect
        |> toBeEqualToTrue;
    }),
  ],
  describe "S2" [
    it "every" (fun () => {
      expect (HashMap.KeyedReducer.every (fun _ _ => false) emptyHashIntMap) |> toBeEqualToTrue;

      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> HashMap.put i i) emptyHashIntMap
        |> HashMap.KeyedReducer.every (fun k v => k >= 0 && v >= 0)
        |> expect
        |> toBeEqualToTrue;

      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> HashMap.put i i) emptyHashIntMap
        |> HashMap.KeyedReducer.every (fun k v => k < 3 && v < 3)
        |> expect
        |> toBeEqualToFalse;
    }),
    it "find" (fun () => {
      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> HashMap.put i i) emptyHashIntMap
        |> HashMap.KeyedReducer.find (fun k v => k == 2 && v == 2)
        |> expect
        |> toBeEqualToSomeOfIntPair (2, 2);

      emptyHashIntMap
        |> HashMap.KeyedReducer.find (fun k v => k == 2 && v == 2)
        |> expect
        |> toBeEqualToNoneOfIntPair;

      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> HashMap.put i i) emptyHashIntMap
        |> HashMap.KeyedReducer.find (fun k v => k == 5 && v == 5)
        |> expect
        |> toBeEqualToNoneOfIntPair;
    }),
    it "findOrRaise" (fun () => {
      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> HashMap.put i i) emptyHashIntMap
        |> HashMap.KeyedReducer.findOrRaise (fun k v => k == 2 && v == 2)
        |> expect
        |> toBeEqualToIntPair (2, 2);

      (fun () => emptyHashIntMap
        |> HashMap.KeyedReducer.findOrRaise (fun k v => k == 2 && v == 2)
      ) |> shouldRaise;

      (fun () => IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> HashMap.put i i) emptyHashIntMap
        |> HashMap.KeyedReducer.findOrRaise (fun k v => k == 5 && v == 5)
      ) |> shouldRaise;
    }),
    it "forEach" (fun () => {
      let last = ref 0;
      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> HashMap.put i i) emptyHashIntMap
        |> HashMap.KeyedReducer.forEach while_::(fun k v => k < 3) (fun k v => { last := k });
      expect !last |> toBeEqualToInt 2;
    }),
    it "none" (fun () => {
      expect (HashMap.KeyedReducer.none (fun _ _ => false) emptyHashIntMap) |> toBeEqualToTrue;

      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> HashMap.put i i) emptyHashIntMap
        |> HashMap.KeyedReducer.none (fun k v => k >= 5 && v >= 5)
        |> expect
        |> toBeEqualToTrue;

      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> HashMap.put i i) emptyHashIntMap
        |> HashMap.KeyedReducer.none (fun k v => k < 3 && v < 3)
        |> expect
        |> toBeEqualToFalse;
    }),
    it "some" (fun () => {
      expect (HashMap.KeyedReducer.some (fun _ _ => false) emptyHashIntMap) |> toBeEqualToFalse;

      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> SortedIntMap.put i i) SortedIntMap.empty
        |> SortedIntMap.KeyedReducer.some (fun k v => k >= 5 && v >= 5)
        |> expect
        |> toBeEqualToFalse;

      IntRange.create start::0 count::5
        |> IntRange.reduce (fun acc i => acc |> SortedIntMap.put i i) SortedIntMap.empty
        |> SortedIntMap.KeyedReducer.some (fun k v => k < 3 && k > 1 && v < 3 && v > 1)
        |> expect
        |> toBeEqualToTrue;
    }),
  ],
];
