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

let module SortedIntSet = SortedSet.Make {
  type t = int;
  let compare = Comparator.structural;
};

let module Set = {
  type t = SortedIntSet.t;

  let add = SortedIntSet.add;
  let addAll = SortedIntSet.addAll;
  let contains = SortedIntSet.contains;
  let count = SortedIntSet.count;
  let empty = fun () => SortedIntSet.empty;
  let equals = SortedIntSet.equals;
  let every = SortedIntSet.every;
  let find = SortedIntSet.find;
  let forEach = SortedIntSet.forEach;
  let from = SortedIntSet.from;
  let hash = SortedIntSet.hash;
  let intersect = SortedIntSet.intersect;
  let isEmpty = SortedIntSet.isEmpty;
  let isNotEmpty = SortedIntSet.isNotEmpty;
  let none = SortedIntSet.none;
  let reduce = SortedIntSet.reduce;
  let remove = SortedIntSet.remove;
  let removeAll = SortedIntSet.removeAll;
  let some = SortedIntSet.some;
  let subtract = SortedIntSet.subtract;
  let toSet = SortedIntSet.toSet;
  let toMap = SortedIntSet.toMap;
  let toSequence = SortedIntSet.toSequence;
  let tryFind = SortedIntSet.tryFind;
  let union = SortedIntSet.union;
};

let count = 10000;

let test = describe "SortedSet" [
  it "compare" (fun () => {
    let set = IntRange.create 0 count
      |> IntRange.toIterator
      |> SortedIntSet.from;

    let setEqual = IntRange.create 0 count
      |> IntRange.toIterator
      |> SortedIntSet.from;
    expect (SortedIntSet.compare set setEqual) |> toBeEqualTo (fun _ => "") Ordering.equal;

    let setSameLengthLessThan = IntRange.create (-1) count
      |> IntRange.toIterator
      |> SortedIntSet.from;
    expect (SortedIntSet.compare set setSameLengthLessThan) |> toBeEqualTo (fun _ => "") Ordering.greaterThan;


    let setSameLengthGreaterThan = IntRange.create 1 count
      |> IntRange.toIterator
      |> SortedIntSet.from;
    expect (SortedIntSet.compare set setSameLengthGreaterThan) |> toBeEqualTo (fun _ => "") Ordering.lessThan;

    let setLonger = IntRange.create 0 (count + 1)
      |> IntRange.toIterator
      |> SortedIntSet.from;
    expect (SortedIntSet.compare set setLonger) |> toBeEqualTo (fun _ => "") Ordering.lessThan;

    let setShorter = IntRange.create 0 (count - 1)
      |> IntRange.toIterator
      |> SortedIntSet.from;
    expect (SortedIntSet.compare set setShorter) |> toBeEqualTo (fun _ => "") Ordering.greaterThan;
  }),
  it "last and tryLast" (fun () => {
    let set = IntRange.create 0 count
      |> IntRange.toIterator
      |> SortedIntSet.from;

    expect (set |> SortedIntSet.last) |> toBeEqualToInt (count - 1);
    expect (set |> SortedIntSet.tryLast) |> toBeEqualToSomeOfInt (count - 1);

    defer (fun () => SortedIntSet.empty |> SortedIntSet.last) |> throws;
    expect (SortedIntSet.empty |> SortedIntSet.tryLast) |> toBeEqualToNoneOfInt;
  }),
  it "first and tryFirst" (fun () => {
    let set = IntRange.create 0 count
      |> IntRange.toIterator
      |> SortedIntSet.from;

    expect (set |> SortedIntSet.first) |> toBeEqualToInt 0;
    expect (set |> SortedIntSet.tryFirst) |> toBeEqualToSomeOfInt 0;

    defer (fun () => SortedIntSet.empty |> SortedIntSet.first) |> throws;
    expect (SortedIntSet.empty |> SortedIntSet.tryFirst) |> toBeEqualToNoneOfInt;
  }),
  it "reduceRight" (fun () => {
    IntRange.create 0 count
      |> IntRange.toIterator
      |> SortedIntSet.from
      |> SortedIntSet.reduceRight
        (fun acc i => { expect (i < acc) |> toBeEqualToTrue; i })
        count
      |> ignore;
  }),
  it "removeLast" (fun () => {
    let set = IntRange.create 0 count
      |> IntRange.toIterator
      |> SortedIntSet.from
      |> SortedIntSet.removeLast;
    expect (set |> Set.contains (count - 1)) |> toBeEqualToFalse;
  }),
  it "removeFirst" (fun () => {
    let set = IntRange.create 0 count
      |> IntRange.toIterator
      |> SortedIntSet.from
      |> SortedIntSet.removeFirst;
    expect (set |> Set.contains 0) |> toBeEqualToFalse;
  }),
  ...(SetTester.test count (module Set))
];
