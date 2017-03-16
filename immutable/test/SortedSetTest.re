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

/*
let emptyWith: (Comparator.t 'a) => (t 'a);
let fromSeqWith: (Comparator.t 'a)  => (Seq.t 'a) => (t 'a);
*/

let module Set = {
  type t = SortedSet.t int;

  let add = SortedSet.add;
  let addAll = SortedSet.addAll;
  let contains = SortedSet.contains;
  let count = SortedSet.count;
  let empty = fun () => SortedSet.empty;
  let equals = SortedSet.equals;
  let every = SortedSet.every;
  let find = SortedSet.find;
  let forEach = SortedSet.forEach;
  let from = SortedSet.from;
  let hash = SortedSet.hash;
  let intersect = SortedSet.intersect;
  let isEmpty = SortedSet.isEmpty;
  let isNotEmpty = SortedSet.isNotEmpty;
  let none = SortedSet.none;
  let reduce = SortedSet.reduce;
  let remove = SortedSet.remove;
  let removeAll = SortedSet.removeAll;
  let some = SortedSet.some;
  let subtract = SortedSet.subtract;
  let toSet = SortedSet.toSet;
  let toMap = SortedSet.toMap;
  let toSeq = SortedSet.toSeq;
  let tryFind = SortedSet.tryFind;
  let union = SortedSet.union;
};

let count = 10000;

let test = describe "SortedSet" [
  it "compare" (fun () => {
    let set = IntRange.create 0 count
      |> IntRange.toIterable
      |> SortedSet.from;

    let setEqual = IntRange.create 0 count
      |> IntRange.toIterable
      |> SortedSet.from;
    expect (SortedSet.compare set setEqual) |> toBeEqualTo (fun _ => "") Ordering.equal;

    let setSameLengthLessThan = IntRange.create (-1) count
      |> IntRange.toIterable
      |> SortedSet.from;
    expect (SortedSet.compare set setSameLengthLessThan) |> toBeEqualTo (fun _ => "") Ordering.greaterThan;


    let setSameLengthGreaterThan = IntRange.create 1 count
      |> IntRange.toIterable
      |> SortedSet.from;
    expect (SortedSet.compare set setSameLengthGreaterThan) |> toBeEqualTo (fun _ => "") Ordering.lessThan;

    let setLonger = IntRange.create 0 (count + 1)
      |> IntRange.toIterable
      |> SortedSet.from;
    expect (SortedSet.compare set setLonger) |> toBeEqualTo (fun _ => "") Ordering.lessThan;

    let setShorter = IntRange.create 0 (count - 1)
      |> IntRange.toIterable
      |> SortedSet.from;
    expect (SortedSet.compare set setShorter) |> toBeEqualTo (fun _ => "") Ordering.greaterThan;

    let setWithRandomComparator1 = SortedSet.emptyWith (fun _ _ => Ordering.greaterThan);
    let setWithRandomComparator2 = SortedSet.emptyWith (fun _ _ => Ordering.lessThan);
    defer (fun () => SortedSet.compare setWithRandomComparator1 setWithRandomComparator2) |> throws;
  }),
  it "last and tryLast" (fun () => {
    let set = IntRange.create 0 count
      |> IntRange.toIterable
      |> SortedSet.from;

    expect (set |> SortedSet.last) |> toBeEqualToInt (count - 1);
    expect (set |> SortedSet.tryLast) |> toBeEqualToSomeOfInt (count - 1);

    defer (fun () => SortedSet.empty |> SortedSet.last) |> throws;
    expect (SortedSet.empty |> SortedSet.tryLast) |> toBeEqualToNoneOfInt;
  }),
  it "first and tryFirst" (fun () => {
    let set = IntRange.create 0 count
      |> IntRange.toIterable
      |> SortedSet.from;

    expect (set |> SortedSet.first) |> toBeEqualToInt 0;
    expect (set |> SortedSet.tryFirst) |> toBeEqualToSomeOfInt 0;

    defer (fun () => SortedSet.empty |> SortedSet.first) |> throws;
    expect (SortedSet.empty |> SortedSet.tryFirst) |> toBeEqualToNoneOfInt;
  }),
  it "reduceRight" (fun () => {
    IntRange.create 0 count
      |> IntRange.toIterable
      |> SortedSet.from
      |> SortedSet.reduceRight
        (fun acc i => { expect (i < acc) |> toBeEqualToTrue; i })
        count
      |> ignore;
  }),
  it "removeLast" (fun () => {
    let set = IntRange.create 0 count
      |> IntRange.toIterable
      |> SortedSet.from
      |> SortedSet.removeLast;
    expect (set |> Set.contains (count - 1)) |> toBeEqualToFalse;
  }),
  it "removeFirst" (fun () => {
    let set = IntRange.create 0 count
      |> IntRange.toIterable
      |> SortedSet.from
      |> SortedSet.removeFirst;
    expect (set |> Set.contains 0) |> toBeEqualToFalse;
  }),
  ...(SetTester.test count (module Set))
];
