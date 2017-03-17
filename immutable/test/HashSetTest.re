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
open ReUnit.Expect;
open ReUnit.Test;

let module ComparatorSet = {
  type t = HashSet.t int;

  let add = HashSet.add;
  let addAll = HashSet.addAll;
  let contains = HashSet.contains;
  let count = HashSet.count;
  let empty = fun () => HashSet.empty;
  let equals = HashSet.equals;
  let every = HashSet.every;
  let find = HashSet.find;
  let forEach = HashSet.forEach;
  let from = HashSet.from;
  let hash = HashSet.hash;
  let intersect = HashSet.intersect;
  let isEmpty = HashSet.isEmpty;
  let isNotEmpty = HashSet.isNotEmpty;
  let none = HashSet.none;
  let reduce = HashSet.reduce;
  let remove = HashSet.remove;
  let removeAll = HashSet.removeAll;
  let some = HashSet.some;
  let subtract = HashSet.subtract;
  let toSet = HashSet.toSet;
  let toMap = HashSet.toMap;
  let toSequence = HashSet.toSequence;
  let tryFind = HashSet.tryFind;
  let union = HashSet.union;
};

let module EqualitySet = {
  type t = HashSet.t int;

  let add = HashSet.add;
  let addAll = HashSet.addAll;
  let contains = HashSet.contains;
  let count = HashSet.count;
  let empty = fun () => HashSet.emptyWith HashStrategy.structuralEquality;
  let equals = HashSet.equals;
  let every = HashSet.every;
  let find = HashSet.find;
  let forEach = HashSet.forEach;
  let from = HashSet.from;
  let hash = HashSet.hash;
  let intersect = HashSet.intersect;
  let isEmpty = HashSet.isEmpty;
  let isNotEmpty = HashSet.isNotEmpty;
  let none = HashSet.none;
  let reduce = HashSet.reduce;
  let remove = HashSet.remove;
  let removeAll = HashSet.removeAll;
  let some = HashSet.some;
  let subtract = HashSet.subtract;
  let toSet = HashSet.toSet;
  let toMap = HashSet.toMap;
  let toSequence = HashSet.toSequence;
  let tryFind = HashSet.tryFind;
  let union = HashSet.union;
};

let module BadHashComparisonSet = {
  type t = HashSet.t int;

  let add = HashSet.add;
  let addAll = HashSet.addAll;
  let contains = HashSet.contains;
  let count = HashSet.count;
  let empty = fun () => {
    let badHashStrategy = HashStrategy.createWithComparator (fun _ => 10) Comparator.structural;
    HashSet.emptyWith badHashStrategy;
  };
  let equals = HashSet.equals;
  let every = HashSet.every;
  let find = HashSet.find;
  let forEach = HashSet.forEach;
  let from = HashSet.from;
  let hash = HashSet.hash;
  let intersect = HashSet.intersect;
  let isEmpty = HashSet.isEmpty;
  let isNotEmpty = HashSet.isNotEmpty;
  let none = HashSet.none;
  let reduce = HashSet.reduce;
  let remove = HashSet.remove;
  let removeAll = HashSet.removeAll;
  let some = HashSet.some;
  let subtract = HashSet.subtract;
  let toSet = HashSet.toSet;
  let toMap = HashSet.toMap;
  let toSequence = HashSet.toSequence;
  let tryFind = HashSet.tryFind;
  let union = HashSet.union;
};

let module BadHashEqualitySet = {
  type t = HashSet.t int;

  let add = HashSet.add;
  let addAll = HashSet.addAll;
  let contains = HashSet.contains;
  let count = HashSet.count;
  let empty = fun () => {
    let badHashStrategy = HashStrategy.createWithEquality (fun _ => 10) Equality.structural;
    HashSet.emptyWith badHashStrategy;
  };
  let equals = HashSet.equals;
  let every = HashSet.every;
  let find = HashSet.find;
  let forEach = HashSet.forEach;
  let from = HashSet.from;
  let hash = HashSet.hash;
  let intersect = HashSet.intersect;
  let isEmpty = HashSet.isEmpty;
  let isNotEmpty = HashSet.isNotEmpty;
  let none = HashSet.none;
  let reduce = HashSet.reduce;
  let remove = HashSet.remove;
  let removeAll = HashSet.removeAll;
  let some = HashSet.some;
  let subtract = HashSet.subtract;
  let toSet = HashSet.toSet;
  let toMap = HashSet.toMap;
  let toSequence = HashSet.toSequence;
  let tryFind = HashSet.tryFind;
  let union = HashSet.union;
};

let transientHashSetTest (count: int): (list Test.t) => [
  it (sprintf "add with %i elements" count) (fun () => {
    let src = IntRange.create 0 count |> IntRange.toSequence;

    let (_, mapOfSizeN) = src
      |> Sequence.scan
        (fun (_, acc) i => (i, acc |> TransientHashSet.add i))
        (0, HashSet.empty |> HashSet.mutate)
      |> Sequence.doOnNext(fun (i, acc) => {
        expect (acc |> TransientHashSet.contains i) |> toBeEqualToTrue;
        expect (acc |> TransientHashSet.count) |> toBeEqualToInt (i + 1);
        expect (acc |> TransientHashSet.isEmpty) |> toBeEqualToFalse;
        expect (acc |> TransientHashSet.isNotEmpty) |> toBeEqualToTrue;
      }) |> Sequence.last;

    src |> Sequence.forEach (fun i =>
      expect (mapOfSizeN |> TransientHashSet.contains i) |> toBeEqualToTrue
    );
  }),
  it (sprintf "remove with %i elements" count) (fun () => {
    let hash = Hash.random ();
    let transient = HashSet.empty
      |> HashSet.mutate
      |> TransientHashSet.addAll (
        IntRange.create 0 count |> IntRange.toIterator |> Iterator.map hash
      );

    Sequence.generate (fun i => i + 2) 0
      |> Sequence.take (count / 2)
      |> Sequence.map hash
      |> Sequence.forEach (fun i => {
        transient |> TransientHashSet.remove i |> ignore;
      });

    Sequence.generate (fun i => i + 2) 1
      |> Sequence.take (count / 2)
      |> Sequence.map hash
      |> Sequence.forEach (fun i => {
        expect (transient |> TransientHashSet.contains i) |> toBeEqualToTrue;
      });

    Sequence.generate (fun i => i + 2) 0
      |> Sequence.take (count / 2)
      |> Sequence.map hash
      |> Sequence.forEach (fun i => {
        expect (transient |> TransientHashSet.contains i) |> toBeEqualToFalse;
      });
  }),
  it (sprintf "removeAll with %i elements" count) (fun () => {
    let hash = Hash.random ();
    let transient = HashSet.empty
      |> HashSet.mutate
      |> TransientHashSet.addAll (
          IntRange.create 0 count |> IntRange.toIterator |> Iterator.map hash
      );
    transient |> TransientHashSet.removeAll |> ignore;
    expect (transient |> TransientHashSet.isEmpty) |> toBeEqualToTrue;
  }),
];

let test = describe "HashSet" (List.fromReversed @@ Iterator.concat @@ [
  describe "poor hash function comparator" (SetTester.test 100 (module BadHashComparisonSet)) |> Iterator.return,
  describe "poor hash function equality" (SetTester.test 100 (module BadHashEqualitySet)) |> Iterator.return,
  describe "comparator" (List.fromReversed @@ Iterator.concat @@ [
    (SetTester.test 10 (module ComparatorSet)) |> List.toIterator,
    (SetTester.test 48 (module ComparatorSet)) |> List.toIterator,
    (SetTester.test 90 (module ComparatorSet)) |> List.toIterator,
    (SetTester.test 500 (module ComparatorSet)) |> List.toIterator,
    (SetTester.test 5000 (module ComparatorSet)) |> List.toIterator,
  ]) |> Iterator.return,
  describe "equality" (List.fromReversed @@ Iterator.concat @@ [
    (SetTester.test 10 (module EqualitySet)) |> List.toIterator,
    (SetTester.test 48 (module EqualitySet)) |> List.toIterator,
    (SetTester.test 90 (module EqualitySet)) |> List.toIterator,
    (SetTester.test 500 (module EqualitySet)) |> List.toIterator,
    (SetTester.test 5000 (module EqualitySet)) |> List.toIterator,
  ]) |> Iterator.return,
  describe "TransientHashSet" (List.fromReversed @@ Iterator.concat @@ [
    transientHashSetTest 10 |> List.toIterator,
    transientHashSetTest 48 |> List.toIterator,
    transientHashSetTest 90 |> List.toIterator,
    transientHashSetTest 500 |> List.toIterator,
    transientHashSetTest 5000 |> List.toIterator,
  ]) |> Iterator.return,
]);
