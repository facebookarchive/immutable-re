/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

let module CamlSet = Set;
open Immutable;
open Printf;
open ReUnit;
open ReUnit.Test;

/* Hash the indexes to ensure that results aren't skewed by continuous keys */
let hash = Hash.random ();

let generateTests
    (getTestData: unit => 'set)
    (keys: unit => IntRange.t)
    (empty: unit => 'set)
    (add: int => 'set => 'set)
    (remove: int => 'set => 'set)
    (contains: int => 'set => bool)
    (n: int): (list Test.t) => [
  it (sprintf "add %i elements" n) (fun () => {
    IntRange.create start::0 count::n
      |> IntRange.reduce
        (fun acc i => acc |> add (hash i))
        (empty ())
      |> ignore
  }),

  it (sprintf "set with %i elements, remove %i elements" n (n / 3)) (fun () => {
    let map = getTestData ();
    let keysToRemove = keys ()
      |> IntRange.toSequence
      |> Sequence.buffer count::1 skip::3
      |> Sequence.map (fun [i] => i);

    keysToRemove |> Sequence.reduce (fun acc i => acc |> remove i) map |> ignore;
  }),

  it (sprintf "set with %i elements, update %i elements" n (n / 3)) (fun () => {
    let map = getTestData ();
    let keysToUpdate = keys ()
      |> IntRange.toSequence
      |> Sequence.buffer count::1 skip::3
      |> Sequence.map (fun [i] => i);

    keysToUpdate |> Sequence.reduce (fun acc i => acc |> add i) map |> ignore;
  }),

  it (sprintf "contains %i values" n) (fun () => {
    let map = getTestData ();

    keys () |> IntRange.toIterator |> Iterator.forEach (fun i => map |> contains i |> ignore);
  }),
];

let module CamlIntSet = CamlSet.Make {
  type t = int;
  let compare = Pervasives.compare;
};

let module SortedIntSet = SortedSet.Make {
  type t = int;

  let compare = Comparator.structural;
  let equals = Equality.structural;
};


let test (n: int) (count: int): Test.t => {
  let keys = IntRange.create start::0 count::count;

  let camlIntSet = keys
    |> IntRange.reduce
      (fun acc i => acc |> CamlIntSet.add (hash i))
      CamlIntSet.empty;

  let hashStrategyComparator = HashStrategy.createWithComparator
    hash::(fun i => i)
    comparator::Comparator.structural;

  let hashStrategyEquality = HashStrategy.createWithEquality
    hash::(fun i => i)
    equality::Equality.structural;

  let hashSetComparison = keys
    |> IntRange.reduce
      (fun acc i => acc |> TransientHashSet.add i)
      (TransientHashSet.emptyWith hashStrategyComparator)
    |> TransientHashSet.persist;

  let hashSetEquality = keys
    |> IntRange.reduce
      (fun acc i => acc |> TransientHashSet.add i)
      (TransientHashSet.emptyWith hashStrategyEquality)
    |> TransientHashSet.persist;

  let intSet = keys
    |> IntRange.reduce
      (fun acc i => acc |> TransientIntSet.add i)
      (TransientIntSet.empty ())
    |> TransientIntSet.persist;

  let emptyHashSetEquality = HashSet.emptyWith HashStrategy.structuralEquality;

  let sortedSet = keys
    |> IntRange.reduce
      (fun acc i => acc |> SortedIntSet.add i)
      SortedIntSet.empty;

  let testGroup = [
    describe "CamlIntSet" (
      generateTests
        (fun () => camlIntSet)
        (fun () => keys)
        (fun () => CamlIntSet.empty)
        CamlIntSet.add
        CamlIntSet.remove
        (fun k map => CamlIntSet.mem k map)
        count
    ),

    describe "SortedIntSet" (
      generateTests
        (fun () => sortedSet)
        (fun () => keys)
        (fun () => SortedIntSet.empty)
        SortedIntSet.add
        SortedIntSet.remove
        SortedIntSet.contains
        count
    ),

    describe "HashSet" [
      describe "Comparison" (
        generateTests
          (fun () => hashSetComparison)
          (fun () => keys)
          (fun () => HashSet.emptyWith hashStrategyComparator)
          HashSet.add
          HashSet.remove
          HashSet.contains
          count
      ),
      describe "Equality" (
        generateTests
          (fun () => hashSetEquality)
          (fun () => keys)
          (fun () => emptyHashSetEquality)
          HashSet.add
          HashSet.remove
          HashSet.contains
          count
      ),
    ],

    describe "TransientHashSet" [
      describe "Comparison" (
        generateTests
          (fun () => hashSetComparison |> HashSet.mutate)
          (fun () => keys)
          (fun () => TransientHashSet.emptyWith hashStrategyComparator)
          TransientHashSet.add
          TransientHashSet.remove
          TransientHashSet.contains
          count
      ),

      describe "Equality" (
        generateTests
          (fun () => hashSetEquality |> HashSet.mutate)
          (fun () => keys)
          (fun () => TransientHashSet.emptyWith hashStrategyEquality)
          TransientHashSet.add
          TransientHashSet.remove
          TransientHashSet.contains
          count
      ),
    ],

    describe "IntSet" (
      generateTests
        (fun () => intSet)
        (fun () => keys)
        (fun () => IntSet.empty)
        IntSet.add
        IntSet.remove
        IntSet.contains
        count
    ),

    describe "TransientIntSet" (
      generateTests
        (fun () => intSet |> IntSet.mutate)
        (fun () => keys)
        (fun () => IntSet.empty |> IntSet.mutate)
        TransientIntSet.add
        TransientIntSet.remove
        TransientIntSet.contains
        count
    ),
  ];

  let tests = Sequence.repeat testGroup
    |> Sequence.take n
    |> Sequence.flatMap List.toSequence
    |> Sequence.toIterator
    |> List.fromReverse;
  describe (sprintf "SetPerf") tests
};
