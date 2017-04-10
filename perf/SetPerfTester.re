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
let hash = Hashtbl.hash;

let generateTests
    (getTestData: unit => 'set)
    (keys: unit => Iterable.t int)
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
      |> Iterable.buffer count::1 skip::3
      |> Iterable.map (fun [i] => i);

    keysToRemove |> Iterable.reduce (fun acc i => acc |> remove i) map |> ignore;
  }),

  it (sprintf "set with %i elements, update %i elements" n (n / 3)) (fun () => {
    let map = getTestData ();
    let keysToUpdate = keys ()
      |> Iterable.buffer count::1 skip::3
      |> Iterable.map (fun [i] => i);

    keysToUpdate |> Iterable.reduce (fun acc i => acc |> add i) map |> ignore;
  }),

  it (sprintf "contains %i values" n) (fun () => {
    let map = getTestData ();

    keys () |> Iterable.forEach (fun i => map |> contains i |> ignore);
  }),
];

let module CamlIntSet = CamlSet.Make {
  type t = int;
  let compare (this: int) (that: int): int =>
    if (this === that) 0
    else if (this > that) 1
    else (-1);
};

let module SortedIntSet = SortedSet.Make {
  type t = int;

  let compare = Comparator.int;
  let equals = Equality.int;
};

let test (n: int) (count: int): Test.t => {
  let keys = IntRange.create start::0 count::count |> IntRange.toIterable |> Iterable.map hash;

  let camlIntSet = keys
    |> Iterable.reduce
      (fun acc i => acc |> CamlIntSet.add (hash i))
      CamlIntSet.empty;

  let hashSetEmpty = HashSet.emptyWith
    hash::Functions.identity
    comparator::Comparator.int;

  let (<|) (f: 'a => 'b) (a: 'a): ('b) => f a;

  let hashSet = keys
    |> HashSet.addAll
    <| hashSetEmpty;

  let intSet = keys
    |> Iterable.reduce
      (fun acc i => acc |> IntSet.Transient.add i)
      (IntSet.Transient.empty ())
    |> IntSet.Transient.persist;

  let sortedSet = keys
    |> Iterable.reduce
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

    describe "HashSet" (
      generateTests
        (fun () => hashSet)
        (fun () => keys)
        (fun () => hashSetEmpty)
        HashSet.add
        HashSet.remove
        HashSet.contains
        count
    ),

    describe "HashSet.Transient" (
      generateTests
        (fun () => hashSet |> HashSet.mutate)
        (fun () => keys)
        (fun () => hashSetEmpty |> HashSet.mutate)
        HashSet.Transient.add
        HashSet.Transient.remove
        HashSet.Transient.contains
        count
    ),

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

    describe "IntSet.Transient" (
      generateTests
        (fun () => intSet |> IntSet.mutate)
        (fun () => keys)
        (fun () => IntSet.empty |> IntSet.mutate)
        IntSet.Transient.add
        IntSet.Transient.remove
        IntSet.Transient.contains
        count
    ),
  ];

  let tests = Sequence.generate (fun i => i) testGroup
    |> Sequence.take n
    |> Sequence.flatMap List.toSequence
    |> Sequence.toIterable
    |> List.fromReverse;
  describe (sprintf "SetPerf") tests
};
