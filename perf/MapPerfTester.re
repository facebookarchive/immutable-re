/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

let module CamlMap = Map;
open Immutable;
open Printf;
open ReUnit;
open ReUnit.Test;

let hash = Hashtbl.hash;

let generateTests
    (getTestData: unit => 'map)
    (keys: unit => Iterable.t int)
    (empty: unit => 'map)
    (put: int => int => 'map => 'map)
    (remove: int => 'map => 'map)
    (get: int => 'map => option int)
    (n: int): (list Test.t) => [
  it (sprintf "put %i elements" n) (fun () => {
    IntRange.create start::0 count::n
      |> IntRange.reduce (fun acc i => acc |> put (hash i) i) (empty ())
      |> ignore;
  }),

  it (sprintf "map with %i elements, remove %i elements" n (n / 3)) (fun () => {
    let map = getTestData ();
    let keysToRemove = keys ()
      |> Iterable.buffer count::1 skip::3
      |> Iterable.map (fun [i] => i);

    keysToRemove |> Iterable.reduce (fun acc i => acc |> remove i) map |> ignore;
  }),

  it (sprintf "map with %i elements, update %i elements" n (n / 3)) (fun () => {
    let map = getTestData ();
    let keysToUpdate = keys ()
      |> Iterable.buffer count::1 skip::3
      |> Iterable.map (fun [i] => i);

    /* Multiply the updated value to avoid optimizations */
    keysToUpdate |> Iterable.reduce (fun acc i => acc |> put i (i + 1)) map |> ignore;
  }),

  it (sprintf "get %i values" n) (fun () => {
    let map = getTestData ();

    keys () |> Iterable.forEach (fun i => map |> get i |> ignore);
  }),
];

let module CamlIntMap = CamlMap.Make {
  type t = int;
  let compare (this: int) (that: int): int =>
    if (this < that) (-1)
    else if (this > that) 1
    else 0;
};

let module SortedIntMap = SortedMap.Make1 {
  type t = int;

  let compare = Comparator.int;
  let equals = Equality.int;
};

let test (n: int) (count: int): Test.t => {
  let keys = IntRange.create start::0 count::count |> IntRange.toIterable |> Iterable.map hash;

  let camlIntMap = keys |> Iterable.reduce
    (fun acc i => acc |> CamlIntMap.add i i)
    CamlIntMap.empty;

  let hashMapEmpty = HashMap.emptyWith
    hash::(fun i => i)
    comparator::Comparator.int;

  let (<|) (f: 'a => 'b) (a: 'a): ('b) => f a;

  let hashMap = keys
    |> Iterable.map (fun i => (i, i))
    |> HashMap.putAllEntries
    <| hashMapEmpty;

  let intMap = keys
    |> Iterable.map (fun i => (i, i))
    |> IntMap.fromEntries;

  let sortedMap = keys
    |> Iterable.map (fun i => (i, i))
    |> SortedIntMap.fromEntries;

  let testGroup = [
    describe "CamlIntMap" (
      generateTests
        (fun () => camlIntMap)
        (fun () => keys)
        (fun () => CamlIntMap.empty)
        CamlIntMap.add
        CamlIntMap.remove
        (fun k map =>
          if (CamlIntMap.mem k map) (Some (CamlIntMap.find k map))
          else None
        )
        count
    ),

    describe "SortedIntMap" (
      generateTests
        (fun () => sortedMap)
        (fun () => keys)
        (fun () => SortedIntMap.empty ())
        SortedIntMap.put
        SortedIntMap.remove
        SortedIntMap.get
        count
    ),

    describe "HashMap" (
      generateTests
        (fun () => hashMap)
        (fun () => keys)
        (fun () => hashMapEmpty)
        HashMap.put
        HashMap.remove
        HashMap.get
        count
    ),

    describe "HashMap.Transient" (
      generateTests
        (fun () => hashMap |> HashMap.mutate)
        (fun () => keys)
        (fun () => hashMapEmpty |> HashMap.mutate)
        HashMap.Transient.put
        HashMap.Transient.remove
        HashMap.Transient.get
        count
    ),

    describe "IntMap" (
      generateTests
        (fun () => intMap)
        (fun () => keys)
        (fun () => IntMap.empty ())
        IntMap.put
        IntMap.remove
        IntMap.get
        count
    ),

    describe "IntMap.Transient" (
      generateTests
        (fun () => intMap |> IntMap.mutate)
        (fun () => keys)
        IntMap.Transient.empty
        IntMap.Transient.put
        IntMap.Transient.remove
        IntMap.Transient.get
        count
    ),
  ];

  let tests = Sequence.generate (fun i => i) testGroup
    |> Sequence.take n
    |> Sequence.flatMap List.toSequence
    |> Sequence.toIterable
    |> List.fromReverse;
  describe (sprintf "MapPerf") tests
};
