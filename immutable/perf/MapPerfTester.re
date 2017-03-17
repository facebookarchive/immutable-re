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

/* Hash the indexes to ensure that results aren't skewed by continuous keys */
let hash = Hash.random ();

let generateTests
    (getTestData: unit => 'map)
    (keys: unit => IntRange.t)
    (empty: unit => 'map)
    (put: int => int => 'map => 'map)
    (remove: int => 'map => 'map)
    (tryGet: int => 'map => option int)
    (n: int): (list Test.t) => [
  it (sprintf "put %i elements" n) (fun () => {
    IntRange.create 0 n
      |> IntRange.reduce (fun acc i => acc |> put (hash i) i) (empty ())
      |> ignore;
  }),

  it (sprintf "map with %i elements, remove %i elements" n (n / 3)) (fun () => {
    let map = getTestData ();
    let keysToRemove = keys () |> IntRange.toSeq |> Seq.buffer 1 3 |> Seq.map (fun [i] => i);

    keysToRemove |> Seq.reduce (fun acc i => acc |> remove i) map |> ignore;
  }),

  it (sprintf "map with %i elements, update %i elements" n (n / 3)) (fun () => {
    let map = getTestData ();
    let keysToUpdate = keys () |> IntRange.toSeq |> Seq.buffer 1 3 |> Seq.map (fun [i] => i);

    /* Multiply the updated value to avoid optimizations */
    keysToUpdate |> Seq.reduce (fun acc i => acc |> put i (i + 1)) map |> ignore;
  }),

  it (sprintf "tryGet %i values" n) (fun () => {
    let map = getTestData ();

    keys () |> IntRange.forEach (fun i => map |> tryGet i |> ignore);
  }),
];

let module CamlIntMap = CamlMap.Make {
  type t = int;
  let compare = Pervasives.compare;
};

let test (n: int) (count: int): Test.t => {
  let keys = IntRange.create 0 count;

  let camlIntMap = keys |> IntRange.reduce
    (fun acc i => acc |> CamlIntMap.add (hash i) i)
    CamlIntMap.empty;

  let hashMapComparison = keys
    |> IntRange.reduce
      (fun acc i => acc |> TransientHashMap.put (hash i) i)
      (TransientHashMap.empty ())
    |> TransientHashMap.persist;

  let hashMapEquality = keys
    |> IntRange.reduce
      (fun acc i => acc |> TransientHashMap.put (hash i) i)
      (TransientHashMap.emptyWith HashStrategy.structuralEquality)
    |> TransientHashMap.persist;

  let intMap = keys
    |> IntRange.reduce
      (fun acc i => acc |> TransientIntMap.put i i)
      (TransientIntMap.empty ())
    |> TransientIntMap.persist;

  let sortedMap = keys
    |> IntRange.reduce
      (fun acc i => acc |> SortedMap.put i i)
      SortedMap.empty;

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

    describe "SortedMap" (
      generateTests
        (fun () => sortedMap)
        (fun () => keys)
        (fun () => SortedMap.empty)
        SortedMap.put
        SortedMap.remove
        SortedMap.tryGet
        count
    ),

    describe "HashMap" [
      describe "Comparison" (
        generateTests
          (fun () => hashMapComparison)
          (fun () => keys)
          (fun () => HashMap.empty)
          HashMap.put
          HashMap.remove
          HashMap.tryGet
          count
      ),
      describe "Equality" (
        generateTests
          (fun () => hashMapEquality)
          (fun () => keys)
          (fun () => HashMap.emptyWith HashStrategy.structuralEquality)
          HashMap.put
          HashMap.remove
          HashMap.tryGet
          count
      ),
    ],

    describe "TransientHashMap" [
      describe "Comparison" (
        generateTests
          (fun () => hashMapComparison |> HashMap.mutate)
          (fun () => keys)
          TransientHashMap.empty
          TransientHashMap.put
          TransientHashMap.remove
          TransientHashMap.tryGet
          count
      ),

      describe "Equality" (
        generateTests
          (fun () => hashMapEquality |> HashMap.mutate)
          (fun () => keys)
          (fun () => TransientHashMap.emptyWith HashStrategy.structuralEquality)
          TransientHashMap.put
          TransientHashMap.remove
          TransientHashMap.tryGet
          count
      ),
    ],

    describe "IntMap" (
      generateTests
        (fun () => intMap)
        (fun () => keys)
        (fun () => IntMap.empty)
        IntMap.put
        IntMap.remove
        IntMap.tryGet
        count
    ),

    describe "TransientIntMap" (
      generateTests
        (fun () => intMap |> IntMap.mutate)
        (fun () => keys)
        TransientIntMap.empty
        TransientIntMap.put
        TransientIntMap.remove
        TransientIntMap.tryGet
        count
    ),
  ];

  let tests = Seq.repeat testGroup
    |> Seq.take n
    |> Seq.flatMap List.toSeq
    |> Seq.toIterable
    |> List.fromReversed;
  describe (sprintf "MapPerf") tests
};
