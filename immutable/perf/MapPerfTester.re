/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Hash;
open Immutable;
open Map;
open Printf;
open ReUnit;
open ReUnit.Expect;
open ReUnit.Test;

/* Hash the indexes to ensure that results aren't skewed by continuous keys */
let hash = Hash.random ();

let generateTests
    (getTestData: unit => 'map)
    (keys: unit => Seq.t string)
    (empty: unit => 'map)
    (put: string => string => 'map => 'map)
    (remove: string => 'map => 'map)
    (tryGet: string => 'map => option string)
    (n: int): (list Test.t) => [
  it (sprintf "put %i elements" n) (fun () => {
    let keys = Seq.inRange 0 (Some n) 1 |> Seq.map hash |> Seq.map string_of_int;

    Seq.reduce
      (fun acc i => acc |> put i i)
      (empty ())
      keys
    |> ignore;
  }),

  it (sprintf "map with %i elements, remove %i elements" n (n / 3)) (fun () => {
    let map = getTestData ();
    let keysToRemove = keys () |> Seq.buffer 1 3 |> Seq.map (fun [i] => i);

    keysToRemove |> Seq.reduce (fun acc i => acc |> remove i) map |> ignore;
  }),

  it (sprintf "map with %i elements, update %i elements" n (n / 3)) (fun () => {
    let map = getTestData ();
    let keysToUpdate = keys () |> Seq.buffer 1 3 |> Seq.map (fun [i] => i);

    /* Multiply the updated value to avoid optimizations */
    keysToUpdate |> Seq.reduce (fun acc i => acc |> put i "a") map |> ignore;
  }),

  it (sprintf "tryGet %i values" n) (fun () => {
    let map = getTestData ();

    keys () |> Seq.forEach (fun i => map |> tryGet i |> ignore);
  }),
];

let module CamlStringMap = Map.Make {
  type t = string;
  let compare = Pervasives.compare;
};

let test (n: int): Test.t => {
  let keys = Seq.inRange 0 (Some n) 1 |> Seq.map hash |> Seq.map string_of_int;

  let camlIntMap = keys |> Seq.reduce
    (fun acc i => acc |> CamlStringMap.add i i)
    CamlStringMap.empty;

  let hashMapComparison = keys |> Seq.reduce
    (fun acc i => acc |> HashMap.put i i)
    (HashMap.empty ());

  let hashMapEquality = keys |> Seq.reduce
    (fun acc i => acc |> HashMap.put i i)
    (HashMap.emptyWith @@ HashStrategy.structuralEquality @@ ());

  let sortedMap = keys |> Seq.reduce
    (fun acc i => acc |> SortedMap.put i i)
    (SortedMap.empty ());

  describe (sprintf "MapPerf")[
    describe "HashMap" [
      describe "Comparison" (
        generateTests
          (fun () => hashMapComparison)
          (fun () => keys)
          HashMap.empty
          HashMap.put
          HashMap.remove
          HashMap.tryGet
          n
      ),
      describe "Equality" (
        generateTests
          (fun () => hashMapEquality)
          (fun () => keys)
          (fun () => HashMap.emptyWith @@ HashStrategy.structuralEquality @@ ())
          HashMap.put
          HashMap.remove
          HashMap.tryGet
          n
      ),
    ],

    describe "CamlStringMap" (
      generateTests
        (fun () => camlIntMap)
        (fun () => keys)
        (fun () => CamlStringMap.empty)
        CamlStringMap.add
        CamlStringMap.remove
        (fun k map => (CamlStringMap.mem k map) ? Some (CamlStringMap.find k map) : None)
        n
    ),

    describe "SortedMap" (
      generateTests
        (fun () => sortedMap)
        (fun () => keys)
        SortedMap.empty
        SortedMap.put
        SortedMap.remove
        SortedMap.tryGet
        n
    ),

    describe "TransientHashMap" [
      describe "Comparison" (
        generateTests
          (fun () => hashMapComparison |> HashMap.mutate)
          (fun () => keys)
          (fun () => HashMap.empty () |> HashMap.mutate)
          TransientHashMap.put
          TransientHashMap.remove
          TransientHashMap.tryGet
          n
      ),

      describe "Equality" (
        generateTests
          (fun () => hashMapEquality |> HashMap.mutate)
          (fun () => keys)
          (fun () => HashMap.emptyWith @@ HashStrategy.structuralEquality @@ () |> HashMap.mutate)
          TransientHashMap.put
          TransientHashMap.remove
          TransientHashMap.tryGet
          n
      ),
    ],
  ]
};
