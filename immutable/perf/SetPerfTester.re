open Immutable;
open Printf;
open ReUnit;
open ReUnit.Expect;
open ReUnit.Test;

/* Hash the indexes to ensure that results aren't skewed by continuous keys */
let hash = Hash.random ();

let generateTests
    (getTestData: unit => 'set)
    (keys: unit => Seq.t string)
    (empty: unit => 'set)
    (add: string => 'set => 'set)
    (remove: string => 'set => 'set)
    (contains: string => 'set => bool)
    (n: int): (list Test.t) => [
  it (sprintf "add %i elements" n) (fun () => {
    let keys = Seq.inRange 0 (Some n) 1 |> Seq.map hash |> Seq.map string_of_int;

    keys |> Seq.reduce (fun acc i => acc |> add i) (empty ()) |> ignore;
  }),

  it (sprintf "set with %i elements, remove %i elements" n (n / 3)) (fun () => {
    let map = getTestData ();
    let keysToRemove = keys () |> Seq.buffer 1 3 |> Seq.map (fun [i] => i);

    keysToRemove |> Seq.reduce (fun acc i => acc |> remove i) map |> ignore;
  }),

  it (sprintf "set with %i elements, update %i elements" n (n / 3)) (fun () => {
    let map = getTestData ();
    let keysToUpdate = keys () |> Seq.buffer 1 3 |> Seq.map (fun [i] => i);

    keysToUpdate |> Seq.reduce (fun acc i => acc |> add i) map |> ignore;
  }),

  it (sprintf "contains %i values" n) (fun () => {
    let map = getTestData ();

    keys () |> Seq.forEach (fun i => map |> contains i |> ignore);
  }),
];

let module CamlStringSet = Set.Make {
  type t = string;
  let compare = Pervasives.compare;
};

let test (n: int) (count: int): Test.t => {
  let keys = Seq.inRange 0 (Some count) 1 |> Seq.map hash |> Seq.map string_of_int;

  let camlStringSet = keys |> Seq.reduce
    (fun acc i => acc |> CamlStringSet.add i)
    CamlStringSet.empty;

  let hashSetComparison = keys |> Seq.reduce
    (fun acc i => acc |> HashSet.add i)
    HashSet.empty;

  let hashSetEquality = keys |> Seq.reduce
    (fun acc i => acc |> HashSet.add i)
    (HashSet.emptyWith HashStrategy.structuralEquality);

  let emptyHashSetEquality = HashSet.emptyWith HashStrategy.structuralEquality;

  let sortedSet = keys |> Seq.reduce
    (fun acc i => acc |> SortedSet.add i)
    SortedSet.empty;

  let testGroup = [
    describe "CamlStringSet" (
      generateTests
        (fun () => camlStringSet)
        (fun () => keys)
        (fun () => CamlStringSet.empty)
        CamlStringSet.add
        CamlStringSet.remove
        (fun k map => CamlStringSet.mem k map)
        count
    ),

    describe "SortedSet" (
      generateTests
        (fun () => sortedSet)
        (fun () => keys)
        (fun () => SortedSet.empty)
        SortedSet.add
        SortedSet.remove
        SortedSet.contains
        count
    ),

    describe "HashSet" [
      describe "Comparison" (
        generateTests
          (fun () => hashSetComparison)
          (fun () => keys)
          (fun () => HashSet.empty)
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
          (fun () => HashSet.empty |> HashSet.mutate)
          TransientHashSet.add
          TransientHashSet.remove
          TransientHashSet.contains
          count
      ),

      describe "Equality" (
        generateTests
          (fun () => hashSetEquality |> HashSet.mutate)
          (fun () => keys)
          (fun () => HashSet.emptyWith HashStrategy.structuralEquality |> HashSet.mutate)
          TransientHashSet.add
          TransientHashSet.remove
          TransientHashSet.contains
          count
      ),
    ],
  ];

  let tests = Seq.repeat testGroup (Some n) |> Seq.flatMap List.toSeq |> List.fromSeqReversed;
  describe (sprintf "SetPerf") tests
};
