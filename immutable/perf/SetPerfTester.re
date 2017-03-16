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
    IntRange.create 0 n
      |> IntRange.reduce
        (fun acc i => acc |> add (hash i))
        (empty ())
      |> ignore
  }),

  it (sprintf "set with %i elements, remove %i elements" n (n / 3)) (fun () => {
    let map = getTestData ();
    let keysToRemove = keys () |> IntRange.toSeq |> Seq.buffer 1 3 |> Seq.map (fun [i] => i);

    keysToRemove |> Seq.reduce (fun acc i => acc |> remove i) map |> ignore;
  }),

  it (sprintf "set with %i elements, update %i elements" n (n / 3)) (fun () => {
    let map = getTestData ();
    let keysToUpdate = keys () |> IntRange.toSeq |> Seq.buffer 1 3 |> Seq.map (fun [i] => i);

    keysToUpdate |> Seq.reduce (fun acc i => acc |> add i) map |> ignore;
  }),

  it (sprintf "contains %i values" n) (fun () => {
    let map = getTestData ();

    keys () |> IntRange.forEach (fun i => map |> contains i |> ignore);
  }),
];

let module CamlIntSet = CamlSet.Make {
  type t = int;
  let compare = Pervasives.compare;
};

let test (n: int) (count: int): Test.t => {
  let keys = IntRange.create 0 count;

  let camlIntSet = keys
    |> IntRange.reduce
      (fun acc i => acc |> CamlIntSet.add (hash i))
      CamlIntSet.empty;

  let hashSetComparison = keys
    |> IntRange.reduce
      (fun acc i => acc |> TransientHashSet.add i)
      (TransientHashSet.empty ())
    |> TransientHashSet.persist;

  let hashSetEquality = keys
    |> IntRange.reduce
      (fun acc i => acc |> TransientHashSet.add i)
      (TransientHashSet.emptyWith HashStrategy.structuralEquality)
    |> TransientHashSet.persist;

  let intSet = keys
    |> IntRange.reduce
      (fun acc i => acc |> TransientIntSet.add i)
      (TransientIntSet.empty ())
    |> TransientIntSet.persist;

  let emptyHashSetEquality = HashSet.emptyWith HashStrategy.structuralEquality;

  let sortedSet = keys
    |> IntRange.reduce
      (fun acc i => acc |> SortedSet.add i)
      SortedSet.empty;

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

  let tests = Seq.repeat testGroup (Some n) |> Seq.flatMap List.toSeq |> List.fromSeqReversed;
  describe (sprintf "SetPerf") tests
};
