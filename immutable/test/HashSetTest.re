open Immutable;
open ReUnit;
open ReUnit.Test;

let numOfElements = 10000;

let test = describe "HashSet" [
  describe "Comparator" (
    SetTester.test
      HashSet.empty
      HashSet.put
      HashSet.remove
      HashSet.contains
      numOfElements
  ),

  describe "Equality" (
    SetTester.test
      (fun () => HashSet.emptyWith @@ HashStrategy.structuralEquality @@ ())
      HashSet.put
      HashSet.remove
      HashSet.contains
      numOfElements
  ),

  describe "TransientHashSet" [
    describe "Comparator" (
      SetTester.test
        (fun () => HashSet.empty () |> HashSet.mutate)
        TransientHashSet.put
        TransientHashSet.remove
        TransientHashSet.contains
        numOfElements
    ),

    describe "Equality" (
      SetTester.test
        (fun () => (HashSet.emptyWith @@ HashStrategy.structuralEquality @@ ()) |> HashSet.mutate)
        TransientHashSet.put
        TransientHashSet.remove
        TransientHashSet.contains
        numOfElements
    ),
  ],
];
