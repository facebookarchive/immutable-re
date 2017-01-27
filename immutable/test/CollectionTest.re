/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Functions;
open Immutable;
open ReUnit;
open ReUnit.Expect;
open ReUnit.Test;

let test = describe "Collection" [
  it "contains" (fun () => {
    let collection = ["a", "b", "c"] |> List.toSeq |> HashSet.fromSeq |> HashSet.toCollection;
    expect (collection |> Collection.contains "b") |> toBeEqualToTrue;
    expect (collection |> Collection.contains "d") |> toBeEqualToFalse;
  }),
  it "count" (fun () => {
    let collection = ["a", "b", "c"] |> List.toSeq |> HashSet.fromSeq |> HashSet.toCollection;
    expect (collection |> Collection.count) |> toBeEqualToInt 3;
  }),
  it "empty" (fun () => {
    expect (Collection.empty |> Collection.count) |> toBeEqualToInt 0;
    expect (Collection.empty |> Collection.contains "b") |> toBeEqualToFalse;
    expect (Collection.empty |> Collection.toSeq) |> toBeEqualToEmptySeq (Functions.return "");
  }),
  describe "equals" [],
  describe "hash" [],
  it "intersect" (fun () => {
    let collectionA = ["a", "b", "c"] |> List.toSeq |> HashSet.fromSeq |> HashSet.toCollection;
    let collectionB = ["b", "c", "d"] |> List.toSeq |> HashSet.fromSeq |> HashSet.toCollection;

    let intersection = Collection.intersect collectionA collectionB
      |> HashSet.fromSeq
      |> HashSet.toCollection;

    let expected = ["b", "c"] |> List.toSeq |> HashSet.fromSeq |> HashSet.toCollection;

    expect (Collection.equals intersection expected) |> toBeEqualToTrue;
  }),
  it "subtractFrom" (fun () => {
    let collectionA = ["a", "b", "c"] |> List.toSeq |> HashSet.fromSeq |> HashSet.toCollection;
    let collectionB = ["b", "c", "d"] |> List.toSeq |> HashSet.fromSeq |> HashSet.toCollection;

    let subtracted = collectionB |> Collection.subtractFrom collectionA
      |> HashSet.fromSeq
      |> HashSet.toCollection;

    let expected = ["a"] |> List.toSeq |> HashSet.fromSeq |> HashSet.toCollection;

    expect (Collection.equals subtracted expected) |> toBeEqualToTrue;
  }),
  describe "toSeq" [],
  it "union"  (fun () => {
    let collectionA = ["a", "b", "c"] |> List.toSeq |> HashSet.fromSeq |> HashSet.toCollection;
    let collectionB = ["b", "c", "d"] |> List.toSeq |> HashSet.fromSeq |> HashSet.toCollection;

    let union =  Collection.union collectionA collectionB
      |> HashSet.fromSeq
      |> HashSet.toCollection;

    let expected = ["a", "b", "c", "d"] |> List.toSeq |> HashSet.fromSeq |> HashSet.toCollection;

    expect (Collection.equals union expected) |> toBeEqualToTrue;
  }),
];
