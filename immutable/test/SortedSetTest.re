 /*
  * vim: set ft=rust:
  * vim: set ft=reason:
  */

open Immutable;
open MapTester;
open ReUnit;
open ReUnit.Test;

let numOfElements = 10000;

let test = describe "SortedSet" (
  SetTester.test
    (fun () => SortedSet.empty)
    SortedSet.put
    SortedSet.remove
    SortedSet.contains
    numOfElements
);
