/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Immutable;
open MapTester;
open ReUnit;
open ReUnit.Test;

let numOfElements = 10000;

let test = describe "SortedMap" (
  MapTester.test
      SortedMap.empty
      SortedMap.put
      SortedMap.remove
      SortedMap.tryGet
      numOfElements
);
