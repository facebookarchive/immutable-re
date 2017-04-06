/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Immutable;
open Printf;
open ReUnit;
open ReUnit.Test;

let generateTests
    (getTestData: unit => 'vector)
    (empty: unit => 'vector)
    (add: int  => 'vector => 'vector)
    (update: int => int => 'vector => 'vector)
    (removeLast: 'vector => 'vector)
    (get: int => 'vector => option int)
    (n: int): list Test.t => [
  it (sprintf "add %i elements" n) (fun () => {
    let src = IntRange.create start::0 count::n;
    src |> IntRange.reduce (fun acc i => acc |> add i) (empty ()) |> ignore;
  }),
  it (sprintf "vector with %i elements, removeLast %i elements" n (n / 2)) (fun () => {
    IntRange.create start::0 count::(n / 2)
      |> IntRange.reduce (fun acc _ => acc |> removeLast) (getTestData ()) |> ignore;
  }),
  it (sprintf "vector with %i elements, update %i elements alternating" n (n / 2)) (fun () => {
    Sequence.generate (fun i => i + 2) 0
      |> Sequence.take (n / 2)
      |> Sequence.reduce (fun acc i => acc |> update i (n - i)) (getTestData ()) |> ignore;
  }),
  it (sprintf "get %i values" n) (fun () => {
    let vec = getTestData ();
    IntRange.create start::0 count::n
      |> IntRange.toIterable
      |> Iterable.forEach
        (fun i => vec |> get i |> ignore);
  }),
];

let test (n: int) (count: int): Test.t => {
  let indexes = IntRange.create start::0 count::count;

  let mutableArray = Array.init count (fun i => i);

  let list = indexes |> IntRange.toIterable |> List.fromReverse;
  let stack = indexes |> IntRange.toIterable |> Stack.fromReverse;
  let vector = indexes
    |> IntRange.reduce (fun acc i => acc |> Vector.Transient.addLast i) (Vector.Transient.empty ())
    |> Vector.Transient.persist;

  let mutableArray = Array.init count (fun i => i);

  let list = indexes |> IntRange.toIterable |> List.fromReverse;
  let stack = indexes |> IntRange.toIterable |> Stack.fromReverse;

  let testGroup = [
    describe "CamlMutableArray" (
      generateTests
        (fun () => mutableArray)
        (fun () => mutableArray)
        (fun a v => { v.(0) = a; v })
        (fun i a v => { v.(i) = a; v })
        (fun v => { v.(0) |> ignore; v })
        (fun i v => { Some (v.(i)) })
        count
    ),
    describe "List" (
      generateTests
        (fun () => list)
        (fun () => [])
        List.addFirst
        (fun _ _ v => v)
        List.removeFirstOrRaise
        (fun _ _ => None)
        count
    ),
    describe "Stack" (
      generateTests
        (fun () => stack)
        Stack.empty
        Stack.addFirst
        (fun _ _ v => v)
        Stack.removeFirstOrRaise
        (fun _ _ => None)
        count
    ),
    describe "Vector" (
      generateTests
        (fun () => vector)
        Vector.empty
        Vector.addLast
        Vector.update
        Vector.removeLastOrRaise
        Vector.get
        count
    ),
    describe "Vector.Transient" (
      generateTests
        (fun () => vector |> Vector.mutate)
        (fun () => Vector.empty () |> Vector.mutate)
        Vector.Transient.addLast
        Vector.Transient.update
        Vector.Transient.removeLastOrRaise
        Vector.Transient.get
        count
    ),
  ];

  let tests = Sequence.generate (fun i => i) testGroup
    |> Sequence.take n
    |> Sequence.flatMap List.toSequence
    |> Sequence.toIterable
    |> List.fromReverse;
  describe "VectorPerf" tests;
};
