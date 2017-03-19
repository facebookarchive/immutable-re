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
    let src = IntRange.create 0 n;
    src |> IntRange.reduce (fun acc i => acc |> add i) (empty ()) |> ignore;
  }),
  it (sprintf "vector with %i elements, removeLast %i elements" n (n / 2)) (fun () => {
    IntRange.create 0 (n / 2)
      |> IntRange.reduce (fun acc _ => acc |> removeLast) (getTestData ()) |> ignore;
  }),
  it (sprintf "vector with %i elements, update %i elements alternating" n (n / 2)) (fun () => {
    Sequence.generate (fun i => i + 2) 0
      |> Sequence.take (n / 2)
      |> Sequence.reduce (fun acc i => acc |> update i (n - i)) (getTestData ()) |> ignore;
  }),
  it (sprintf "get %i values" n) (fun () => {
    let vec = getTestData ();
    IntRange.create 0 n
      |> IntRange.forEach
        (fun i => vec |> get i |> ignore);
  }),
];

let test (n: int) (count: int): Test.t => {
  let indexes = IntRange.create 0 count;

  let mutableArray = Array.init count (fun i => i);

  let list = indexes |> IntRange.toIterator |> List.fromReverse;
  let stack = indexes |> IntRange.toIterator |> Stack.fromReverse;
  let vector = indexes
    |> IntRange.reduce (fun acc i => acc |> TransientVector.addLast i) (TransientVector.empty ())
    |> TransientVector.persist;

  let mutableArray = Array.init count (fun i => i);

  let list = indexes |> IntRange.toIterator |> List.fromReverse;
  let stack = indexes |> IntRange.toIterator |> Stack.fromReverse;

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
        (fun () => Stack.empty)
        Stack.addFirst
        (fun _ _ v => v)
        Stack.removeFirstOrRaise
        (fun _ _ => None)
        count
    ),
    describe "Vector" (
      generateTests
        (fun () => vector)
        (fun () => Vector.empty)
        Vector.addLast
        Vector.update
        Vector.removeLastOrRaise
        Vector.get
        count
    ),
    describe "TransientVector" (
      generateTests
        (fun () => vector |> Vector.mutate)
        (fun () => Vector.empty |> Vector.mutate)
        TransientVector.addLast
        TransientVector.update
        TransientVector.removeLastOrRaise
        TransientVector.get
        count
    ),
  ];

  let tests = Sequence.repeat testGroup
    |> Sequence.take n
    |> Sequence.flatMap List.toSequence
    |> Sequence.toIterator
    |> List.fromReverse;
  describe "VectorPerf" tests;
};
