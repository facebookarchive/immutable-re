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
open ReUnit.Expect;
open ReUnit.Test;

let module Set = {
  type t = IntSet.t;

  let add = IntSet.add;
  let addAll = IntSet.addAll;
  let contains = IntSet.contains;
  let count = IntSet.count;
  let empty = fun () => IntSet.empty;
  let equals = IntSet.equals;
  let every = IntSet.every;
  let find = IntSet.find;
  let forEach = IntSet.forEach;
  let from = IntSet.from;
  let hash = IntSet.hash;
  let intersect = IntSet.intersect;
  let isEmpty = IntSet.isEmpty;
  let isNotEmpty = IntSet.isNotEmpty;
  let none = IntSet.none;
  let reduce = IntSet.reduce;
  let remove = IntSet.remove;
  let removeAll = IntSet.removeAll;
  let some = IntSet.some;
  let subtract = IntSet.subtract;
  let toSet = IntSet.toSet;
  let toMap = IntSet.toMap;
  let toSequence = IntSet.toSequence;
  let tryFind = IntSet.tryFind;
  let union = IntSet.union;
};

let transientIntSetTest (count: int): (list Test.t) => [
  it (sprintf "add with %i elements" count) (fun () => {
    let src = IntRange.create 0 count |> IntRange.toSequence;

    let (_, mapOfSizeN) = src
      |> Sequence.scan
        (fun (_, acc) i => (i, acc |> TransientIntSet.add i))
        (0, IntSet.empty |> IntSet.mutate)
      |> Sequence.doOnNext(fun (i, acc) => {
        expect (acc |> TransientIntSet.contains i) |> toBeEqualToTrue;
        expect (acc |> TransientIntSet.count) |> toBeEqualToInt (i + 1);
        expect (acc |> TransientIntSet.isEmpty) |> toBeEqualToFalse;
        expect (acc |> TransientIntSet.isNotEmpty) |> toBeEqualToTrue;
      }) |> Sequence.last;

    src |> Sequence.forEach (fun i =>
      expect (mapOfSizeN |> TransientIntSet.contains i) |> toBeEqualToTrue
    );
  }),
  it (sprintf "remove with %i elements" count) (fun () => {
    let hash = Hash.random ();
    let transient = IntSet.empty
      |> IntSet.mutate
      |> TransientIntSet.addAll (
        IntRange.create 0 count |> IntRange.toIterator |> Iterator.map hash
      );

    Sequence.generate (fun i => i + 2) 0
      |> Sequence.take (count / 2)
      |> Sequence.map hash |> Sequence.forEach (fun i => {
        transient |> TransientIntSet.remove i |> ignore;
      });

    Sequence.generate (fun i => i + 2) 0
      |> Sequence.take (count / 2)
      |> Sequence.map hash |> Sequence.forEach (fun i => {
        expect (transient |> TransientIntSet.contains i) |> toBeEqualToTrue;
      });

    Sequence.generate (fun i => i + 2) 0
      |> Sequence.take (count / 2)
      |> Sequence.map hash |> Sequence.forEach (fun i => {
        expect (transient |> TransientIntSet.contains i) |> toBeEqualToFalse;
      });
  }),
  it (sprintf "removeAll with %i elements" count) (fun () => {
    let hash = Hash.random ();
    let transient = IntSet.empty
      |> IntSet.mutate
      |> TransientIntSet.addAll (
        IntRange.create 0 count |> IntRange.toIterator |> Iterator.map hash
      );
    transient |> TransientIntSet.removeAll |> ignore;
    expect (transient |> TransientIntSet.isEmpty) |> toBeEqualToTrue;
  }),
];

let test = describe "IntSet" (List.fromReversed @@ Iterator.concat @@ [
  describe "IntSet" (List.fromReversed @@ Iterator.concat @@ [
    (SetTester.test 10 (module Set)) |> List.toIterator,
    (SetTester.test 48 (module Set)) |> List.toIterator,
    (SetTester.test 90 (module Set)) |> List.toIterator,
    (SetTester.test 500 (module Set)) |> List.toIterator,
    (SetTester.test 5000 (module Set)) |> List.toIterator,
  ]) |> Iterator.return,
  describe "TransientIntSet" (List.fromReversed @@ Iterator.concat @@ [
    transientIntSetTest 10 |> List.toIterator,
    transientIntSetTest 48 |> List.toIterator,
    transientIntSetTest 90 |> List.toIterator,
    transientIntSetTest 500 |> List.toIterator,
    transientIntSetTest 5000 |> List.toIterator,
  ]) |> Iterator.return,
]);
