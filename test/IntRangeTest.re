/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Immutable;
open ReUnit;
open ReUnit.Test;

let test = describe "IntRange" [
  it "compare" (fun () => {
    IntRange.compare
        (IntRange.create start::0 count::20)
        (IntRange.create start::1 count::2)
      |> Pervasives.(===) Ordering.lessThan
      |> Expect.toBeEqualToTrue;

    IntRange.compare
        (IntRange.create start::1 count::20)
        (IntRange.create start::(-10) count::100)
      |> Pervasives.(===) Ordering.greaterThan
      |> Expect.toBeEqualToTrue;

    IntRange.compare
        (IntRange.create start::1 count::20)
        (IntRange.create start::1 count::100)
      |> Pervasives.(===) Ordering.lessThan
      |> Expect.toBeEqualToTrue;

    IntRange.compare
        (IntRange.create start::1 count::20)
        (IntRange.create start::1 count::100)
      |> Pervasives.(===) Ordering.lessThan
      |> Expect.toBeEqualToTrue;

    IntRange.compare
        (IntRange.create start::1 count::200)
        (IntRange.create start::1 count::100)
      |> Pervasives.(===) Ordering.greaterThan
      |> Expect.toBeEqualToTrue;

    IntRange.compare
        (IntRange.create start::1 count::200)
        (IntRange.create start::1 count::200)
      |> Pervasives.(===) Ordering.equal
      |> Expect.toBeEqualToTrue;
  }),
  it "contains" (fun () => {
    let range = IntRange.create start::0 count::200;
    range |> IntRange.contains (-1) |> Expect.toBeEqualToFalse;
    range |> IntRange.contains 10 |> Expect.toBeEqualToTrue;
    range |> IntRange.contains 200 |> Expect.toBeEqualToFalse;
  }),
  it "count" (fun () => {
    IntRange.create start::(-10) count::200
      |> IntRange.count
      |> Expect.toBeEqualToInt 200;
  }),
  it "create" (fun () => {
    (fun () => IntRange.create start::(-10) count::(-10)) |> Expect.shouldRaise;
  }),
  it "equals" (fun () => {
    IntRange.equals
        (IntRange.create start::(-10) count::0)
        IntRange.empty
      |> Expect.toBeEqualToTrue;

    IntRange.equals
        (IntRange.create start::1 count::20)
        (IntRange.create start::1 count::20)
      |> Expect.toBeEqualToTrue;

    IntRange.equals
        (IntRange.create start::0 count::20)
        (IntRange.create start::1 count::20)
      |> Expect.toBeEqualToFalse;

    IntRange.equals
        (IntRange.create start::1 count::20)
        (IntRange.create start::1 count::21)
      |> Expect.toBeEqualToFalse;
  }),
  it "first"  (fun () => {
    IntRange.create start::1 count::20
      |> IntRange.first
      |> Expect.toBeEqualToSomeOfInt 1;

    IntRange.empty
      |> IntRange.first
      |> Expect.toBeEqualToNoneOfInt;
  }),
  it "firstOrRaise"  (fun () => {
    IntRange.create start::1 count::20
      |> IntRange.firstOrRaise
      |> Expect.toBeEqualToInt 1;
    (fun () => IntRange.empty |> IntRange.firstOrRaise) |> Expect.shouldRaise;
  }),
  it "hash"  (fun () => ()),
  it "isEmpty"  (fun () => {
    IntRange.empty |> IntRange.isEmpty |> Expect.toBeEqualToTrue;
    IntRange.create start::1 count::20 |> IntRange.isEmpty |> Expect.toBeEqualToFalse;
  }),
  it "isNotEmpty"  (fun () => {
    IntRange.empty |> IntRange.isNotEmpty |> Expect.toBeEqualToFalse;
    IntRange.create start::1 count::20 |> IntRange.isNotEmpty |> Expect.toBeEqualToTrue;
  }),
  it "last"  (fun () => {
    IntRange.create start::1 count::20
      |> IntRange.last
      |> Expect.toBeEqualToSomeOfInt 20;
    IntRange.empty
      |> IntRange.last
      |> Expect.toBeEqualToNoneOfInt;
  }),
  it "lastOrRaise"  (fun () => {
    IntRange.create start::1 count::20
      |> IntRange.lastOrRaise
      |> Expect.toBeEqualToInt 20;
    (fun () => IntRange.empty |> IntRange.lastOrRaise) |> Expect.shouldRaise;
  }),
  it "reduce"  (fun () => {
    IntRange.create start::1 count::20
      |> IntRange.reduce while_::(fun _ i => i < 5) (fun acc i => i + acc) 0
      |> Expect.toBeEqualToInt 10;
  }),
  it "reduceReversed"  (fun () => {
    IntRange.create start::1 count::20
      |> IntRange.reduceReversed while_::(fun _ i => i > 15) (fun acc i => i + acc) 0
      |> Expect.toBeEqualToInt 90;
  }),
  it "toIterable"  (fun () => {
    IntRange.create start::1 count::20
      |> IntRange.toIterable
      |> Iterable.reduce while_::(fun _ i => i < 5) (fun acc i => i + acc) 0
      |> Expect.toBeEqualToInt 10;
  }),
  it "toIterableReversed"  (fun () => {
    IntRange.create start::1 count::20
      |> IntRange.toIterableReversed
      |> Iterable.reduce while_::(fun _ i => i > 15) (fun acc i => i + acc) 0
      |> Expect.toBeEqualToInt 90;
  }),
  it "toMap"  (fun () => ()),
  it "toSequence"  (fun () => {
    IntRange.create start::1 count::20
      |> IntRange.toSequence
      |> Sequence.reduce while_::(fun _ i => i < 5) (fun acc i => i + acc) 0
      |> Expect.toBeEqualToInt 10;
  }),
  it "toSequenceReversed"  (fun () => {
    IntRange.create start::1 count::20
      |> IntRange.toSequenceReversed
      |> Sequence.reduce while_::(fun _ i => i > 15) (fun acc i => i + acc) 0
      |> Expect.toBeEqualToInt 90;
  }),
  it "toSet"  (fun () => {
    let set = IntRange.create start::0 count::200 |> IntRange.toSet;
    set |> Set.contains (-1) |> Expect.toBeEqualToFalse;
    set |> Set.contains 10 |> Expect.toBeEqualToTrue;
    set |> Set.contains 200 |> Expect.toBeEqualToFalse;

    set |> Set.count |> Expect.toBeEqualToInt 200;

    set
      |> Set.reduce while_::(fun _ i => i < 5) (fun acc i => i + acc) 0
      |> Expect.toBeEqualToInt 10;

    set
      |> Set.toSequence
      |> Sequence.reduce while_::(fun _ i => i < 5) (fun acc i => i + acc) 0
      |> Expect.toBeEqualToInt 10;
  }),
];
