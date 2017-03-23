/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Immutable;
open ReUnit.Expect;
open ReUnit.Test;

let test = describe "IntRange" [
  it "compare" (fun () => {
    IntRange.compare
        (IntRange.create start::0 count::20)
        (IntRange.create start::1 count::2)
      |> Pervasives.(===) Ordering.lessThan
      |> expect
      |> toBeEqualToTrue;

    IntRange.compare
        (IntRange.create start::1 count::20)
        (IntRange.create start::(-10) count::100)
      |> Pervasives.(===) Ordering.greaterThan
      |> expect
      |> toBeEqualToTrue;

    IntRange.compare
        (IntRange.create start::1 count::20)
        (IntRange.create start::1 count::100)
      |> Pervasives.(===) Ordering.lessThan
      |> expect
      |> toBeEqualToTrue;

    IntRange.compare
        (IntRange.create start::1 count::20)
        (IntRange.create start::1 count::100)
      |> Pervasives.(===) Ordering.lessThan
      |> expect
      |> toBeEqualToTrue;

    IntRange.compare
        (IntRange.create start::1 count::200)
        (IntRange.create start::1 count::100)
      |> Pervasives.(===) Ordering.greaterThan
      |> expect
      |> toBeEqualToTrue;

    IntRange.compare
        (IntRange.create start::1 count::200)
        (IntRange.create start::1 count::200)
      |> Pervasives.(===) Ordering.equal
      |> expect
      |> toBeEqualToTrue;
  }),
  it "contains" (fun () => {
    let range = IntRange.create start::0 count::200;
    range |> IntRange.contains (-1) |> expect |> toBeEqualToFalse;
    range |> IntRange.contains 10 |> expect |> toBeEqualToTrue;
    range |> IntRange.contains 200 |> expect |> toBeEqualToFalse;
  }),
  it "count" (fun () => {
    IntRange.create start::(-10) count::200
      |> IntRange.count
      |> expect
      |> toBeEqualToInt 200;
  }),
  it "create" (fun () => {
    (fun () => IntRange.create start::(-10) count::(-10)) |> shouldRaise;
  }),
  it "equals" (fun () => {
    IntRange.equals
        (IntRange.create start::(-10) count::0)
        IntRange.empty
      |> expect
      |> toBeEqualToTrue;

    IntRange.equals
        (IntRange.create start::1 count::20)
        (IntRange.create start::1 count::20)
      |> expect
      |> toBeEqualToTrue;

    IntRange.equals
        (IntRange.create start::0 count::20)
        (IntRange.create start::1 count::20)
      |> expect
      |> toBeEqualToFalse;

    IntRange.equals
        (IntRange.create start::1 count::20)
        (IntRange.create start::1 count::21)
      |> expect
      |> toBeEqualToFalse;
  }),
  it "first"  (fun () => {
    IntRange.create start::1 count::20
      |> IntRange.first
      |> expect
      |> toBeEqualToSomeOfInt 1;
    IntRange.empty
    |> IntRange.first
    |> expect
    |> toBeEqualToNoneOfInt;
  }),
  it "firstOrRaise"  (fun () => {
    IntRange.create start::1 count::20
      |> IntRange.firstOrRaise
      |> expect
      |> toBeEqualToInt 1;
    (fun () => IntRange.empty |> IntRange.firstOrRaise) |> shouldRaise;
  }),
  it "hash"  (fun () => ()),
  it "isEmpty"  (fun () => {
    IntRange.empty |> IntRange.isEmpty |> expect |> toBeEqualToTrue;
    IntRange.create start::1 count::20 |> IntRange.isEmpty |> expect |> toBeEqualToFalse;
  }),
  it "isNotEmpty"  (fun () => {
    IntRange.empty |> IntRange.isNotEmpty |> expect |> toBeEqualToFalse;
    IntRange.create start::1 count::20 |> IntRange.isNotEmpty |> expect |> toBeEqualToTrue;
  }),
  it "last"  (fun () => {
    IntRange.create start::1 count::20
      |> IntRange.last
      |> expect
      |> toBeEqualToSomeOfInt 20;
    IntRange.empty
    |> IntRange.last
    |> expect
    |> toBeEqualToNoneOfInt;
  }),
  it "lastOrRaise"  (fun () => {
    IntRange.create start::1 count::20
      |> IntRange.lastOrRaise
      |> expect
      |> toBeEqualToInt 20;
    (fun () => IntRange.empty |> IntRange.lastOrRaise) |> shouldRaise;
  }),
  it "reduce"  (fun () => {
    IntRange.create start::1 count::20
      |> IntRange.reduce while_::(fun acc i => i < 5) (fun acc i => i + acc) 0
      |> expect
      |> toBeEqualToInt 10;
  }),
  it "reduceRight"  (fun () => {
    IntRange.create start::1 count::20
      |> IntRange.reduceRight while_::(fun acc i => i > 15) (fun acc i => i + acc) 0
      |> expect
      |> toBeEqualToInt 90;
  }),
  it "toIterator"  (fun () => {
    IntRange.create start::1 count::20
      |> IntRange.toIterator
      |> Iterator.reduce while_::(fun acc i => i < 5) (fun acc i => i + acc) 0
      |> expect
      |> toBeEqualToInt 10;
  }),
  it "toIteratorRight"  (fun () => {
    IntRange.create start::1 count::20
      |> IntRange.toIteratorRight
      |> Iterator.reduce while_::(fun acc i => i > 15) (fun acc i => i + acc) 0
      |> expect
      |> toBeEqualToInt 90;
  }),
  it "toMap"  (fun () => ()),
  it "toSequence"  (fun () => {
    IntRange.create start::1 count::20
      |> IntRange.toSequence
      |> Sequence.reduce while_::(fun acc i => i < 5) (fun acc i => i + acc) 0
      |> expect
      |> toBeEqualToInt 10;
  }),
  it "toSequenceRight"  (fun () => {
    IntRange.create start::1 count::20
      |> IntRange.toSequenceRight
      |> Sequence.reduce while_::(fun acc i => i > 15) (fun acc i => i + acc) 0
      |> expect
      |> toBeEqualToInt 90;
  }),
  it "toSet"  (fun () => {
    let set = IntRange.create start::0 count::200 |> IntRange.toSet;
    set |> Set.contains (-1) |> expect |> toBeEqualToFalse;
    set |> Set.contains 10 |> expect |> toBeEqualToTrue;
    set |> Set.contains 200 |> expect |> toBeEqualToFalse;

    set |> Set.count |> expect |> toBeEqualToInt 200;

    set
      |> Set.reduce while_::(fun acc i => i < 5) (fun acc i => i + acc) 0
      |> expect
      |> toBeEqualToInt 10;

    set
      |> Set.toSequence
      |> Sequence.reduce while_::(fun acc i => i < 5) (fun acc i => i + acc) 0
      |> expect
      |> toBeEqualToInt 10;
  }),
];
