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

let test = describe "Sequence" [
  it "buffer" (fun () => {
    let src = [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] |> List.toSequence;

    Sequence.buffer count::3 skip::3 src
      |> Sequence.toIterator
      |> Iterator.flatMap List.toIterator
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [ 7, 8, 9, 4, 5, 6, 1, 2, 3 ];

    Sequence.buffer count::2 skip::3 src
      |> Sequence.toIterator
      |> Iterator.flatMap List.toIterator
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [ 7, 8, 4, 5, 1, 2 ];

    Sequence.buffer count::2 skip::1 src
      |> Sequence.toIterator
      |> Iterator.flatMap List.toIterator
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [ 8, 9, 7, 8, 6, 7, 5, 6, 4, 5, 3, 4, 2, 3, 1, 2 ];

    Sequence.empty
      |> Sequence.buffer count::3 skip::3
      |> Sequence.toIterator
      |> Iterator.flatMap List.toIterator
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt []
  }),
  it "concat" (fun () => {
    Sequence.concat [
        IntRange.create start::0 count::2 |> IntRange.toSequence,
        IntRange.create start::2 count::2 |> IntRange.toSequence,
        IntRange.create start::4 count::2 |> IntRange.toSequence,
      ]
      |> Sequence.toIterator
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [5, 4, 3, 2, 1, 0];
  }),
  it "defer" (fun () => ()),
  it "distinctUntilChangedWith" (fun () => {
    [ 1, 1, 1, 2, 2, 2, 3, 3, 4, 4 ]
      |> List.toSequence
      |> Sequence.distinctUntilChangedWith Equality.structural
      |> Sequence.toIterator
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [ 4, 3, 2, 1 ];
  }),
  it "doOnNext" (fun () => {
    let last = ref 0;
    IntRange.create start::0 count::5
      |> IntRange.toSequence
      |> Sequence.doOnNext (fun i => { last := i })
      |> Sequence.toIterator
      |> Iterator.forEach ignore;
    expect !last |> toBeEqualToInt 4;
  }),
  it "filter" (fun () => {
    IntRange.create start::0 count::5
      |> IntRange.toSequence
      |> Sequence.filter (fun i => i mod 2 == 0)
      |> Sequence.toIterator
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [4, 2, 0];
  }),
  it "first" (fun () => {
    IntRange.create start::0 count::10
      |> IntRange.toSequence
      |> Sequence.first
      |> expect
      |> toBeEqualToSomeOfInt 0;

    Sequence.empty |> Sequence.first |> expect |> toBeEqualToNoneOfInt;
  }),
  it "firstOrRaise" (fun () => {
    IntRange.create start::0 count::10
      |> IntRange.toSequence
      |> Sequence.firstOrRaise
      |> expect
      |> toBeEqualToInt 0;

    (fun () => Sequence.empty |> Sequence.firstOrRaise) |> shouldRaise;
  }),
  it "flatMap" (fun () => {
    IntRange.create start::0 count::3
      |> IntRange.toSequence
      |> Sequence.flatMap (fun i => List.toSequence [1, 1, 1])
      |> Sequence.toIterator
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [1, 1, 1, 1, 1, 1, 1, 1, 1];
  }),
  it "flatten" (fun () => {
    [
      IntRange.create start::0 count::2 |> IntRange.toSequence,
      IntRange.create start::2 count::2 |> IntRange.toSequence,
      IntRange.create start::4 count::2 |> IntRange.toSequence,
    ] |> List.toSequence
      |> Sequence.flatten
      |> Sequence.take 5
      |> Sequence.toIterator
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [4, 3, 2, 1, 0];
  }),
  it "generate" (fun () => {
    Sequence.generate (fun i => i + 1) 0
      |> Sequence.skip 3
      |> Sequence.take 2
      |> Sequence.toIterator
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [4, 3];
  }),
  it "map" (fun () => {
    IntRange.create start::0 count::5
      |> IntRange.toSequence
      |> Sequence.map (fun i => i * 3)
      |> Sequence.take 3
      |> Sequence.toIterator
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [6, 3, 0];
  }),
  it "repeat" (fun () => {
    Sequence.repeat 5
      |> Sequence.take 10
      |> Sequence.toIterator
      |> Iterator.reduce (fun acc _ => acc + 5) 0
      |> expect
      |> toBeEqualToInt 50;
  }),
  it "return" (fun () => {
    Sequence.return 1
      |> Sequence.toIterator
      |> Iterator.reduce (fun acc i => acc + i) 0
      |> expect
      |> toBeEqualToInt 1;

    Sequence.return 1
      |> Sequence.toIterator
      |> Iterator.reduce while_::(fun _ i => i < 0) (fun acc i => acc + i) 0
      |> expect
      |> toBeEqualToInt 0;
  }),
  it "scan" (fun () => {
    IntRange.create start::0 count::5
      |> IntRange.toSequence
      |> Sequence.scan (fun acc i => acc + i) 0
      |> Sequence.toIterator
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [10, 6, 3, 1, 0, 0];
  }),
  it "skip" (fun () => {
    IntRange.create start::0 count::5
      |> IntRange.toSequence
      |> Sequence.skip 3
      |> Sequence.toIterator
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [4, 3];
  }),
  it "skipWhile" (fun () => {
    IntRange.create start::0 count::5
      |> IntRange.toSequence
      |> Sequence.skipWhile (fun i => i < 3)
      |> Sequence.toIterator
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [4, 3];
  }),
  it "startWith" (fun () => {
    IntRange.create start::0 count::5
      |> IntRange.toSequence
      |> Sequence.startWith (-1)
      |> Sequence.toIterator
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [4, 3, 2, 1, 0, (-1)];
  }),
  it "take" (fun () => {
    IntRange.create start::0 count::5
      |> IntRange.toSequence
      |> Sequence.take 3
      |> Sequence.toIterator
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [2, 1, 0];
  }),
  it "takeWhile" (fun () => {
    IntRange.create start::0 count::5
      |> IntRange.toSequence
      |> Sequence.takeWhile (fun i => i < 3)
      |> Sequence.toIterator
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [2, 1, 0];
  }),
  it "zip" (fun () => {
    Sequence.zip [
        (List.toSequence [1, 2]),
        (List.toSequence [4, 5, 6]),
        (List.toSequence [7, 8, 9]),
      ]
      |> Sequence.flatMap List.toSequence
      |> Sequence.toIterator
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [8, 5, 2, 7, 4, 1];

    Sequence.zip [
        (List.toSequence [1, 2, 3]),
        (List.toSequence [4, 5, 6]),
        (List.toSequence [7, 8]),
      ]
      |> Sequence.flatMap List.toSequence
      |> Sequence.toIterator
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [8, 5, 2, 7,4, 1];

    Sequence.zip [
        (List.toSequence [1, 2, 3]),
        (List.toSequence [4, 5, 6]),
        (List.toSequence [7, 8, 9]),
      ]
      |> Sequence.flatMap List.toSequence
      |> Sequence.toIterator
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [9, 6, 3, 8, 5, 2, 7, 4, 1];

    Sequence.zip [
        (List.toSequence [1, 2, 3]),
        (List.toSequence [4, 5]),
        (List.toSequence [7, 8, 9]),
      ]
      |> Sequence.flatMap List.toSequence
      |> Sequence.toIterator
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [8, 5, 2, 7, 4, 1];
  }),
  it "zip2With" (fun () => {
    Sequence.zip2With (fun a b => [a, b])
        (List.toSequence [1, 2,])
        (List.toSequence [4, 5, 6])
      |> Sequence.flatMap List.toSequence
      |> Sequence.toIterator
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [5, 2, 4, 1];

    Sequence.zip2With (fun a b => [a, b])
        (List.toSequence [1, 2, 3])
        (List.toSequence [4, 5])
      |> Sequence.flatMap List.toSequence
      |> Sequence.toIterator
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [5, 2, 4, 1];

    Sequence.zip2With (fun a b => [a, b])
        (List.toSequence [1, 2, 3])
        (List.toSequence [4, 5, 6])
      |> Sequence.flatMap List.toSequence
      |> Sequence.toIterator
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [6, 3, 5, 2, 4, 1];
  }),
  it "zip3With" (fun () => {
    Sequence.zip3With (fun a b c => [a, b, c])
        (List.toSequence [1, 2,])
        (List.toSequence [4, 5, 6])
        (List.toSequence [7, 8, 9])
      |> Sequence.flatMap List.toSequence
      |> Sequence.toIterator
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [8, 5, 2, 7, 4, 1];

    Sequence.zip3With (fun a b c => [a, b, c])
        (List.toSequence [1, 2, 3])
        (List.toSequence [4, 5, 6])
        (List.toSequence [7, 8])
      |> Sequence.flatMap List.toSequence
      |> Sequence.toIterator
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [8, 5, 2, 7,4, 1];

    Sequence.zip3With (fun a b c => [a, b, c])
        (List.toSequence [1, 2, 3])
        (List.toSequence [4, 5, 6])
        (List.toSequence [7, 8, 9])
      |> Sequence.flatMap List.toSequence
      |> Sequence.toIterator
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [9, 6, 3, 8, 5, 2, 7, 4, 1];

    Sequence.zip3With (fun a b c => [a, b, c])
        (List.toSequence [1, 2, 3])
        (List.toSequence [4, 5])
        (List.toSequence [7, 8, 9])
      |> Sequence.flatMap List.toSequence
      |> Sequence.toIterator
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [8, 5, 2, 7, 4, 1];
  }),
  it "zipLongest" (fun () => {
    Sequence.zipLongest [
        (List.toSequence [1, 2, 3]),
        (List.toSequence [4, 5, 6]),
        (List.toSequence [7, 8, 9]),
      ]
      |> Sequence.flatMap List.toSequence
      |> Sequence.toIterator
      |> Iterator.filter Option.isNotEmpty
      |> Iterator.map Option.firstOrRaise
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [ 9, 6, 3, 8, 5, 2, 7, 4, 1];

    Sequence.zipLongest [
        (List.toSequence [1, 2]),
        (List.toSequence [4, 5, 6]),
        (List.toSequence [7, 8, 9]),
      ]
      |> Sequence.flatMap List.toSequence
      |> Sequence.toIterator
      |> Iterator.filter Option.isNotEmpty
      |> Iterator.map Option.firstOrRaise
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [ 9, 6, 8, 5, 2, 7, 4, 1];

    Sequence.zipLongest [
        (List.toSequence [1, 2, 3]),
        (List.toSequence [4, 5, 6]),
        (List.toSequence [7, 8]),
      ]
      |> Sequence.flatMap List.toSequence
      |> Sequence.toIterator
      |> Iterator.filter Option.isNotEmpty
      |> Iterator.map Option.firstOrRaise
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [ 6, 3, 8, 5, 2, 7, 4, 1];

    Sequence.zipLongest [
        (List.toSequence [1, 2, 3]),
        (List.toSequence [4, 5]),
        (List.toSequence [7, 8, 9]),
      ]
      |> Sequence.flatMap List.toSequence
      |> Sequence.toIterator
      |> Iterator.filter Option.isNotEmpty
      |> Iterator.map Option.firstOrRaise
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [ 9, 3, 8, 5, 2, 7, 4, 1];

    Sequence.zipLongest [
        (List.toSequence [1, 2]),
        (List.toSequence [4, 5]),
        (List.toSequence [7, 8, 9]),
      ]
      |> Sequence.flatMap List.toSequence
      |> Sequence.toIterator
      |> Iterator.filter Option.isNotEmpty
      |> Iterator.map Option.firstOrRaise
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [ 9, 8, 5, 2, 7, 4, 1];

    Sequence.zipLongest [
        (List.toSequence [1, 2]),
        (List.toSequence [4, 5, 6]),
        (List.toSequence [7, 8]),
      ]
      |> Sequence.flatMap List.toSequence
      |> Sequence.toIterator
      |> Iterator.filter Option.isNotEmpty
      |> Iterator.map Option.firstOrRaise
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [ 6, 8, 5, 2, 7, 4, 1];

    Sequence.zipLongest [
        (List.toSequence [1, 2, 3]),
        (List.toSequence [4, 5]),
        (List.toSequence [7, 8]),
      ]
      |> Sequence.flatMap List.toSequence
      |> Sequence.toIterator
      |> Iterator.filter Option.isNotEmpty
      |> Iterator.map Option.firstOrRaise
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [ 3, 8, 5, 2, 7, 4, 1];
  }),
  it "zipLongest2With" (fun () => {
    Sequence.zipLongest2With
        (fun a b => [a, b])
        (List.toSequence [1, 2, 3])
        (List.toSequence [4, 5, 6])
      |> Sequence.flatMap List.toSequence
      |> Sequence.toIterator
      |> Iterator.filter Option.isNotEmpty
      |> Iterator.map Option.firstOrRaise
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [ 6, 3, 5, 2, 4, 1];

    Sequence.zipLongest2With
        (fun a b => [a, b])
        (List.toSequence [1, 2])
        (List.toSequence [4, 5, 6])
      |> Sequence.flatMap List.toSequence
      |> Sequence.toIterator
      |> Iterator.filter Option.isNotEmpty
      |> Iterator.map Option.firstOrRaise
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [ 6, 5, 2, 4, 1];

    Sequence.zipLongest2With
        (fun a b => [a, b])
        (List.toSequence [1, 2, 3])
        (List.toSequence [4, 5])
      |> Sequence.flatMap List.toSequence
      |> Sequence.toIterator
      |> Iterator.filter Option.isNotEmpty
      |> Iterator.map Option.firstOrRaise
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [ 3, 5, 2, 4, 1];
  }),
  it "zipLongest3With" (fun () => {
    Sequence.zipLongest3With
        (fun a b c => [a, b, c])
        (List.toSequence [1, 2, 3])
        (List.toSequence [4, 5, 6])
        (List.toSequence [7, 8, 9])
      |> Sequence.flatMap List.toSequence
      |> Sequence.toIterator
      |> Iterator.filter Option.isNotEmpty
      |> Iterator.map Option.firstOrRaise
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [ 9, 6, 3, 8, 5, 2, 7, 4, 1];

    Sequence.zipLongest3With
        (fun a b c => [a, b, c])
        (List.toSequence [1, 2])
        (List.toSequence [4, 5, 6])
        (List.toSequence [7, 8, 9])
      |> Sequence.flatMap List.toSequence
      |> Sequence.toIterator
      |> Iterator.filter Option.isNotEmpty
      |> Iterator.map Option.firstOrRaise
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [ 9, 6, 8, 5, 2, 7, 4, 1];

    Sequence.zipLongest3With
        (fun a b c => [a, b, c])
        (List.toSequence [1, 2, 3])
        (List.toSequence [4, 5, 6])
        (List.toSequence [7, 8])
      |> Sequence.flatMap List.toSequence
      |> Sequence.toIterator
      |> Iterator.filter Option.isNotEmpty
      |> Iterator.map Option.firstOrRaise
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [ 6, 3, 8, 5, 2, 7, 4, 1];

    Sequence.zipLongest3With
        (fun a b c => [a, b, c])
        (List.toSequence [1, 2, 3])
        (List.toSequence [4, 5])
        (List.toSequence [7, 8, 9])
      |> Sequence.flatMap List.toSequence
      |> Sequence.toIterator
      |> Iterator.filter Option.isNotEmpty
      |> Iterator.map Option.firstOrRaise
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [ 9, 3, 8, 5, 2, 7, 4, 1];

    Sequence.zipLongest3With
        (fun a b c => [a, b, c])
        (List.toSequence [1, 2])
        (List.toSequence [4, 5])
        (List.toSequence [7, 8, 9])
      |> Sequence.flatMap List.toSequence
      |> Sequence.toIterator
      |> Iterator.filter Option.isNotEmpty
      |> Iterator.map Option.firstOrRaise
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [ 9, 8, 5, 2, 7, 4, 1];

    Sequence.zipLongest3With
        (fun a b c => [a, b, c])
        (List.toSequence [1, 2])
        (List.toSequence [4, 5, 6])
        (List.toSequence [7, 8])
      |> Sequence.flatMap List.toSequence
      |> Sequence.toIterator
      |> Iterator.filter Option.isNotEmpty
      |> Iterator.map Option.firstOrRaise
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [ 6, 8, 5, 2, 7, 4, 1];

    Sequence.zipLongest3With
        (fun a b c => [a, b, c])
        (List.toSequence [1, 2, 3])
        (List.toSequence [4, 5])
        (List.toSequence [7, 8])
      |> Sequence.flatMap List.toSequence
      |> Sequence.toIterator
      |> Iterator.filter Option.isNotEmpty
      |> Iterator.map Option.firstOrRaise
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [ 3, 8, 5, 2, 7, 4, 1];

  }),
];
