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

let test = describe "Iterator" [
  it "concat" (fun () => {
    Iterator.concat [
        IntRange.create start::0 count::2 |> IntRange.toIterator,
        IntRange.create start::2 count::2 |> IntRange.toIterator,
        IntRange.create start::4 count::2 |> IntRange.toIterator,
      ]
      |> Iterator.take 5
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [4, 3, 2, 1, 0];

    Pervasives.(===) (Iterator.concat []) Iterator.empty
      |> expect
      |> toBeEqualToTrue;
  }),
  it "doOnNext" (fun () => {
    let last = ref 0;
    IntRange.create start::0 count::5
      |> IntRange.toIterator
      |> Iterator.doOnNext (fun i => { last := i })
      |> Iterator.forEach ignore;
    expect !last |> toBeEqualToInt 4;

    Pervasives.(===) (Iterator.doOnNext (fun _ => ()) Iterator.empty) Iterator.empty
      |> expect
      |> toBeEqualToTrue;
  }),
  it "filter" (fun () => {
    IntRange.create start::0 count::5
      |> IntRange.toIterator
      |> Iterator.filter (fun i => i mod 2 == 0)
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [4, 2, 0];

    Pervasives.(===) (Iterator.filter (fun _ => true) Iterator.empty) Iterator.empty
      |> expect
      |> toBeEqualToTrue;
  }),
  it "every" (fun () => {
    expect (Iterator.every (fun _ => false) Iterator.empty) |> toBeEqualToTrue;

    IntRange.create start::0 count::5
      |> IntRange.toIterator
      |> Iterator.every (fun i => i >= 0)
      |> expect
      |> toBeEqualToTrue;

    IntRange.create start::0 count::5
      |> IntRange.toIterator
      |> Iterator.every (fun i => i < 3)
      |> expect
      |> toBeEqualToFalse;
  }),
  it "find" (fun () => {
    IntRange.create start::0 count::5
      |> IntRange.toIterator
      |> Iterator.find (fun i => i == 2)
      |> expect
      |> toBeEqualToSomeOfInt 2;

    Iterator.empty
      |> Iterator.find (fun i => i == 2)
      |> expect
      |> toBeEqualToNoneOfInt;

    IntRange.create start::0 count::5
      |> IntRange.toIterator
      |> Iterator.find (fun i => i == 5)
      |> expect
      |> toBeEqualToNoneOfInt;
  }),
  it "findOrRaise" (fun () => {
    IntRange.create start::0 count::5
      |> IntRange.toIterator
      |> Iterator.findOrRaise (fun i => i == 2)
      |> expect
      |> toBeEqualToInt 2;

    (fun () => Iterator.empty |> Iterator.findOrRaise (fun i => i == 2))
      |> shouldRaise;
  }),
  it "flatMap" (fun () => {
    IntRange.create start::0 count::3
      |> IntRange.toIterator
      |> Iterator.flatMap (fun i => List.toIterator [1, 1, 1] )
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [1, 1, 1, 1, 1, 1, 1, 1, 1];
  }),
  it "flatten" (fun () => {
    [
      IntRange.create start::0 count::2 |> IntRange.toIterator,
      IntRange.create start::2 count::2 |> IntRange.toIterator,
      IntRange.create start::4 count::2 |> IntRange.toIterator,
    ] |> List.toIterator
      |> Iterator.flatten
      |> Iterator.take 5
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [4, 3, 2, 1, 0];
  }),
  it "forEach" (fun () => {
    let last = ref 0;
    IntRange.create start::0 count::5
      |> IntRange.toIterator
      |> Iterator.forEach while_::(fun i => i < 3) (fun i => { last := i });
    expect !last |> toBeEqualToInt 2;
  }),
  it "map" (fun () => {
    IntRange.create start::0 count::5
      |> IntRange.toIterator
      |> Iterator.map (fun i => i * 3)
      |> Iterator.take 3
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [6, 3, 0];

    Pervasives.(===) (Iterator.map (fun _ => 1) Iterator.empty) Iterator.empty
      |> expect
      |> toBeEqualToTrue;
  }),
  it "none" (fun () => {
    expect (Iterator.none (fun _ => false) Iterator.empty) |> toBeEqualToTrue;

    IntRange.create start::0 count::5
      |> IntRange.toIterator
      |> Iterator.none (fun i => i >= 2)
      |> expect
      |> toBeEqualToFalse;

    IntRange.create start::0 count::5
      |> IntRange.toIterator
      |> Iterator.none (fun i => i < 0)
      |> expect
      |> toBeEqualToTrue;
  }),
  it "return" (fun () => {
    Iterator.return 1
      |> Iterator.reduce (fun acc i => acc + i) 0
      |> expect
      |> toBeEqualToInt 1;

    Iterator.return 1
      |> Iterator.reduce while_::(fun _ i => i < 0) (fun acc i => acc + i) 0
      |> expect
      |> toBeEqualToInt 0;
  }),
  it "skip" (fun () => {
    IntRange.create start::0 count::5
      |> IntRange.toIterator
      |> Iterator.skip 3
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [4, 3];

    Pervasives.(===) (Iterator.skip 5 Iterator.empty) Iterator.empty
      |> expect
      |> toBeEqualToTrue;

    let x = Iterator.return 8;
    Pervasives.(===) (Iterator.skip 0 x) x
      |> expect
      |> toBeEqualToTrue;
  }),
  it "skipWhile" (fun () => {
    IntRange.create start::0 count::5
      |> IntRange.toIterator
      |> Iterator.skipWhile (fun i => i < 3)
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [4, 3];

    Pervasives.(===) (Iterator.skipWhile (fun _ => true) Iterator.empty) Iterator.empty
      |> expect
      |> toBeEqualToTrue;
  }),
  it "some" (fun () => {
    expect (Iterator.some (fun _ => false) Iterator.empty) |> toBeEqualToFalse;

    IntRange.create start::0 count::5
      |> IntRange.toIterator
      |> Iterator.some (fun i => i >= 2)
      |> expect
      |> toBeEqualToTrue;

    IntRange.create start::0 count::5
      |> IntRange.toIterator
      |> Iterator.some (fun i => i < 0)
      |> expect
      |> toBeEqualToFalse;
  }),
  it "take" (fun () => {
    IntRange.create start::0 count::5
      |> IntRange.toIterator
      |> Iterator.take 3
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [2, 1, 0];

    Pervasives.(===) (Iterator.take 5 Iterator.empty) Iterator.empty
      |> expect
      |> toBeEqualToTrue;

    Pervasives.(===) (Iterator.return 8 |> Iterator.take 0) Iterator.empty
      |> expect
      |> toBeEqualToTrue;
  }),
  it "takeWhile" (fun () => {
    IntRange.create start::0 count::5
      |> IntRange.toIterator
      |> Iterator.takeWhile (fun i => i < 3)
      |> Iterator.take 2
      |> List.fromReverse
      |> expect
      |> toBeEqualToListOfInt [1, 0];

    Pervasives.(===) (Iterator.skipWhile (fun _ => true) Iterator.empty) Iterator.empty
      |> expect
      |> toBeEqualToTrue;
  }),
];
