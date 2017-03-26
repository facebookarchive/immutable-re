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

let test = describe "Iterator" [
  it "buffer" (fun () => {
    let src = [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] |> List.toIterator;

    Iterator.buffer count::3 skip::3 src
      |> Iterator.flatMap List.toIterator
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [ 7, 8, 9, 4, 5, 6, 1, 2, 3 ];

    Iterator.buffer count::2 skip::3 src
      |> Iterator.flatMap List.toIterator
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [ 7, 8, 4, 5, 1, 2 ];

    Iterator.buffer count::2 skip::1 src
      |> Iterator.flatMap List.toIterator
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [ 8, 9, 7, 8, 6, 7, 5, 6, 4, 5, 3, 4, 2, 3, 1, 2 ];

    (Iterator.empty ())
      |> Iterator.buffer count::3 skip::3
      |> Iterator.flatMap List.toIterator
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt []
  }),
  it "concat" (fun () => {
    Iterator.concat [
        IntRange.create start::0 count::2 |> IntRange.toIterator,
        IntRange.create start::2 count::2 |> IntRange.toIterator,
        IntRange.create start::4 count::2 |> IntRange.toIterator,
      ]
      |> Iterator.take 5
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [4, 3, 2, 1, 0];
  }),
  it "defer" (fun () => ()),
  it "distinctUntilChangedWith" (fun () => {
    [ 1, 1, 1, 2, 2, 2, 3, 3, 4, 4 ]
      |> List.toIterator
      |> Iterator.distinctUntilChangedWith Equality.int
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [ 4, 3, 2, 1 ];
  }),
  it "doOnNext" (fun () => {
    let last = ref 0;
    IntRange.create start::0 count::5
      |> IntRange.toIterator
      |> Iterator.doOnNext (fun i => { last := i })
      |> Iterator.Reducer.forEach ignore;
    !last |> Expect.toBeEqualToInt 4;

    let empty = Iterator.empty ();
    Pervasives.(===) (Iterator.doOnNext (fun _ => ()) empty) empty
      |> Expect.toBeEqualToTrue;
  }),
  it "filter" (fun () => {
    IntRange.create start::0 count::5
      |> IntRange.toIterator
      |> Iterator.filter (fun i => i mod 2 === 0)
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [4, 2, 0];

    let empty = Iterator.empty ();
    Pervasives.(===) (Iterator.filter (fun _ => true) empty) empty
      |> Expect.toBeEqualToTrue;
  }),
  it "flatMap" (fun () => {
    IntRange.create start::0 count::3
      |> IntRange.toIterator
      |> Iterator.flatMap (fun _ => List.toIterator [1, 1, 1] )
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [1, 1, 1, 1, 1, 1, 1, 1, 1];
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
      |> Expect.toBeEqualToListOfInt [4, 3, 2, 1, 0];
  }),
  it "generate" (fun () => {
    Iterator.generate (fun i => i + 1) 0
      |> Iterator.skip 3
      |> Iterator.take 2
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [4, 3];
  }),
  it "map" (fun () => {
    IntRange.create start::0 count::5
      |> IntRange.toIterator
      |> Iterator.map (fun i => i * 3)
      |> Iterator.take 3
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [6, 3, 0];
  }),
  it "repeat" (fun () => {
    Iterator.repeat 5
      |> Iterator.take 10
      |> Iterator.reduce (fun acc _ => acc + 5) 0
      |> Expect.toBeEqualToInt 50;
  }),
  it "return" (fun () => {
    Iterator.return 1
      |> Iterator.reduce (fun acc i => acc + i) 0
      |> Expect.toBeEqualToInt 1;

    Iterator.return 1
      |> Iterator.reduce while_::(fun _ i => i < 0) (fun acc i => acc + i) 0
      |> Expect.toBeEqualToInt 0;
  }),
  it "scan" (fun () => {
    IntRange.create start::0 count::5
      |> IntRange.toIterator
      |> Iterator.scan (fun acc i => acc + i) 0
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [10, 6, 3, 1, 0, 0];
  }),
  it "skip" (fun () => {
    IntRange.create start::0 count::5
      |> IntRange.toIterator
      |> Iterator.skip 3
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [4, 3];

    let empty = Iterator.empty ();
    Pervasives.(===) (Iterator.skip 5 empty) empty
      |> Expect.toBeEqualToTrue;

    let x = Iterator.return 8;
    Pervasives.(===) (Iterator.skip 0 x) x
      |> Expect.toBeEqualToTrue;
  }),
  it "skipWhile" (fun () => {
    IntRange.create start::0 count::5
      |> IntRange.toIterator
      |> Iterator.skipWhile (fun i => i < 3)
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [4, 3];

    let empty = Iterator.empty ();
    Pervasives.(===) (Iterator.skipWhile (fun _ => true) empty) empty
      |> Expect.toBeEqualToTrue;
  }),
  it "startWith" (fun () => {
    IntRange.create start::0 count::5
      |> IntRange.toIterator
      |> Iterator.startWith (-1)
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [4, 3, 2, 1, 0, (-1)];
  }),
  it "take" (fun () => {
    IntRange.create start::0 count::5
      |> IntRange.toIterator
      |> Iterator.take 3
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [2, 1, 0];

    let empty = Iterator.empty ();
    Pervasives.(===) (Iterator.take 5 empty) empty
      |> Expect.toBeEqualToTrue;
  }),
  it "takeWhile" (fun () => {
    IntRange.create start::0 count::5
      |> IntRange.toIterator
      |> Iterator.takeWhile (fun i => i < 3)
      |> Iterator.take 2
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [1, 0];

    let empty = Iterator.empty ();
    Pervasives.(===) (Iterator.takeWhile (fun _ => true) empty) empty
      |> Expect.toBeEqualToTrue;
  }),
];
