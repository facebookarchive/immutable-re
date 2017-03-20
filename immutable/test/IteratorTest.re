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
    let range1 = IntRange.create start::0 count::2;
    let range2 = IntRange.create start::2 count::2;
    let range3 = IntRange.create start::4 count::2;

    let concatted = Iterator.concat [
      range1 |> IntRange.toIterator,
      range2 |> IntRange.toIterator,
      range3 |> IntRange.toIterator,
    ];

    let acc = concatted |> Iterator.reduceWhile
      (fun _ i => i < 4)
      (fun acc i => acc + i)
      0;
    expect acc |> toBeEqualToInt 6;

    let emptyConcat = Iterator.concat [];
    expect (emptyConcat === Iterator.empty) |> toBeEqualToTrue;
  }),
  it "map" (fun () => {
    let range = IntRange.create start::0 count::5;
    let mapped = range |> IntRange.toIterator |> Iterator.map (fun i => i * 3);

    let acc = mapped |> Iterator.reduceWhile
      (fun _ i => i < 9)
      (fun acc i => acc + i)
      0;
    expect acc |> toBeEqualToInt 9;

    let emptyMapped = Iterator.empty |> Iterator.map (fun i => i * 3);
    expect (emptyMapped === Iterator.empty) |> toBeEqualToTrue;
  }),
  it "doOnNext" (fun () => ()),
  it "filter" (fun () => ()),
  it "flatMap" (fun () => ()),
  it "flatten" (fun () => ()),
  it "reduce" (fun () => ()),
  it "reduceWhile" (fun () => ()),
  it "every" (fun () => ()),
  it "empty" (fun () => ()),
  it "find" (fun () => ()),
  it "findOrRaise" (fun () => ()),
  it "forEach" (fun () => ()),
  it "forEachWhile" (fun () => ()),
  it "none" (fun () => ()),
  it "return" (fun () => ()),
  it "skip" (fun () => ()),
  it "skipWhile" (fun () => ()),
  it "some" (fun () => ()),
  it "take" (fun () => ()),
  it "takeWhile" (fun () => ()),
];
