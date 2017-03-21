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

let test = describe "List" [
  it "addFirst" (fun () => {
    [] |> List.addFirst 1|> expect |> toBeEqualToListOfInt [1];
  }),
  it "addFirstAll" (fun () => {
    [] |> List.addFirstAll
        (IntRange.create start::(-10) count::10 |> IntRange.toIterator)
      |> expect
      |> toBeEqualToListOfInt [(-1), (-2), (-3), (-4), (-5), (-6), (-7), (-8), (-9), (-10)];
  }),
  it "first" (fun () => {
    expect (List.first []) |> toBeEqualToNoneOfInt;
    expect (List.first [1]) |> toBeEqualToSomeOfInt 1;
  }),
  it "firstOrRaise" (fun () => {
    (fun ()=> List.firstOrRaise []) |> shouldThrow;
    expect (List.firstOrRaise [1]) |> toBeEqualToInt 1;
  }),
  it "fromReverse" (fun () => {
    List.fromReverse (IntRange.create start::(-10) count::10 |> IntRange.toIterator)
      |> expect
      |> toBeEqualToListOfInt [(-1), (-2), (-3), (-4), (-5), (-6), (-7), (-8), (-9), (-10)];
  }),
  it "mapReverse" (fun () => {
    List.fromReverse (IntRange.create start::(-10) count::10 |> IntRange.toIterator)
      |> List.mapReverse (fun i => 0 - i)
      |> expect
      |> toBeEqualToListOfInt [10, 9, 8, 7, 6, 5, 4, 3, 2, 1];
  }),
  it "reduce" (fun () => {
    [0, 1, 2, 3, 4, 5] |> List.reduce
        while_::(fun _ i => i < 4) (fun acc i => acc + i) 0
      |> expect
      |> toBeEqualToInt 6;
  }),
  it "removeAll" (fun () => {
    [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
      |> List.removeAll
      |> expect
      |> toBeEqualToListOfInt [];
  }),
  it "removeFirstOrRaise" (fun () => {
    [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
      |> List.removeFirstOrRaise
      |> expect
      |> toBeEqualToListOfInt [9, 8, 7, 6, 5, 4, 3, 2, 1];

    (fun () => List.removeFirstOrRaise [] |> ignore)
      |> shouldThrow;
  }),
  it "return" (fun () => {
    List.return 1 |> expect |> toBeEqualToListOfInt [1];
  }),
  it "toIterator" (fun () => {
    [0, 1, 2, 3, 4, 5] |> List.toIterator |> Iterator.reduce
        while_::(fun _ i => i < 4) (fun acc i => acc + i) 0
      |> expect
      |> toBeEqualToInt 6;
  }),
  it "toSequence" (fun () => {
    List.toSequence [1, 2, 3]
      |> Sequence.toIterator
      |> Iterator.reduce (fun acc i => acc + i) 0
      |> expect
      |> toBeEqualToInt 6;
  }),
];
