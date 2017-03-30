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

let test = describe "List" [
  it "addFirst" (fun () => {
    [] |> List.addFirst 1|> Expect.toBeEqualToListOfInt [1];
  }),
  it "addFirstAll" (fun () => {
    [] |> List.addFirstAll
        (IntRange.create start::(-10) count::10 |> IntRange.toIterable)
      |> Expect.toBeEqualToListOfInt [(-1), (-2), (-3), (-4), (-5), (-6), (-7), (-8), (-9), (-10)];
  }),
  it "first" (fun () => {
    List.first [] |> Expect.toBeEqualToNoneOfInt;
    List.first [1] |> Expect.toBeEqualToSomeOfInt 1;
  }),
  it "firstOrRaise" (fun () => {
    (fun () => List.firstOrRaise []) |> Expect.shouldRaise;
    List.firstOrRaise [1] |> Expect.toBeEqualToInt 1;
  }),
  it "fromReverse" (fun () => {
    List.fromReverse (IntRange.create start::(-10) count::10 |> IntRange.toIterable)
      |> Expect.toBeEqualToListOfInt [(-1), (-2), (-3), (-4), (-5), (-6), (-7), (-8), (-9), (-10)];
  }),
  it "reduce" (fun () => {
    [0, 1, 2, 3, 4, 5]
      |> List.reduce
        while_::(fun _ i => i < 4) (fun acc i => acc + i) 0
      |> Expect.toBeEqualToInt 6;
  }),
  it "removeAll" (fun () => {
    [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
      |> List.removeAll
      |> Expect.toBeEqualToListOfInt [];
  }),
  it "removeFirstOrRaise" (fun () => {
    [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
      |> List.removeFirstOrRaise
      |> Expect.toBeEqualToListOfInt [9, 8, 7, 6, 5, 4, 3, 2, 1];

    (fun () => List.removeFirstOrRaise [] |> ignore) |> Expect.shouldRaise;
  }),
  it "return" (fun () => {
    List.return 1 |> Expect.toBeEqualToListOfInt [1];
  }),
  it "toIterable" (fun () => {
    [0, 1, 2, 3, 4, 5] |> List.toIterable |> Iterable.reduce
        while_::(fun _ i => i < 4) (fun acc i => acc + i) 0
      |> Expect.toBeEqualToInt 6;
  }),
  it "toSequence" (fun () => {
    List.toSequence [1, 2, 3]
      |> Sequence.toIterable
      |> Iterable.reduce (fun acc i => acc + i) 0
      |> Expect.toBeEqualToInt 6;
  }),
];
