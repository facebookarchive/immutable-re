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

let test = describe "Option" [
  it "count" (fun () => {
    Option.return 1 |> Option.count |> expect |> toBeEqualToInt 1;
    Option.empty |> Option.count |> expect |> toBeEqualToInt 0;
  }),
  it "first" (fun () => {
    Option.return 1 |> Option.first |> expect |> toBeEqualToSomeOfInt 1;
    Option.empty |> Option.first |> expect |> toBeEqualToNoneOfInt;
  }),
  it "firstOrRaise" (fun () => {
    Option.return 1 |> Option.firstOrRaise |> expect |> toBeEqualToInt 1;
    (fun () => Option.empty |> Option.firstOrRaise) |> shouldThrow;
  }),
  it "flatMap" (fun () => {
    Option.return 1 |> Option.flatMap (fun i => Option.return (i + 2)) |> expect |> toBeEqualToSomeOfInt 3;
    Option.return 1 |> Option.flatMap (fun i => Option.empty) |> expect |> toBeEqualToNoneOfInt;
    Option.empty |> Option.flatMap (fun i => Option.return (i + 2)) |> expect |> toBeEqualToNoneOfInt;
  }),
  it "flatten" (fun () => {
    Option.empty |> Option.flatten |> expect |> toBeEqualToNoneOfInt;
    Option.return Option.empty |> Option.flatten |> expect |> toBeEqualToNoneOfInt;
    Option.return (Option.return 1) |> Option.flatten |> expect |> toBeEqualToSomeOfInt 1;
  }),
  it "isEmpty" (fun () => {
    Option.return 1 |> Option.isEmpty |> expect |> toBeEqualToFalse;
    Option.empty |> Option.isEmpty |> expect |> toBeEqualToTrue;
  }),
  it "isNotEmpty" (fun () => {
    Option.return 1 |> Option.isNotEmpty |> expect |> toBeEqualToTrue;
    Option.empty |> Option.isNotEmpty |> expect |> toBeEqualToFalse;
  }),
  it "map" (fun () => {
    Option.return 1 |> Option.map (fun i => i + 2) |> expect |> toBeEqualToSomeOfInt 3;
    Option.empty |> Option.map (fun i => i + 2) |> expect |> toBeEqualToNoneOfInt;
  }),
  it "reduce" (fun () => {
    Option.empty |> Option.reduce (fun acc i => acc + i) 0 |> expect |> toBeEqualToInt 0;
    Option.return 1 |> Option.reduce (fun acc i => acc + i) 0 |> expect |> toBeEqualToInt 1;
  }),
  it "return" (fun () => {
    Option.return 1 |> expect |> toBeEqualToSomeOfInt 1;
  }),
];
