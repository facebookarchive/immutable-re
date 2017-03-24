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

let test = describe "Option" [
  it "count" (fun () => {
    Option.return 1 |> Option.count |> Expect.toBeEqualToInt 1;
    (Option.empty ()) |> Option.count |> Expect.toBeEqualToInt 0;
  }),
  it "first" (fun () => {
    Option.return 1 |> Option.first |> Expect.toBeEqualToSomeOfInt 1;
    (Option.empty ()) |> Option.first |> Expect.toBeEqualToNoneOfInt;
  }),
  it "firstOrRaise" (fun () => {
    Option.return 1 |> Option.firstOrRaise |> Expect.toBeEqualToInt 1;
    (fun () => (Option.empty ()) |> Option.firstOrRaise) |> Expect.shouldRaise;
  }),
  it "flatMap" (fun () => {
    Option.return 1 |> Option.flatMap (fun i => Option.return (i + 2)) |> Expect.toBeEqualToSomeOfInt 3;
    Option.return 1 |> Option.flatMap (fun i => (Option.empty ())) |> Expect.toBeEqualToNoneOfInt;
    (Option.empty ()) |> Option.flatMap (fun i => Option.return (i + 2)) |> Expect.toBeEqualToNoneOfInt;
  }),
  it "flatten" (fun () => {
    (Option.empty ()) |> Option.flatten |> Expect.toBeEqualToNoneOfInt;
    Option.return (Option.empty ()) |> Option.flatten |> Expect.toBeEqualToNoneOfInt;
    Option.return (Option.return 1) |> Option.flatten |> Expect.toBeEqualToSomeOfInt 1;
  }),
  it "isEmpty" (fun () => {
    Option.return 1 |> Option.isEmpty |> Expect.toBeEqualToFalse;
    (Option.empty ()) |> Option.isEmpty |> Expect.toBeEqualToTrue;
  }),
  it "isNotEmpty" (fun () => {
    Option.return 1 |> Option.isNotEmpty |> Expect.toBeEqualToTrue;
    (Option.empty ()) |> Option.isNotEmpty |> Expect.toBeEqualToFalse;
  }),
  it "map" (fun () => {
    Option.return 1 |> Option.map (fun i => i + 2) |> Expect.toBeEqualToSomeOfInt 3;
    (Option.empty ()) |> Option.map (fun i => i + 2) |> Expect.toBeEqualToNoneOfInt;
  }),
  it "reduce" (fun () => {
    (Option.empty ()) |> Option.reduce (fun acc i => acc + i) 0 |> Expect.toBeEqualToInt 0;
    Option.return 1 |> Option.reduce (fun acc i => acc + i) 0 |> Expect.toBeEqualToInt 1;
  }),
  it "return" (fun () => {
    Option.return 1 |> Expect.toBeEqualToSomeOfInt 1;
  }),
];
