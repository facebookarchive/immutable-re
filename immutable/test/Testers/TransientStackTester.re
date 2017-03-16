/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Immutable;
open Printf;
open ReUnit;
open ReUnit.Expect;
open ReUnit.Test;

module type TransientStack = {
  type t 'a;

  let addFirst: 'a => (t 'a) => (t 'a);
  let count: (t 'a) => int;
  let empty: unit => (t 'a);
  let first: (t 'a) => 'a;
  let isEmpty: t 'a => bool;
  let isNotEmpty: t 'a => bool;
  let removeAll: (t 'a) => (t 'a);
  let removeFirst: (t 'a) => (t 'a);
  let reverse: (t 'a) => (t 'a);
  let tryFirst: (t 'a) => (option 'a);
};

let test  (count: int) (module TransientStack: TransientStack): (list Test.t) => [
  it (sprintf "addFirst and removeFirst %i elements" count) (fun () => {
    let empty = TransientStack.empty ();

    expect (TransientStack.isNotEmpty empty) |> toBeEqualToFalse;
    expect (TransientStack.isEmpty empty) |> toBeEqualToTrue;
    expect (TransientStack.count empty) |> toBeEqualToInt 0;
    defer (fun () => empty |> TransientStack.first) |> throws;
    expect (empty |> TransientStack.tryFirst) |> toBeEqualToNoneOfInt;

    let stack = IntRange.create 0 count |> IntRange.reduce (fun acc i => {
      let acc = acc |> TransientStack.addFirst i;

      expect (TransientStack.isNotEmpty acc) |> toBeEqualToTrue;
      expect (TransientStack.isEmpty acc) |> toBeEqualToFalse;
      expect (TransientStack.count acc) |> toBeEqualToInt (i + 1);
      expect (acc |> TransientStack.first) |> toBeEqualToInt i;
      expect (acc |> TransientStack.tryFirst) |> toBeEqualToSomeOfInt i;

      acc;
    }) empty;

    let shouldBeEmpty = IntRange.create 0 count |> IntRange.reduceRight (fun acc i => {
      expect (TransientStack.isNotEmpty acc) |> toBeEqualToTrue;
      expect (TransientStack.isEmpty acc) |> toBeEqualToFalse;
      expect (TransientStack.count acc) |> toBeEqualToInt (i + 1);
      expect (acc |> TransientStack.first) |> toBeEqualToInt i;
      expect (acc |> TransientStack.tryFirst) |> toBeEqualToSomeOfInt i;
      acc |> TransientStack.removeFirst;
    }) stack;

    expect (TransientStack.isNotEmpty shouldBeEmpty) |> toBeEqualToFalse;
    expect (TransientStack.isEmpty shouldBeEmpty) |> toBeEqualToTrue;
    expect (TransientStack.count shouldBeEmpty) |> toBeEqualToInt 0;
    defer (fun () => shouldBeEmpty |> TransientStack.first) |> throws;
    expect (shouldBeEmpty |> TransientStack.tryFirst) |>  toBeEqualToNoneOfInt;
  }),
];
