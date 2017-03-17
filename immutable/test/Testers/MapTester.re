/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

/*open Immutable;
open Printf;
open ReUnit;
open ReUnit.Expect;
open ReUnit.Test;

let test
    (empty: unit => 'map)
    (put: int => int => 'map => 'map)
    (remove: int => 'map => 'map)
    (tryGet: int => 'map => option int)
    (n: int): list Test.t => [
  it (sprintf "put %i elements" n) (fun () => {
    let src = IntRange.create 0 n |> IntRange.toSequence |> Sequence.map (Hash.random ());

    let (_, mapOfSizeN) = src
      |> Sequence.scan
        (fun (_, acc) i => (i, acc |> put i i))
        (0, empty ())
      |> Sequence.doOnNext(fun (i, acc) =>
        expect (acc |> tryGet i) |> toBeEqualToSomeOfInt i
      )
      |> Sequence.tryLast |> Option.get;

    src |> Sequence.forEach (fun i =>
      expect (mapOfSizeN |> tryGet i) |> toBeEqualToSomeOfInt i
    )
  }),

  it (sprintf "remove %i elements" n) (fun () => {
    let src = IntRange.create 0 n |> IntRange.toSequence
    let mapOfSizeN = src |> Sequence.reduce (fun acc i => acc |> put i i) (empty ());

    /* FIMXE: Maybe add Sequence.sample, Sequence.filteri */
    let removed = src
      |> Sequence.map(fun i => (i, i))
      |> Sequence.filter (fun (i, v) => i mod 3 == 0)
      |> Sequence.map (fun (i, v) => v);

    let remaining = src
      |> Sequence.map (fun i => (i, i))
      |> Sequence.filter (fun (i, v) => i mod 3 != 0)
      |> Sequence.map (fun (i, v) => v);

    let mapOfSizeNdiv3 = removed |> Sequence.reduce (fun acc i => acc |> remove i) mapOfSizeN;

    removed |> Sequence.forEach(fun i =>
      expect (mapOfSizeNdiv3 |> tryGet i) |> toBeEqualToNoneOfInt
    );

    remaining |> Sequence.forEach(fun i =>
      expect (mapOfSizeNdiv3 |> tryGet i) |> toBeEqualToSomeOfInt i
    );
  }),
];*/
