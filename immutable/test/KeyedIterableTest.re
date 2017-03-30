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

let (>>) (f1: 'a => 'b) (f2: 'b => 'c) (a: 'a) => f2 (f1 a);

let test = describe "KeyedIterable" [
  it "concat" (fun () => {
    KeyedIterable.concat [
      IntRange.create start::0 count::2
        |> IntRange.toIterable
        |> Iterable.map (fun i: (int, int) => (i, i))
        |> KeyedIterable.fromEntries,
      IntRange.create start::2 count::2
        |> IntRange.toIterable
        |> Iterable.map (fun i: (int, int) => (i, i))
        |> KeyedIterable.fromEntries,
      IntRange.create start::4 count::2
        |> IntRange.toIterable
        |> Iterable.map (fun i: (int, int) => (i, i))
        |> KeyedIterable.fromEntries,
    ] |> KeyedIterable.take 5
      |> KeyedIterable.keys
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [4, 3, 2, 1, 0];
  }),
  it "defer" (fun () => ()),
  it "distinctUntilChangedWith" (fun () => {
    KeyedIterable.concat [
      KeyedIterable.return 1 1,
      KeyedIterable.return 1 1,
      KeyedIterable.return 1 1,

      KeyedIterable.return 2 2,
      KeyedIterable.return 2 2,
      KeyedIterable.return 2 2,

      KeyedIterable.return 3 3,
      KeyedIterable.return 3 3,
      KeyedIterable.return 3 3,

      KeyedIterable.return 4 4,
      KeyedIterable.return 4 4,
      KeyedIterable.return 4 4,
    ]
      |> KeyedIterable.distinctUntilChangedWith
          keyEquals::Equality.int
          valueEquals::Equality.int
      |> KeyedIterable.values
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [4, 3, 2, 1];
  }),
  it "doOnNext" (fun () => {
    let last = ref 0;

    KeyedIterable.concat [
      KeyedIterable.return 0 0,
      KeyedIterable.return 1 1,
      KeyedIterable.return 2 2,
      KeyedIterable.return 3 3,
      KeyedIterable.return 4 4,
    ]
      |> KeyedIterable.doOnNext (fun k _ => { last := k })
      |> KeyedIterable.KeyedReducer.forEach (fun _ _ => ());
    !last |> Expect.toBeEqualToInt 4;

    let empty = KeyedIterable.empty ();
    Pervasives.(===) (KeyedIterable.doOnNext (fun _ _ => ()) empty) empty
      |> Expect.toBeEqualToTrue;
  }),
  it "filter" (fun () => {
    KeyedIterable.concat [
      KeyedIterable.return 0 0,
      KeyedIterable.return 1 1,
      KeyedIterable.return 2 2,
      KeyedIterable.return 3 3,
      KeyedIterable.return 4 4,
    ]
      |> KeyedIterable.filter (fun k _ => k mod 2 === 0)
      |> KeyedIterable.keys
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [4, 2, 0];

    let empty = KeyedIterable.empty ();
    Pervasives.(===) (KeyedIterable.filter (fun _ _ => true) empty) empty
      |> Expect.toBeEqualToTrue;
  }),
  it "flatMap" (fun () => {
    let flatMapped = KeyedIterable.concat [
      KeyedIterable.return 0 0,
      KeyedIterable.return 1 1,
    ] |> KeyedIterable.flatMap (fun k _ => KeyedIterable.concat [
          KeyedIterable.return k 0,
          KeyedIterable.return k 1,
          KeyedIterable.return k 2,
        ]);

    flatMapped
      |> KeyedIterable.keys
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [1, 1, 1, 0, 0, 0];

    flatMapped
      |> KeyedIterable.values
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [2, 1, 0, 2, 1, 0];
  }),
  it "generate" (fun () => { () }),
  it "keys" (fun () => { () }),
  it "map" (fun () => {
    let mapped = KeyedIterable.generate genKey::(fun k _ => k) genValue::(fun _ v => v) 2 3
      |> KeyedIterable.take 5
      |> KeyedIterable.map
          keyMapper::(fun _ v => v)
          valueMapper::(fun k _ => k);

    KeyedIterable.keys mapped
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [3, 3, 3, 3, 3];

    KeyedIterable.values mapped
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [2, 2, 2, 2, 2];
  }),
  it "mapKeys" (fun () => {
    KeyedIterable.generate genKey::(fun k _ => k) genValue::(fun _ v => v) 2 3
      |> KeyedIterable.take 5
      |> KeyedIterable.mapKeys (fun _ _ => 4)
      |> KeyedIterable.keys
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [4, 4, 4, 4, 4];

    KeyedIterable.generate genKey::(fun k _ => k) genValue::(fun _ v => v) 2 3
      |> KeyedIterable.take 5
      |> KeyedIterable.mapKeys (fun _ _ => 4)
      |> KeyedIterable.values
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [3, 3, 3, 3, 3];
  }),
  it "mapValues" (fun () => {
    KeyedIterable.generate genKey::(fun k _ => k) genValue::(fun _ v => v) 2 3
      |> KeyedIterable.take 5
      |> KeyedIterable.mapValues (fun _ _ => 4)
      |> KeyedIterable.values
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [4, 4, 4, 4, 4];

    KeyedIterable.generate genKey::(fun k _ => k) genValue::(fun _ v => v) 2 3
      |> KeyedIterable.take 5
      |> KeyedIterable.mapKeys (fun _ _ => 4)
      |> KeyedIterable.values
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [3, 3, 3, 3, 3];
  }),
  it "repeat" (fun () => {
    KeyedIterable.generate genKey::(fun k _ => k) genValue::(fun _ v => v) 2 3
      |> KeyedIterable.take 10
      |> KeyedIterable.doOnNext (fun k v => {
          k |> Expect.toBeEqualToInt 2;
          v |> Expect.toBeEqualToInt 3;
        })
      |> KeyedIterable.reduce (fun acc _ _ => acc + 5) 0
      |> Expect.toBeEqualToInt 50;
  }),
  it "return" (fun () => {
    KeyedIterable.return 1 2
      |> KeyedIterable.reduce (fun acc k v => acc + k + v) 0
      |> Expect.toBeEqualToInt 3;

    KeyedIterable.return 1 2
      |> KeyedIterable.reduce while_::(fun _  k _ => k < 0) (fun acc k v => acc + k + v) 0
      |> Expect.toBeEqualToInt 0;
  }),
  it "scan" (fun () => {
    KeyedIterable.generate genKey::(fun k _ => k) genValue::(fun _ v => v) 2 3
      |> KeyedIterable.scan (fun _ k v => k + v) 0
      |> Iterable.take 5
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [5, 5, 5, 5, 0];
  }),
  it "skip" (fun () => {
    KeyedIterable.concat [
      KeyedIterable.generate genKey::(fun k _ => k) genValue::(fun _ v => v) 2 2 |> KeyedIterable.take 2,
      KeyedIterable.generate genKey::(fun k _ => k) genValue::(fun _ v => v) 3 3 |> KeyedIterable.take 5,
    ] |> KeyedIterable.skip 2
      |> KeyedIterable.keys
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [3, 3 ,3, 3, 3];

    let empty = KeyedIterable.empty ();
    Pervasives.(===) (KeyedIterable.skip 5 empty) empty
      |> Expect.toBeEqualToTrue;
  }),
  it "skipWhile" (fun () => {
    KeyedIterable.concat [
      KeyedIterable.generate genKey::(fun k _ => k) genValue::(fun _ v => v) 2 2 |> KeyedIterable.take 2,
      KeyedIterable.generate genKey::(fun k _ => k) genValue::(fun _ v => v) 3 3 |> KeyedIterable.take 5,
    ] |> KeyedIterable.skipWhile (fun k _ => k < 3)
      |> KeyedIterable.keys
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [3, 3, 3, 3, 3];

    let empty = KeyedIterable.empty ();
    Pervasives.(===)
        (KeyedIterable.skipWhile (fun _ _ => true) empty)
        empty
      |> Expect.toBeEqualToTrue;
  }),
  it "startWith" (fun () => {
    KeyedIterable.generate genKey::(fun k _ => k) genValue::(fun _ v => v) 3 3
      |> KeyedIterable.startWith 1 1
      |> KeyedIterable.take 3
      |> KeyedIterable.keys
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [3, 3, 1];
  }),
  it "take" (fun () => {
    KeyedIterable.generate genKey::(fun k _ => k) genValue::(fun _ v => v) 3 3
      |> KeyedIterable.take 3
      |> KeyedIterable.values
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [3, 3, 3];

    let empty = KeyedIterable.empty ();
    Pervasives.(===) (KeyedIterable.take 5 empty) empty
      |> Expect.toBeEqualToTrue;
  }),
  it "takeWhile" (fun () => {
    KeyedIterable.concat [
      KeyedIterable.generate genKey::(fun k _ => k) genValue::(fun _ v => v) 2 2 |> KeyedIterable.take 2,
      KeyedIterable.generate genKey::(fun k _ => k) genValue::(fun _ v => v) 3 3 |> KeyedIterable.take 2,
    ] |> KeyedIterable.takeWhile (fun k _ => k < 3)
      |> KeyedIterable.keys
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [2, 2];

    let empty = KeyedIterable.empty ();
    Pervasives.(===) (KeyedIterable.takeWhile (fun _ _ => true) empty) empty
      |> Expect.toBeEqualToTrue;
  }),
  it "values" (fun () => ()),
];
