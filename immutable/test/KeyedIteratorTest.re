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

let test = describe "KeyedIterator" [
  it "concat" (fun () => {
    KeyedIterator.concat [
        IntRange.create start::0 count::2 |> IntRange.toMap |> Map.toKeyedIterator,
        IntRange.create start::2 count::2 |> IntRange.toMap |> Map.toKeyedIterator,
        IntRange.create start::4 count::2 |> IntRange.toMap |> Map.toKeyedIterator,
      ]
      |> KeyedIterator.take 5
      |> KeyedIterator.keys
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [4, 3, 2, 1, 0];
  }),
  it "defer" (fun () => ()),
  it "distinctUntilChangedWith" (fun () => {
    KeyedIterator.concat [
      KeyedIterator.return 1 1,
      KeyedIterator.return 1 1,
      KeyedIterator.return 1 1,

      KeyedIterator.return 2 2,
      KeyedIterator.return 2 2,
      KeyedIterator.return 2 2,

      KeyedIterator.return 3 3,
      KeyedIterator.return 3 3,
      KeyedIterator.return 3 3,

      KeyedIterator.return 4 4,
      KeyedIterator.return 4 4,
      KeyedIterator.return 4 4,
    ]
      |> KeyedIterator.distinctUntilChangedWith
          keyEquals::Equality.int
          valueEquals::Equality.int
      |> KeyedIterator.values
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [4, 3, 2, 1];
  }),
  it "doOnNext" (fun () => {
    let last = ref 0;

    KeyedIterator.concat [
      KeyedIterator.return 0 0,
      KeyedIterator.return 1 1,
      KeyedIterator.return 2 2,
      KeyedIterator.return 3 3,
      KeyedIterator.return 4 4,
    ]
      |> KeyedIterator.doOnNext (fun k v => { last := k })
      |> KeyedIterator.KeyedReducer.forEach (fun k v => ());
    !last |> Expect.toBeEqualToInt 4;

    let empty = KeyedIterator.empty ();
    Pervasives.(===) (KeyedIterator.doOnNext (fun _ _ => ()) empty) empty
      |> Expect.toBeEqualToTrue;
  }),
  it "filter" (fun () => {
    KeyedIterator.concat [
      KeyedIterator.return 0 0,
      KeyedIterator.return 1 1,
      KeyedIterator.return 2 2,
      KeyedIterator.return 3 3,
      KeyedIterator.return 4 4,
    ]
      |> KeyedIterator.filter (fun k v => k mod 2 == 0)
      |> KeyedIterator.keys
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [4, 2, 0];

    let empty = KeyedIterator.empty ();
    Pervasives.(===) (KeyedIterator.filter (fun _ _ => true) empty) empty
      |> Expect.toBeEqualToTrue;
  }),
  it "flatMap" (fun () => {
    let flatMapped = KeyedIterator.concat [
      KeyedIterator.return 0 0,
      KeyedIterator.return 1 1,
    ] |> KeyedIterator.flatMap (fun k v => KeyedIterator.concat [
          KeyedIterator.return k 0,
          KeyedIterator.return k 1,
          KeyedIterator.return k 2,
        ]);

    flatMapped
      |> KeyedIterator.keys
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [1, 1, 1, 0, 0, 0];

    flatMapped
      |> KeyedIterator.values
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [2, 1, 0, 2, 1, 0];
  }),
  it "keys" (fun () => { () }),
  it "map" (fun () => {
    let mapped = KeyedIterator.repeat 2 3
      |> KeyedIterator.take 5
      |> KeyedIterator.map
          keyMapper::(fun k v => v)
          valueMapper::(fun k v => k);

    KeyedIterator.keys mapped
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [3, 3, 3, 3, 3];

    KeyedIterator.values mapped
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [2, 2, 2, 2, 2];
  }),
  it "mapKeys" (fun () => {
    KeyedIterator.repeat 2 3
      |> KeyedIterator.take 5
      |> KeyedIterator.mapKeys (fun k v => 4)
      |> KeyedIterator.keys
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [4, 4, 4, 4, 4];

    KeyedIterator.repeat 2 3
      |> KeyedIterator.take 5
      |> KeyedIterator.mapKeys (fun k v => 4)
      |> KeyedIterator.values
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [3, 3, 3, 3, 3];
  }),
  it "mapValues" (fun () => {
    KeyedIterator.repeat 2 3
      |> KeyedIterator.take 5
      |> KeyedIterator.mapValues (fun k v => 4)
      |> KeyedIterator.values
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [4, 4, 4, 4, 4];

    KeyedIterator.repeat 2 3
      |> KeyedIterator.take 5
      |> KeyedIterator.mapKeys (fun k v => 4)
      |> KeyedIterator.values
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [3, 3, 3, 3, 3];
  }),
  it "repeat" (fun () => {
    KeyedIterator.repeat 2 3
      |> KeyedIterator.take 10
      |> KeyedIterator.doOnNext (fun k v => {
          k |> Expect.toBeEqualToInt 2;
          v |> Expect.toBeEqualToInt 3;
        })
      |> KeyedIterator.reduce (fun acc k v => acc + 5) 0
      |> Expect.toBeEqualToInt 50;
  }),
  it "return" (fun () => {
    KeyedIterator.return 1 2
      |> KeyedIterator.reduce (fun acc k v => acc + k + v) 0
      |> Expect.toBeEqualToInt 3;

    KeyedIterator.return 1 2
      |> KeyedIterator.reduce while_::(fun _  k v => k < 0) (fun acc k v => acc + k + v) 0
      |> Expect.toBeEqualToInt 0;
  }),
  it "scan" (fun () => {
    KeyedIterator.repeat 2 3
      |> KeyedIterator.scan (fun acc k v => k + v) 0
      |> Iterator.take 5
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [5, 5, 5, 5, 0];
  }),
  it "skip" (fun () => {
    KeyedIterator.concat [
      KeyedIterator.repeat 2 2 |> KeyedIterator.take 2,
      KeyedIterator.repeat 3 3 |> KeyedIterator.take 5,
    ] |> KeyedIterator.skip 2
      |> KeyedIterator.keys
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [3, 3 ,3, 3, 3];

    let empty = KeyedIterator.empty ();
    Pervasives.(===) (KeyedIterator.skip 5 empty) empty
      |> Expect.toBeEqualToTrue;
  }),
  it "skipWhile" (fun () => {
    KeyedIterator.concat [
      KeyedIterator.repeat 2 2 |> KeyedIterator.take 5,
      KeyedIterator.repeat 3 3 |> KeyedIterator.take 5,
    ] |> KeyedIterator.skipWhile (fun k v => k < 3)
      |> KeyedIterator.keys
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [3, 3, 3, 3, 3];

    let empty = KeyedIterator.empty ();
    Pervasives.(===)
        (KeyedIterator.skipWhile (fun _ _ => true) empty)
        empty
      |> Expect.toBeEqualToTrue;
  }),
  it "startWith" (fun () => {
    KeyedIterator.repeat 3 3
      |> KeyedIterator.startWith 1 1
      |> KeyedIterator.take 3
      |> KeyedIterator.keys
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [3, 3, 1];
  }),
  it "take" (fun () => {
    KeyedIterator.repeat 3 3
      |> KeyedIterator.take 3
      |> KeyedIterator.values
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [3, 3, 3];

    let empty = KeyedIterator.empty ();
    Pervasives.(===) (KeyedIterator.take 5 empty) empty
      |> Expect.toBeEqualToTrue;
  }),
  it "takeWhile" (fun () => {
    KeyedIterator.concat [
      KeyedIterator.repeat 2 2 |> KeyedIterator.take 2,
      KeyedIterator.repeat 3 3 |> KeyedIterator.take 5,
    ] |> KeyedIterator.takeWhile (fun k v => k < 3)
      |> KeyedIterator.keys
      |> List.fromReverse
      |> Expect.toBeEqualToListOfInt [2, 2];

    let empty = KeyedIterator.empty ();
    Pervasives.(===) (KeyedIterator.takeWhile (fun _ _ => true) empty) empty
      |> Expect.toBeEqualToTrue;
  }),
  it "values" (fun () => ()),
];
