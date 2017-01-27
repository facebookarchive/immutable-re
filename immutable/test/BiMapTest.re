/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Immutable;
open Functions;
open ReUnit;
open ReUnit.Expect;
open ReUnit.Test;

let test = describe "BiMap" [
  describe "put" [
    it "unique items" (fun () => {
      let map = BiMap.empty () |> BiMap.put "a" "b" |> BiMap.put "c" "d";
      expect (map |> BiMap.tryGet "a") |> toBeEqualToSomeOfString "b";
      expect (map |> BiMap.tryGet "b") |> toBeEqualToNoneOfString;
      expect (map |> BiMap.tryGet "c") |> toBeEqualToSomeOfString "d";
      expect (map |> BiMap.tryGet "d") |> toBeEqualToNoneOfString;
      expect (map |> BiMap.count) |> toBeEqualToInt 2;

      let inverse = map |> BiMap.inverse;
      expect (inverse |> BiMap.tryGet "a") |> toBeEqualToNoneOfString;
      expect (inverse |> BiMap.tryGet "b") |> toBeEqualToSomeOfString "a";
      expect (inverse |> BiMap.tryGet "c") |> toBeEqualToNoneOfString;
      expect (inverse |> BiMap.tryGet "d") |> toBeEqualToSomeOfString "c";
      expect (inverse |> BiMap.count) |> toBeEqualToInt 2;
    }),

    it "item with existing value in the map" (fun () => {
      let map = BiMap.empty ()
        |> BiMap.put "a" "b"
        |> BiMap.put "c" "d"
        |> BiMap.put "e" "b";

      expect (map |> BiMap.tryGet "a") |> toBeEqualToNoneOfString;
      expect (map |> BiMap.tryGet "b") |> toBeEqualToNoneOfString;
      expect (map |> BiMap.tryGet "c") |> toBeEqualToSomeOfString "d";
      expect (map |> BiMap.tryGet "d") |> toBeEqualToNoneOfString;
      expect (map |> BiMap.tryGet "e") |> toBeEqualToSomeOfString "b";
      expect (map |> BiMap.count) |> toBeEqualToInt 2;

      let inverse = map |> BiMap.inverse;
      expect (inverse |> BiMap.tryGet "a") |> toBeEqualToNoneOfString;
      expect (inverse |> BiMap.tryGet "b") |> toBeEqualToSomeOfString "e";
      expect (inverse |> BiMap.tryGet "c") |> toBeEqualToNoneOfString;
      expect (inverse |> BiMap.tryGet "d") |> toBeEqualToSomeOfString "c";
      expect (inverse |> BiMap.tryGet "e") |> toBeEqualToNoneOfString;
      expect (inverse |> BiMap.count) |> toBeEqualToInt 2;
    }),

    it "value is reference equivalent" (fun () => {
      let valueRef = "b";
      let mapA = BiMap.empty () |> BiMap.put "a" valueRef;
      let mapB = mapA |> BiMap.put "a" valueRef;

      expect mapA |> toBeEqualToWith Equality.reference (Functions.return "") mapB;
      expect (mapA |> BiMap.count) |> toBeEqualToInt 1;
      expect (mapA |> BiMap.inverse |> BiMap.count) |> toBeEqualToInt 1;
      expect (mapB |> BiMap.inverse |> BiMap.count) |> toBeEqualToInt 1;
    }),
  ],

  describe "tryPut" [
    it "unique items" (fun () => {
      let map = BiMap.empty ()
        |> BiMap.tryPut "a" "b"
        |> BiMap.tryPut "c" "d";

      expect (map |> BiMap.tryGet "a") |> toBeEqualToSomeOfString "b";
      expect (map |> BiMap.tryGet "b") |> toBeEqualToNoneOfString;
      expect (map |> BiMap.tryGet "c") |> toBeEqualToSomeOfString "d";
      expect (map |> BiMap.tryGet "d") |> toBeEqualToNoneOfString;
      expect (map |> BiMap.count) |> toBeEqualToInt 2;

      let inverse = map |> BiMap.inverse;
      expect (inverse |> BiMap.tryGet "a") |> toBeEqualToNoneOfString;
      expect (inverse |> BiMap.tryGet "b") |> toBeEqualToSomeOfString "a";
      expect (inverse |> BiMap.tryGet "c") |> toBeEqualToNoneOfString;
      expect (inverse |> BiMap.tryGet "d") |> toBeEqualToSomeOfString "c";
      expect (inverse |> BiMap.count) |> toBeEqualToInt 2;
    }),

    it "item with existing value in the map" (fun () => {
      let map = BiMap.empty ()
        |> BiMap.tryPut "a" "b"
        |> BiMap.tryPut "c" "d"
        |> BiMap.tryPut "e" "b";

      expect (map |> BiMap.tryGet "a") |> toBeEqualToSomeOfString "b";
      expect (map |> BiMap.tryGet "b") |> toBeEqualToNoneOfString;
      expect (map |> BiMap.tryGet "c") |> toBeEqualToSomeOfString "d";
      expect (map |> BiMap.tryGet "d") |> toBeEqualToNoneOfString;
      expect (map |> BiMap.tryGet "e") |> toBeEqualToNoneOfString;
      expect (map |> BiMap.count) |> toBeEqualToInt 2;
    }),

    it "value is reference equivalent" (fun () => {
      let valueRef = "b";
      let mapA = BiMap.empty () |> BiMap.tryPut "a" valueRef;
      let mapB = mapA |> BiMap.tryPut "a" valueRef;

      expect mapA |> toBeEqualToWith Equality.reference (Functions.return "") mapB;
    }),
  ],

  describe "remove" [
    it "key in map" (fun () => {
      let map = BiMap.empty ()
        |> BiMap.put "a" "b"
        |> BiMap.put "c" "d"
        |> BiMap.remove "a";

      expect (map |> BiMap.tryGet "a") |> toBeEqualToNoneOfString;
      expect (map |> BiMap.tryGet "b") |> toBeEqualToNoneOfString;
      expect (map |> BiMap.tryGet "c") |> toBeEqualToSomeOfString "d";
      expect (map |> BiMap.tryGet "d") |> toBeEqualToNoneOfString;
      expect (map |> BiMap.count) |> toBeEqualToInt 1;

      let inverse = map |> BiMap.inverse;

      expect (inverse |> BiMap.tryGet "a") |> toBeEqualToNoneOfString;
      expect (inverse |> BiMap.tryGet "b") |> toBeEqualToNoneOfString;
      expect (inverse |> BiMap.tryGet "c") |> toBeEqualToNoneOfString;
      expect (inverse |> BiMap.tryGet "d") |> toBeEqualToSomeOfString "c";
      expect (inverse |> BiMap.count) |> toBeEqualToInt 1;
    }),
  ],

  describe "TransientBiMap" [
    describe "put" [
      it "unique items" (fun () => {
        let map = BiMap.empty ()
          |> BiMap.mutate
          |> TransientBiMap.put "a" "b"
          |> TransientBiMap.put "c" "d"
          |> TransientBiMap.persist;

        expect (map |> BiMap.tryGet "a") |> toBeEqualToSomeOfString "b";
        expect (map |> BiMap.tryGet "b") |> toBeEqualToNoneOfString;
        expect (map |> BiMap.tryGet "c") |> toBeEqualToSomeOfString "d";
        expect (map |> BiMap.tryGet "d") |> toBeEqualToNoneOfString;
        expect (map |> BiMap.count) |> toBeEqualToInt 2;

        let inverse = map |> BiMap.inverse;
        expect (inverse |> BiMap.tryGet "a") |> toBeEqualToNoneOfString;
        expect (inverse |> BiMap.tryGet "b") |> toBeEqualToSomeOfString "a";
        expect (inverse |> BiMap.tryGet "c") |> toBeEqualToNoneOfString;
        expect (inverse |> BiMap.tryGet "d") |> toBeEqualToSomeOfString "c";
        expect (inverse |> BiMap.count) |> toBeEqualToInt 2;
      }),

      it "item with existing value in the map" (fun () => {
        let map = BiMap.empty ()
          |> BiMap.mutate
          |> TransientBiMap.put "a" "b"
          |> TransientBiMap.put "c" "d"
          |> TransientBiMap.put "e" "b"
          |> TransientBiMap.persist;

        expect (map |> BiMap.tryGet "a") |> toBeEqualToNoneOfString;
        expect (map |> BiMap.tryGet "b") |> toBeEqualToNoneOfString;
        expect (map |> BiMap.tryGet "c") |> toBeEqualToSomeOfString "d";
        expect (map |> BiMap.tryGet "d") |> toBeEqualToNoneOfString;
        expect (map |> BiMap.tryGet "e") |> toBeEqualToSomeOfString "b";
        expect (map |> BiMap.count) |> toBeEqualToInt 2;

        let inverse = map |> BiMap.inverse;
        expect (inverse |> BiMap.tryGet "a") |> toBeEqualToNoneOfString;
        expect (inverse |> BiMap.tryGet "b") |> toBeEqualToSomeOfString "e";
        expect (inverse |> BiMap.tryGet "c") |> toBeEqualToNoneOfString;
        expect (inverse |> BiMap.tryGet "d") |> toBeEqualToSomeOfString "c";
        expect (inverse |> BiMap.tryGet "e") |> toBeEqualToNoneOfString;
        expect (inverse |> BiMap.count) |> toBeEqualToInt 2;
      }),
    ],

    describe "tryPut" [
      it "unique items" (fun () => {
        let map = BiMap.empty ()
          |> BiMap.mutate
          |> TransientBiMap.tryPut "a" "b"
          |> TransientBiMap.tryPut "c" "d"
          |> TransientBiMap.persist;

        expect (map |> BiMap.tryGet "a") |> toBeEqualToSomeOfString "b";
        expect (map |> BiMap.tryGet "b") |> toBeEqualToNoneOfString;
        expect (map |> BiMap.tryGet "c") |> toBeEqualToSomeOfString "d";
        expect (map |> BiMap.tryGet "d") |> toBeEqualToNoneOfString;
        expect (map |> BiMap.count) |> toBeEqualToInt 2;

        let inverse = map |> BiMap.inverse;
        expect (inverse |> BiMap.tryGet "a") |> toBeEqualToNoneOfString;
        expect (inverse |> BiMap.tryGet "b") |> toBeEqualToSomeOfString "a";
        expect (inverse |> BiMap.tryGet "c") |> toBeEqualToNoneOfString;
        expect (inverse |> BiMap.tryGet "d") |> toBeEqualToSomeOfString "c";
        expect (inverse |> BiMap.count) |> toBeEqualToInt 2;
      }),

      it "item with existing value in the map" (fun () => {
        let map = BiMap.empty ()
          |> BiMap.mutate
          |> TransientBiMap.tryPut "a" "b"
          |> TransientBiMap.tryPut "c" "d"
          |> TransientBiMap.tryPut "e" "b"
          |> TransientBiMap.persist;

        expect (map |> BiMap.tryGet "a") |> toBeEqualToSomeOfString "b";
        expect (map |> BiMap.tryGet "b") |> toBeEqualToNoneOfString;
        expect (map |> BiMap.tryGet "c") |> toBeEqualToSomeOfString "d";
        expect (map |> BiMap.tryGet "d") |> toBeEqualToNoneOfString;
        expect (map |> BiMap.tryGet "e") |> toBeEqualToNoneOfString;
        expect (map |> BiMap.count) |> toBeEqualToInt 2;
      }),
    ],

    describe "remove" [
      it "key in map" (fun () => {
        let map = BiMap.empty ()
          |> BiMap.mutate
          |> TransientBiMap.put "a" "b"
          |> TransientBiMap.put "c" "d"
          |> TransientBiMap.remove "a"
          |> TransientBiMap.persist;

        expect (map |> BiMap.tryGet "a") |> toBeEqualToNoneOfString;
        expect (map |> BiMap.tryGet "b") |> toBeEqualToNoneOfString;
        expect (map |> BiMap.tryGet "c") |> toBeEqualToSomeOfString "d";
        expect (map |> BiMap.tryGet "d") |> toBeEqualToNoneOfString;
        expect (map |> BiMap.count) |> toBeEqualToInt 1;

        let inverse = map |> BiMap.inverse;

        expect (inverse |> BiMap.tryGet "a") |> toBeEqualToNoneOfString;
        expect (inverse |> BiMap.tryGet "b") |> toBeEqualToNoneOfString;
        expect (inverse |> BiMap.tryGet "c") |> toBeEqualToNoneOfString;
        expect (inverse |> BiMap.tryGet "d") |> toBeEqualToSomeOfString "c";
        expect (inverse |> BiMap.count) |> toBeEqualToInt 1;
      }),
    ],
  ],
];
