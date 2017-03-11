/*open Immutable;
open ReUnit;
open ReUnit.Expect;
open ReUnit.Test;

let test = describe "HashBiMap" [
  describe "put" [
    it "unique items" (fun () => {
      let map = HashBiMap.empty () |> HashBiMap.put "a" "b" |> HashBiMap.put "c" "d";
      expect (map |> HashBiMap.tryGet "a") |> toBeEqualToSomeOfString "b";
      expect (map |> HashBiMap.tryGet "b") |> toBeEqualToNoneOfString;
      expect (map |> HashBiMap.tryGet "c") |> toBeEqualToSomeOfString "d";
      expect (map |> HashBiMap.tryGet "d") |> toBeEqualToNoneOfString;
      expect (map |> HashBiMap.count) |> toBeEqualToInt 2;

      let inverse = map |> HashBiMap.inverse;
      expect (inverse |> HashBiMap.tryGet "a") |> toBeEqualToNoneOfString;
      expect (inverse |> HashBiMap.tryGet "b") |> toBeEqualToSomeOfString "a";
      expect (inverse |> HashBiMap.tryGet "c") |> toBeEqualToNoneOfString;
      expect (inverse |> HashBiMap.tryGet "d") |> toBeEqualToSomeOfString "c";
      expect (inverse |> HashBiMap.count) |> toBeEqualToInt 2;
    }),

    it "item with existing value in the map" (fun () => {
      let map = HashBiMap.empty ()
        |> HashBiMap.put "a" "b"
        |> HashBiMap.put "c" "d"
        |> HashBiMap.put "e" "b";

      expect (map |> HashBiMap.tryGet "a") |> toBeEqualToNoneOfString;
      expect (map |> HashBiMap.tryGet "b") |> toBeEqualToNoneOfString;
      expect (map |> HashBiMap.tryGet "c") |> toBeEqualToSomeOfString "d";
      expect (map |> HashBiMap.tryGet "d") |> toBeEqualToNoneOfString;
      expect (map |> HashBiMap.tryGet "e") |> toBeEqualToSomeOfString "b";
      expect (map |> HashBiMap.count) |> toBeEqualToInt 2;

      let inverse = map |> HashBiMap.inverse;
      expect (inverse |> HashBiMap.tryGet "a") |> toBeEqualToNoneOfString;
      expect (inverse |> HashBiMap.tryGet "b") |> toBeEqualToSomeOfString "e";
      expect (inverse |> HashBiMap.tryGet "c") |> toBeEqualToNoneOfString;
      expect (inverse |> HashBiMap.tryGet "d") |> toBeEqualToSomeOfString "c";
      expect (inverse |> HashBiMap.tryGet "e") |> toBeEqualToNoneOfString;
      expect (inverse |> HashBiMap.count) |> toBeEqualToInt 2;
    }),

    it "value is reference equivalent" (fun () => {
      let valueRef = "b";
      let mapA = HashBiMap.empty () |> HashBiMap.put "a" valueRef;
      let mapB = mapA |> HashBiMap.put "a" valueRef;

      expect mapA |> toBeEqualToWith Equality.reference (Functions.return "") mapB;
      expect (mapA |> HashBiMap.count) |> toBeEqualToInt 1;
      expect (mapA |> HashBiMap.inverse |> HashBiMap.count) |> toBeEqualToInt 1;
      expect (mapB |> HashBiMap.inverse |> HashBiMap.count) |> toBeEqualToInt 1;
    }),
  ],

  describe "tryPut" [
    it "unique items" (fun () => {
      let map = HashBiMap.empty ()
        |> HashBiMap.tryPut "a" "b"
        |> HashBiMap.tryPut "c" "d";

      expect (map |> HashBiMap.tryGet "a") |> toBeEqualToSomeOfString "b";
      expect (map |> HashBiMap.tryGet "b") |> toBeEqualToNoneOfString;
      expect (map |> HashBiMap.tryGet "c") |> toBeEqualToSomeOfString "d";
      expect (map |> HashBiMap.tryGet "d") |> toBeEqualToNoneOfString;
      expect (map |> HashBiMap.count) |> toBeEqualToInt 2;

      let inverse = map |> HashBiMap.inverse;
      expect (inverse |> HashBiMap.tryGet "a") |> toBeEqualToNoneOfString;
      expect (inverse |> HashBiMap.tryGet "b") |> toBeEqualToSomeOfString "a";
      expect (inverse |> HashBiMap.tryGet "c") |> toBeEqualToNoneOfString;
      expect (inverse |> HashBiMap.tryGet "d") |> toBeEqualToSomeOfString "c";
      expect (inverse |> HashBiMap.count) |> toBeEqualToInt 2;
    }),

    it "item with existing value in the map" (fun () => {
      let map = HashBiMap.empty ()
        |> HashBiMap.tryPut "a" "b"
        |> HashBiMap.tryPut "c" "d"
        |> HashBiMap.tryPut "e" "b";

      expect (map |> HashBiMap.tryGet "a") |> toBeEqualToSomeOfString "b";
      expect (map |> HashBiMap.tryGet "b") |> toBeEqualToNoneOfString;
      expect (map |> HashBiMap.tryGet "c") |> toBeEqualToSomeOfString "d";
      expect (map |> HashBiMap.tryGet "d") |> toBeEqualToNoneOfString;
      expect (map |> HashBiMap.tryGet "e") |> toBeEqualToNoneOfString;
      expect (map |> HashBiMap.count) |> toBeEqualToInt 2;
    }),

    it "value is reference equivalent" (fun () => {
      let valueRef = "b";
      let mapA = HashBiMap.empty () |> HashBiMap.tryPut "a" valueRef;
      let mapB = mapA |> HashBiMap.tryPut "a" valueRef;

      expect mapA |> toBeEqualToWith Equality.reference (Functions.return "") mapB;
    }),
  ],

  describe "remove" [
    it "key in map" (fun () => {
      let map = HashBiMap.empty ()
        |> HashBiMap.put "a" "b"
        |> HashBiMap.put "c" "d"
        |> HashBiMap.remove "a";

      expect (map |> HashBiMap.tryGet "a") |> toBeEqualToNoneOfString;
      expect (map |> HashBiMap.tryGet "b") |> toBeEqualToNoneOfString;
      expect (map |> HashBiMap.tryGet "c") |> toBeEqualToSomeOfString "d";
      expect (map |> HashBiMap.tryGet "d") |> toBeEqualToNoneOfString;
      expect (map |> HashBiMap.count) |> toBeEqualToInt 1;

      let inverse = map |> HashBiMap.inverse;

      expect (inverse |> HashBiMap.tryGet "a") |> toBeEqualToNoneOfString;
      expect (inverse |> HashBiMap.tryGet "b") |> toBeEqualToNoneOfString;
      expect (inverse |> HashBiMap.tryGet "c") |> toBeEqualToNoneOfString;
      expect (inverse |> HashBiMap.tryGet "d") |> toBeEqualToSomeOfString "c";
      expect (inverse |> HashBiMap.count) |> toBeEqualToInt 1;
    }),
  ],

  describe "TransientHashBiMap" [
    describe "put" [
      it "unique items" (fun () => {
        let map = HashBiMap.empty ()
          |> HashBiMap.mutate
          |> TransientHashBiMap.put "a" "b"
          |> TransientHashBiMap.put "c" "d"
          |> TransientHashBiMap.persist;

        expect (map |> HashBiMap.tryGet "a") |> toBeEqualToSomeOfString "b";
        expect (map |> HashBiMap.tryGet "b") |> toBeEqualToNoneOfString;
        expect (map |> HashBiMap.tryGet "c") |> toBeEqualToSomeOfString "d";
        expect (map |> HashBiMap.tryGet "d") |> toBeEqualToNoneOfString;
        expect (map |> HashBiMap.count) |> toBeEqualToInt 2;

        let inverse = map |> HashBiMap.inverse;
        expect (inverse |> HashBiMap.tryGet "a") |> toBeEqualToNoneOfString;
        expect (inverse |> HashBiMap.tryGet "b") |> toBeEqualToSomeOfString "a";
        expect (inverse |> HashBiMap.tryGet "c") |> toBeEqualToNoneOfString;
        expect (inverse |> HashBiMap.tryGet "d") |> toBeEqualToSomeOfString "c";
        expect (inverse |> HashBiMap.count) |> toBeEqualToInt 2;
      }),

      it "item with existing value in the map" (fun () => {
        let map = HashBiMap.empty ()
          |> HashBiMap.mutate
          |> TransientHashBiMap.put "a" "b"
          |> TransientHashBiMap.put "c" "d"
          |> TransientHashBiMap.put "e" "b"
          |> TransientHashBiMap.persist;

        expect (map |> HashBiMap.tryGet "a") |> toBeEqualToNoneOfString;
        expect (map |> HashBiMap.tryGet "b") |> toBeEqualToNoneOfString;
        expect (map |> HashBiMap.tryGet "c") |> toBeEqualToSomeOfString "d";
        expect (map |> HashBiMap.tryGet "d") |> toBeEqualToNoneOfString;
        expect (map |> HashBiMap.tryGet "e") |> toBeEqualToSomeOfString "b";
        expect (map |> HashBiMap.count) |> toBeEqualToInt 2;

        let inverse = map |> HashBiMap.inverse;
        expect (inverse |> HashBiMap.tryGet "a") |> toBeEqualToNoneOfString;
        expect (inverse |> HashBiMap.tryGet "b") |> toBeEqualToSomeOfString "e";
        expect (inverse |> HashBiMap.tryGet "c") |> toBeEqualToNoneOfString;
        expect (inverse |> HashBiMap.tryGet "d") |> toBeEqualToSomeOfString "c";
        expect (inverse |> HashBiMap.tryGet "e") |> toBeEqualToNoneOfString;
        expect (inverse |> HashBiMap.count) |> toBeEqualToInt 2;
      }),
    ],

    describe "tryPut" [
      it "unique items" (fun () => {
        let map = HashBiMap.empty ()
          |> HashBiMap.mutate
          |> TransientHashBiMap.tryPut "a" "b"
          |> TransientHashBiMap.tryPut "c" "d"
          |> TransientHashBiMap.persist;

        expect (map |> HashBiMap.tryGet "a") |> toBeEqualToSomeOfString "b";
        expect (map |> HashBiMap.tryGet "b") |> toBeEqualToNoneOfString;
        expect (map |> HashBiMap.tryGet "c") |> toBeEqualToSomeOfString "d";
        expect (map |> HashBiMap.tryGet "d") |> toBeEqualToNoneOfString;
        expect (map |> HashBiMap.count) |> toBeEqualToInt 2;

        let inverse = map |> HashBiMap.inverse;
        expect (inverse |> HashBiMap.tryGet "a") |> toBeEqualToNoneOfString;
        expect (inverse |> HashBiMap.tryGet "b") |> toBeEqualToSomeOfString "a";
        expect (inverse |> HashBiMap.tryGet "c") |> toBeEqualToNoneOfString;
        expect (inverse |> HashBiMap.tryGet "d") |> toBeEqualToSomeOfString "c";
        expect (inverse |> HashBiMap.count) |> toBeEqualToInt 2;
      }),

      it "item with existing value in the map" (fun () => {
        let map = HashBiMap.empty ()
          |> HashBiMap.mutate
          |> TransientHashBiMap.tryPut "a" "b"
          |> TransientHashBiMap.tryPut "c" "d"
          |> TransientHashBiMap.tryPut "e" "b"
          |> TransientHashBiMap.persist;

        expect (map |> HashBiMap.tryGet "a") |> toBeEqualToSomeOfString "b";
        expect (map |> HashBiMap.tryGet "b") |> toBeEqualToNoneOfString;
        expect (map |> HashBiMap.tryGet "c") |> toBeEqualToSomeOfString "d";
        expect (map |> HashBiMap.tryGet "d") |> toBeEqualToNoneOfString;
        expect (map |> HashBiMap.tryGet "e") |> toBeEqualToNoneOfString;
        expect (map |> HashBiMap.count) |> toBeEqualToInt 2;
      }),
    ],

    describe "remove" [
      it "key in map" (fun () => {
        let map = HashBiMap.empty ()
          |> HashBiMap.mutate
          |> TransientHashBiMap.put "a" "b"
          |> TransientHashBiMap.put "c" "d"
          |> TransientHashBiMap.remove "a"
          |> TransientHashBiMap.persist;

        expect (map |> HashBiMap.tryGet "a") |> toBeEqualToNoneOfString;
        expect (map |> HashBiMap.tryGet "b") |> toBeEqualToNoneOfString;
        expect (map |> HashBiMap.tryGet "c") |> toBeEqualToSomeOfString "d";
        expect (map |> HashBiMap.tryGet "d") |> toBeEqualToNoneOfString;
        expect (map |> HashBiMap.count) |> toBeEqualToInt 1;

        let inverse = map |> HashBiMap.inverse;

        expect (inverse |> HashBiMap.tryGet "a") |> toBeEqualToNoneOfString;
        expect (inverse |> HashBiMap.tryGet "b") |> toBeEqualToNoneOfString;
        expect (inverse |> HashBiMap.tryGet "c") |> toBeEqualToNoneOfString;
        expect (inverse |> HashBiMap.tryGet "d") |> toBeEqualToSomeOfString "c";
        expect (inverse |> HashBiMap.count) |> toBeEqualToInt 1;
      }),
    ],
  ],
];*/
