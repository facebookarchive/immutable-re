open Immutable;
open ReUnit;
open ReUnit.Expect;
open ReUnit.Test;

let test = describe "Keyed" [
  describe "empty"  [
    it "containsWith" (fun () => {
      expect (Keyed.empty |> Keyed.contains 1 1) |> toBeEqualToFalse
    }),
    it "containsKey" (fun () => {
      expect (Keyed.empty |> Keyed.containsKey 1) |> toBeEqualToFalse
    }),
    it "count" (fun () => {
      expect (Keyed.empty |> Keyed.count) |>  toBeEqualToInt 0;
    }),
    it "every" (fun () => {
      expect (Keyed.empty |> Keyed.every (fun _ _ => true)) |> toBeEqualToFalse;
    }),
    it "find" (fun () => {
      defer (fun () => Keyed.empty |> Keyed.find (fun _ _ => true)) |> throws;
    }),
    it "forEach" (fun () => {
      Keyed.empty |> Keyed.forEach (fun _ => Functions.alwaysFailWith "expected empty");
    }),
    it "get" (fun () => {
      defer (fun () => Keyed.empty |> Keyed.get 1) |> throws;
    }),
    it "none" (fun () => {
      expect (Keyed.empty |> Keyed.none (fun _ _ => true)) |> toBeEqualToTrue;
    }),
    it "reduce" (fun () => {
      Keyed.empty |> Keyed.reduce (fun _ _ => Functions.alwaysFailWith "expected empty") ();
    }),
    it "some" (fun () => {
      expect (Keyed.empty |> Keyed.some (fun _ _ => true)) |> toBeEqualToFalse;
    }),
    it "toSeq" (fun () => {
      expect (Keyed.empty |> Keyed.toSeq |> Seq.isEmpty) |> toBeEqualToTrue;
    }),
    it "tryFind" (fun () => {
      expect (Keyed.empty |> Keyed.tryFind (fun _ _ => true)) |> toBeEqualToNone (fun _ => "");
    }),
    it "tryGet" (fun () => {
      expect (Keyed.empty |> Keyed.tryGet 1) |> toBeEqualToNoneOfInt;
    }),
    it "values" (fun () => {
      expect (Keyed.empty |> Keyed.values |> Seq.isEmpty) |> toBeEqualToTrue;
    }),
  ],
  it "equalsWith" (fun () => {
    expect (Keyed.empty |> Keyed.equals Keyed.empty) |> toBeEqualToTrue;
    expect (Vector.fromSeq @@ List.toSeq @@ [] |> Vector.toKeyed |> Keyed.equals Keyed.empty)
      |> toBeEqualToTrue;
    expect (Vector.fromSeq @@ List.toSeq @@ [1, 2, 3] |> Vector.toKeyed |> Keyed.equals Keyed.empty)
      |> toBeEqualToFalse;
    expect (Keyed.equals
      (Vector.fromSeq @@ List.toSeq @@ [1, 2, 3] |> Vector.toKeyed)
      (Vector.fromSeq @@ List.toSeq @@ [1, 2, 3] |> Vector.toKeyed)
    ) |> toBeEqualToTrue;
  }),
  it "hashWith" (fun () => {
    expect (Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.hash)
      |> toBeEqualToInt (Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.hash)
  }),
  it "isEmpty" (fun () => {
    expect (Keyed.isEmpty Keyed.empty) |> toBeEqualToTrue;
    expect (Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed |> Keyed.isEmpty)
      |> toBeEqualToFalse;
  }),
  it "isNotEmpty" (fun () => {
    expect (Keyed.isNotEmpty Keyed.empty) |> toBeEqualToFalse;
    expect (Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed |> Keyed.isNotEmpty)
      |> toBeEqualToTrue;
  }),
  describe "keys" [
    it "contains" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let keys = keyed |> Keyed.keys;
      expect (keys |> Collection.contains 0) |> toBeEqualToTrue;
      expect (keys |> Collection.contains (-1)) |> toBeEqualToFalse;
    }),
    it "count" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let keys = keyed |> Keyed.keys;
      expect (keys |> Collection.count) |> toBeEqualToInt 3;
    }),
    it "every" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let keys = keyed |> Keyed.keys;
      expect (keys |> Collection.every (fun _ => true)) |> toBeEqualToTrue;
      expect (keys |> Collection.every (fun _ => false)) |> toBeEqualToFalse;
    }),
    it "find" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let keys = keyed |> Keyed.keys;
      expect (keys |> Collection.find (fun _ => true)) |> toBeEqualToInt 0;
      defer (fun () => keys |> Collection.find (fun _ => false)) |> throws;
    }),
    it "forEach" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let keys = keyed |> Keyed.keys;
      let prev = ref (-1);
      keys |> Collection.forEach (fun i => {
        expect i |> toBeEqualToInt (!prev + 1);
        prev := !prev + 1;
      });
    }),
    it "none" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let keys = keyed |> Keyed.keys;
      expect (keys |> Collection.none (fun _ => true)) |> toBeEqualToFalse;
      expect (keys |> Collection.none (fun _ => false)) |> toBeEqualToTrue;
    }),
    it "reduce" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let keys = keyed |> Keyed.keys;
      let reduced = keys |> Collection.reduce (fun acc next => acc + next) 0;
      expect reduced |> toBeEqualToInt 3;
    }),
    it "some" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let keys = keyed |> Keyed.keys;
      expect (keys |> Collection.some (fun _ => true)) |> toBeEqualToTrue;
      expect (keys |> Collection.some (fun _ => false)) |> toBeEqualToFalse;
    }),
    it "toSeq" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let keys = keyed |> Keyed.keys;
      expect (keys |> Collection.toSeq) |> toBeEqualToSeqOfInt (List.toSeq [0, 1, 2]);
    }),
    it "tryFind" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let keys = keyed |> Keyed.keys;
      expect (keys |> Collection.tryFind (fun _ => true)) |> toBeEqualToSomeOfInt 0;
      expect (keys |> Collection.tryFind (fun _ => false)) |> toBeEqualToNoneOfInt;
    }),
  ],
  describe "map"  [
    it "containsWith" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let mapped = keyed |> Keyed.map (fun k v => v + 3);
      expect (mapped |> Keyed.contains 1 1) |> toBeEqualToFalse;
      expect (mapped |> Keyed.contains 1 4) |> toBeEqualToTrue;
    }),
    it "containsKey" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let mapped = keyed |> Keyed.map (fun k v => v + 3);
      expect (mapped |> Keyed.containsKey 1) |> toBeEqualToTrue;
    }),
    it "count" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let mapped = keyed |> Keyed.map (fun k v => v + 3);
      expect (mapped |> Keyed.count) |> toBeEqualToInt 3;
    }),
    it "every" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let mapped = keyed |> Keyed.map (fun k v => v + 3);
      expect (mapped |> Keyed.every (fun k v => v >= 3)) |> toBeEqualToTrue;
      expect (mapped |> Keyed.every (fun k v => v > 3)) |> toBeEqualToFalse;
    }),
    it "find" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let mapped = keyed |> Keyed.map (fun k v => v + 3);
      expect (mapped |> Keyed.find (fun k v => v == 3)) |> toBeEqualTo (fun _ => "") (0, 3);
      defer (fun () => mapped |> Keyed.find (fun k v => v == 1)) |> throws;
    }),
    it "forEach" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let mapped = keyed |> Keyed.map (fun k v => v + 3);
      mapped |> Keyed.forEach (fun k v => {
        expect (v == k + 3) |> toBeEqualToTrue;
      });
    }),
    it "get" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let mapped = keyed |> Keyed.map (fun k v => v + 3);
      expect (mapped |> Keyed.get 0) |> toBeEqualToInt 3;
      defer (fun () => mapped |> Keyed.get 3) |> throws;
    }),
    it "none" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let mapped = keyed |> Keyed.map (fun k v => v + 3);
      expect (mapped |> Keyed.none (fun k v => v < 3)) |> toBeEqualToTrue;
      expect (mapped |> Keyed.none (fun k v => v > 3)) |> toBeEqualToFalse;
    }),
    it "reduce" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let mapped = keyed |> Keyed.map (fun k v => v + 3);
      let reduced = mapped |> Keyed.reduce (fun acc k v => v + acc) 0;
      expect reduced |> toBeEqualToInt 12;
    }),
    it "some" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let mapped = keyed |> Keyed.map (fun k v => v + 3);
      expect (mapped |> Keyed.some (fun k v => v < 3)) |> toBeEqualToFalse;
      expect (mapped |> Keyed.some (fun k v => v > 3)) |> toBeEqualToTrue;
    }),
    it "toSeq" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let mapped = keyed |> Keyed.map (fun k v => v + 3);
      expect (mapped |> Keyed.toSeq) |> toBeEqualToSeq (fun _ => "") (List.toSeq [(0, 3), (1, 4), (2, 5)]);
    }),
    it "tryFind" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let mapped = keyed |> Keyed.map (fun k v => v + 3);
      expect (mapped |> Keyed.tryFind (fun k v => v == 3)) |> toBeEqualToSome (fun _ => "") (0, 3);
      expect (mapped |> Keyed.tryFind (fun k v => v == 1)) |> toBeEqualToNone (fun _ => "");
    }),
    it "tryGet" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let mapped = keyed |> Keyed.map (fun k v => v + 3);
      expect (mapped |> Keyed.tryGet 0) |> toBeEqualToSomeOfInt 3;
      expect (mapped |> Keyed.tryGet 3) |> toBeEqualToNoneOfInt;
    }),
    it "values" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let mapped = keyed |> Keyed.map (fun k v => v + 3);
      expect (mapped |> Keyed.values) |> toBeEqualToSeqOfInt (List.toSeq [3, 4, 5]);
    }),
  ],
  describe "toCollection" [
    it "contains" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let collection = keyed |> Keyed.toCollection;
      expect (collection |> Collection.contains (0, 0)) |> toBeEqualToTrue;
      expect (collection |> Collection.contains (0, 1)) |> toBeEqualToFalse;
    }),
    it "count" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let collection = keyed |> Keyed.toCollection;
      expect (collection |> Collection.count) |> toBeEqualToInt 3;
    }),
    it "every" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let collection = keyed |> Keyed.toCollection;
      expect (collection |> Collection.every (fun _ => true)) |> toBeEqualToTrue;
      expect (collection |> Collection.every (fun _ => false)) |> toBeEqualToFalse;
    }),
    it "find" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let collection = keyed |> Keyed.toCollection;
      expect (collection |> Collection.find (fun _ => true)) |> toBeEqualTo (fun _ => "") (0, 0);
      defer (fun () => collection |> Collection.find (fun _ => false)) |> throws;
    }),
    it "forEach" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let collection = keyed |> Keyed.toCollection;
      collection |> Collection.forEach (fun (k, v) => {
        expect k |> toBeEqualToInt v;
      });
    }),
    it "none" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let collection = keyed |> Keyed.toCollection;
      expect (collection |> Collection.none (fun _ => true)) |> toBeEqualToFalse;
      expect (collection |> Collection.none (fun _ => false)) |> toBeEqualToTrue;
    }),
    it "reduce" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let collection = keyed |> Keyed.toCollection;
      let reduced = collection |> Collection.reduce (fun acc (k, v) => acc + k + v) 0;
      expect reduced |> toBeEqualToInt 6;
    }),
    it "some" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let collection = keyed |> Keyed.toCollection;
      expect (collection |> Collection.some (fun _ => true)) |> toBeEqualToTrue;
      expect (collection |> Collection.some (fun _ => false)) |> toBeEqualToFalse;
    }),
    it "toSeq" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let collection = keyed |> Keyed.toCollection;
      expect (collection |> Collection.toSeq) |> toBeEqualToSeq (fun _ => "") (List.toSeq [(0, 0), (1, 1), (2, 2)]);
    }),
    it "tryFind" (fun () => {
      let keyed = Vector.fromSeq @@ List.toSeq @@ [0, 1, 2] |> Vector.toKeyed;
      let collection = keyed |> Keyed.toCollection;
      expect (collection |> Collection.tryFind (fun _ => true)) |> toBeEqualToSome (fun _ => "") (0, 0);
      expect (collection |> Collection.tryFind (fun _ => false)) |> toBeEqualToNone (fun _ => "");
    }),
  ],
];
