open Immutable;
open ReUnit.Expect;
open ReUnit.Test;

let test = describe "Map" [
  describe "empty"  [
    it "containsWith" (fun () => {
      expect (Map.empty |> Map.contains 1 1) |> toBeEqualToFalse
    }),
    it "containsKey" (fun () => {
      expect (Map.empty |> Map.containsKey 1) |> toBeEqualToFalse
    }),
    it "count" (fun () => {
      expect (Map.empty |> Map.count) |>  toBeEqualToInt 0;
    }),
    it "every" (fun () => {
      expect (Map.empty |> Map.every (fun _ _ => true)) |> toBeEqualToFalse;
    }),
    it "find" (fun () => {
      defer (fun () => Map.empty |> Map.find (fun _ _ => true)) |> throws;
    }),
    it "forEach" (fun () => {
      Map.empty |> Map.forEach (fun _ => Functions.alwaysFailWith "expected empty");
    }),
    it "get" (fun () => {
      defer (fun () => Map.empty |> Map.get 1) |> throws;
    }),
    it "none" (fun () => {
      expect (Map.empty |> Map.none (fun _ _ => true)) |> toBeEqualToTrue;
    }),
    it "reduce" (fun () => {
      Map.empty |> Map.reduce (fun _ _ => Functions.alwaysFailWith "expected empty") ();
    }),
    it "some" (fun () => {
      expect (Map.empty |> Map.some (fun _ _ => true)) |> toBeEqualToFalse;
    }),
    it "toSeq" (fun () => {
      expect (Map.empty |> Map.toSeq |> Seq.isEmpty) |> toBeEqualToTrue;
    }),
    it "tryFind" (fun () => {
      expect (Map.empty |> Map.tryFind (fun _ _ => true)) |> toBeEqualToNone (fun _ => "");
    }),
    it "tryGet" (fun () => {
      expect (Map.empty |> Map.tryGet 1) |> toBeEqualToNoneOfInt;
    }),
    it "values" (fun () => {
      expect (Map.empty |> Map.values |> Iterable.count) |> toBeEqualToInt 0;
    }),
  ],
  it "equalsWith" (fun () => {
    expect (Map.empty |> Map.equals Map.empty) |> toBeEqualToTrue;
    expect (Vector.from @@ List.toIterable @@ [] |> Vector.toMap |> Map.equals Map.empty)
      |> toBeEqualToTrue;
    expect (Vector.from @@ List.toIterable @@ [1, 2, 3] |> Vector.toMap |> Map.equals Map.empty)
      |> toBeEqualToFalse;
    expect (Map.equals
      (Vector.from @@ List.toIterable @@ [1, 2, 3] |> Vector.toMap)
      (Vector.from @@ List.toIterable @@ [1, 2, 3] |> Vector.toMap)
    ) |> toBeEqualToTrue;
  }),
  it "hashWith" (fun () => {
    expect (Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.hash)
      |> toBeEqualToInt (Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.hash)
  }),
  it "isEmpty" (fun () => {
    expect (Map.isEmpty Map.empty) |> toBeEqualToTrue;
    expect (Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap |> Map.isEmpty)
      |> toBeEqualToFalse;
  }),
  it "isNotEmpty" (fun () => {
    expect (Map.isNotEmpty Map.empty) |> toBeEqualToFalse;
    expect (Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap |> Map.isNotEmpty)
      |> toBeEqualToTrue;
  }),
  describe "keys" [
    it "contains" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let keys = map |> Map.keys;
      expect (keys |> Set.contains 0) |> toBeEqualToTrue;
      expect (keys |> Set.contains (-1)) |> toBeEqualToFalse;
    }),
    it "count" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let keys = map |> Map.keys;
      expect (keys |> Set.count) |> toBeEqualToInt 3;
    }),
    it "every" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let keys = map |> Map.keys;
      expect (keys |> Set.every (fun _ => true)) |> toBeEqualToTrue;
      expect (keys |> Set.every (fun _ => false)) |> toBeEqualToFalse;
    }),
    it "find" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let keys = map |> Map.keys;
      expect (keys |> Set.find (fun _ => true)) |> toBeEqualToInt 0;
      defer (fun () => keys |> Set.find (fun _ => false)) |> throws;
    }),
    it "forEach" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let keys = map |> Map.keys;
      let prev = ref (-1);
      keys |> Set.forEach (fun i => {
        expect i |> toBeEqualToInt (!prev + 1);
        prev := !prev + 1;
      });
    }),
    it "none" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let keys = map |> Map.keys;
      expect (keys |> Set.none (fun _ => true)) |> toBeEqualToFalse;
      expect (keys |> Set.none (fun _ => false)) |> toBeEqualToTrue;
    }),
    it "reduce" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let keys = map |> Map.keys;
      let reduced = keys |> Set.reduce (fun acc next => acc + next) 0;
      expect reduced |> toBeEqualToInt 3;
    }),
    it "some" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let keys = map |> Map.keys;
      expect (keys |> Set.some (fun _ => true)) |> toBeEqualToTrue;
      expect (keys |> Set.some (fun _ => false)) |> toBeEqualToFalse;
    }),
    it "toSeq" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let keys = map |> Map.keys;
      expect (keys |> Set.toSeq) |> toBeEqualToSeqOfInt (List.toSeq [0, 1, 2]);
    }),
    it "tryFind" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let keys = map |> Map.keys;
      expect (keys |> Set.tryFind (fun _ => true)) |> toBeEqualToSomeOfInt 0;
      expect (keys |> Set.tryFind (fun _ => false)) |> toBeEqualToNoneOfInt;
    }),
  ],
  describe "map"  [
    it "containsWith" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let mapped = map |> Map.map (fun _ v => v + 3);
      expect (mapped |> Map.contains 1 1) |> toBeEqualToFalse;
      expect (mapped |> Map.contains 1 4) |> toBeEqualToTrue;
    }),
    it "containsKey" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let mapped = map |> Map.map (fun _ v => v + 3);
      expect (mapped |> Map.containsKey 1) |> toBeEqualToTrue;
    }),
    it "count" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let mapped = map |> Map.map (fun _ v => v + 3);
      expect (mapped |> Map.count) |> toBeEqualToInt 3;
    }),
    it "every" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let mapped = map |> Map.map (fun _ v => v + 3);
      expect (mapped |> Map.every (fun _ v => v >= 3)) |> toBeEqualToTrue;
      expect (mapped |> Map.every (fun _ v => v > 3)) |> toBeEqualToFalse;
    }),
    it "find" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let mapped = map |> Map.map (fun _ v => v + 3);
      expect (mapped |> Map.find (fun _ v => v == 3)) |> toBeEqualTo (fun _ => "") (0, 3);
      defer (fun () => mapped |> Map.find (fun _ v => v == 1)) |> throws;
    }),
    it "forEach" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let mapped = map |> Map.map (fun _ v => v + 3);
      mapped |> Map.forEach (fun k v => {
        expect (v == k + 3) |> toBeEqualToTrue;
      });
    }),
    it "get" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let mapped = map |> Map.map (fun _ v => v + 3);
      expect (mapped |> Map.get 0) |> toBeEqualToInt 3;
      defer (fun () => mapped |> Map.get 3) |> throws;
    }),
    it "none" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let mapped = map |> Map.map (fun _ v => v + 3);
      expect (mapped |> Map.none (fun _ v => v < 3)) |> toBeEqualToTrue;
      expect (mapped |> Map.none (fun _ v => v > 3)) |> toBeEqualToFalse;
    }),
    it "reduce" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let mapped = map |> Map.map (fun _ v => v + 3);
      let reduced = mapped |> Map.reduce (fun acc _ v => v + acc) 0;
      expect reduced |> toBeEqualToInt 12;
    }),
    it "some" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let mapped = map |> Map.map (fun _ v => v + 3);
      expect (mapped |> Map.some (fun _ v => v < 3)) |> toBeEqualToFalse;
      expect (mapped |> Map.some (fun _ v => v > 3)) |> toBeEqualToTrue;
    }),
    it "toSeq" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let mapped = map |> Map.map (fun _ v => v + 3);
      expect (mapped |> Map.toSeq) |> toBeEqualToSeq (fun _ => "") (List.toSeq [(0, 3), (1, 4), (2, 5)]);
    }),
    it "tryFind" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let mapped = map |> Map.map (fun _ v => v + 3);
      expect (mapped |> Map.tryFind (fun _ v => v == 3)) |> toBeEqualToSome (fun _ => "") (0, 3);
      expect (mapped |> Map.tryFind (fun _ v => v == 1)) |> toBeEqualToNone (fun _ => "");
    }),
    it "tryGet" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let mapped = map |> Map.map (fun _ v => v + 3);
      expect (mapped |> Map.tryGet 0) |> toBeEqualToSomeOfInt 3;
      expect (mapped |> Map.tryGet 3) |> toBeEqualToNoneOfInt;
    }),
    it "values" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let mapped = map |> Map.map (fun _ v => v + 3);
      expect (mapped |> Map.values |> List.fromReversed |> List.toSeq)
        |> toBeEqualToSeqOfInt (List.toSeq [5, 4, 3]);
    }),
  ],
  describe "toSet" [
    it "contains" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let set = map |> Map.toSet;
      expect (set |> Set.contains (0, 0)) |> toBeEqualToTrue;
      expect (set |> Set.contains (0, 1)) |> toBeEqualToFalse;
    }),
    it "count" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let set = map |> Map.toSet;
      expect (set |> Set.count) |> toBeEqualToInt 3;
    }),
    it "every" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let set = map |> Map.toSet;
      expect (set |> Set.every (fun _ => true)) |> toBeEqualToTrue;
      expect (set |> Set.every (fun _ => false)) |> toBeEqualToFalse;
    }),
    it "find" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let set = map |> Map.toSet;
      expect (set |> Set.find (fun _ => true)) |> toBeEqualTo (fun _ => "") (0, 0);
      defer (fun () => set |> Set.find (fun _ => false)) |> throws;
    }),
    it "forEach" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let set = map |> Map.toSet;
      set |> Set.forEach (fun (k, v) => {
        expect k |> toBeEqualToInt v;
      });
    }),
    it "none" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let set = map |> Map.toSet;
      expect (set |> Set.none (fun _ => true)) |> toBeEqualToFalse;
      expect (set |> Set.none (fun _ => false)) |> toBeEqualToTrue;
    }),
    it "reduce" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let set = map |> Map.toSet;
      let reduced = set |> Set.reduce (fun acc (k, v) => acc + k + v) 0;
      expect reduced |> toBeEqualToInt 6;
    }),
    it "some" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let set = map |> Map.toSet;
      expect (set |> Set.some (fun _ => true)) |> toBeEqualToTrue;
      expect (set |> Set.some (fun _ => false)) |> toBeEqualToFalse;
    }),
    it "toSeq" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let set = map |> Map.toSet;
      expect (set |> Set.toSeq) |> toBeEqualToSeq (fun _ => "") (List.toSeq [(0, 0), (1, 1), (2, 2)]);
    }),
    it "tryFind" (fun () => {
      let map = Vector.from @@ List.toIterable @@ [0, 1, 2] |> Vector.toMap;
      let set = map |> Map.toSet;
      expect (set |> Set.tryFind (fun _ => true)) |> toBeEqualToSome (fun _ => "") (0, 0);
      expect (set |> Set.tryFind (fun _ => false)) |> toBeEqualToNone (fun _ => "");
    }),
  ],
];
