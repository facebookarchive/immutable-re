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

module type SetImpl = {
  type t;

  let add: int => t => t;
  let addAll: (Iterator.t int) => t => t;
  let contains: int => t => bool;
  let count: t => int;
  let empty: unit => t;
  let equals: t => t => bool;
  let every: (int => bool) => t => bool;
  let find: (int => bool) => t => int;
  let forEach: (int => unit) => t => unit;
  let from: (Iterator.t int) => t;
  let hash: (Hash.t t);
  let intersect: t => t => t;
  let isEmpty: t => bool;
  let isNotEmpty: t => bool;
  let none: (int => bool) => t => bool;
  let reduce: ('acc => int => 'acc) => 'acc => t => 'acc;
  let remove: int => t => t;
  let removeAll: t => t;
  let some: (int => bool) => t => bool;
  let subtract: t => t => t;
  let toSet: t => (Set.t int);
  let toMap: t => (Map.t int int);
  let toSequence: t => (Sequence.t int);
  let tryFind: (int => bool) => t => (option int);
  let union: t => t => t;
};

let intTupleToString (i, v) =>
  "(" ^ (string_of_int i) ^ "," ^ (string_of_int v) ^ ")";

let test (count: int) (module SetImpl: SetImpl): (list Test.t) => [
  it (sprintf "add %i elements" count) (fun () => {
    let src = IntRange.create 0 count
      |> IntRange.toSequence
      |> Sequence.map (Hash.random ());

    let (_, mapOfSizeN) = src
      |> Sequence.scan
        (fun (_, acc) i => (i, acc |> SetImpl.add i))
        (0, SetImpl.empty ())
      |> Sequence.doOnNext(fun (i, acc) =>
        expect (acc |> SetImpl.contains i) |> toBeEqualToTrue
      )
      |> Sequence.last;

    src |> Sequence.forEach (fun i =>
      expect (mapOfSizeN |> SetImpl.contains i) |> toBeEqualToTrue
    );
  }),
  it (sprintf "removeAll %i elements" count) (fun () => {
    let set = IntRange.create 0 count
      |> IntRange.toIterator
      |> SetImpl.from;

    expect (SetImpl.isNotEmpty set) |> toBeEqualToTrue;
    expect (SetImpl.isEmpty set) |> toBeEqualToFalse;
    expect (SetImpl.count set) |> toBeEqualToInt count;

    let shouldBeEmpty = SetImpl.removeAll set;
    expect (SetImpl.isNotEmpty shouldBeEmpty) |> toBeEqualToFalse;
    expect (SetImpl.isEmpty shouldBeEmpty) |> toBeEqualToTrue;
    expect (SetImpl.count shouldBeEmpty) |> toBeEqualToInt 0;
  }),
  it (sprintf "remove %i elements" count) (fun () => {
    let empty = SetImpl.empty ();

    expect (SetImpl.isNotEmpty empty) |> toBeEqualToFalse;
    expect (SetImpl.isEmpty empty) |> toBeEqualToTrue;
    expect (SetImpl.count empty) |> toBeEqualToInt 0;

    let set = empty
      |> SetImpl.addAll (IntRange.create 0 count |> IntRange.toIterator);

    let shouldBeEmpty = IntRange.create 0 count |> IntRange.reduceRight (fun acc i => {
      expect (SetImpl.isNotEmpty acc) |> toBeEqualToTrue;
      expect (SetImpl.isEmpty acc) |> toBeEqualToFalse;
      expect (SetImpl.count acc) |> toBeEqualToInt (i + 1);
      acc |> SetImpl.remove i;
    }) set;

    expect (SetImpl.isNotEmpty shouldBeEmpty) |> toBeEqualToFalse;
    expect (SetImpl.isEmpty shouldBeEmpty) |> toBeEqualToTrue;
    expect (SetImpl.count shouldBeEmpty) |> toBeEqualToInt 0;
  }),
  it (sprintf "contains with %i elements" count) (fun () => {
    let values = Sequence.generate (fun i => i + 5) (-count) |> Sequence.take count;
    let set = values |> Sequence.toIterator |> SetImpl.from;
    values |> Sequence.forEach (fun i => {
      expect (SetImpl.contains i set) |> toBeEqualToTrue;
    });
  }),
  it (sprintf "count with %i elements" count) (fun () => {
    let set = Sequence.generate (fun i => i + 5) (-count)
      |> Sequence.take count
      |> Sequence.toIterator
      |> SetImpl.from;
    expect (SetImpl.count set) |> toBeEqualToInt count;
  }),
  it (sprintf "equals %i elements" count) (fun () => {
    let setCountMinusOne = IntRange.create 0 (count - 1) |> IntRange.toIterator |> SetImpl.from;
    let setCount = IntRange.create 0 count |> IntRange.toIterator |> SetImpl.from;
    let setCountDup = IntRange.create 0 count |> IntRange.toIterator|> SetImpl.from;
    let setCountPlusOne = IntRange.create 0 (count + 1) |> IntRange.toIterator |> SetImpl.from;

    expect (SetImpl.equals setCount setCount) |> toBeEqualToTrue;
    expect (SetImpl.equals setCount setCountDup) |> toBeEqualToTrue;
    expect (SetImpl.equals setCount setCountMinusOne) |> toBeEqualToFalse;
    expect (SetImpl.equals setCount setCountPlusOne) |> toBeEqualToFalse;
  }),
  it (sprintf "every with %i elements" count) (fun () => {
    IntRange.create 0 count
      |> IntRange.toIterator
      |> SetImpl.from
      |> SetImpl.every (fun v => v > (count / 2))
      |> expect |> toBeEqualToFalse;

    IntRange.create 0 count
      |> IntRange.toIterator
      |> SetImpl.from
      |> SetImpl.every (fun v => v > 0)
      |> expect |> toBeEqualToFalse;

    IntRange.create 0 count
      |> IntRange.toIterator
      |> SetImpl.from
      |> SetImpl.every (fun v => v > (count - 2))
      |> expect |> toBeEqualToFalse;

    IntRange.create 0 count
      |> IntRange.toIterator
      |> SetImpl.from
      |> SetImpl.every (fun v => v < count)
      |> expect |> toBeEqualToTrue;
  }),
  it (sprintf "find and tryFind in %i elements" count) (fun () => {
    let set = IntRange.create 0 count
      |> IntRange.toIterator
      |> SetImpl.from;

    let find0 i => i == 0;
    let findCountMinusOne i => i == (count - 1);
    let findCountDividedByTwo i => i == (count / 2);

    expect (SetImpl.find find0 set) |> toBeEqualToInt 0;
    expect (SetImpl.find findCountMinusOne set) |> toBeEqualToInt (count - 1);
    expect (SetImpl.find findCountDividedByTwo set) |> toBeEqualToInt (count / 2);

    expect (SetImpl.tryFind find0 set) |> toBeEqualToSomeOfInt 0;
    expect (SetImpl.tryFind findCountMinusOne set) |> toBeEqualToSomeOfInt (count - 1);
    expect (SetImpl.tryFind findCountDividedByTwo set) |> toBeEqualToSomeOfInt (count / 2);

    defer (fun () => SetImpl.find Functions.alwaysFalse set) |> throws;
    expect (SetImpl.tryFind Functions.alwaysFalse set) |> toBeEqualToNoneOfInt;
  }),
  it (sprintf "forEach with %i elements" count) (fun () => {
    let seq = IntRange.create 0 count
      |> IntRange.toIteratorReversed;
    let set = SetImpl.from seq;
    set |> SetImpl.forEach (fun i => {
      expect (set |> SetImpl.contains i) |> toBeEqualToTrue;
    });
  }),
  it (sprintf "hash with %i elements" count) (fun () => {
    let col = IntRange.create 0 count
      |> IntRange.toIterator
      |> SetImpl.from;

    let dup = IntRange.create 0 count
      |> IntRange.toIteratorReversed
      |> SetImpl.from;

    expect (SetImpl.hash col) |> toBeEqualToInt (SetImpl.hash col);
    expect (SetImpl.hash col) |> toBeEqualToInt (SetImpl.hash dup);
  }),
  it (sprintf "intersect with %i elements" count) (fun () => {
    let setA = IntRange.create 0 count |> IntRange.toIterator |> SetImpl.from;
    let setB = Sequence.generate (fun i => i + 2) 0
      |> Sequence.take (count / 2)
      |> Sequence.toIterator
      |> SetImpl.from;

    let intersection = SetImpl.intersect setA setB;
    let expected = Sequence.generate (fun i => i + 2) 0
      |> Sequence.take (count / 2)
      |> Sequence.toIterator
      |> SetImpl.from;

    expect (SetImpl.equals intersection expected) |> toBeEqualToTrue;
  }),
  it (sprintf "none with %i elements" count) (fun () => {
    IntRange.create 0 count
      |> IntRange.toIterator
      |> SetImpl.from
      |> SetImpl.none (fun v => v > 0)
      |> expect |> toBeEqualToFalse;

    IntRange.create 0 count
      |> IntRange.toIterator
      |> SetImpl.from
      |> SetImpl.none (fun v => v > (count / 2))
      |> expect |> toBeEqualToFalse;

    IntRange.create 0 count
      |> IntRange.toIterator
      |> SetImpl.from
      |> SetImpl.none (fun v => v > (count - 2))
      |> expect |> toBeEqualToFalse;

    IntRange.create 0 count
      |> IntRange.toIterator
      |> SetImpl.from
      |> SetImpl.none (fun v => v < 0)
      |> expect |> toBeEqualToTrue;
  }),
  it (sprintf "reduce with %i elements" count) (fun () => {
    /* FIXME: This test could be better by not using a single repeated value. */
    IntRange.create 0 count
      |> IntRange.toIterator
      |> SetImpl.from
      |> SetImpl.reduce (fun acc _ => acc + 1) 0
      |> expect
      |> toBeEqualToInt count;
  }),
  it (sprintf "remove %i elements" count) (fun () => {
    let src = IntRange.create 0 count |> IntRange.toIterator;
    let mapOfSizeN = src |> SetImpl.from;

    /* FIMXE: Maybe add Sequence.sample, Sequence.filteri */
    let removed = src
      |> Iterator.map (fun i => (i, i))
      |> Iterator.filter (fun (i, _) => i mod 3 == 0)
      |> Iterator.map (fun (_, v) => v);

    let remaining = src
      |> Iterator.map (fun i => (i, i))
      |> Iterator.filter (fun (i, _) => i mod 3 != 0)
      |> Iterator.map (fun (_, v) => v);

    let mapOfSizeNdiv3 = removed |> Iterator.reduce (fun acc i => acc |> SetImpl.remove i) mapOfSizeN;

    removed |> Iterator.forEach(fun i =>
      expect (mapOfSizeNdiv3 |> SetImpl.contains i) |> toBeEqualToFalse
    );

    remaining |> Iterator.forEach(fun i =>
      expect (mapOfSizeNdiv3 |> SetImpl.contains i) |> toBeEqualToTrue
    );
  }),
  it (sprintf "some with %i elements" count) (fun () => {
    IntRange.create 0 count
      |> IntRange.toIterator
      |> SetImpl.from
      |> SetImpl.some (fun v => v > 0)
      |> expect |> toBeEqualToTrue;

    IntRange.create 0 count
      |> IntRange.toIterator
      |> SetImpl.from
      |> SetImpl.some (fun v => v > (count - 2))
      |> expect |> toBeEqualToTrue;

    IntRange.create 0 count
      |> IntRange.toIterator
      |> SetImpl.from
      |> SetImpl.some (fun v => v > (count / 2))
      |> expect |> toBeEqualToTrue;

    IntRange.create 0 count
      |> IntRange.toIterator
      |> SetImpl.from
      |> SetImpl.some (fun v => v < 0)
      |> expect |> toBeEqualToFalse;

  }),
  it (sprintf "subtract with %i elements" count) (fun () => {
    let setA = IntRange.create 0 count
      |> IntRange.toIterator
      |> SetImpl.from;
    let setB =  Sequence.generate (fun i => i + 2) 0
      |> Sequence.take (count / 2)
      |> Sequence.toIterator
      |> SetImpl.from;

    let subtracted = SetImpl.subtract setA setB;
    let expected =  Sequence.generate (fun i => i + 2) 1
      |> Sequence.take (count / 2)
      |> Sequence.toIterator
      |> SetImpl.from;

    expect (SetImpl.equals subtracted expected) |> toBeEqualToTrue;
  }),
  describe "toSet" [
    it (sprintf "contains with %i elements" count) (fun () => {
      let set = Sequence.generate (fun i => i + 3) 0
        |> Sequence.take count
        |> Sequence.toIterator
        |> SetImpl.from
        |> SetImpl.toSet;

      Sequence.generate (fun i => i + 3) 0 |> Sequence.take count |> Sequence.forEach (fun i => {
        expect (set |> Set.contains i) |> toBeEqualToTrue;
      });
    }),
    it (sprintf "count with %i elements" count) (fun () => {
      let set = Sequence.generate (fun i => i + 3) 0
        |> Sequence.take count
        |> Sequence.toIterator
        |> SetImpl.from
        |> SetImpl.toSet;

      expect (Set.count set) |> toBeEqualToInt count;
    }),
    it (sprintf "every with %i elements" count) (fun () => {
      IntRange.create 0 count
        |> IntRange.toIterator
        |> SetImpl.from
        |> SetImpl.toSet
        |> Set.every (fun v => v > (count / 2))
        |> expect |> toBeEqualToFalse;

      IntRange.create 0 count
        |> IntRange.toIterator
        |> SetImpl.from
        |> SetImpl.toSet
        |> Set.every (fun v => v > 0)
        |> expect |> toBeEqualToFalse;

      IntRange.create 0 count
        |> IntRange.toIterator
        |> SetImpl.from
        |> SetImpl.toSet
        |> Set.every (fun v => v > (count - 2))
        |> expect |> toBeEqualToFalse;

      IntRange.create 0 count
        |> IntRange.toIterator
        |> SetImpl.from
        |> SetImpl.toSet
        |> Set.every (fun v => v < count)
        |> expect |> toBeEqualToTrue;
    }),
    it (sprintf "find and tryFind with %i elements" count) (fun () => {
      let set = IntRange.create 0 count
        |> IntRange.toIterator
        |> SetImpl.from
        |> SetImpl.toSet;

      let find0 i => i == 0;
      let findCountMinusOne i => i == (count - 1);
      let findCountDividedByTwo i => i == (count / 2);

      expect (Set.find find0 set) |> toBeEqualToInt 0;
      expect (Set.find findCountMinusOne set) |> toBeEqualToInt (count - 1);
      expect (Set.find findCountDividedByTwo set) |> toBeEqualToInt (count / 2);

      expect (Set.tryFind find0 set) |> toBeEqualToSomeOfInt 0;
      expect (Set.tryFind findCountMinusOne set) |> toBeEqualToSomeOfInt (count - 1);
      expect (Set.tryFind findCountDividedByTwo set) |> toBeEqualToSomeOfInt (count / 2);

      defer (fun () => Set.find Functions.alwaysFalse set) |> throws;
      expect (Set.tryFind Functions.alwaysFalse set) |> toBeEqualToNoneOfInt;
    }),
    it (sprintf "forEach and tryFind with %i elements" count) (fun () => {
      let set = IntRange.create 0 count
        |> IntRange.toIteratorReversed
        |> SetImpl.from
        |> SetImpl.toSet;
      set |> Set.forEach (fun i => {
        expect (set |> Set.contains i) |> toBeEqualToTrue;
      });
    }),
    it (sprintf "none with %i elements" count) (fun () => {
      IntRange.create 0 count
        |> IntRange.toIterator
        |> SetImpl.from
        |> SetImpl.toSet
        |> Set.none (fun v => v > 0)
        |> expect |> toBeEqualToFalse;

      IntRange.create 0 count
        |> IntRange.toIterator
        |> SetImpl.from
        |> SetImpl.toSet
        |> Set.none (fun v => v > (count / 2))
        |> expect |> toBeEqualToFalse;

      IntRange.create 0 count
        |> IntRange.toIterator
        |> SetImpl.from
        |> SetImpl.toSet
        |> Set.none (fun v => v > (count - 2))
        |> expect |> toBeEqualToFalse;

      IntRange.create 0 count
        |> IntRange.toIterator
        |> SetImpl.from
        |> SetImpl.toSet
        |> Set.none (fun v => v < 0)
        |> expect |> toBeEqualToTrue;
    }),
    it (sprintf "reduce with %i elements" count) (fun () => {
      IntRange.create 0 count
        |> IntRange.toIterator
        |> SetImpl.from
        |> SetImpl.toSet
        |> Set.reduce (fun acc _ => acc + 1) 0
        |> expect
        |> toBeEqualToInt count;
    }),
    it (sprintf "some with %i elements" count) (fun () => {
      IntRange.create 0 count
        |> IntRange.toIterator
        |> SetImpl.from
        |> SetImpl.toSet
        |> Set.some (fun v => v > 0)
        |> expect |> toBeEqualToTrue;

      IntRange.create 0 count
        |> IntRange.toIterator
        |> SetImpl.from
        |> SetImpl.toSet
        |> Set.some (fun v => v > (count - 2))
        |> expect |> toBeEqualToTrue;

      IntRange.create 0 count
        |> IntRange.toIterator
        |> SetImpl.from
        |> SetImpl.toSet
        |> Set.some (fun v => v > (count / 2))
        |> expect |> toBeEqualToTrue;

      IntRange.create 0 count
        |> IntRange.toIterator
        |> SetImpl.from
        |> SetImpl.toSet
        |> Set.some (fun v => v < 0)
        |> expect |> toBeEqualToFalse;
    }),
    describe "toSequence" [

    ],
  ],
  describe "toMap" [
    it (sprintf "containsWith with %i elements" count) (fun () => {
      let map = IntRange.create 0 count
        |> IntRange.toIterator
        |> SetImpl.from
        |> SetImpl.toMap;

      IntRange.create 0 count |> IntRange.forEach (fun i => {
        expect (map |> Map.contains i i) |> toBeEqualToTrue;
      });

      expect (map |> Map.contains (-1) (-1)) |> toBeEqualToFalse;
      expect (map |> Map.contains count count) |> toBeEqualToFalse;
      expect (map |> Map.contains 0 1) |> toBeEqualToFalse;
    }),
    it (sprintf "containsKey with %i elements" count) (fun () => {
      let map = IntRange.create 0 count
        |> IntRange.toIterator
        |> SetImpl.from
        |> SetImpl.toMap;
      IntRange.create 0 count |> IntRange.forEach (fun i => {
        expect (map |> Map.containsKey i) |> toBeEqualToTrue;
      });

      expect (map |> Map.containsKey (-1)) |> toBeEqualToFalse;
      expect (map |> Map.containsKey count) |> toBeEqualToFalse;
    }),
    it (sprintf "count with %i elements" count) (fun () => {
      let map = IntRange.create 0 count
        |> IntRange.toIterator
        |> SetImpl.from
        |> SetImpl.toMap;
      expect (map |> Map.count) |> toBeEqualToInt count;
    }),
    it (sprintf "every with %i elements" count) (fun () => {
      let map = IntRange.create 0 count
        |> IntRange.toIterator
        |> SetImpl.from
        |> SetImpl.toMap;
      expect (map |> Map.every (fun i v => i == v)) |> toBeEqualToTrue;
      expect (map |> Map.every (fun i v => i != v)) |> toBeEqualToFalse;
    }),
    it (sprintf "find with %i elements" count) (fun () => {
      let map = IntRange.create 0 count
        |> IntRange.toIterator
        |> SetImpl.from
        |> SetImpl.toMap;
      expect (map |> Map.find (fun i v =>
        (i == (count - 1)) && (v == (count - 1))
      )) |> toBeEqualTo intTupleToString (count - 1, count - 1);
      defer (fun () => map |> Map.find (fun i v => i != v)) |> throws;
    }),
    it (sprintf "forEach with %i elements" count) (fun () => {
      let map = IntRange.create 0 count
        |> IntRange.toIterator
        |> SetImpl.from
        |> SetImpl.toMap;
      let loopCount = ref 0;
      map |> Map.forEach (fun i v => {
        expect i |> toBeEqualToInt v;
        loopCount := !loopCount + 1;
      });

      expect !loopCount |> toBeEqualToInt count;
    }),
    it (sprintf "get with %i elements" count) (fun () => {
      let map = IntRange.create 0 count
        |> IntRange.toIterator
        |> SetImpl.from
        |> SetImpl.toMap;
      IntRange.create 0 count |> IntRange.forEach (fun i => {
        expect (map |> Map.get i) |> toBeEqualToInt i;
      });

      defer (fun () => map |> Map.get (-1)) |> throws;
      defer (fun () => map |> Map.get count) |> throws;
    }),
    it (sprintf "keys with %i elements" count) (fun () => {
      let map = IntRange.create 0 count
        |> IntRange.toIterator
        |> SetImpl.from
        |> SetImpl.toMap;
      map
        |> Map.keys
        |> Set.equals (IntRange.create 0 count |> IntRange.toSet)
        |> expect
        |> toBeEqualToTrue;
    }),
    it (sprintf "none with %i elements" count) (fun () => {
      let map = IntRange.create 0 count
        |> IntRange.toIterator
        |> SetImpl.from
        |> SetImpl.toMap;
      expect (map |> Map.none (fun i v => i != v)) |> toBeEqualToTrue;
      expect (map |> Map.none (fun i v => i == v)) |> toBeEqualToFalse;
    }),
    it (sprintf "reduce with %i elements" count) (fun () => {
      let map = IntRange.create 0 count
        |> IntRange.toIterator
        |> SetImpl.from
        |> SetImpl.toMap;
      let reduced = map |> Map.reduce (fun acc _ _ => acc + 1) 0;
      expect reduced |> toBeEqualToInt count;
    }),
    it (sprintf "some with %i elements" count) (fun () => {
      let map = IntRange.create 0 count
        |> IntRange.toIterator
        |> SetImpl.from
        |> SetImpl.toMap;
      expect (map |> Map.some (fun i v => i == v)) |> toBeEqualToTrue;
      expect (map |> Map.some (fun i v => i != v)) |> toBeEqualToFalse;
    }),
    describe "toSequence" [

    ],
    it (sprintf "tryFind with %i elements" count) (fun () => {
      let map = IntRange.create 0 count
        |> IntRange.toIterator
        |> SetImpl.from
        |> SetImpl.toMap;
      expect (map |> Map.tryFind (fun i v =>
        (i == (count - 1)) && (v == (count - 1))
      )) |> toBeEqualToSome intTupleToString (count - 1, count - 1);
      expect (map |> Map.tryFind (fun i v => i != v)) |> toBeEqualToNone intTupleToString;
    }),
    it (sprintf "tryGet with %i elements" count) (fun () => {
      let map = IntRange.create 0 count
        |> IntRange.toIterator
        |> SetImpl.from
        |> SetImpl.toMap;
      IntRange.create 0 count |> IntRange.forEach (fun i => {
        expect (map |> Map.tryGet i) |> toBeEqualToSomeOfInt i;
      });

      expect (map |> Map.tryGet (-1)) |> toBeEqualToNoneOfInt;
      expect (map |> Map.tryGet count) |> toBeEqualToNoneOfInt;
    }),
    it (sprintf "values with %i elements" count) (fun () => {
      let map = IntRange.create 0 count
        |> IntRange.toIterator
        |> SetImpl.from
        |> SetImpl.toMap;
      map
        |> Map.values
        |> SetImpl.from
        |> SetImpl.toSet
        |> Set.equals (IntRange.create 0 count |> IntRange.toSet)
        |> expect
        |> toBeEqualToTrue;
    }),
  ],
  describe "toSequence" [

  ],
  it (sprintf "union with %i elements" count) (fun () => {
    let setA = Sequence.generate (fun i => i + 2) 1
      |> Sequence.take (count / 2)
      |> Sequence.toIterator
      |> SetImpl.from;

    let setB = Sequence.generate (fun i => i + 2) 0
      |> Sequence.take (count / 2)
      |> Sequence.toIterator
      |> SetImpl.from;

    let union = SetImpl.union setA setB;
    let expected = IntRange.create 0 count
    |> IntRange.toIterator
    |> SetImpl.from;

    expect (SetImpl.equals union expected) |> toBeEqualToTrue;
  }),
];
