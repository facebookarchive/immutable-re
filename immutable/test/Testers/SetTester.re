open Immutable;
open Printf;
open ReUnit;
open ReUnit.Expect;
open ReUnit.Test;

module type SetImpl = {
  type t;

  let add: int => t => t;
  let addAll: (Seq.t int) => t => t;
  let contains: int => t => bool;
  let count: t => int;
  let empty: unit => t;
  let equals: t => t => bool;
  let every: (int => bool) => t => bool;
  let find: (int => bool) => t => int;
  let forEach: (int => unit) => t => unit;
  let fromSeq: (Seq.t int) => t;
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
  let toSeq: t => (Seq.t int);
  let tryFind: (int => bool) => t => (option int);
  let union: t => t => t;
};

let intTupleToString (i, v) =>
  "(" ^ (string_of_int i) ^ "," ^ (string_of_int v) ^ ")";

let test (count: int) (module SetImpl: SetImpl): (list Test.t) => [
  it (sprintf "add %i elements" count) (fun () => {
    let src = Seq.inRange 0 (Some count) 1 |> Seq.map (Hash.random ());

    let (_, mapOfSizeN) = src
      |> Seq.scan
        (fun (_, acc) i => (i, acc |> SetImpl.add i))
        (0, SetImpl.empty ())
      |> Seq.doOnNext(fun (i, acc) =>
        expect (acc |> SetImpl.contains i) |> toBeEqualToTrue
      )
      |> Seq.last;

    src |> Seq.forEach (fun i =>
      expect (mapOfSizeN |> SetImpl.contains i) |> toBeEqualToTrue
    );
  }),
  it (sprintf "removeAll %i elements" count) (fun () => {
    let set = Seq.inRange 0 (Some count) 1 |> SetImpl.fromSeq;

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

    let set = empty |> SetImpl.addAll (Seq.inRange 0 (Some count) 1);

    let shouldBeEmpty = Seq.inRange (count - 1) (Some count) (-1) |> Seq.reduce (fun acc i => {
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
    let values = Seq.inRange (-count) (Some count) 5;
    let set = SetImpl.fromSeq values;
    values |> Seq.forEach (fun i => {
      expect (SetImpl.contains i set) |> toBeEqualToTrue;
    });
  }),
  it (sprintf "count with %i elements" count) (fun () => {
    let set = Seq.inRange (-count) (Some count) 5 |> SetImpl.fromSeq;
    expect (SetImpl.count set) |> toBeEqualToInt count;
  }),
  it (sprintf "equals %i elements" count) (fun () => {
    let setCountMinusOne = Seq.inRange 0 (Some (count - 1)) 1 |> SetImpl.fromSeq;
    let setCount = Seq.inRange 0 (Some count) 1 |> SetImpl.fromSeq;
    let setCountDup = Seq.inRange 0 (Some count) 1 |> SetImpl.fromSeq;
    let setCountPlusOne = Seq.inRange 0 (Some (count + 1)) 1 |> SetImpl.fromSeq;

    expect (SetImpl.equals setCount setCount) |> toBeEqualToTrue;
    expect (SetImpl.equals setCount setCountDup) |> toBeEqualToTrue;
    expect (SetImpl.equals setCount setCountMinusOne) |> toBeEqualToFalse;
    expect (SetImpl.equals setCount setCountPlusOne) |> toBeEqualToFalse;
  }),
  it (sprintf "every with %i elements" count) (fun () => {
    Seq.inRange 0 (Some count) 1
      |> SetImpl.fromSeq
      |> SetImpl.every (fun v => v > (count / 2))
      |> expect |> toBeEqualToFalse;

    Seq.inRange 0 (Some count) 1
      |> SetImpl.fromSeq
      |> SetImpl.every (fun v => v > 0)
      |> expect |> toBeEqualToFalse;

    Seq.inRange 0 (Some count) 1
      |> SetImpl.fromSeq
      |> SetImpl.every (fun v => v > (count - 2))
      |> expect |> toBeEqualToFalse;

    Seq.inRange 0 (Some count) 1
      |> SetImpl.fromSeq
      |> SetImpl.every (fun v => v < count)
      |> expect |> toBeEqualToTrue;
  }),
  it (sprintf "find and tryFind in %i elements" count) (fun () => {
    let set = Seq.inRange 0 (Some count) 1 |> SetImpl.fromSeq;

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
    let seq = Seq.inRange (count - 1) (Some count) (-1);
    let set = SetImpl.fromSeq seq;
    set |> SetImpl.forEach (fun i => {
      expect (set |> SetImpl.contains i) |> toBeEqualToTrue;
    });
  }),
  it (sprintf "hash with %i elements" count) (fun () => {
    let col = Seq.inRange 0 (Some count) 1 |> SetImpl.fromSeq;
    let dup = Seq.inRange (count - 1) (Some count) (-1) |> SetImpl.fromSeq;

    expect (SetImpl.hash col) |> toBeEqualToInt (SetImpl.hash col);
    expect (SetImpl.hash col) |> toBeEqualToInt (SetImpl.hash dup);
  }),
  it (sprintf "intersect with %i elements" count) (fun () => {
    let setA = Seq.inRange 0 (Some count) 1 |> SetImpl.fromSeq;
    let setB = Seq.inRange 0 (Some (count / 2)) 2 |> SetImpl.fromSeq;

    let intersection = SetImpl.intersect setA setB;
    let expected = Seq.inRange 0 (Some (count / 2)) 2 |> SetImpl.fromSeq;
    expect (SetImpl.equals intersection expected) |> toBeEqualToTrue;
  }),
  it (sprintf "none with %i elements" count) (fun () => {
    Seq.inRange 0 (Some count) 1
      |> SetImpl.fromSeq
      |> SetImpl.none (fun v => v > 0)
      |> expect |> toBeEqualToFalse;

    Seq.inRange 0 (Some count) 1
      |> SetImpl.fromSeq
      |> SetImpl.none (fun v => v > (count / 2))
      |> expect |> toBeEqualToFalse;

    Seq.inRange 0 (Some count) 1
      |> SetImpl.fromSeq
      |> SetImpl.none (fun v => v > (count - 2))
      |> expect |> toBeEqualToFalse;

    Seq.inRange 0 (Some count) 1
      |> SetImpl.fromSeq
      |> SetImpl.none (fun v => v < 0)
      |> expect |> toBeEqualToTrue;
  }),
  it (sprintf "reduce with %i elements" count) (fun () => {
    /* FIXME: This test could be better by not using a single repeated value. */
    Seq.inRange 0 (Some count) 1
      |> SetImpl.fromSeq
      |> SetImpl.reduce (fun acc _ => acc + 1) 0
      |> expect
      |> toBeEqualToInt count;
  }),
  it (sprintf "remove %i elements" count) (fun () => {
    let src = Seq.inRange 0 (Some count) 1;
    let mapOfSizeN = src |> SetImpl.fromSeq;

    /* FIMXE: Maybe add Seq.sample, Seq.filteri */
    let removed = src
      |> Seq.map (fun i => (i, i))
      |> Seq.filter (fun (i, _) => i mod 3 == 0)
      |> Seq.map (fun (_, v) => v);

    let remaining = src
      |> Seq.map (fun i => (i, i))
      |> Seq.filter (fun (i, _) => i mod 3 != 0)
      |> Seq.map (fun (_, v) => v);

    let mapOfSizeNdiv3 = removed |> Seq.reduce (fun acc i => acc |> SetImpl.remove i) mapOfSizeN;

    removed |> Seq.forEach(fun i =>
      expect (mapOfSizeNdiv3 |> SetImpl.contains i) |> toBeEqualToFalse
    );

    remaining |> Seq.forEach(fun i =>
      expect (mapOfSizeNdiv3 |> SetImpl.contains i) |> toBeEqualToTrue
    );
  }),
  it (sprintf "some with %i elements" count) (fun () => {
    Seq.inRange 0 (Some count) 1
      |> SetImpl.fromSeq
      |> SetImpl.some (fun v => v > 0)
      |> expect |> toBeEqualToTrue;

    Seq.inRange 0 (Some count) 1
      |> SetImpl.fromSeq
      |> SetImpl.some (fun v => v > (count - 2))
      |> expect |> toBeEqualToTrue;

    Seq.inRange 0 (Some count) 1
      |> SetImpl.fromSeq
      |> SetImpl.some (fun v => v > (count / 2))
      |> expect |> toBeEqualToTrue;

    Seq.inRange 0 (Some count) 1
      |> SetImpl.fromSeq
      |> SetImpl.some (fun v => v < 0)
      |> expect |> toBeEqualToFalse;

  }),
  it (sprintf "subtract with %i elements" count) (fun () => {
    let setA = Seq.inRange 0 (Some count) 1 |> SetImpl.fromSeq;
    let setB = Seq.inRange 0 (Some (count / 2)) 2 |> SetImpl.fromSeq;

    let subtracted = SetImpl.subtract setA setB;
    let expected = Seq.inRange 1 (Some (count / 2)) 2 |> SetImpl.fromSeq;

    expect (SetImpl.equals subtracted expected) |> toBeEqualToTrue;
  }),
  describe "toSet" [
    it (sprintf "contains with %i elements" count) (fun () => {
      let set = Seq.inRange 0 (Some count) 3 |> SetImpl.fromSeq |> SetImpl.toSet;

      Set.inRange 0 count 3 |> Set.forEach (fun i => {
        expect (set |> Set.contains i) |> toBeEqualToTrue;
      });
    }),
    it (sprintf "count with %i elements" count) (fun () => {
      let set = Seq.inRange 0 (Some count) 3 |> SetImpl.fromSeq |> SetImpl.toSet;
      expect (Set.count set) |> toBeEqualToInt count;
    }),
    it (sprintf "every with %i elements" count) (fun () => {
      Seq.inRange 0 (Some count) 1
        |> SetImpl.fromSeq
        |> SetImpl.toSet
        |> Set.every (fun v => v > (count / 2))
        |> expect |> toBeEqualToFalse;

      Seq.inRange 0 (Some count) 1
        |> SetImpl.fromSeq
        |> SetImpl.toSet
        |> Set.every (fun v => v > 0)
        |> expect |> toBeEqualToFalse;

      Seq.inRange 0 (Some count) 1
        |> SetImpl.fromSeq
        |> SetImpl.toSet
        |> Set.every (fun v => v > (count - 2))
        |> expect |> toBeEqualToFalse;

      Seq.inRange 0 (Some count) 1
        |> SetImpl.fromSeq
        |> SetImpl.toSet
        |> Set.every (fun v => v < count)
        |> expect |> toBeEqualToTrue;
    }),
    it (sprintf "find and tryFind with %i elements" count) (fun () => {
      let set = Seq.inRange 0 (Some count) 1 |> SetImpl.fromSeq |> SetImpl.toSet;

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
      let set = Seq.inRange (count - 1) (Some count) (-1) |> SetImpl.fromSeq |> SetImpl.toSet;
      set |> Set.forEach (fun i => {
        expect (set |> Set.contains i) |> toBeEqualToTrue;
      });
    }),
    it (sprintf "none with %i elements" count) (fun () => {
      Seq.inRange 0 (Some count) 1
        |> SetImpl.fromSeq
        |> SetImpl.toSet
        |> Set.none (fun v => v > 0)
        |> expect |> toBeEqualToFalse;

      Seq.inRange 0 (Some count) 1
        |> SetImpl.fromSeq
        |> SetImpl.toSet
        |> Set.none (fun v => v > (count / 2))
        |> expect |> toBeEqualToFalse;

      Seq.inRange 0 (Some count) 1
        |> SetImpl.fromSeq
        |> SetImpl.toSet
        |> Set.none (fun v => v > (count - 2))
        |> expect |> toBeEqualToFalse;

      Seq.inRange 0 (Some count) 1
        |> SetImpl.fromSeq
        |> SetImpl.toSet
        |> Set.none (fun v => v < 0)
        |> expect |> toBeEqualToTrue;
    }),
    it (sprintf "reduce with %i elements" count) (fun () => {
      Seq.inRange 0 (Some count) 1
        |> SetImpl.fromSeq
        |> SetImpl.toSet
        |> Set.reduce (fun acc _ => acc + 1) 0
        |> expect
        |> toBeEqualToInt count;
    }),
    it (sprintf "some with %i elements" count) (fun () => {
      Seq.inRange 0 (Some count) 1
        |> SetImpl.fromSeq
        |> SetImpl.toSet
        |> Set.some (fun v => v > 0)
        |> expect |> toBeEqualToTrue;

      Seq.inRange 0 (Some count) 1
        |> SetImpl.fromSeq
        |> SetImpl.toSet
        |> Set.some (fun v => v > (count - 2))
        |> expect |> toBeEqualToTrue;

      Seq.inRange 0 (Some count) 1
        |> SetImpl.fromSeq
        |> SetImpl.toSet
        |> Set.some (fun v => v > (count / 2))
        |> expect |> toBeEqualToTrue;

      Seq.inRange 0 (Some count) 1
        |> SetImpl.fromSeq
        |> SetImpl.toSet
        |> Set.some (fun v => v < 0)
        |> expect |> toBeEqualToFalse;
    }),
    describe "toSeq" [

    ],
  ],
  describe "toMap" [
    it (sprintf "containsWith with %i elements" count) (fun () => {
      let map = Seq.inRange 0 (Some count) 1 |> SetImpl.fromSeq |> SetImpl.toMap;
      Seq.inRange 0 (Some count) 1 |> Seq.forEach (fun i => {
        expect (map |> Map.contains i i) |> toBeEqualToTrue;
      });

      expect (map |> Map.contains (-1) (-1)) |> toBeEqualToFalse;
      expect (map |> Map.contains count count) |> toBeEqualToFalse;
      expect (map |> Map.contains 0 1) |> toBeEqualToFalse;
    }),
    it (sprintf "containsKey with %i elements" count) (fun () => {
      let map = Seq.inRange 0 (Some count) 1 |> SetImpl.fromSeq |> SetImpl.toMap;
      Seq.inRange 0 (Some count) 1 |> Seq.forEach (fun i => {
        expect (map |> Map.containsKey i) |> toBeEqualToTrue;
      });

      expect (map |> Map.containsKey (-1)) |> toBeEqualToFalse;
      expect (map |> Map.containsKey count) |> toBeEqualToFalse;
    }),
    it (sprintf "count with %i elements" count) (fun () => {
      let map = Seq.inRange 0 (Some count) 1 |> SetImpl.fromSeq |> SetImpl.toMap;
      expect (map |> Map.count) |> toBeEqualToInt count;
    }),
    it (sprintf "every with %i elements" count) (fun () => {
      let map = Seq.inRange 0 (Some count) 1 |> SetImpl.fromSeq |> SetImpl.toMap;
      expect (map |> Map.every (fun i v => i == v)) |> toBeEqualToTrue;
      expect (map |> Map.every (fun i v => i != v)) |> toBeEqualToFalse;
    }),
    it (sprintf "find with %i elements" count) (fun () => {
      let map = Seq.inRange 0 (Some count) 1 |> SetImpl.fromSeq |> SetImpl.toMap;
      expect (map |> Map.find (fun i v =>
        (i == (count - 1)) && (v == (count - 1))
      )) |> toBeEqualTo intTupleToString (count - 1, count - 1);
      defer (fun () => map |> Map.find (fun i v => i != v)) |> throws;
    }),
    it (sprintf "forEach with %i elements" count) (fun () => {
      let map = Seq.inRange 0 (Some count) 1 |> SetImpl.fromSeq |> SetImpl.toMap;
      let loopCount = ref 0;
      map |> Map.forEach (fun i v => {
        expect i |> toBeEqualToInt v;
        loopCount := !loopCount + 1;
      });

      expect !loopCount |> toBeEqualToInt count;
    }),
    it (sprintf "get with %i elements" count) (fun () => {
      let map = Seq.inRange 0 (Some count) 1 |> SetImpl.fromSeq |> SetImpl.toMap;
      Seq.inRange 0 (Some count) 1 |> Seq.forEach (fun i => {
        expect (map |> Map.get i) |> toBeEqualToInt i;
      });

      defer (fun () => map |> Map.get (-1)) |> throws;
      defer (fun () => map |> Map.get count) |> throws;
    }),
    it (sprintf "keys with %i elements" count) (fun () => {
      let map = Seq.inRange 0 (Some count) 1 |> SetImpl.fromSeq |> SetImpl.toMap;
      map
        |> Map.keys
        |> Set.equals (Set.inRange 0 count 1)
        |> expect
        |> toBeEqualToTrue;
    }),
    it (sprintf "none with %i elements" count) (fun () => {
      let map = Seq.inRange 0 (Some count) 1 |> SetImpl.fromSeq |> SetImpl.toMap;
      expect (map |> Map.none (fun i v => i != v)) |> toBeEqualToTrue;
      expect (map |> Map.none (fun i v => i == v)) |> toBeEqualToFalse;
    }),
    it (sprintf "reduce with %i elements" count) (fun () => {
      let map = Seq.inRange 0 (Some count) 1 |> SetImpl.fromSeq |> SetImpl.toMap;
      let reduced = map |> Map.reduce (fun acc _ _ => acc + 1) 0;
      expect reduced |> toBeEqualToInt count;
    }),
    it (sprintf "some with %i elements" count) (fun () => {
      let map = Seq.inRange 0 (Some count) 1 |> SetImpl.fromSeq |> SetImpl.toMap;
      expect (map |> Map.some (fun i v => i == v)) |> toBeEqualToTrue;
      expect (map |> Map.some (fun i v => i != v)) |> toBeEqualToFalse;
    }),
    describe "toSeq" [

    ],
    it (sprintf "tryFind with %i elements" count) (fun () => {
      let map = Seq.inRange 0 (Some count) 1 |> SetImpl.fromSeq |> SetImpl.toMap;
      expect (map |> Map.tryFind (fun i v =>
        (i == (count - 1)) && (v == (count - 1))
      )) |> toBeEqualToSome intTupleToString (count - 1, count - 1);
      expect (map |> Map.tryFind (fun i v => i != v)) |> toBeEqualToNone intTupleToString;
    }),
    it (sprintf "tryGet with %i elements" count) (fun () => {
      let map = Seq.inRange 0 (Some count) 1 |> SetImpl.fromSeq |> SetImpl.toMap;
      Seq.inRange 0 (Some count) 1 |> Seq.forEach (fun i => {
        expect (map |> Map.tryGet i) |> toBeEqualToSomeOfInt i;
      });

      expect (map |> Map.tryGet (-1)) |> toBeEqualToNoneOfInt;
      expect (map |> Map.tryGet count) |> toBeEqualToNoneOfInt;
    }),
    it (sprintf "values with %i elements" count) (fun () => {
      let map = Seq.inRange 0 (Some count) 1 |> SetImpl.fromSeq |> SetImpl.toMap;
      map
        |> Map.values
        |> SetImpl.fromSeq
        |> SetImpl.toSet
        |> Set.equals (Set.inRange 0 count 1)
        |> expect
        |> toBeEqualToTrue;
    }),
  ],
  describe "toSeq" [

  ],
  it (sprintf "union with %i elements" count) (fun () => {
    let setA = Seq.inRange 1 (Some (count / 2)) 2 |> SetImpl.fromSeq;
    let setB = Seq.inRange 0 (Some (count / 2)) 2 |> SetImpl.fromSeq;

    let union = SetImpl.union setA setB;
    let expected = Seq.inRange 0 (Some count) 1 |> SetImpl.fromSeq;

    expect (SetImpl.equals union expected) |> toBeEqualToTrue;
  }),
];
