open Immutable;
open Printf;
open ReUnit;
open ReUnit.Expect;
open ReUnit.Test;

module type Set = {
  type t 'a;

  let add: 'a => (t 'a) => (t 'a);
  let addAll: (Seq.t 'a) => (t 'a) => (t 'a);
  let contains: 'a => (t 'a) => bool;
  let count: (t 'a) => int;
  let empty: unit => t 'a;
  let equals: (t 'a) => (t 'a) => bool;
  let every: ('a => bool) => (t 'a) => bool;
  let find: ('a => bool) => (t 'a) => 'a;
  let forEach: ('a => unit) => (t 'a) => unit;
  let fromSeq: (Seq.t 'a) => (t 'a);
  let hash: (Hash.t (t 'a));
  let intersect: (t 'a) => (t 'a) => (t 'a);
  let isEmpty: t 'a => bool;
  let isNotEmpty: t 'a => bool;
  let none: ('a => bool) => (t 'a) => bool;
  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let remove: 'a => (t 'a) => (t 'a);
  let removeAll: (t 'a) => (t 'a);
  let some: ('a => bool) => (t 'a) => bool;
  let subtract: (t 'a) => (t 'a) => (t 'a);
  let toCollection: (t 'a) => (Collection.t 'a);
  let toKeyed: (t 'a) => (Keyed.t 'a 'a);
  let toSeq: (t 'a) => (Seq.t 'a);
  let tryFind: ('a => bool) => (t 'a) => (option 'a);
  let union: (t 'a) => (t 'a) => (t 'a);
};

let intTupleToString (i, v) =>
  "(" ^ (string_of_int i) ^ "," ^ (string_of_int v) ^ ")";

let test (count: int) (module Set: Set): (list Test.t) => [
  it (sprintf "add %i elements" count) (fun () => {
    let src = Seq.inRange 0 (Some count) 1 |> Seq.map (Hash.random ());

    let (_, mapOfSizeN) = src
      |> Seq.scan
        (fun (hash, acc) i => (i, acc |> Set.add i))
        (0, Set.empty ())
      |> Seq.doOnNext(fun (i, acc) =>
        expect (acc |> Set.contains i) |> toBeEqualToTrue
      )
      |> Seq.last;

    src |> Seq.forEach (fun i =>
      expect (mapOfSizeN |> Set.contains i) |> toBeEqualToTrue
    );
  }),
  it (sprintf "removeAll %i elements" count) (fun () => {
    let set = Seq.inRange 0 (Some count) 1 |> Set.fromSeq;

    expect (Set.isNotEmpty set) |> toBeEqualToTrue;
    expect (Set.isEmpty set) |> toBeEqualToFalse;
    expect (Set.count set) |> toBeEqualToInt count;

    let shouldBeEmpty = Set.removeAll set;
    expect (Set.isNotEmpty shouldBeEmpty) |> toBeEqualToFalse;
    expect (Set.isEmpty shouldBeEmpty) |> toBeEqualToTrue;
    expect (Set.count shouldBeEmpty) |> toBeEqualToInt 0;
  }),
  it (sprintf "remove %i elements" count) (fun () => {
    let empty = Set.empty ();

    expect (Set.isNotEmpty empty) |> toBeEqualToFalse;
    expect (Set.isEmpty empty) |> toBeEqualToTrue;
    expect (Set.count empty) |> toBeEqualToInt 0;

    let set = empty |> Set.addAll (Seq.inRange 0 (Some count) 1);

    let shouldBeEmpty = Seq.inRange (count - 1) (Some count) (-1) |> Seq.reduce (fun acc i => {
      expect (Set.isNotEmpty acc) |> toBeEqualToTrue;
      expect (Set.isEmpty acc) |> toBeEqualToFalse;
      expect (Set.count acc) |> toBeEqualToInt (i + 1);
      acc |> Set.remove i;
    }) set;

    expect (Set.isNotEmpty shouldBeEmpty) |> toBeEqualToFalse;
    expect (Set.isEmpty shouldBeEmpty) |> toBeEqualToTrue;
    expect (Set.count shouldBeEmpty) |> toBeEqualToInt 0;
  }),
  it (sprintf "contains with %i elements" count) (fun () => {
    let values = Seq.inRange (-count) (Some count) 5;
    let set = Set.fromSeq values;
    values |> Seq.forEach (fun i => {
      expect (Set.contains i set) |> toBeEqualToTrue;
    });
  }),
  it (sprintf "count with %i elements" count) (fun () => {
    let set = Seq.inRange (-count) (Some count) 5 |> Set.fromSeq;
    expect (Set.count set) |> toBeEqualToInt count;
  }),
  it (sprintf "equals %i elements" count) (fun () => {
    let setCountMinusOne = Seq.inRange 0 (Some (count - 1)) 1 |> Set.fromSeq;
    let setCount = Seq.inRange 0 (Some count) 1 |> Set.fromSeq;
    let setCountDup = Seq.inRange 0 (Some count) 1 |> Set.fromSeq;
    let setCountPlusOne = Seq.inRange 0 (Some (count + 1)) 1 |> Set.fromSeq;

    expect (Set.equals setCount setCount) |> toBeEqualToTrue;
    expect (Set.equals setCount setCountDup) |> toBeEqualToTrue;
    expect (Set.equals setCount setCountMinusOne) |> toBeEqualToFalse;
    expect (Set.equals setCount setCountPlusOne) |> toBeEqualToFalse;
  }),
  it (sprintf "every with %i elements" count) (fun () => {
    Seq.inRange 0 (Some count) 1
      |> Set.fromSeq
      |> Set.every (fun v => v > (count / 2))
      |> expect |> toBeEqualToFalse;

    Seq.inRange 0 (Some count) 1
      |> Set.fromSeq
      |> Set.every (fun v => v > 0)
      |> expect |> toBeEqualToFalse;

    Seq.inRange 0 (Some count) 1
      |> Set.fromSeq
      |> Set.every (fun v => v > (count - 2))
      |> expect |> toBeEqualToFalse;

    Seq.inRange 0 (Some count) 1
      |> Set.fromSeq
      |> Set.every (fun v => v < count)
      |> expect |> toBeEqualToTrue;
  }),
  it (sprintf "find and tryFind in %i elements" count) (fun () => {
    let set = Seq.inRange 0 (Some count) 1 |> Set.fromSeq;

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
  it (sprintf "forEach with %i elements" count) (fun () => {
    let seq = Seq.inRange (count - 1) (Some count) (-1);
    let set = Set.fromSeq seq;
    set |> Set.forEach (fun i => {
      expect (set |> Set.contains i) |> toBeEqualToTrue;
    });
  }),
  it (sprintf "hash with %i elements" count) (fun () => {
    let col = Seq.inRange 0 (Some count) 1 |> Set.fromSeq;
    let dup = Seq.inRange (count - 1) (Some count) (-1) |> Set.fromSeq;

    expect (Set.hash col) |> toBeEqualToInt (Set.hash col);
    expect (Set.hash col) |> toBeEqualToInt (Set.hash dup);
  }),
  it (sprintf "intersect with %i elements" count) (fun () => {
    let setA = Seq.inRange 0 (Some count) 1 |> Set.fromSeq;
    let setB = Seq.inRange 0 (Some (count / 2)) 2 |> Set.fromSeq;

    let intersection = Set.intersect setA setB;
    let expected = Seq.inRange 0 (Some (count / 2)) 2 |> Set.fromSeq;
    expect (Set.equals intersection expected) |> toBeEqualToTrue;
  }),
  it (sprintf "none with %i elements" count) (fun () => {
    Seq.inRange 0 (Some count) 1
      |> Set.fromSeq
      |> Set.none (fun v => v > 0)
      |> expect |> toBeEqualToFalse;

    Seq.inRange 0 (Some count) 1
      |> Set.fromSeq
      |> Set.none (fun v => v > (count / 2))
      |> expect |> toBeEqualToFalse;

    Seq.inRange 0 (Some count) 1
      |> Set.fromSeq
      |> Set.none (fun v => v > (count - 2))
      |> expect |> toBeEqualToFalse;

    Seq.inRange 0 (Some count) 1
      |> Set.fromSeq
      |> Set.none (fun v => v < 0)
      |> expect |> toBeEqualToTrue;
  }),
  it (sprintf "reduce with %i elements" count) (fun () => {
    /* FIXME: This test could be better by not using a single repeated value. */
    Seq.inRange 0 (Some count) 1
      |> Set.fromSeq
      |> Set.reduce (fun acc i => acc + 1) 0
      |> expect
      |> toBeEqualToInt count;
  }),
  it (sprintf "remove %i elements" count) (fun () => {
    let src = Seq.inRange 0 (Some count) 1;
    let mapOfSizeN = src |> Set.fromSeq;

    /* FIMXE: Maybe add Seq.sample, Seq.filteri */
    let removed = src
      |> Seq.map (fun i => (i, i))
      |> Seq.filter (fun (i, v) => i mod 3 == 0)
      |> Seq.map (fun (i, v) => v);

    let remaining = src
      |> Seq.map (fun i => (i, i))
      |> Seq.filter (fun (i, v) => i mod 3 != 0)
      |> Seq.map (fun (i, v) => v);

    let mapOfSizeNdiv3 = removed |> Seq.reduce (fun acc i => acc |> Set.remove i) mapOfSizeN;

    removed |> Seq.forEach(fun i =>
      expect (mapOfSizeNdiv3 |> Set.contains i) |> toBeEqualToFalse
    );

    remaining |> Seq.forEach(fun i =>
      expect (mapOfSizeNdiv3 |> Set.contains i) |> toBeEqualToTrue
    );
  }),
  it (sprintf "some with %i elements" count) (fun () => {
    Seq.inRange 0 (Some count) 1
      |> Set.fromSeq
      |> Set.some (fun v => v > 0)
      |> expect |> toBeEqualToTrue;

    Seq.inRange 0 (Some count) 1
      |> Set.fromSeq
      |> Set.some (fun v => v > (count - 2))
      |> expect |> toBeEqualToTrue;

    Seq.inRange 0 (Some count) 1
      |> Set.fromSeq
      |> Set.some (fun v => v > (count / 2))
      |> expect |> toBeEqualToTrue;

    Seq.inRange 0 (Some count) 1
      |> Set.fromSeq
      |> Set.some (fun v => v < 0)
      |> expect |> toBeEqualToFalse;

  }),
  it (sprintf "subtract with %i elements" count) (fun () => {
    let setA = Seq.inRange 0 (Some count) 1 |> Set.fromSeq;
    let setB = Seq.inRange 0 (Some (count / 2)) 2 |> Set.fromSeq;

    let subtracted = Set.subtract setA setB;
    let expected = Seq.inRange 1 (Some (count / 2)) 2 |> Set.fromSeq;

    expect (Set.equals subtracted expected) |> toBeEqualToTrue;
  }),
  describe "toCollection" [
    it (sprintf "contains with %i elements" count) (fun () => {
      let set = Seq.inRange 0 (Some count) 3 |> Set.fromSeq |> Set.toCollection;

      Collection.inRange 0 count 3 |> Collection.forEach (fun i => {
        expect (set |> Collection.contains i) |> toBeEqualToTrue;
      });
    }),
    it (sprintf "count with %i elements" count) (fun () => {
      let set = Seq.inRange 0 (Some count) 3 |> Set.fromSeq |> Set.toCollection;
      expect (Collection.count set) |> toBeEqualToInt count;
    }),
    it (sprintf "every with %i elements" count) (fun () => {
      Seq.inRange 0 (Some count) 1
        |> Set.fromSeq
        |> Set.toCollection
        |> Collection.every (fun v => v > (count / 2))
        |> expect |> toBeEqualToFalse;

      Seq.inRange 0 (Some count) 1
        |> Set.fromSeq
        |> Set.toCollection
        |> Collection.every (fun v => v > 0)
        |> expect |> toBeEqualToFalse;

      Seq.inRange 0 (Some count) 1
        |> Set.fromSeq
        |> Set.toCollection
        |> Collection.every (fun v => v > (count - 2))
        |> expect |> toBeEqualToFalse;

      Seq.inRange 0 (Some count) 1
        |> Set.fromSeq
        |> Set.toCollection
        |> Collection.every (fun v => v < count)
        |> expect |> toBeEqualToTrue;
    }),
    it (sprintf "find and tryFind with %i elements" count) (fun () => {
      let collection = Seq.inRange 0 (Some count) 1 |> Set.fromSeq |> Set.toCollection;

      let find0 i => i == 0;
      let findCountMinusOne i => i == (count - 1);
      let findCountDividedByTwo i => i == (count / 2);

      expect (Collection.find find0 collection) |> toBeEqualToInt 0;
      expect (Collection.find findCountMinusOne collection) |> toBeEqualToInt (count - 1);
      expect (Collection.find findCountDividedByTwo collection) |> toBeEqualToInt (count / 2);

      expect (Collection.tryFind find0 collection) |> toBeEqualToSomeOfInt 0;
      expect (Collection.tryFind findCountMinusOne collection) |> toBeEqualToSomeOfInt (count - 1);
      expect (Collection.tryFind findCountDividedByTwo collection) |> toBeEqualToSomeOfInt (count / 2);

      defer (fun () => Collection.find Functions.alwaysFalse collection) |> throws;
      expect (Collection.tryFind Functions.alwaysFalse collection) |> toBeEqualToNoneOfInt;
    }),
    it (sprintf "forEach and tryFind with %i elements" count) (fun () => {
      let collection = Seq.inRange (count - 1) (Some count) (-1) |> Set.fromSeq |> Set.toCollection;
      collection |> Collection.forEach (fun i => {
        expect (collection |> Collection.contains i) |> toBeEqualToTrue;
      });
    }),
    it (sprintf "none with %i elements" count) (fun () => {
      Seq.inRange 0 (Some count) 1
        |> Set.fromSeq
        |> Set.toCollection
        |> Collection.none (fun v => v > 0)
        |> expect |> toBeEqualToFalse;

      Seq.inRange 0 (Some count) 1
        |> Set.fromSeq
        |> Set.toCollection
        |> Collection.none (fun v => v > (count / 2))
        |> expect |> toBeEqualToFalse;

      Seq.inRange 0 (Some count) 1
        |> Set.fromSeq
        |> Set.toCollection
        |> Collection.none (fun v => v > (count - 2))
        |> expect |> toBeEqualToFalse;

      Seq.inRange 0 (Some count) 1
        |> Set.fromSeq
        |> Set.toCollection
        |> Collection.none (fun v => v < 0)
        |> expect |> toBeEqualToTrue;
    }),
    it (sprintf "reduce with %i elements" count) (fun () => {
      Seq.inRange 0 (Some count) 1
        |> Set.fromSeq
        |> Set.toCollection
        |> Collection.reduce (fun acc i => acc + 1) 0
        |> expect
        |> toBeEqualToInt count;
    }),
    it (sprintf "some with %i elements" count) (fun () => {
      Seq.inRange 0 (Some count) 1
        |> Set.fromSeq
        |> Set.toCollection
        |> Collection.some (fun v => v > 0)
        |> expect |> toBeEqualToTrue;

      Seq.inRange 0 (Some count) 1
        |> Set.fromSeq
        |> Set.toCollection
        |> Collection.some (fun v => v > (count - 2))
        |> expect |> toBeEqualToTrue;

      Seq.inRange 0 (Some count) 1
        |> Set.fromSeq
        |> Set.toCollection
        |> Collection.some (fun v => v > (count / 2))
        |> expect |> toBeEqualToTrue;

      Seq.inRange 0 (Some count) 1
        |> Set.fromSeq
        |> Set.toCollection
        |> Collection.some (fun v => v < 0)
        |> expect |> toBeEqualToFalse;
    }),
    describe "toSeq" [

    ],
  ],
  describe "toKeyed" [
    it (sprintf "containsWith with %i elements" count) (fun () => {
      let keyed = Seq.inRange 0 (Some count) 1 |> Set.fromSeq |> Set.toKeyed;
      Seq.inRange 0 (Some count) 1 |> Seq.forEach (fun i => {
        expect (keyed |> Keyed.contains i i) |> toBeEqualToTrue;
      });

      expect (keyed |> Keyed.contains (-1) (-1)) |> toBeEqualToFalse;
      expect (keyed |> Keyed.contains count count) |> toBeEqualToFalse;
      expect (keyed |> Keyed.contains 0 1) |> toBeEqualToFalse;
    }),
    it (sprintf "containsKey with %i elements" count) (fun () => {
      let keyed = Seq.inRange 0 (Some count) 1 |> Set.fromSeq |> Set.toKeyed;
      Seq.inRange 0 (Some count) 1 |> Seq.forEach (fun i => {
        expect (keyed |> Keyed.containsKey i) |> toBeEqualToTrue;
      });

      expect (keyed |> Keyed.containsKey (-1)) |> toBeEqualToFalse;
      expect (keyed |> Keyed.containsKey count) |> toBeEqualToFalse;
    }),
    it (sprintf "count with %i elements" count) (fun () => {
      let keyed = Seq.inRange 0 (Some count) 1 |> Set.fromSeq |> Set.toKeyed;
      expect (keyed |> Keyed.count) |> toBeEqualToInt count;
    }),
    it (sprintf "every with %i elements" count) (fun () => {
      let keyed = Seq.inRange 0 (Some count) 1 |> Set.fromSeq |> Set.toKeyed;
      expect (keyed |> Keyed.every (fun i v => i == v)) |> toBeEqualToTrue;
      expect (keyed |> Keyed.every (fun i v => i != v)) |> toBeEqualToFalse;
    }),
    it (sprintf "find with %i elements" count) (fun () => {
      let keyed = Seq.inRange 0 (Some count) 1 |> Set.fromSeq |> Set.toKeyed;
      expect (keyed |> Keyed.find (fun i v =>
        (i == (count - 1)) && (v == (count - 1))
      )) |> toBeEqualTo intTupleToString (count - 1, count - 1);
      defer (fun () => keyed |> Keyed.find (fun i v => i != v)) |> throws;
    }),
    it (sprintf "forEach with %i elements" count) (fun () => {
      let keyed = Seq.inRange 0 (Some count) 1 |> Set.fromSeq |> Set.toKeyed;
      let loopCount = ref 0;
      keyed |> Keyed.forEach (fun i v => {
        expect i |> toBeEqualToInt v;
        loopCount := !loopCount + 1;
      });

      expect !loopCount |> toBeEqualToInt count;
    }),
    it (sprintf "get with %i elements" count) (fun () => {
      let keyed = Seq.inRange 0 (Some count) 1 |> Set.fromSeq |> Set.toKeyed;
      Seq.inRange 0 (Some count) 1 |> Seq.forEach (fun i => {
        expect (keyed |> Keyed.get i) |> toBeEqualToInt i;
      });

      defer (fun () => keyed |> Keyed.get (-1)) |> throws;
      defer (fun () => keyed |> Keyed.get count) |> throws;
    }),
    it (sprintf "keys with %i elements" count) (fun () => {
      let keyed = Seq.inRange 0 (Some count) 1 |> Set.fromSeq |> Set.toKeyed;
      keyed
        |> Keyed.keys
        |> Collection.equals (Collection.inRange 0 count 1)
        |> expect
        |> toBeEqualToTrue;
    }),
    it (sprintf "none with %i elements" count) (fun () => {
      let keyed = Seq.inRange 0 (Some count) 1 |> Set.fromSeq |> Set.toKeyed;
      expect (keyed |> Keyed.none (fun i v => i != v)) |> toBeEqualToTrue;
      expect (keyed |> Keyed.none (fun i v => i == v)) |> toBeEqualToFalse;
    }),
    it (sprintf "reduce with %i elements" count) (fun () => {
      let keyed = Seq.inRange 0 (Some count) 1 |> Set.fromSeq |> Set.toKeyed;
      let reduced = keyed |> Keyed.reduce (fun acc i v => acc + 1) 0;
      expect reduced |> toBeEqualToInt count;
    }),
    it (sprintf "some with %i elements" count) (fun () => {
      let keyed = Seq.inRange 0 (Some count) 1 |> Set.fromSeq |> Set.toKeyed;
      expect (keyed |> Keyed.some (fun i v => i == v)) |> toBeEqualToTrue;
      expect (keyed |> Keyed.some (fun i v => i != v)) |> toBeEqualToFalse;
    }),
    describe "toSeq" [

    ],
    it (sprintf "tryFind with %i elements" count) (fun () => {
      let keyed = Seq.inRange 0 (Some count) 1 |> Set.fromSeq |> Set.toKeyed;
      expect (keyed |> Keyed.tryFind (fun i v =>
        (i == (count - 1)) && (v == (count - 1))
      )) |> toBeEqualToSome intTupleToString (count - 1, count - 1);
      expect (keyed |> Keyed.tryFind (fun i v => i != v)) |> toBeEqualToNone intTupleToString;
    }),
    it (sprintf "tryGet with %i elements" count) (fun () => {
      let keyed = Seq.inRange 0 (Some count) 1 |> Set.fromSeq |> Set.toKeyed;
      Seq.inRange 0 (Some count) 1 |> Seq.forEach (fun i => {
        expect (keyed |> Keyed.tryGet i) |> toBeEqualToSomeOfInt i;
      });

      expect (keyed |> Keyed.tryGet (-1)) |> toBeEqualToNoneOfInt;
      expect (keyed |> Keyed.tryGet count) |> toBeEqualToNoneOfInt;
    }),
    it (sprintf "values with %i elements" count) (fun () => {
      let keyed = Seq.inRange 0 (Some count) 1 |> Set.fromSeq |> Set.toKeyed;
      keyed
        |> Keyed.values
        |> Set.fromSeq
        |> Set.toCollection
        |> Collection.equals (Collection.inRange 0 count 1)
        |> expect
        |> toBeEqualToTrue;
    }),
  ],
  describe "toSeq" [

  ],
  it (sprintf "union with %i elements" count) (fun () => {
    let setA = Seq.inRange 1 (Some (count / 2)) 2 |> Set.fromSeq;
    let setB = Seq.inRange 0 (Some (count / 2)) 2 |> Set.fromSeq;

    let union = Set.union setA setB;
    let expected = Seq.inRange 0 (Some count) 1 |> Set.fromSeq;

    expect (Set.equals union expected) |> toBeEqualToTrue;
  }),
];
