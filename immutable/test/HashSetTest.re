open Immutable;
open Printf;
open ReUnit;
open ReUnit.Expect;
open ReUnit.Test;

let module ComparatorSet = {
  type t = HashSet.t int;

  let add = HashSet.add;
  let addAll = HashSet.addAll;
  let contains = HashSet.contains;
  let count = HashSet.count;
  let empty = fun () => HashSet.empty;
  let equals = HashSet.equals;
  let every = HashSet.every;
  let find = HashSet.find;
  let forEach = HashSet.forEach;
  let fromSeq = HashSet.fromSeq;
  let hash = HashSet.hash;
  let intersect = HashSet.intersect;
  let isEmpty = HashSet.isEmpty;
  let isNotEmpty = HashSet.isNotEmpty;
  let none = HashSet.none;
  let reduce = HashSet.reduce;
  let remove = HashSet.remove;
  let removeAll = HashSet.removeAll;
  let some = HashSet.some;
  let subtract = HashSet.subtract;
  let toSet = HashSet.toSet;
  let toMap = HashSet.toMap;
  let toSeq = HashSet.toSeq;
  let tryFind = HashSet.tryFind;
  let union = HashSet.union;
};

let module EqualitySet = {
  type t = HashSet.t int;

  let add = HashSet.add;
  let addAll = HashSet.addAll;
  let contains = HashSet.contains;
  let count = HashSet.count;
  let empty = fun () => HashSet.emptyWith HashStrategy.structuralEquality;
  let equals = HashSet.equals;
  let every = HashSet.every;
  let find = HashSet.find;
  let forEach = HashSet.forEach;
  let fromSeq = HashSet.fromSeq;
  let hash = HashSet.hash;
  let intersect = HashSet.intersect;
  let isEmpty = HashSet.isEmpty;
  let isNotEmpty = HashSet.isNotEmpty;
  let none = HashSet.none;
  let reduce = HashSet.reduce;
  let remove = HashSet.remove;
  let removeAll = HashSet.removeAll;
  let some = HashSet.some;
  let subtract = HashSet.subtract;
  let toSet = HashSet.toSet;
  let toMap = HashSet.toMap;
  let toSeq = HashSet.toSeq;
  let tryFind = HashSet.tryFind;
  let union = HashSet.union;
};

let module BadHashComparisonSet = {
  type t = HashSet.t int;

  let add = HashSet.add;
  let addAll = HashSet.addAll;
  let contains = HashSet.contains;
  let count = HashSet.count;
  let empty = fun () => {
    let badHashStrategy = HashStrategy.createWithComparator (fun _ => 10) Comparator.structural;
    HashSet.emptyWith badHashStrategy;
  };
  let equals = HashSet.equals;
  let every = HashSet.every;
  let find = HashSet.find;
  let forEach = HashSet.forEach;
  let fromSeq = HashSet.fromSeq;
  let hash = HashSet.hash;
  let intersect = HashSet.intersect;
  let isEmpty = HashSet.isEmpty;
  let isNotEmpty = HashSet.isNotEmpty;
  let none = HashSet.none;
  let reduce = HashSet.reduce;
  let remove = HashSet.remove;
  let removeAll = HashSet.removeAll;
  let some = HashSet.some;
  let subtract = HashSet.subtract;
  let toSet = HashSet.toSet;
  let toMap = HashSet.toMap;
  let toSeq = HashSet.toSeq;
  let tryFind = HashSet.tryFind;
  let union = HashSet.union;
};

let module BadHashEqualitySet = {
  type t = HashSet.t int;

  let add = HashSet.add;
  let addAll = HashSet.addAll;
  let contains = HashSet.contains;
  let count = HashSet.count;
  let empty = fun () => {
    let badHashStrategy = HashStrategy.createWithEquality (fun _ => 10) Equality.structural;
    HashSet.emptyWith badHashStrategy;
  };
  let equals = HashSet.equals;
  let every = HashSet.every;
  let find = HashSet.find;
  let forEach = HashSet.forEach;
  let fromSeq = HashSet.fromSeq;
  let hash = HashSet.hash;
  let intersect = HashSet.intersect;
  let isEmpty = HashSet.isEmpty;
  let isNotEmpty = HashSet.isNotEmpty;
  let none = HashSet.none;
  let reduce = HashSet.reduce;
  let remove = HashSet.remove;
  let removeAll = HashSet.removeAll;
  let some = HashSet.some;
  let subtract = HashSet.subtract;
  let toSet = HashSet.toSet;
  let toMap = HashSet.toMap;
  let toSeq = HashSet.toSeq;
  let tryFind = HashSet.tryFind;
  let union = HashSet.union;
};

let transientHashSetTest (count: int): (list Test.t) => [
  it (sprintf "add with %i elements" count) (fun () => {
    let src = IntRange.create 0 count |> IntRange.toSeq;

    let (_, mapOfSizeN) = src
      |> Seq.scan
        (fun (_, acc) i => (i, acc |> TransientHashSet.add i))
        (0, HashSet.empty |> HashSet.mutate)
      |> Seq.doOnNext(fun (i, acc) => {
        expect (acc |> TransientHashSet.contains i) |> toBeEqualToTrue;
        expect (acc |> TransientHashSet.count) |> toBeEqualToInt (i + 1);
        expect (acc |> TransientHashSet.isEmpty) |> toBeEqualToFalse;
        expect (acc |> TransientHashSet.isNotEmpty) |> toBeEqualToTrue;
      }) |> Seq.last;

    src |> Seq.forEach (fun i =>
      expect (mapOfSizeN |> TransientHashSet.contains i) |> toBeEqualToTrue
    );
  }),
  it (sprintf "remove with %i elements" count) (fun () => {
    let hash = Hash.random ();
    let transient = HashSet.empty
      |> HashSet.mutate
      |> TransientHashSet.addAll (
        IntRange.create 0 count |> IntRange.toSeq |> Seq.map hash
      );

    Seq.generate (fun i => i + 2) 0
      |> Seq.take (count / 2)
      |> Seq.map hash
      |> Seq.forEach (fun i => {
        transient |> TransientHashSet.remove i |> ignore;
      });

    Seq.generate (fun i => i + 2) 1
      |> Seq.take (count / 2)
      |> Seq.map hash
      |> Seq.forEach (fun i => {
        expect (transient |> TransientHashSet.contains i) |> toBeEqualToTrue;
      });

    Seq.generate (fun i => i + 2) 0
      |> Seq.take (count / 2)
      |> Seq.map hash
      |> Seq.forEach (fun i => {
        expect (transient |> TransientHashSet.contains i) |> toBeEqualToFalse;
      });
  }),
  it (sprintf "removeAll with %i elements" count) (fun () => {
    let hash = Hash.random ();
    let transient = HashSet.empty
      |> HashSet.mutate
      |> TransientHashSet.addAll (
          IntRange.create 0 count |> IntRange.toSeq |> Seq.map hash
      );
    transient |> TransientHashSet.removeAll |> ignore;
    expect (transient |> TransientHashSet.isEmpty) |> toBeEqualToTrue;
  }),
];

let test = describe "HashSet" (List.fromSeqReversed @@ Seq.concat @@ [
  describe "poor hash function comparator" (SetTester.test 100 (module BadHashComparisonSet)) |> Seq.return,
  describe "poor hash function equality" (SetTester.test 100 (module BadHashEqualitySet)) |> Seq.return,
  describe "comparator" (List.fromSeqReversed @@ Seq.concat @@ [
    (SetTester.test 10 (module ComparatorSet)) |> List.toSeq,
    (SetTester.test 48 (module ComparatorSet)) |> List.toSeq,
    (SetTester.test 90 (module ComparatorSet)) |> List.toSeq,
    (SetTester.test 500 (module ComparatorSet)) |> List.toSeq,
    (SetTester.test 5000 (module ComparatorSet)) |> List.toSeq,
  ]) |> Seq.return,
  describe "equality" (List.fromSeqReversed @@ Seq.concat @@ [
    (SetTester.test 10 (module EqualitySet)) |> List.toSeq,
    (SetTester.test 48 (module EqualitySet)) |> List.toSeq,
    (SetTester.test 90 (module EqualitySet)) |> List.toSeq,
    (SetTester.test 500 (module EqualitySet)) |> List.toSeq,
    (SetTester.test 5000 (module EqualitySet)) |> List.toSeq,
  ]) |> Seq.return,
  describe "TransientHashSet" (List.fromSeqReversed @@ Seq.concat @@ [
    transientHashSetTest 10 |> List.toSeq,
    transientHashSetTest 48 |> List.toSeq,
    transientHashSetTest 90 |> List.toSeq,
    transientHashSetTest 500 |> List.toSeq,
    transientHashSetTest 5000 |> List.toSeq,
  ]) |> Seq.return,
]);
