open Immutable;
open Printf;
open ReUnit;
open ReUnit.Expect;
open ReUnit.Test;

let module Set = {
  type t = IntSet.t;

  let add = IntSet.add;
  let addAll = IntSet.addAll;
  let contains = IntSet.contains;
  let count = IntSet.count;
  let empty = fun () => IntSet.empty;
  let equals = IntSet.equals;
  let every = IntSet.every;
  let find = IntSet.find;
  let forEach = IntSet.forEach;
  let fromSeq = IntSet.fromSeq;
  let hash = IntSet.hash;
  let intersect = IntSet.intersect;
  let isEmpty = IntSet.isEmpty;
  let isNotEmpty = IntSet.isNotEmpty;
  let none = IntSet.none;
  let reduce = IntSet.reduce;
  let remove = IntSet.remove;
  let removeAll = IntSet.removeAll;
  let some = IntSet.some;
  let subtract = IntSet.subtract;
  let toSet = IntSet.toSet;
  let toMap = IntSet.toMap;
  let toSeq = IntSet.toSeq;
  let tryFind = IntSet.tryFind;
  let union = IntSet.union;
};

let transientIntSetTest (count: int): (list Test.t) => [
  it (sprintf "add with %i elements" count) (fun () => {
    let src = IntRange.create 0 count |> IntRange.toSeq;

    let (_, mapOfSizeN) = src
      |> Seq.scan
        (fun (_, acc) i => (i, acc |> TransientIntSet.add i))
        (0, IntSet.empty |> IntSet.mutate)
      |> Seq.doOnNext(fun (i, acc) => {
        expect (acc |> TransientIntSet.contains i) |> toBeEqualToTrue;
        expect (acc |> TransientIntSet.count) |> toBeEqualToInt (i + 1);
        expect (acc |> TransientIntSet.isEmpty) |> toBeEqualToFalse;
        expect (acc |> TransientIntSet.isNotEmpty) |> toBeEqualToTrue;
      }) |> Seq.last;

    src |> Seq.forEach (fun i =>
      expect (mapOfSizeN |> TransientIntSet.contains i) |> toBeEqualToTrue
    );
  }),
  it (sprintf "remove with %i elements" count) (fun () => {
    let hash = Hash.random ();
    let transient = IntSet.empty
      |> IntSet.mutate
      |> TransientIntSet.addAll (
        IntRange.create 0 count |> IntRange.toSeq |> Seq.map hash
      );

    Seq.generate (fun i => i + 2) 0
      |> Seq.take (count / 2)
      |> Seq.map hash |> Seq.forEach (fun i => {
        transient |> TransientIntSet.remove i |> ignore;
      });

    Seq.generate (fun i => i + 2) 0
      |> Seq.take (count / 2)
      |> Seq.map hash |> Seq.forEach (fun i => {
        expect (transient |> TransientIntSet.contains i) |> toBeEqualToTrue;
      });

    Seq.generate (fun i => i + 2) 0
      |> Seq.take (count / 2)
      |> Seq.map hash |> Seq.forEach (fun i => {
        expect (transient |> TransientIntSet.contains i) |> toBeEqualToFalse;
      });
  }),
  it (sprintf "removeAll with %i elements" count) (fun () => {
    let hash = Hash.random ();
    let transient = IntSet.empty
      |> IntSet.mutate
      |> TransientIntSet.addAll (
        IntRange.create 0 count |> IntRange.toSeq |> Seq.map hash
      );
    transient |> TransientIntSet.removeAll |> ignore;
    expect (transient |> TransientIntSet.isEmpty) |> toBeEqualToTrue;
  }),
];

let test = describe "IntSet" (List.fromSeqReversed @@ Seq.concat @@ [
  describe "IntSet" (List.fromSeqReversed @@ Seq.concat @@ [
    (SetTester.test 10 (module Set)) |> List.toSeq,
    (SetTester.test 48 (module Set)) |> List.toSeq,
    (SetTester.test 90 (module Set)) |> List.toSeq,
    (SetTester.test 500 (module Set)) |> List.toSeq,
    (SetTester.test 5000 (module Set)) |> List.toSeq,
  ]) |> Seq.return,
  describe "TransientIntSet" (List.fromSeqReversed @@ Seq.concat @@ [
    transientIntSetTest 10 |> List.toSeq,
    transientIntSetTest 48 |> List.toSeq,
    transientIntSetTest 90 |> List.toSeq,
    transientIntSetTest 500 |> List.toSeq,
    transientIntSetTest 5000 |> List.toSeq,
  ]) |> Seq.return,
]);
