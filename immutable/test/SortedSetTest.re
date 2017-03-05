open Immutable;
open ReUnit;
open ReUnit.Expect;
open ReUnit.Test;

/*
let emptyWith: (Comparator.t 'a) => (t 'a);
let fromSeqWith: (Comparator.t 'a)  => (Seq.t 'a) => (t 'a);
*/

let module Set = {
  type t = SortedSet.t int;

  let add = SortedSet.add;
  let addAll = SortedSet.addAll;
  let contains = SortedSet.contains;
  let count = SortedSet.count;
  let empty = fun () => SortedSet.empty;
  let equals = SortedSet.equals;
  let every = SortedSet.every;
  let find = SortedSet.find;
  let forEach = SortedSet.forEach;
  let fromSeq = SortedSet.fromSeq;
  let hash = SortedSet.hash;
  let intersect = SortedSet.intersect;
  let isEmpty = SortedSet.isEmpty;
  let isNotEmpty = SortedSet.isNotEmpty;
  let none = SortedSet.none;
  let reduce = SortedSet.reduce;
  let remove = SortedSet.remove;
  let removeAll = SortedSet.removeAll;
  let some = SortedSet.some;
  let subtract = SortedSet.subtract;
  let toCollection = SortedSet.toCollection;
  let toKeyed = SortedSet.toKeyed;
  let toSeq = SortedSet.toSeq;
  let tryFind = SortedSet.tryFind;
  let union = SortedSet.union;
};

let count = 10000;

let test = describe "SortedSet" [
  it "compare" (fun () => {
    let set = Seq.inRange 0 (Some count) 1 |> SortedSet.fromSeq;

    let setEqual = Seq.inRange 0 (Some count) 1 |> SortedSet.fromSeq;
    expect (SortedSet.compare set setEqual) |> toBeEqualTo (fun _ => "") Ordering.equal;

    let setSameLengthLessThan = Seq.inRange (-1) (Some count) 1 |> SortedSet.fromSeq;
    expect (SortedSet.compare set setSameLengthLessThan) |> toBeEqualTo (fun _ => "") Ordering.greaterThan;


    let setSameLengthGreaterThan = Seq.inRange 1 (Some count) 1 |> SortedSet.fromSeq;
    expect (SortedSet.compare set setSameLengthGreaterThan) |> toBeEqualTo (fun _ => "") Ordering.lessThan;


    let setLonger = Seq.inRange 0 (Some (count + 1)) 1 |> SortedSet.fromSeq;
    expect (SortedSet.compare set setLonger) |> toBeEqualTo (fun _ => "") Ordering.lessThan;

    let setShorter = Seq.inRange 0 (Some (count - 1)) 1 |> SortedSet.fromSeq;
    expect (SortedSet.compare set setShorter) |> toBeEqualTo (fun _ => "") Ordering.greaterThan;
  }),
  it "last and tryLast" (fun () => {
    let set = Seq.inRange 0 (Some count) 1 |> SortedSet.fromSeq;
    expect (set |> SortedSet.last) |> toBeEqualToInt (count - 1);
    expect (set |> SortedSet.tryLast) |> toBeEqualToSomeOfInt (count - 1);


    defer (fun () => SortedSet.empty |> SortedSet.last) |> throws;
    expect (SortedSet.empty |> SortedSet.tryLast) |> toBeEqualToNoneOfInt;
  }),
  it "first and tryFirst" (fun () => {
    let set = Seq.inRange 0 (Some count) 1 |> SortedSet.fromSeq;
    expect (set |> SortedSet.first) |> toBeEqualToInt 0;
    expect (set |> SortedSet.tryFirst) |> toBeEqualToSomeOfInt 0;


    defer (fun () => SortedSet.empty |> SortedSet.first) |> throws;
    expect (SortedSet.empty |> SortedSet.tryFirst) |> toBeEqualToNoneOfInt;
  }),
  it "reduceRight" (fun () => {
    Seq.inRange 0 (Some count) 1 |> SortedSet.fromSeq |> SortedSet.reduceRight (fun acc i => {
      expect (i < acc) |> toBeEqualToTrue;
      i
    }) count |> ignore;
  }),
  it "removeLast" (fun () => {
    let set = Seq.inRange 0 (Some count) 1 |> SortedSet.fromSeq |> SortedSet.removeLast;
    expect (set |> Set.contains (count - 1)) |> toBeEqualToFalse;
  }),
  it "removeFirst" (fun () => {
    let set = Seq.inRange 0 (Some count) 1 |> SortedSet.fromSeq |> SortedSet.removeFirst;
    expect (set |> Set.contains 0) |> toBeEqualToFalse;
  }),
  it "search and trySearch" (fun () => {
    let set = Seq.inRange 0 (Some count) 1 |> Set.fromSeq;

    let find0 = Comparator.structural 0;
    let findCountMinusOne = Comparator.structural (count - 1);
    let findCountDividedByTwo = Comparator.structural (count / 2);

    expect (SortedSet.search find0 set) |> toBeEqualToInt 0;
    expect (SortedSet.search findCountMinusOne set) |> toBeEqualToInt (count - 1);
    expect (SortedSet.search findCountDividedByTwo set) |> toBeEqualToInt (count / 2);

    expect (SortedSet.trySearch find0 set) |> toBeEqualToSomeOfInt 0;
    expect (SortedSet.trySearch  findCountMinusOne set) |> toBeEqualToSomeOfInt (count - 1);
    expect (SortedSet.trySearch  findCountDividedByTwo set) |> toBeEqualToSomeOfInt (count / 2);

    defer (fun () => SortedSet.search (fun i => Ordering.lessThan) set) |> throws;
    expect (SortedSet.trySearch (fun i => Ordering.greaterThan) set) |> toBeEqualToNoneOfInt;
  }),
  ...(SetTester.test count (module Set))
];
