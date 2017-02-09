/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Immutable;
open Printf;
open ReUnit;
open ReUnit.Expect;
open ReUnit.Test;

module type Vector = {
  type t 'a;

  let addFirst: 'a => (t 'a) => (t 'a);
  let addFirstAll: (Seq.t 'a) => (t 'a) => (t 'a);
  let addLast: 'a => (t 'a) => (t 'a);
  let addLastAll: (Seq.t 'a) => (t 'a) => (t 'a);
  let compare: (Comparator.t (t 'a));
  let compareWith: (Comparator.t 'a) => (Comparator.t (t 'a));
  let count: (t 'a) => int;
  let empty: (t 'a);
  let equals: (Equality.t (t 'a));
  let equalsWith: (Equality.t 'a) => (Equality.t (t 'a));
  let every: ('a => bool) => (t 'a) => bool;
  let find: ('a => bool) => (t 'a) => 'a;
  let first: (t 'a) => 'a;
  let fromSeq: (Seq.t 'a) => (t 'a);
  let fromSeqReversed: (Seq.t 'a) => (t 'a);
  let get: int => (t 'a) => 'a;
  let hash: (Hash.t (t 'a));
  let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
  let isEmpty: t 'a => bool;
  let isNotEmpty: t 'a => bool;
  let last: (t 'a) => 'a;
  let map: ('a => 'b) => (t 'a) => (t 'b);
  let mapWithIndex: (int => 'a => 'b) => (t 'a) => (t 'b);
  let mapReverse: ('a => 'b) => (t 'a) => (t 'b);
  let mapReverseWithIndex: (int => 'a => 'b) => (t 'a) => (t 'b);
  let none: ('a => bool) => (t 'a) => bool;
  let range: int => (option int) => (t 'a) => (t 'a);
  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let reduceWithIndex: ('acc => int => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let reduceRight: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let reduceRightWithIndex: ('acc => int => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let removeAll: (t 'a) => (t 'a);
  let removeFirst: (t 'a) => (t 'a);
  let removeLast: (t 'a) => (t 'a);
  let reverse: (t 'a) => (t 'a);
  let skip: int => (t 'a) => (t 'a);
  let some: ('a => bool) => (t 'a) => bool;
  let toSeq: (t 'a) => (Seq.t 'a);
  let toSeqReversed: (t 'a) => (Seq.t 'a);
  let tryFind: ('a => bool) => (t 'a) => (option 'a);
  let tryFirst: (t 'a) => (option 'a);
  let tryGet: int => (t 'a) => (option 'a);
  let tryLast: (t 'a) => option 'a;
  let take: int => (t 'a) => (t 'a);
  let update: int => 'a => (t 'a) => (t 'a);
};

let test (count: int) (module Vector: Vector): (list Test.t) => {
  let dequeTests = DequeTester.test count (module {
    type t 'a = Vector.t 'a;

    let addFirst = Vector.addFirst;
    let addFirstAll = Vector.addFirstAll;
    let addLast = Vector.addLast;
    let addLastAll = Vector.addLastAll;
    let compare = Vector.compare;
    let compareWith = Vector.compareWith;
    let count = Vector.count;
    let equals = Vector.equals;
    let equalsWith = Vector.equalsWith;
    let empty = Vector.empty;
    let every = Vector.every;
    let find = Vector.find;
    let first = Vector.first;
    let fromSeq = Vector.fromSeq;
    let fromSeqReversed = Vector.fromSeqReversed;
    let hash = Vector.hash;
    let hashWith = Vector.hashWith;
    let isEmpty = Vector.isEmpty;
    let isNotEmpty = Vector.isNotEmpty;
    let last = Vector.last;
    let map = Vector.map;
    let mapReverse = Vector.mapReverse;
    let none = Vector.none;
    let reduce = Vector.reduce;
    let reduceRight = Vector.reduceRight;
    let removeAll = Vector.removeAll;
    let removeFirst = Vector.removeFirst;
    let removeLast = Vector.removeLast;
    let reverse = Vector.reverse;
    let some = Vector.some;
    let toSeq = Vector.toSeq;
    let toSeqReversed = Vector.toSeqReversed;
    let tryFind = Vector.tryFind;
    let tryFirst = Vector.tryFirst;
    let tryLast = Vector.tryLast;
  });

  [
    it (sprintf "update %i elements" count) (fun () => {
      let vector = Seq.inRange 0 (Some count) 1 |> Seq.reduce (fun acc i =>
        acc |> Vector.addLast i
      ) Vector.empty;

      Seq.inRange 0 (Some count) 1 |> Seq.forEach (fun i => {
        expect (vector |> Vector.get i) |> toBeEqualToInt i;
        expect (vector |> Vector.tryGet i) |> toBeEqualToSomeOfInt i;
      });

      let updated = Seq.inRange 0 (Some count) 1 |> Seq.reduce (fun acc i =>
        acc |> Vector.update i (i + 1)
      ) vector;

      Seq.inRange 0 (Some count) 1 |> Seq.forEach (fun i => {
        expect (updated |> Vector.get i) |> toBeEqualToInt (i + 1);
        expect (updated |> Vector.tryGet i) |> toBeEqualToSomeOfInt (i + 1);
      });
    }),

    it (sprintf "mapWithIndex %i elements" count) (fun () => {
      Seq.inRange 0 (Some count) 1
        |> Seq.reduce (fun acc i => acc |> Vector.addLast i) Vector.empty
        |> Vector.mapWithIndex (fun i v => i + 1)
        |> Vector.toSeq
        |> expect
        |> toBeEqualToSeq string_of_int (Seq.inRange 1 (Some count) 1);
    }),

    it (sprintf "mapReverseWithIndex %i elements" count) (fun () => {
      Seq.inRange 0 (Some count) 1
        |> Seq.reduce (fun acc i => acc |> Vector.addLast i) Vector.empty
        |> Vector.mapReverseWithIndex (fun i v => i + 1)
        |> Vector.toSeq
        |> expect
        |> toBeEqualToSeq string_of_int (Seq.inRange count (Some count) (-1));
    }),

    it (sprintf "reduceWithIndex with %i elements" count) (fun () => {
      let result = Seq.repeat 1 (Some count)
        |> Seq.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.reduceWithIndex (fun acc i v => acc + i) 0;

      let expected = Seq.inRange 0 (Some count) 1
        |> Seq.reduce (fun acc i => acc + i) 0;

      expect result |> toBeEqualToInt expected;
    }),

    it (sprintf "reduceRightWithIndex %i elements" count) (fun () => {
      let result = Seq.repeat 1 (Some count)
        |> Seq.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.reduceRightWithIndex (fun acc i v => acc + i) 0;

      let expected = Seq.inRange (count - 1) (Some count) (-1)
        |> Seq.reduce (fun acc i => acc + i) 0;

      expect result |> toBeEqualToInt expected;
    }),

    it (sprintf "take with %i elements" count) (fun () => {
      let vector = Seq.inRange 0 (Some count) 1 |> Seq.reduce (fun acc i =>
        acc |> Vector.addLast i
      ) Vector.empty;

      let taken = vector |> Vector.take 1;
      let seqsEquality = Seq.equals (Vector.toSeq taken) (Seq.return 0);
      expect seqsEquality |> toBeEqualToTrue;

      let taken = vector |> Vector.take (count / 2);
      let seqsEquality = Seq.equals (Vector.toSeq taken) (Seq.inRange 0 (Some (count / 2)) 1);
      expect seqsEquality |> toBeEqualToTrue;

      let taken = vector |> Vector.take (count - 1);
      let seqsEquality = Seq.equals (Vector.toSeq taken) (Seq.inRange 0 (Some (count - 1)) 1);
      expect seqsEquality |> toBeEqualToTrue;
    }),
    it (sprintf "skip with %i elements" count) (fun () => {
      let vector = Seq.inRange 0 (Some count) 1 |> Seq.reduce (fun acc i =>
        acc |> Vector.addLast i
      ) Vector.empty;

      let skipped = vector |> Vector.skip 1;
      let seqsEquality = Seq.equals (Vector.toSeq skipped) (Seq.inRange 1 (Some (count - 1)) 1);
      expect seqsEquality |> toBeEqualToTrue;

      let skipped = vector |> Vector.skip (count / 2);
      let seqsEquality = Seq.equals (Vector.toSeq skipped) (Seq.inRange (count / 2) (Some (count / 2)) 1);
      expect seqsEquality |> toBeEqualToTrue;

      let skipped = vector |> Vector.skip (count - 1);
      let seqsEquality = Seq.equals (Vector.toSeq skipped) (Seq.return (count - 1));
      expect seqsEquality |> toBeEqualToTrue;
    }),
    it (sprintf "range with %i elements" count) (fun () => {
      let vector = Seq.inRange 0 (Some count) 1 |> Seq.reduce (fun acc i =>
        acc |> Vector.addLast i
      ) Vector.empty;

      let rangeOneToCountMinusTwo = vector |> Vector.range 1 (count - 2 |> Option.return);

      let seqsEquality = Seq.equals
        (Vector.toSeq rangeOneToCountMinusTwo)
        (Seq.inRange 1 (count - 2 |> Option.return) 1);

      expect seqsEquality |> toBeEqualToTrue;
    }),
    ...dequeTests
  ]
};
