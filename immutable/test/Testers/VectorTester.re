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
  let addLast: 'a => (t 'a) => (t 'a);
  let count: (t 'a) => int;
  let empty: (t 'a);
  let every: ('a => bool) => (t 'a) => bool;
  let find: ('a => bool) => (t 'a) => 'a;
  let first: (t 'a) => 'a;
  let get: int => (t 'a) => 'a;
  let isEmpty: t 'a => bool;
  let isNotEmpty: t 'a => bool;
  let last: (t 'a) => 'a;
  let map: ('a => 'b) => (t 'a) => (t 'b);
  let mapReverse: ('a => 'b) => (t 'a) => (t 'b);
  let none: ('a => bool) => (t 'a) => bool;
  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let reduceRight: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
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
    let addLast = Vector.addLast;
    let count = Vector.count;
    let empty = Vector.empty;
    let every = Vector.every;
    let find = Vector.find;
    let first = Vector.first;
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
    ...dequeTests
  ]
};
