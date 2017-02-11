/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Immutable;
open Printf;
open ReUnit;
open ReUnit.Expect;
open ReUnit.Test;

module type TransientVector = {
  type t 'a;

  let addFirst: 'a => (t 'a) => (t 'a);
  let addLast: 'a => (t 'a) => (t 'a);
  let count: (t 'a) => int;
  let empty: unit => (t 'a);
  let first: (t 'a) => 'a;
  let get: int => (t 'a) => 'a;
  /*let insertAt: int => 'a => (t 'a) => (t 'a);*/
  let isEmpty: t 'a => bool;
  let isNotEmpty: t 'a => bool;
  let last: (t 'a) => 'a;
  /*let removeAt: int => (t 'a) => (t 'a);*/
  let removeAll: (t 'a) => (t 'a);
  let removeFirst: (t 'a) => (t 'a);
  let removeLast: (t 'a) => (t 'a);
  let reverse: (t 'a) => (t 'a);
  let tryFirst: (t 'a) => option 'a;
  let tryGet: int => (t 'a) => (option 'a);
  let tryLast: (t 'a) => option 'a;
  let update: int => 'a => (t 'a) => (t 'a);
  let updateAll: (int => 'a => 'a) => (t 'a) => (t 'a);
  let updateWith: int => ('a => 'a) => (t 'a) => (t 'a);
};


let test (count: int) (module TransientVector: TransientVector): (list Test.t) => {
  let dequeTests = TransientDequeTester.test count (module {
    type t 'a = TransientVector.t 'a;

    let addFirst = TransientVector.addFirst;
    let addLast = TransientVector.addLast;
    let count = TransientVector.count;
    let empty = TransientVector.empty;
    let first = TransientVector.first;
    let isEmpty = TransientVector.isEmpty;
    let isNotEmpty = TransientVector.isNotEmpty;
    let last = TransientVector.last;
    let removeAll = TransientVector.removeAll;
    let removeFirst = TransientVector.removeFirst;
    let removeLast = TransientVector.removeLast;
    let reverse = TransientVector.reverse;
    let tryFirst = TransientVector.tryFirst;
    let tryLast = TransientVector.tryLast;
  });

  [
    it (sprintf "update %i elements" count) (fun () => {
      let vector = Seq.inRange 0 (Some count) 1 |> Seq.reduce (fun acc i =>
        acc |> TransientVector.addLast i
      ) (TransientVector.empty ());

      Seq.inRange 0 (Some count) 1 |> Seq.forEach (fun i => {
        expect (vector |> TransientVector.get i) |> toBeEqualToInt i;
        expect (vector |> TransientVector.tryGet i) |> toBeEqualToSomeOfInt i;
      });

      let updated = Seq.inRange 0 (Some count) 1 |> Seq.reduce (fun acc i =>
        acc |> TransientVector.update i (i + 1)
      ) vector;

      Seq.inRange 0 (Some count) 1 |> Seq.forEach (fun i => {
        expect (updated |> TransientVector.get i) |> toBeEqualToInt (i + 1);
        expect (updated |> TransientVector.tryGet i) |> toBeEqualToSomeOfInt (i + 1);
      });
    }),

    it (sprintf "updateWith %i elements" count) (fun () => {
      let vector = Seq.inRange 0 (Some count) 1 |> Seq.reduce (fun acc i =>
        acc |> TransientVector.addLast i
      ) (TransientVector.empty ());

      Seq.inRange 0 (Some count) 1 |> Seq.forEach (fun i => {
        expect (vector |> TransientVector.get i) |> toBeEqualToInt i;
        expect (vector |> TransientVector.tryGet i) |> toBeEqualToSomeOfInt i;
      });

      let updated = Seq.inRange 0 (Some count) 1 |> Seq.reduce (fun acc i =>
        acc |> TransientVector.updateWith i (fun v => v + 1)
      ) vector;

      Seq.inRange 0 (Some count) 1 |> Seq.forEach (fun i => {
        expect (updated |> TransientVector.get i) |> toBeEqualToInt (i + 1);
        expect (updated |> TransientVector.tryGet i) |> toBeEqualToSomeOfInt (i + 1);
      });
    }),

    ...dequeTests
  ]
};
