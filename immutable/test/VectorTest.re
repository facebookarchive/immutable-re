/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Immutable;
open ReUnit;
open ReUnit.Test;

let module Vector = {
  type t 'a = Vector.t 'a;

  let addFirst = Vector.addFirst;
  let addLast = Vector.addLast;
  let count = Vector.count;
  let empty = Vector.empty;
  let every = Vector.every;
  let first = Vector.first;
  let get = Vector.get;
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
  let tryFirst = Vector.tryFirst;
  let tryGet = Vector.tryGet;
  let tryLast = Vector.tryLast;
  let update = Vector.update;
};

let test = describe "Vector" (List.fromSeq @@ Seq.concat @@ [
  (VectorTester.test 10 (module Vector)) |> List.toSeq,
  (VectorTester.test 48 (module Vector)) |> List.toSeq,
  (VectorTester.test 90 (module Vector)) |> List.toSeq,
  describe "TransientVector" (List.fromSeq @@ Seq.concat @@ [
    (TransientVectorTester.test 10 (module TransientVector)) |> List.toSeq,
    (TransientVectorTester.test 48 (module TransientVector)) |> List.toSeq,
    (TransientVectorTester.test 90 (module TransientVector)) |> List.toSeq,
  ]) |> Seq.return,
]);
