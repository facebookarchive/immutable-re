/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Immutable;
open ReUnit;
open ReUnit.Test;

let module Vector = {
  type t 'a = CopyOnWriteArray.t 'a;

  let addFirst = CopyOnWriteArray.addFirst;
  let addLast = CopyOnWriteArray.addLast;
  let count = CopyOnWriteArray.count;
  let empty = CopyOnWriteArray.empty;
  let every = CopyOnWriteArray.every;
  let find = CopyOnWriteArray.find;
  let first = CopyOnWriteArray.first;
  let get = CopyOnWriteArray.get;
  let last = CopyOnWriteArray.last;
  let isEmpty = CopyOnWriteArray.isEmpty;
  let isNotEmpty = CopyOnWriteArray.isNotEmpty;
  let map = CopyOnWriteArray.map;
  let mapWithIndex = CopyOnWriteArray.mapWithIndex;
  let mapReverse = CopyOnWriteArray.mapReverse;
  let mapReverseWithIndex = CopyOnWriteArray.mapReverseWithIndex;
  let none = CopyOnWriteArray.none;
  let reduce = CopyOnWriteArray.reduce;
  let reduceWithIndex = CopyOnWriteArray.reduceWithIndex;
  let reduceRight = CopyOnWriteArray.reduceRight;
  let reduceRightWithIndex = CopyOnWriteArray.reduceRightWithIndex;
  let removeAll = CopyOnWriteArray.removeAll;
  let removeFirst = CopyOnWriteArray.removeFirst;
  let removeLast = CopyOnWriteArray.removeLast;
  let reverse = CopyOnWriteArray.reverse;
  let skip = CopyOnWriteArray.skip;
  let some = CopyOnWriteArray.some;
  let take = CopyOnWriteArray.take;
  let toSeq = CopyOnWriteArray.toSeq;
  let toSeqReversed = CopyOnWriteArray.toSeqReversed;
  let tryFind = CopyOnWriteArray.tryFind;
  let tryFirst = CopyOnWriteArray.tryFirst;
  let tryGet = CopyOnWriteArray.tryGet;
  let tryLast = CopyOnWriteArray.tryLast;
  let update = CopyOnWriteArray.update;
};

let test = describe "CopyOnWriteArray" (List.fromSeq @@ Seq.concat @@ [
  (VectorTester.test 10 (module Vector)) |> List.toSeq,
]);
