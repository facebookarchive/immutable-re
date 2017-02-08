/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Immutable;
open ReUnit;
open ReUnit.Test;

let module Deque = {
  type t 'a = Deque.t 'a;

  let addFirst = Deque.addFirst;
  let addLast = Deque.addLast;
  let compare = Deque.compare;
  let compareWith = Deque.compareWith;
  let count = Deque.count;
  let empty = Deque.empty;
  let equals = Deque.equals;
  let equalsWith = Deque.equalsWith;
  let every = Deque.every;
  let find = Deque.find;
  let first = Deque.first;
  let hash = Deque.hash;
  let hashWith = Deque.hashWith;
  let isEmpty = Deque.isEmpty;
  let isNotEmpty = Deque.isNotEmpty;
  let last = Deque.last;
  let map = Deque.map;
  let mapReverse = Deque.mapReverse;
  let none = Deque.none;
  let reduce = Deque.reduce;
  let reduceRight = Deque.reduceRight;
  let removeAll = Deque.removeAll;
  let removeFirst = Deque.removeFirst;
  let removeLast = Deque.removeLast;
  let reverse = Deque.reverse;
  let some = Deque.some;
  let toSeq = Deque.toSeq;
  let toSeqReversed = Deque.toSeqReversed;
  let tryFind = Deque.tryFind;
  let tryFirst = Deque.tryFirst;
  let tryLast = Deque.tryLast;
};

let test = describe "Deque" (List.fromSeq @@ Seq.concat @@ [
  (DequeTester.test 1000 (module Deque)) |> List.toSeq,
  describe "TransientDeque" (TransientDequeTester.test 1000 (module TransientDeque)) |> Seq.return,
]);
