open Immutable;
open ReUnit;
open ReUnit.Test;

let module Vector = {
  type t 'a = Vector.t 'a;

  let addFirst = Vector.addFirst;
  let addFirstAll = Vector.addFirstAll;
  let addLast = Vector.addLast;
  let addLastAll = Vector.addLastAll;
  let compare = Vector.compare;
  let compareWith = Vector.compareWith;
  let concat = Vector.concat;
  let contains = Vector.contains;
  let containsWith = Vector.containsWith;
  let count = Vector.count;
  let empty = Vector.empty;
  let equals = Vector.equals;
  let equalsWith = Vector.equalsWith;
  let every = Vector.every;
  let everyWithIndex = Vector.everyWithIndex;
  let find = Vector.find;
  let findWithIndex = Vector.findWithIndex;
  let first = Vector.first;
  let forEach = Vector.forEach;
  let forEachReverse = Vector.forEachReverse;
  let forEachReverseWithIndex = Vector.forEachReverseWithIndex;
  let forEachWithIndex = Vector.forEachWithIndex;
  let fromSeq = Vector.fromSeq;
  let fromSeqReversed = Vector.fromSeqReversed;
  let get = Vector.get;
  let hash = Vector.hash;
  let hashWith = Vector.hashWith;
  let indexOf = Vector.indexOf;
  let indexOfWithIndex = Vector.indexOfWithIndex;
  let init = Vector.init;
  let insertAt = Vector.insertAt;
  let isEmpty = Vector.isEmpty;
  let isNotEmpty = Vector.isNotEmpty;
  let last = Vector.last;
  let map = Vector.map;
  let mapWithIndex = Vector.mapWithIndex;
  let mapReverse = Vector.mapReverse;
  let mapReverseWithIndex = Vector.mapReverseWithIndex;
  let none = Vector.none;
  let noneWithIndex = Vector.noneWithIndex;
  let range = Vector.range;
  let reduce = Vector.reduce;
  let reduceWithIndex = Vector.reduceWithIndex;
  let reduceRight = Vector.reduceRight;
  let reduceRightWithIndex = Vector.reduceRightWithIndex;
  let removeAll = Vector.removeAll;
  let removeAt = Vector.removeAt;
  let removeFirst = Vector.removeFirst;
  let removeLast = Vector.removeLast;
  let return = Vector.return;
  let reverse = Vector.reverse;
  let skip = Vector.skip;
  let some = Vector.some;
  let someWithIndex = Vector.someWithIndex;
  let take = Vector.take;
  let toKeyed = Vector.toKeyed;
  let toSeq = Vector.toSeq;
  let toSeqReversed = Vector.toSeqReversed;
  let tryFind = Vector.tryFind;
  let tryFindWithIndex = Vector.tryFindWithIndex;
  let tryFirst = Vector.tryFirst;
  let tryGet = Vector.tryGet;
  let tryIndexOf = Vector.tryIndexOf;
  let tryIndexOfWithIndex = Vector.tryIndexOfWithIndex;
  let tryLast = Vector.tryLast;
  let update = Vector.update;
  let updateAll = Vector.updateAll;
  let updateWith = Vector.updateWith;
};

let test = describe "Vector" (List.fromSeqReversed @@ Seq.concat @@ [
  (VectorTester.test 10 (module Vector)) |> List.toSeq,
  (VectorTester.test 48 (module Vector)) |> List.toSeq,
  (VectorTester.test 90 (module Vector)) |> List.toSeq,
  (VectorTester.test 500 (module Vector)) |> List.toSeq,
  (VectorTester.test 5000 (module Vector)) |> List.toSeq,
  (VectorTester.test 50000 (module Vector)) |> List.toSeq,
  describe "TransientVector" (List.fromSeqReversed @@ Seq.concat @@ [
    (TransientVectorTester.test 10 (module TransientVector)) |> List.toSeq,
    (TransientVectorTester.test 48 (module TransientVector)) |> List.toSeq,
    (TransientVectorTester.test 90 (module TransientVector)) |> List.toSeq,
    (TransientVectorTester.test 500 (module TransientVector)) |> List.toSeq,
    (TransientVectorTester.test 5000 (module TransientVector)) |> List.toSeq,
    (TransientVectorTester.test 50000 (module TransientVector)) |> List.toSeq,
  ]) |> Seq.return,
]);
