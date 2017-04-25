/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

include PersistentVector;

let module Transient = TransientVector;
 
let mutate = Transient.mutate;

let addFirstAll (iter: Iterable.t 'a) (vec: t 'a): (t 'a) => vec
  |> mutate
  |> Transient.addFirstAll iter
  |> Transient.persist;

let addLastAll (iter: Iterable.t 'a) (vec: t 'a): (t 'a) => vec
  |> mutate
  |> Transient.addLastAll iter
  |> Transient.persist;

let from (iter: Iterable.t 'a): (t 'a) =>
  empty () |> addLastAll iter;

let fromReverse (iter: Iterable.t 'a): (t 'a) =>
  empty () |> addFirstAll iter;

let init (count: int) (f: int => 'a): (t 'a) => IntRange.create start::0 count::count
  |> IntRange.reduce (fun acc next =>
      acc |> Transient.addLast (f next)) (mutate (empty ())
    )
  |> Transient.persist;

let return (value: 'a): (t 'a) =>
  empty () |> addLast value;

let skip (skipCount: int) ({ left, middle, right } as vec: t 'a): (t 'a) => {
  let vectorCount = count vec;
  let leftCount = CopyOnWriteArray.count left;
  let middleCount = IndexedTrie.count middle;

  if (skipCount >= vectorCount) (empty ())
  else if (skipCount <= 0) vec
  else if (skipCount < leftCount) {
    left: left |> CopyOnWriteArray.skip skipCount,
    middle,
    right,
  }
  else if (skipCount === leftCount) {
    let (IndexedTrie.Leaf _ left, middle) = IndexedTrie.removeFirstLeafUsingMutator
      IndexedTrie.updateLevelPersistent
      Transient.Owner.none
      middle;

    { left, middle, right }
  }
  else if (skipCount - leftCount < middleCount) {
    let skipCount = skipCount - leftCount;
    let (left, middle) = IndexedTrie.skip Transient.Owner.none skipCount middle;
    { left, middle, right }
  }
  else {
    let skipCount = skipCount - leftCount - middleCount;
    {
      left:  right |> CopyOnWriteArray.skip skipCount,
      middle: IndexedTrie.empty,
      right: [||],
    }
  }
};

let take (takeCount: int) ({ left, middle, right } as vec: t 'a): (t 'a) => {
  let vectorCount = count vec;
  let leftCount = CopyOnWriteArray.count left;
  let middleCount = IndexedTrie.count middle;

  if (takeCount >= vectorCount) vec
  else if (takeCount <= leftCount) {
    left: left |> CopyOnWriteArray.take takeCount,
    middle: IndexedTrie.empty,
    right: [||],
  }
  else if (takeCount - leftCount < middleCount) {
    let takeCount = takeCount - leftCount;
    let (middle, right) = IndexedTrie.take Transient.Owner.none takeCount middle;
    { left, middle, right }
  }
  else if (takeCount - leftCount === middleCount) {
    let (middle, IndexedTrie.Leaf _ right) = IndexedTrie.removeLastLeafUsingMutator
      IndexedTrie.updateLevelPersistent
      Transient.Owner.none
      middle;

    { left, middle, right }
  }
  else {
    let takeCount = takeCount - leftCount - middleCount;
    { left, middle, right: right |> CopyOnWriteArray.take takeCount }
  }
};

let slice start::(start: int)=0 end_::(end_: option int)=? (vec: t 'a): (t 'a) => {
  let vecCount = count vec;

  let end_ = switch end_ {
    | Some end_ => end_
    | None => vecCount
  };

  let start = if (start < 0) (start + vecCount) else start;
  let end_ = if (end_ < 0) (end_ + vecCount) else end_;

  let skipCount = start;
  let takeCount = max (end_ - start) 0;

  if (skipCount === 0 && takeCount === vecCount) vec
  else if (takeCount === 0) (empty ())
  else vec |> skip skipCount |> take takeCount;
};

let updateAll (f: int => 'a => 'a) (vec: t 'a): (t 'a) => vec
  |> mutate
  |> Transient.updateAll f
  |> Transient.persist;

/* Unimplemented functions */
let concat (vectors: list (t 'a)): (t 'a) =>
  failwith "Not Implemented";

let insertAt (index: int) (value: 'a) (vec: t 'a): (t 'a) =>
  failwith "Not Implemented";

let removeAt (index: int) (vec: t 'a): (t 'a) =>
  failwith "Not Implemented";
