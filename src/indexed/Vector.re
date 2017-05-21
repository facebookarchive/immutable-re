/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

include PersistentVector;

let mutate = TransientVector.mutate;

let init (count: int) (f: int => 'a): (t 'a) => IntRange.create start::0 count::count
  |> IntRange.reduce while_::Functions.alwaysTrue2 (fun acc next =>
      acc |> TransientVector.addLast (f next)) (mutate (empty ())
    )
  |> TransientVector.persist;

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
    let firstLeaf = ref IndexedTrie.Empty;
    let middle = IndexedTrie.removeFirstLeaf
      IndexedTrie.updateLevelPersistent
      Transient.Owner.none
      firstLeaf
      middle;
    let (IndexedTrie.Leaf _ left) = !firstLeaf;

    { left, middle, right }
  }
  else if (skipCount - leftCount < middleCount) {
    let skipCount = skipCount - leftCount;
    let left = ref [||];
    let middle = IndexedTrie.skip Transient.Owner.none skipCount left middle;
    { left: !left, middle, right }
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
    let right = ref [||];
    let middle = IndexedTrie.take Transient.Owner.none takeCount right middle;
    { left, middle, right: !right }
  }
  else if (takeCount - leftCount === middleCount) {
    let lastLeaf = ref IndexedTrie.Empty;
    let middle = IndexedTrie.removeLastLeaf
      IndexedTrie.updateLevelPersistent
      Transient.Owner.none
      lastLeaf
      middle;
    let (IndexedTrie.Leaf _ right) = !lastLeaf;

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
  |> TransientVector.updateAll f
  |> TransientVector.persist;

let concat (vectors: list (t 'a)): (t 'a) => switch vectors {
  /* FIXME: This is a trivial O(N) implementation. The underlying vector
   * is an RRB tree, and can support implementation of efficient log32N
   * concatenation.
   */
  | [] => empty ()
  | [ vector ] => vector
  | [head, ...tail] =>
    tail
      |> ImmList.reduce
        (fun acc next => reduce while_::Functions.alwaysTrue2
          (fun acc next => acc |> TransientVector.addLast next)
          acc
          next
        )
        (mutate head)
      |> TransientVector.persist;
};

let insertAt (index: int) (value: 'a) (vec: t 'a): (t 'a) => {
  /* FIXME: This is a trivial O(N) implementation. The underlying vector
   * is an RRB tree, and can support implementation of efficient log32N
   * concatenation.
   */
  let start = vec |> take index |> addLast value;
  concat [start, (vec |> skip index)];
};

let removeAt (index: int) (vec: t 'a): (t 'a) => {
  /* FIXME: This is a trivial O(N) implementation. The underlying vector
   * is an RRB tree, and can support implementation of efficient log32N
   * concatenation.
   */
  let start = vec |> take index;
  let end_ = vec |> skip (index + 1);
  concat [start, end_];
};
