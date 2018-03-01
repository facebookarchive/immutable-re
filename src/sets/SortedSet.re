/***
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
module Make = (Comparable: Comparable.S) => {
  type a = Comparable.t;
  type t = {
    count: int,
    tree: AVLTreeSet.t(a)
  };
  let comparator = Comparable.compare;
  let contains = (x: a, {tree}: t) : bool => AVLTreeSet.contains(comparator, x, tree);
  let count = ({count}: t) : int => count;
  let firstOrRaise = ({tree}: t) : a => AVLTreeSet.firstOrRaise(tree);
  let lastOrRaise = ({tree}: t) : a => AVLTreeSet.lastOrRaise(tree);
  let reduce =
      (~while_ as predicate: ('acc, a) => bool, f: ('acc, a) => 'acc, acc: 'acc, {tree}: t)
      : 'acc =>
    AVLTreeSet.reduce(~while_=predicate, f, acc, tree);
  let reduceReversed =
      (~while_ as predicate: ('acc, a) => bool, f: ('acc, a) => 'acc, acc: 'acc, {tree}: t)
      : 'acc =>
    AVLTreeSet.reduceReversed(~while_=predicate, f, acc, tree);
  let toSequence = ({tree}: t) : Sequence.t(a) => tree |> AVLTreeSet.toSequence;
  let toSequenceReversed = ({tree}: t) : Sequence.t(a) => tree |> AVLTreeSet.toSequenceReversed;
  let add = (x: a, {count, tree} as sortedSet: t) : t => {
    let newTree = tree |> AVLTreeSet.add(comparator, x);
    if (newTree === tree) {
      sortedSet
    } else {
      {count: count + 1, tree: newTree}
    }
  };

  let compare = (this: t, that: t) : Ordering.t =>
    Sequence.compareWith(comparator, toSequence(this), toSequence(that));
  let emptyInstance: t = {count: 0, tree: AVLTreeSet.Empty};
  let empty = () : t => emptyInstance;
  let equality = Comparator.toEquality(comparator);
  let equals = (this: t, that: t) : bool =>
    Sequence.equalsWith(equality, toSequence(this), toSequence(that));
  let remove = (x: a, {count, tree} as sortedSet: t) : t => {
    let newTree = AVLTreeSet.remove(comparator, x, tree);
    if (newTree === tree) {
      sortedSet
    } else {
      {count: count - 1, tree: newTree}
    }
  };
  let removeAll = (_: t) : t => emptyInstance;
  let removeFirstOrRaise = ({count, tree}: t) : t => {
    let newTree = AVLTreeSet.removeFirstOrRaise(tree);
    {count: count - 1, tree: newTree}
  };
  let removeLastOrRaise = ({count, tree}: t) : t => {
    let newTree = AVLTreeSet.removeLastOrRaise(tree);
    {count: count - 1, tree: newTree}
  };
};
