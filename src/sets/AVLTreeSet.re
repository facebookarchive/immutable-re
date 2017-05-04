/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'a =
  | Empty
  | Leaf 'a
  | Node int (t 'a) 'a (t 'a);

let height (tree: t 'a): int => switch tree {
  | Empty => 0
  | Leaf _ => 1
  | Node h _ _ _=> h
};

let makeTree (left: t 'a) (v: 'a) (right: t 'a): (t 'a) => {
  let lh = height left;
  let rh = height right;

  switch (left, right) {
    | (Empty, Empty) => Leaf v
    | _ =>
      let h = if (lh >= rh) { lh + 1 } else { rh + 1 };
      Node h left v right;
  }
};

let maxHeightDiff = 2;

let rebalance (left: t 'a) (v: 'a) (right: t 'a): (t 'a) => {
  let lh = height left;
  let rh = height right;

  switch (left, right) {
    | (Node _ ll lv lr, _) when lh > (rh + maxHeightDiff) => switch lr {
        | Node lrh lrl lrv lrr when (height ll) < lrh =>
            makeTree (makeTree ll lv lrl) lrv (makeTree lrr v right)
        | _ => makeTree ll lv (makeTree lr v right)
      }
    | (_, Node _ rl rv rr) when rh > (lh + maxHeightDiff) => switch rl {
        | Node rlh rll rlv rlr when (height rr) < rlh =>
            makeTree (makeTree left v rll) rlv (makeTree rlr rv rr)
        | _ => makeTree (makeTree left v rl) rv rr
      }
    | _ => switch (left, right) {
        | (Empty, Empty) => Leaf v
        | _ =>
          let h = if (lh >= rh) { lh + 1 } else { rh + 1 };
          Node h left v right
      }
  };
};

let rec add (comparator: Comparator.t 'a) (x: 'a) (tree: t 'a): (t 'a) => switch tree {
  | Empty => Leaf x
  | Leaf v =>
      let cmp = comparator x v;
      if (cmp === Ordering.lessThan) {
        Node 2 Empty x tree
      } else if (cmp === Ordering.greaterThan) {
        Node 2 tree x Empty
      } else tree
  | Node _ left v right =>
      let cmp = comparator x v;
      if (cmp === Ordering.lessThan) {
        let newLeft = add comparator x left;
        if (newLeft === left) tree else rebalance newLeft v right
      } else if (cmp === Ordering.greaterThan) {
        let newRight = add comparator x right;
        if (newRight === right) tree else rebalance left v newRight
      } else tree
};

let rec contains (comparator: Comparator.t 'a) (x: 'a) (tree: t 'a): bool => switch tree {
  | Empty => false
  | Leaf v => if (x === v) true else {
      let cmp = comparator x v;
      cmp === Ordering.equal
    }
  | Node _ left v right => if (x === v) true else {
      let cmp = comparator x v;
      if (cmp === Ordering.lessThan) (contains comparator x left)
      else if (cmp === Ordering.greaterThan) (contains comparator x right)
      else true
    }
};

let rec firstOrRaise (tree: t 'a): 'a => switch tree {
  | Leaf v => v
  | Node _ Empty v _ => v
  | Node _ left _ _ => firstOrRaise left
  | Empty => failwith "empty"
};

let rec lastOrRaise (tree: t 'a): 'a => switch tree {
  | Leaf v => v
  | Node _ _ v Empty => v
  | Node _ _ _ right => lastOrRaise right
  | Empty => failwith "empty"
};

let rec reduceWhile
    (predicate: 'acc => 'a => bool)
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (tree: t 'a): 'acc => switch tree {
  | Empty => acc
  | Leaf v => if (predicate acc v) (f acc v) else acc
  | Node _ left v right =>
     let acc = reduceWhile predicate f acc left;
     if (predicate acc v)  {
       let acc = f acc v;
       reduceWhile predicate f acc right;
     }
     else acc;
};

let reduce
    while_::(predicate: 'acc => 'a => bool)
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (tree: t 'a): 'acc =>
  if (predicate === Functions.alwaysTrue2) {
    reduceWhile Functions.alwaysTrue2 f acc tree
  }
  else {
    let shouldContinue = ref true;
    let predicate acc v =>
      if (!shouldContinue) {
        let result = predicate acc v;
        shouldContinue := result;
        result;
      }
      else false;

    reduceWhile predicate f acc tree;
  };

let rec reduceReversedWhile
    (predicate: 'acc => 'a => bool)
    (f: 'acc => 'a=> 'acc)
    (acc: 'acc)
    (tree: t 'a): 'acc => switch tree {
  | Empty => acc
  | Leaf v =>  if (predicate acc v) (f acc v) else acc
  | Node _ left v right =>
     let acc = reduceReversedWhile predicate f acc right;
     if (predicate acc v) {
       let acc = f acc v;
       reduceReversedWhile predicate f acc left;
     }
     else acc;
};

let reduceReversed
    while_::(predicate: 'acc => 'a => bool)
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (tree: t 'a): 'acc =>
  if (predicate === Functions.alwaysTrue2) {
    reduceReversedWhile Functions.alwaysTrue2 f acc tree
  }
  else {
    let shouldContinue = ref true;
    let predicate acc v =>
      if (!shouldContinue) {
        let result = predicate acc v;
        shouldContinue := result;
        result;
      }
      else false;
    reduceReversedWhile predicate f acc tree;
  };

let rec removeFirstOrRaise (tree: t 'a): (t 'a) => switch tree {
  | Empty => failwith "empty"
  | Leaf _ => Empty
  | Node _ Empty _ right => right
  | Node _ left v right => rebalance (removeFirstOrRaise left) v right;
};

let rec removeFirstOrRaiseWithValue (first: ref 'a) (tree: t 'a): (t 'a) => switch tree {
  | Empty => failwith "empty"
  | Leaf v =>
      first := v;
      Empty
  | Node _ Empty v right =>
      first := v;
      right
  | Node _ left v right => rebalance (removeFirstOrRaiseWithValue first left) v right;
};

let rec removeLastOrRaise (tree: t 'a): (t 'a) => switch tree {
  | Empty => failwith "empty"
  | Leaf _ => Empty
  | Node _ left _ Empty => left
  | Node _ left v right => rebalance left v (removeLastOrRaise right);
};

let rec remove (comparator: Comparator.t 'a) (x: 'a) (tree: t 'a): (t 'a) => switch tree {
  | Empty => Empty
  | Leaf v => if (x === v) Empty else {
      let cmp = comparator x v;
      if (cmp === Ordering.equal) Empty else tree
    }
  | Node height left v right => if (x === v) (switch (left, right) {
      | (Empty, _) => right
      | (_, Empty) => left
      | _ => rebalance left (firstOrRaise right) (removeFirstOrRaise right)
    }) else {
      let cmp = comparator x v;
      if (cmp === Ordering.lessThan) {
        let newLeft = remove comparator x left;
        if (newLeft === left) tree else rebalance newLeft v right
      } else if (cmp === Ordering.greaterThan) {
        let newRight = remove comparator x right;
        if (newRight === right) tree else rebalance left v newRight
      } else switch (left, right) {
        | (Empty, _) => right
        | (_, Empty) => left
        | _ =>
            if (height > 4) {
              let first = ref x;
              let newRight = removeFirstOrRaiseWithValue first right;
              rebalance left (!first) newRight
            } else rebalance left (firstOrRaise right) (removeFirstOrRaise right)
      }
    }
};

let rec toSequence (tree: t 'a): (Sequence.t 'a) => switch tree {
  | Empty => Sequence.empty ()
  | Leaf v => Sequence.return v
  | Node _ left v right => Sequence.concat [
      Sequence.defer(fun () => toSequence left),
      Sequence.return v,
      Sequence.defer(fun () => toSequence right),
    ]
};

let rec toSequenceReversed (tree: t 'a): (Sequence.t 'a) => switch tree {
  | Empty => Sequence.empty ()
  | Leaf v => Sequence.return v
  | Node _ left v right => Sequence.concat [
      Sequence.defer(fun () => toSequenceReversed right),
      Sequence.return v,
      Sequence.defer(fun () => toSequenceReversed left),
    ]
};
