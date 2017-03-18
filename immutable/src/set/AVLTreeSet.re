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

let rec validate (tree: t 'a): (t 'a) => switch tree {
  | Empty => tree
  | Leaf _ => tree
  | Node _ left _ right =>
      let lh = height left;
      let rh = height right;
      if ((lh - rh) > 2 || (lh - rh) < -2) (failwith "invalid")
      else {
        validate left |> ignore;
        validate right |> ignore;
        tree
      };
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

let rec first (tree: t 'a): 'a => switch tree {
  | Leaf v => v
  | Node _ Empty v _ => v
  | Node _ left _ _ => first left
  | Empty => failwith "empty"
};

let rec forEach (f: 'a => unit) (tree: t 'a) => switch tree {
  | Empty  => ()
  | Leaf v => f v
  | Node _ left v right =>
     forEach f left;
     f v;
     forEach f right;
};

let rec forEachRight (f: 'a => unit) (tree: t 'a) => switch tree {
  | Empty  => ()
  | Leaf v => f v
  | Node _ left v right =>
     forEachRight f right;
     f v;
     forEachRight f left;
};

let rec last (tree: t 'a): 'a => switch tree {
  | Leaf v => v
  | Node _ _ v Empty => v
  | Node _ _ _ right => last right
  | Empty => failwith "empty"
};

let rec reduce (f: 'acc => 'a => 'acc) (acc: 'acc) (tree: t 'a): 'acc => switch tree {
  | Empty  => acc
  | Leaf v => f acc v
  | Node _ left v right =>
     let acc = reduce f acc left;
     let acc = f acc v;
     let acc = reduce f acc right;
     acc
};

let rec reduceRight (f: 'acc => 'a => 'acc) (acc: 'acc) (tree: t 'a): 'acc => switch tree {
  | Empty => acc
  | Leaf v => f acc v
  | Node _ left v right =>
     let acc = reduceRight f acc right;
     let acc = f acc v;
     let acc = reduceRight f acc left;
     acc
};

let rec removeFirst (tree: t 'a): (t 'a) => switch tree {
  | Empty => Empty
  | Leaf _ => Empty
  | Node _ Empty _ right => right
  | Node _ left v right => rebalance (removeFirst left) v right;
};

let rec removeFirstWithValue (first: ref 'a) (tree: t 'a): (t 'a) => switch tree {
  | Empty => Empty
  | Leaf v =>
      first := v;
      Empty
  | Node _ Empty v right =>
      first := v;
      right
  | Node _ left v right => rebalance (removeFirstWithValue first left) v right;
};

let rec removeLast (tree: t 'a): (t 'a) => switch tree {
  | Empty => Empty
  | Leaf _ => Empty
  | Node _ left _ Empty => left
  | Node _ left v right => rebalance left v (removeLast right);
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
      | _ => rebalance left (first right) (removeFirst right)
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
              let newRight = removeFirstWithValue first right;
              rebalance left (!first) newRight
            } else rebalance left (first right) (removeFirst right)
      }
    }
};

let rec toSequence (tree: t 'a): (Sequence.t 'a) => switch tree {
  | Empty => Sequence.empty
  | Leaf v => Sequence.return v
  | Node _ left v right => Sequence.concat [
      Sequence.defer(fun () => toSequence left),
      Sequence.return v,
      Sequence.defer(fun () => toSequence right),
    ]
};

let rec toSequenceRight (tree: t 'a): (Sequence.t 'a) => switch tree {
  | Empty => Sequence.empty
  | Leaf v => Sequence.return v
  | Node _ left v right => Sequence.concat [
      Sequence.defer(fun () => toSequenceRight right),
      Sequence.return v,
      Sequence.defer(fun () => toSequenceRight left),
    ]
};

let rec tryFirst (tree: t 'a): (option 'a) => switch tree {
  | Empty => None
  | Leaf v => Some v
  | Node _ Empty v _ => Some v
  | Node _ left _ _ => tryFirst left
};

let rec tryLast (tree: t 'a): (option 'a) => switch tree {
  | Empty => None
  | Leaf v => Some v
  | Node _ _ v Empty => Some v
  | Node _ _ _ right => tryLast right
};
