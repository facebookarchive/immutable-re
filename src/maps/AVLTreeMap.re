/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'k 'v =
  | Empty
  | Leaf 'k 'v
  | Node int (t 'k 'v) 'k 'v (t 'k 'v);

let height (tree: t 'k 'v): int => switch tree {
  | Empty => 0
  | Leaf _ => 1
  | Node h _ _ _ _=> h
};

let rec validate (tree: t 'k 'v): (t 'k 'v) => switch tree {
  | Empty => tree
  | Leaf _ _ => tree
  | Node _ left _ _ right =>
      let lh = height left;
      let rh = height right;
      if ((lh - rh) > 2 || (lh - rh) < -2) (failwith "invalid")
      else {
        validate left |> ignore;
        validate right |> ignore;
        tree
      };
};

let makeTree (left: t 'k 'v) (k: 'k) (v: 'v) (right: t 'k 'v): (t 'k 'v) => {
  let lh = height left;
  let rh = height right;

  switch (left, right) {
    | (Empty, Empty) => Leaf k v
    | _ =>
      let h = if (lh >= rh) { lh + 1 } else { rh + 1 };
      Node h left k v right;
  }
};

let rec containsKey
    (comparator: Comparator.t 'k)
    (xK: 'k)
    (tree: t 'k 'v): bool => switch tree {
  | Empty => false
  | Leaf k _ => if (xK === k) true else {
      let cmp = comparator xK k;
      cmp === Ordering.equal
    }
  | Node _ left k _ right => if (xK === k) true else {
      let cmp = comparator xK k;
      if (cmp === Ordering.lessThan) (containsKey comparator xK left)
      else if (cmp === Ordering.greaterThan) (containsKey comparator xK right)
      else true
    }
};

let rec firstOrRaise (selector: 'k => 'v => 'c) (tree: t 'k 'v): 'c => switch tree {
  | Leaf k v => (selector k v)
  | Node _ Empty k v _ => (selector k v)
  | Node _ left _ _ _ => firstOrRaise selector left
  | Empty => failwith "empty"
};

let rec get
    (comparator: Comparator.t 'k)
    (xK: 'k)
    (tree: t 'k 'v): (option 'v) => switch tree {
  | Empty => None
  | Leaf k v => if (xK === k) (Some v) else {
      let cmp = comparator xK k;
      if (cmp === Ordering.equal) (Some v)
      else None
    }
  | Node _ left k v right => if (xK === k) (Some v) else {
      let cmp = comparator xK k;
      if (cmp === Ordering.lessThan) (get comparator xK left)
      else if (cmp === Ordering.greaterThan) (get comparator xK right)
      else (Some v)
    }
};

let rec getOrDefault
    (comparator: Comparator.t 'k)
    default::(default: 'v)
    (xK: 'k)
    (tree: t 'k 'v): 'v => switch tree {
  | Empty => default
  | Leaf k v => if (xK === k) v else {
      let cmp = comparator xK k;
      if (cmp === Ordering.equal) v
      else default
    }
  | Node _ left k v right => if (xK === k) v else {
      let cmp = comparator xK k;
      if (cmp === Ordering.lessThan) (getOrDefault comparator ::default xK left)
      else if (cmp === Ordering.greaterThan) (getOrDefault comparator ::default xK right)
      else v
    }
};

let rec getOrRaise
    (comparator: Comparator.t 'k)
    (xK: 'k)
    (tree: t 'k 'v): 'v => switch tree {
  | Empty => failwith "Not found"
  | Leaf k v => if (xK === k) v else {
      let cmp = comparator xK k;
      if (cmp === Ordering.equal) v
      else (failwith "Not found")
    }
  | Node _ left k v right => if (xK === k) v else {
      let cmp = comparator xK k;
      if (cmp === Ordering.lessThan) (getOrRaise comparator xK left)
      else if (cmp === Ordering.greaterThan) (getOrRaise comparator xK right)
      else v
    }
};

let rec lastOrRaise (selector: 'k => 'v => 'c) (tree: t 'k 'v): 'c => switch tree {
  | Leaf k v => (selector k v)
  | Node _ _ k v Empty => (selector k v)
  | Node _ _ _ _ right => lastOrRaise selector right
  | Empty => failwith "empty"
};

let rec reduce (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) (tree: t 'k 'v): 'acc => switch tree {
  | Empty  => acc
  | Leaf k v => f acc k v
  | Node _ left k v right =>
     let acc = reduce f acc left;
     let acc = f acc k v;
     let acc = reduce f acc right;
     acc
};

let rec reduceReversed (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) (tree: t 'k 'v): 'acc => switch tree {
  | Empty => acc
  | Leaf k v => f acc k v
  | Node _ left k v right =>
     let acc = reduceReversed f acc right;
     let acc = f acc k v;
     let acc = reduceReversed f acc left;
     acc
};

let rec reduceKeys (f: 'acc => 'k => 'acc) (acc: 'acc) (tree: t 'k 'v): 'acc => switch tree {
  | Empty  => acc
  | Leaf k _ => f acc k
  | Node _ left k _ right =>
     let acc = reduceKeys f acc left;
     let acc = f acc k;
     let acc = reduceKeys f acc right;
     acc
};

let rec reduceKeysReversed (f: 'acc => 'k => 'acc) (acc: 'acc) (tree: t 'k 'v): 'acc => switch tree {
  | Empty  => acc
  | Leaf k _ => f acc k
  | Node _ left k _ right =>
     let acc = reduceKeysReversed f acc right;
     let acc = f acc k;
     let acc = reduceKeysReversed f acc left;
     acc
};

let rec reduceValues (f: 'acc => 'v => 'acc) (acc: 'acc) (tree: t 'k 'v): 'acc => switch tree {
  | Empty  => acc
  | Leaf _ v => f acc v
  | Node _ left _ v right =>
     let acc = reduceValues f acc left;
     let acc = f acc v;
     let acc = reduceValues f acc right;
     acc
};

let rec reduceValuesReversed (f: 'acc => 'v => 'acc) (acc: 'acc) (tree: t 'k 'v): 'acc => switch tree {
  | Empty  => acc
  | Leaf _ v => f acc v
  | Node _ left _ v right =>
     let acc = reduceValuesReversed f acc right;
     let acc = f acc v;
     let acc = reduceValuesReversed f acc left;
     acc
};

let rec reduceWhileWithResult
    (shouldContinue: ref bool)
    (predicate: 'acc => 'k => 'v => bool)
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    (tree: t 'k 'v): 'acc => switch tree {
  | Empty => acc
  | Leaf k v =>
      if (!shouldContinue && (predicate acc k v)) (f acc k v)
      else acc
  | Node _ left k v right =>
     let acc =
       if (!shouldContinue) (reduceWhileWithResult shouldContinue predicate f acc left)
       else acc;
     let acc =
       if (!shouldContinue && (predicate acc k v)) (f acc k v)
       else acc;
     let acc =
       if (!shouldContinue) (reduceWhileWithResult shouldContinue predicate f acc right)
       else acc;
     acc
};

let rec reduceReversedWhileWithResult
    (shouldContinue: ref bool)
    (predicate: 'acc => 'k => 'v => bool)
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    (tree: t 'k 'v): 'acc => switch tree {
  | Empty => acc
  | Leaf k v =>
      if (!shouldContinue && (predicate acc k v)) (f acc k v)
      else acc
  | Node _ left k v right =>
    let acc =
      if (!shouldContinue) (reduceReversedWhileWithResult shouldContinue predicate f acc right)
      else acc;
     let acc =
       if (!shouldContinue && (predicate acc k v)) (f acc k v)
       else acc;
     let acc =
       if (!shouldContinue) (reduceReversedWhileWithResult shouldContinue predicate f acc left)
       else acc;
     acc
};

let reduceWhile
    (predicate: 'acc => 'k => 'v => bool)
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    (tree: t 'k 'v): 'acc => {

  let shouldContinue = ref true;
  let predicate acc k v => {
    let result = predicate acc k v;
    shouldContinue := result;
    result;
  };
  reduceWhileWithResult shouldContinue predicate f acc tree;
};

let reduceReversedWhile
    (predicate: 'acc => 'k => 'v => bool)
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    (tree: t 'k 'v): 'acc => {

  let shouldContinue = ref true;
  let predicate acc k v => {
    let result = predicate acc k v;
    shouldContinue := result;
    result;
  };
  reduceReversedWhileWithResult shouldContinue predicate f acc tree;
};

let reduceKeysWhile
    (predicate: 'acc => 'k => bool)
    (f: 'acc => 'k => 'acc)
    (acc: 'acc)
    (tree: t 'k 'v): 'acc => {

  let shouldContinue = ref true;
  let predicate acc k _ => {
    let result = predicate acc k;
    shouldContinue := result;
    result;
  };
  let f acc k _ => f acc k;
  reduceWhileWithResult shouldContinue predicate f acc tree;
};

let reduceKeysReversedWhile
    (predicate: 'acc => 'k => bool)
    (f: 'acc => 'k => 'acc)
    (acc: 'acc)
    (tree: t 'k 'v): 'acc => {

  let shouldContinue = ref true;
  let predicate acc k _ => {
    let result = predicate acc k;
    shouldContinue := result;
    result;
  };
  let f acc k _ => f acc k;
  reduceReversedWhileWithResult shouldContinue predicate f acc tree;
};

let reduceValuesWhile
    (predicate: 'acc => 'v => bool)
    (f: 'acc => 'v => 'acc)
    (acc: 'acc)
    (tree: t 'k 'v): 'acc => {

  let shouldContinue = ref true;
  let predicate acc _ v => {
    let result = predicate acc v;
    shouldContinue := result;
    result;
  };
  let f acc _ v => f acc v;
  reduceWhileWithResult shouldContinue predicate f acc tree;
};

let reduceValuesReversedWhile
    (predicate: 'acc => 'v => bool)
    (f: 'acc => 'v => 'acc)
    (acc: 'acc)
    (tree: t 'k 'v): 'acc => {

  let shouldContinue = ref true;
  let predicate acc _ v => {
    let result = predicate acc v;
    shouldContinue := result;
    result;
  };
  let f acc _ v => f acc v;
  reduceReversedWhileWithResult shouldContinue predicate f acc tree;
};

let rec toSequence (selector: 'k => 'v => 'c) (tree: t 'k 'v): (Sequence.t 'c) => switch tree {
  | Empty => Sequence.empty ()
  | Leaf k v => Sequence.return (selector k v)
  | Node _ left k v right => Sequence.concat [
      Sequence.defer(fun () => toSequence selector left),
      Sequence.return (selector k v),
      Sequence.defer(fun () => toSequence selector right),
    ]
};

let rec toSequenceReversed (selector: 'k => 'v => 'c) (tree: t 'k 'v): (Sequence.t 'c) => switch tree {
  | Empty => Sequence.empty ()
  | Leaf k v => Sequence.return (selector k v)
  | Node _ left k v right => Sequence.concat [
      Sequence.defer(fun () => toSequenceReversed selector right),
      Sequence.return (selector k v),
      Sequence.defer(fun () => toSequenceReversed selector left),
    ]
};

let maxHeightDiff = 2;

let rebalance (left: t 'k 'v) (k: 'k) (v: 'v) (right: t 'k 'v): (t 'k 'v) => {
  let lh = height left;
  let rh = height right;

  switch (left, right) {
    | (Node _ ll lk lv lr, _) when lh > (rh + maxHeightDiff) => switch lr {
        | Node lrh lrl lrk lrv lrr when (height ll) < lrh =>
            makeTree (makeTree ll lk lv lrl) lrk lrv (makeTree lrr k v right)
        | _ => makeTree ll lk lv (makeTree lr k v right)
      }
    | (_, Node _ rl rk rv rr) when rh > (lh + maxHeightDiff) => switch rl {
        | Node rlh rll rlk rlv rlr when (height rr) < rlh =>
            makeTree (makeTree left k v rll) rlk rlv (makeTree rlr rk rv rr)
        | _ => makeTree (makeTree left k v rl) rk rv rr
      }
    | _ => switch (left, right) {
        | (Empty, Empty) => Leaf k v
        | _ =>
          let h = if (lh >= rh) { lh + 1 } else { rh + 1 };
          Node h left k v right
      }
  };
};

let rec removeFirstOrRaise (tree: t 'k 'v): (t 'k 'v) => switch tree {
  | Empty => failwith "empty"
  | Leaf _ _ => Empty
  | Node _ Empty _ _ right => right
  | Node _ left k v right => rebalance (removeFirstOrRaise left) k v right;
};

let rec removeLastOrRaise (tree: t 'k 'v): (t 'k 'v) => switch tree {
  | Empty => failwith "empty"
  | Leaf _ _ => Empty
  | Node _ left _ _ Empty => left
  | Node _ left k v right => rebalance left k v (removeLastOrRaise right);
};

let rec alter
    (comparator: Comparator.t 'k)
    (result: ref AlterResult.t)
    (xK: 'k)
    (f: (option 'v) => (option 'v))
    (tree: t 'k 'v): (t 'k 'v) => switch tree {
  | Empty => switch (f None) {
      | None =>
          result := AlterResult.NoChange;
          tree;
      | Some v =>
          result := AlterResult.Added;
          Leaf xK v;
    }
  | Leaf k v =>
      let cmp = comparator xK k;
      if (cmp === Ordering.lessThan) (switch (f None) {
        | None =>
            result := AlterResult.NoChange;
            tree;
        | Some xV =>
            result := AlterResult.Added;
            Node 2 Empty xK xV tree
      })
      else if (cmp === Ordering.greaterThan) (switch (f None) {
        | None =>
            result := AlterResult.NoChange;
            tree;
        | Some xV =>
            result := AlterResult.Added;
            Node 2 tree xK xV Empty
      })
      else (switch (f @@ Option.return @@ v) {
        | None =>
            result := AlterResult.Removed;
            Empty;
        | Some xV =>
            result := AlterResult.Replace;
            Leaf xK xV;
      })
  | Node height left k v right =>
      let cmp = comparator xK k;
      if (cmp === Ordering.lessThan) {
        let newLeft = alter comparator result xK f left;
        switch !result {
          | AlterResult.Added => rebalance newLeft k v right
          | AlterResult.NoChange => tree
          | AlterResult.Removed => rebalance newLeft k v right
          | AlterResult.Replace => Node height newLeft k v right
        }
      }
      else if (cmp === Ordering.greaterThan) {
        let newRight = alter comparator result xK f right;
        switch !result {
        | AlterResult.Added => rebalance left k v newRight
        | AlterResult.NoChange => tree
        | AlterResult.Removed => Node height left k v newRight
        | AlterResult.Replace => Node height left k v newRight
      }}
      else (switch (f @@ Option.return @@ v) {
        | None => switch (left, right) {
            | (Empty, _) =>
                result := AlterResult.Removed;
                right
            | (_, Empty) =>
                result := AlterResult.Removed;
                left
            | _ =>
              result := AlterResult.Removed;
              let (k, v) = firstOrRaise Functions.pairify right;
              rebalance left k v (removeFirstOrRaise right);
          }
        | Some xV =>
            result := AlterResult.Replace;
            Node height left xK xV right;
      })
};

let rec put
    (comparator: Comparator.t 'k)
    (xK: 'k)
    (xV: 'v)
    (tree: t 'k 'v): (t 'k 'v) => switch tree {
  | Empty => Leaf xK xV
  | Leaf k v =>
      let cmp = comparator xK k;

      if (cmp === Ordering.lessThan) (Node 2 Empty xK xV tree)
      else if (cmp === Ordering.greaterThan) (Node 2 tree xK xV Empty)
      else if (xV === v) tree
      else (Leaf xK xV)
  | Node height left k v right =>
      let cmp = comparator xK k;

      if (cmp === Ordering.lessThan) {
        let newLeft = put comparator xK xV left;
        if (newLeft === left) tree else rebalance newLeft k v right
      } else if (cmp === Ordering.greaterThan) {
        let newRight = put comparator xK xV right;
        if (newRight === right) tree else rebalance left k v newRight
      } else if (xV === v) tree
      else (Node height left xK xV right)
};

let rec putWithResult
    (comparator: Comparator.t 'k)
    (result: ref AlterResult.t)
    (xK: 'k)
    (xV: 'v)
    (tree: t 'k 'v): (t 'k 'v) => switch tree {
  | Empty =>
      result := AlterResult.Added;
      Leaf xK xV
  | Leaf k v =>
      let cmp = comparator xK k;

      if (cmp === Ordering.lessThan) {
        result := AlterResult.Added;
        Node 2 Empty xK xV tree
      }
      else if (cmp === Ordering.greaterThan) {
        result := AlterResult.Added;
        Node 2 tree xK xV Empty
      }
      else if (xV === v) tree
      else {
        result := AlterResult.Replace;
        Leaf xK xV
      }
  | Node height left k v right =>
      let cmp = comparator xK k;

      if (cmp === Ordering.lessThan) {
        let newLeft = putWithResult comparator result xK xV left;
        if (newLeft === left) tree else rebalance newLeft k v right
      } else if (cmp === Ordering.greaterThan) {
        let newRight = putWithResult comparator result xK xV right;
        if (newRight === right) tree else rebalance left k v newRight
      } else if (xV === v) tree
      else {
        result := AlterResult.Replace;
        Node height left xK xV right
      }
};
