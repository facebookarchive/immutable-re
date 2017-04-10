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

let rec first (tree: t 'k 'v): (option ('k, 'v)) => switch tree {
  | Empty => None
  | Leaf k v => Some (k, v)
  | Node _ Empty k v _ => Some (k, v)
  | Node _ left _ _ _ => first left
};

let rec firstOrRaise (tree: t 'k 'v): ('k, 'v) => switch tree {
  | Leaf k v => (k, v)
  | Node _ Empty k v _ => (k, v)
  | Node _ left _ _ _ => firstOrRaise left
  | Empty => failwith "empty"
};

let rec firstKey (tree: t 'k 'v): (option 'k) => switch tree {
  | Empty => None
  | Leaf k _ => Some k
  | Node _ Empty k _ _ => Some k
  | Node _ left _ _ _ => firstKey left
};

let rec firstKeyOrRaise (tree: t 'k 'v): 'k => switch tree {
  | Leaf k v => k
  | Node _ Empty k v _ => k
  | Node _ left _ _ _ => firstKeyOrRaise left
  | Empty => failwith "empty"
};

let rec firstValue (tree: t 'k 'v): (option 'v) => switch tree {
  | Empty => None
  | Leaf _ v => Some v
  | Node _ Empty _ v _ => Some v
  | Node _ left _ _ _ => firstValue left
};

let rec firstValueOrRaise (tree: t 'k 'v): 'v => switch tree {
  | Leaf _ v => v
  | Node _ Empty _ v _ => v
  | Node _ left _ _ _ => firstValueOrRaise left
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

let rec last (tree: t 'k 'v): (option ('k, 'v)) => switch tree {
  | Empty => None
  | Leaf k v  => Some (k, v)
  | Node _ _ k v Empty => Some (k, v)
  | Node _ _ _ _ right => last right
};

let rec lastOrRaise (tree: t 'k 'v): ('k, 'v) => switch tree {
  | Leaf k v => (k, v)
  | Node _ _ k v Empty => (k, v)
  | Node _ _ _ _ right => lastOrRaise right
  | Empty => failwith "empty"
};

let rec lastKey (tree: t 'k 'v): (option 'k) => switch tree {
  | Empty => None
  | Leaf k _  => Some k
  | Node _ _ k _ Empty => Some k
  | Node _ _ _ _ right => lastKey right
};

let rec lastKeyOrRaise (tree: t 'k 'v): 'k => switch tree {
  | Leaf k v => k
  | Node _ _ k v Empty => k
  | Node _ _ _ _ right => lastKeyOrRaise right
  | Empty => failwith "empty"
};

let rec lastValue (tree: t 'k 'v): (option 'v) => switch tree {
  | Empty => None
  | Leaf _ v  => Some v
  | Node _ _ _ v Empty => Some v
  | Node _ _ _ _ right => lastValue right
};

let rec lastValueOrRaise (tree: t 'k 'v): 'v => switch tree {
  | Leaf _ v => v
  | Node _ _ _ v Empty => v
  | Node _ _ _ _ right => lastValueOrRaise right
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

let rec reduceReversed (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) (tree: t 'k 'v): 'acc => switch tree {
  | Empty => acc
  | Leaf k v => f acc k v
  | Node _ left k v right =>
     let acc = reduceReversed f acc right;
     let acc = f acc k v;
     let acc = reduceReversed f acc left;
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

let rec toKeySequence (tree: t 'k 'v): (Sequence.t 'k) => switch tree {
  | Empty => Sequence.empty ()
  | Leaf k v => Sequence.return k
  | Node _ left k _ right => Sequence.concat [
      Sequence.defer(fun () => toKeySequence left),
      Sequence.return k,
      Sequence.defer(fun () => toKeySequence right),
    ]
};

let rec toKeySequenceReversed (tree: t 'k 'v): (Sequence.t 'k) => switch tree {
  | Empty => Sequence.empty ()
  | Leaf k _ => Sequence.return k
  | Node _ left k _ right => Sequence.concat [
      Sequence.defer(fun () => toKeySequenceReversed right),
      Sequence.return k,
      Sequence.defer(fun () => toKeySequenceReversed left),
    ]
};

let rec toSequence (tree: t 'k 'v): (Sequence.t ('k, 'v)) => switch tree {
  | Empty => Sequence.empty ()
  | Leaf k v => Sequence.return (k, v)
  | Node _ left k v right => Sequence.concat [
      Sequence.defer(fun () => toSequence left),
      Sequence.return (k, v),
      Sequence.defer(fun () => toSequence right),
    ]
};

let rec toSequenceReversed (tree: t 'k 'v): (Sequence.t ('k, 'v)) => switch tree {
  | Empty => Sequence.empty ()
  | Leaf k v => Sequence.return (k, v)
  | Node _ left k v right => Sequence.concat [
      Sequence.defer(fun () => toSequenceReversed right),
      Sequence.return (k, v),
      Sequence.defer(fun () => toSequenceReversed left),
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
        let newReversed = alter comparator result xK f right;
        switch !result {
        | AlterResult.Added => rebalance left k v newReversed
        | AlterResult.NoChange => tree
        | AlterResult.Removed => Node height left k v newReversed
        | AlterResult.Replace => Node height left k v newReversed
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
              let (k, v) = firstOrRaise right;
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
        let newReversed = put comparator xK xV right;
        if (newReversed === right) tree else rebalance left k v newReversed
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
        let newReversed = putWithResult comparator result xK xV right;
        if (newReversed === right) tree else rebalance left k v newReversed
      } else if (xV === v) tree
      else {
        result := AlterResult.Replace;
        Node height left xK xV right
      }
};
