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

let contains
    (comparator: Comparator.t 'k)
    (equality: Equality.t 'v)
    (xK: 'k)
    (xV: 'v)
    (tree: t 'k 'v): bool => switch tree {
  | Empty => false
  | Leaf k v => if (xK === k) true else {
      let cmp = comparator xK k;
      (cmp === Ordering.equal) && (equality v xV)
    }
  | Node _ left k v right => if (xK === k) true else {
      let cmp = comparator xK k;
      if (cmp === Ordering.lessThan) (containsKey comparator xK left)
      else if (cmp === Ordering.greaterThan) (containsKey comparator xK right)
      else (equality v xV)
    }
};

let rec every (f: 'k => 'v => bool) (tree: t 'k 'v) => switch tree {
  | Empty => true
  | Leaf k v => f k v
  | Node _ left k v right =>
      (every f left) && (f k v) && (every f right)
};

let rec find (f: 'k => 'v => bool) (tree: t 'k 'v): (option ('k, 'v)) =>  switch tree {
  | Empty => None
  | Leaf k v => if (f k v) (Some (k, v)) else None;
  | Node _ left k v right => switch (find f left) {
      | Some _ as result => result
      | None => if (f k v) (Some (k, v)) else find f right
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

let rec forEach (f: 'k => 'v => unit) (tree: t 'k 'v) => switch tree {
  | Empty  => ()
  | Leaf k v => f k v
  | Node _ left k v right =>
     forEach f left;
     f k v;
     forEach f right;
};

let rec forEachRight (f: 'k => 'v => unit) (tree: t 'k 'v) => switch tree {
  | Empty  => ()
  | Leaf k v => f k v
  | Node _ left k v right =>
     forEachRight f right;
     f k v;
     forEachRight f left;
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

let rec none (f: 'k => 'v => bool) (tree: t 'k 'v) => switch tree {
  | Empty => true
  | Leaf k v => f k v |> not
  | Node _ left k v right =>
      (none f left) && (f k v |> not) && (none f right)
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

let rec reduceRight (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) (tree: t 'k 'v): 'acc => switch tree {
  | Empty => acc
  | Leaf k v => f acc k v
  | Node _ left k v right =>
     let acc = reduceRight f acc right;
     let acc = f acc k v;
     let acc = reduceRight f acc left;
     acc
};

let rec some (f: 'k => 'v => bool) (tree: t 'k 'v) => switch tree {
  | Empty => false
  | Leaf k v => f k v
  | Node _ left k v right =>
      (some f left) || (f k v) || (none f right)
};

let rec toSequence (tree: t 'k 'v): (Sequence.t ('k, 'v)) => switch tree {
  | Empty => Sequence.empty
  | Leaf k v => Sequence.return (k, v)
  | Node _ left k v right => Sequence.concat [
      Sequence.defer(fun () => toSequence left),
      Sequence.return (k, v),
      Sequence.defer(fun () => toSequence right),
    ]
};

let rec toSequenceRight (tree: t 'k 'v): (Sequence.t ('k, 'v)) => switch tree {
  | Empty => Sequence.empty
  | Leaf k v => Sequence.return (k, v)
  | Node _ left k v right => Sequence.concat [
      Sequence.defer(fun () => toSequenceRight right),
      Sequence.return (k, v),
      Sequence.defer(fun () => toSequenceRight left),
    ]
};

let rec values (tree: t 'k 'v): (Iterator.t 'v) => switch tree {
  | Empty => Iterator.empty
  | Leaf _ v => Iterator.return v
  | Node _ left _ v right => Iterator.concat [
      values left,
      Iterator.return v,
      values right,
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

type alterResult =
  | Added
  | NoChange
  | Removed
  | Replace;

let rec alter
    (comparator: Comparator.t 'k)
    (result: ref alterResult)
    (xK: 'k)
    (f: (option 'v) => (option 'v))
    (tree: t 'k 'v): (t 'k 'v) => switch tree {
  | Empty => switch (f None) {
      | None =>
          result := NoChange;
          tree;
      | Some v =>
          result := Added;
          Leaf xK v;
    }
  | Leaf k v =>
      let cmp = comparator xK k;
      if (cmp === Ordering.lessThan) (switch (f None) {
        | None =>
            result := NoChange;
            tree;
        | Some xV =>
            result := Added;
            Node 2 Empty xK xV tree
      })
      else if (cmp === Ordering.greaterThan) (switch (f None) {
        | None =>
            result := NoChange;
            tree;
        | Some xV =>
            result := Added;
            Node 2 tree xK xV Empty
      })
      else (switch (f @@ Option.return @@ v) {
        | None =>
            result := Removed;
            Empty;
        | Some xV =>
            result := Replace;
            Leaf xK xV;
      })
  | Node height left k v right =>
      let cmp = comparator xK k;
      if (cmp === Ordering.lessThan) {
        let newLeft = alter comparator result xK f left;
        switch !result {
          | Added => rebalance newLeft k v right
          | NoChange => tree
          | Removed => rebalance newLeft k v right
          | Replace => Node height newLeft k v right
        }
      }
      else if (cmp === Ordering.greaterThan) {
        let newRight = alter comparator result xK f right;
        switch !result {
        | Added => rebalance left k v newRight
        | NoChange => tree
        | Removed => Node height left k v newRight
        | Replace => Node height left k v newRight
      }}
      else (switch (f @@ Option.return @@ v) {
        | None => switch (left, right) {
            | (Empty, _) =>
                result := Removed;
                right
            | (_, Empty) =>
                result := Removed;
                left
            | _ =>
              result := Removed;
              let (k, v) = firstOrRaise right;
              rebalance left k v (removeFirstOrRaise right);
          }
        | Some xV =>
            result := Replace;
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
    (result: ref alterResult)
    (xK: 'k)
    (xV: 'v)
    (tree: t 'k 'v): (t 'k 'v) => switch tree {
  | Empty =>
      result := Added;
      Leaf xK xV
  | Leaf k v =>
      let cmp = comparator xK k;

      if (cmp === Ordering.lessThan) {
        result := Added;
        Node 2 Empty xK xV tree
      }
      else if (cmp === Ordering.greaterThan) {
        result := Added;
        Node 2 tree xK xV Empty
      }
      else if (xV === v) tree
      else {
        result := Replace;
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
        result := Replace;
        Node height left xK xV right
      }
};
