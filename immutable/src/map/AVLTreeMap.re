open Set;
open Comparator;
open Equality;
open Hash;
open Map;
open Ordering;
open Seq;

type avlTreeMap 'k 'v =
  | Empty
  | Leaf 'k 'v
  | Node int (avlTreeMap 'k 'v) 'k 'v (avlTreeMap 'k 'v);

let height (tree: avlTreeMap 'k 'v): int => switch tree {
  | Empty => 0
  | Leaf _ => 1
  | Node h _ _ _ _=> h
};

let rec validate (tree: avlTreeMap 'k 'v): (avlTreeMap 'k 'v) => switch tree {
  | Empty => tree
  | Leaf _ _ => tree
  | Node h left _ _ right =>
      let lh = height left;
      let rh = height right;
      (lh - rh) > 2 || (lh - rh) < -2 ? failwith "invalid" : {
        validate left |> ignore;
        validate right |> ignore;
        tree
      };
};

let makeTree (left: avlTreeMap 'k 'v) (k: 'k) (v: 'v) (right: avlTreeMap 'k 'v): (avlTreeMap 'k 'v) => {
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
    (comparator: comparator 'k)
    (xK: 'k)
    (tree: avlTreeMap 'k 'v): bool => switch tree {
  | Empty => false
  | Leaf k v => if (xK === k) true else {
      let cmp = comparator xK k;
      cmp === Equal
    }
  | Node _ left k v right => if (xK === k) true else {
      let cmp = comparator xK k;
      if (cmp === LessThan) (containsKey comparator xK left)
      else if (cmp === GreaterThan) (containsKey comparator xK right)
      else true
    }
};

let rec contains
    (comparator: comparator 'k)
    (equality: equality 'v)
    (xK: 'k)
    (xV: 'v)
    (tree: avlTreeMap 'k 'v): bool => switch tree {
  | Empty => false
  | Leaf k v => if (xK === k) true else {
      let cmp = comparator xK k;
      (cmp === Equal) && (equality v xV)
    }
  | Node _ left k v right => if (xK === k) true else {
      let cmp = comparator xK k;
      if (cmp === LessThan) (containsKey comparator xK left)
      else if (cmp === GreaterThan) (containsKey comparator xK right)
      else (equality v xV)
    }
};

let rec every (f: 'k => 'v => bool) (tree: avlTreeMap 'k 'v) => switch tree {
  | Empty => true
  | Leaf k v => f k v
  | Node _ left k v right =>
      (every f left) && (f k v) && (every f right)
};

let rec forEach (f: 'k => 'v => unit) (tree: avlTreeMap 'k 'v) => switch tree {
  | Empty  => ()
  | Leaf k v => f k v
  | Node _ left k v right =>
     forEach f left;
     f k v;
     forEach f right;
};

let rec first (tree: avlTreeMap 'k 'v): ('k, 'v) => switch tree {
  | Leaf k v => (k, v)
  | Node _ Empty k v _ => (k, v)
  | Node _ left _ _ _ => first left
};

let rec get
    (comparator: comparator 'k)
    (xK: 'k)
    (tree: avlTreeMap 'k 'v): 'v => switch tree {
  | Empty => failwith "Not found"
  | Leaf k v => if (xK === k) v else {
      let cmp = comparator xK k;
      if (cmp === Equal) v
      else (failwith "Not found")
    }
  | Node _ left k v right => if (xK === k) v else {
      let cmp = comparator xK k;
      if (cmp === LessThan) (get comparator xK left)
      else if (cmp === GreaterThan) (get comparator xK right)
      else v
    }
};

let rec last (tree: avlTreeMap 'k 'v): ('k, 'v) => switch tree {
  | Leaf k v => (k, v)
  | Node _ _ k v Empty => (k, v)
  | Node _ _ _ _ right => last right
};

let rec none (f: 'k => 'v => bool) (tree: avlTreeMap 'k 'v) => switch tree {
  | Empty => true
  | Leaf k v => f k v |> not
  | Node _ left k v right =>
      (none f left) && (f k v |> not) && (none f right)
};

let rec reduce (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) (tree: avlTreeMap 'k 'v): 'acc => switch tree {
  | Empty  => acc
  | Leaf k v => f acc k v
  | Node _ left k v right =>
     let acc = reduce f acc left;
     let acc = f acc k v;
     let acc = reduce f acc right;
     acc
};

let rec reduceRight (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) (tree: avlTreeMap 'k 'v): 'acc => switch tree {
  | Empty => acc
  | Leaf k v => f acc k v
  | Node _ left k v right =>
     let acc = reduceRight f acc right;
     let acc = f acc k v;
     let acc = reduceRight f acc left;
     acc
};

let rec search (predicate: 'k => ordering) (tree: avlTreeMap 'k 'v): ('k, 'v) => switch tree {
  | Empty => failwith "not found"
  | Leaf k v =>
      let result = predicate k;
      if (result === Equal) (k, v)
      else (failwith "not found")
  | Node _ left k v right =>
      let result = predicate v;
      if (result === LessThan) (search predicate left)
      else if (result === GreaterThan) (search predicate right)
      else (k, v)
};

let rec some (f: 'k => 'v => bool) (tree: avlTreeMap 'k 'v) => switch tree {
  | Empty => false
  | Leaf k v => f k v
  | Node _ left k v right =>
      (some f left) || (f k v) || (none f right)
};

let rec toSeq (tree: avlTreeMap 'k 'v): (seq ('k, 'v)) => switch tree {
  | Empty => Seq.empty
  | Leaf k v => Seq.return (k, v)
  | Node _ left k v right => Seq.concat [
      Seq.defer(fun () => toSeq left),
      Seq.return (k, v),
      Seq.defer(fun () => toSeq right),
    ]
};

let rec tryFind (f: 'k => 'v => bool) (tree: avlTreeMap 'k 'v): (option ('k, 'v)) =>  switch tree {
  | Empty => None
  | Leaf k v => if (f k v) (Some (k, v)) else None;
  | Node _ left k v right => switch (tryFind f left) {
      | Some _ as result => result
      | None => if (f k v) (Some (k, v)) else tryFind f right
    }
};

let rec tryFirst (tree: avlTreeMap 'k 'v): (option ('k, 'v)) => switch tree {
  | Empty => None
  | Leaf k v => Some (k, v)
  | Node _ Empty k v _ => Some (k, v)
  | Node _ left _ _ _ => tryFirst left
};

let rec tryGet
    (comparator: comparator 'k)
    (xK: 'k)
    (tree: avlTreeMap 'k 'v): (option 'v) => switch tree {
  | Empty => None
  | Leaf k v => if (xK === k) (Some v) else {
      let cmp = comparator xK k;
      if (cmp === Equal) (Some v)
      else None
    }
  | Node _ left k v right => if (xK === k) (Some v) else {
      let cmp = comparator xK k;
      if (cmp === LessThan) (tryGet comparator xK left)
      else if (cmp === GreaterThan) (tryGet comparator xK right)
      else (Some v)
    }
};

let rec tryLast (tree: avlTreeMap 'k 'v): (option ('k, 'v)) => switch tree {
  | Empty => None
  | Leaf k v  => Some (k, v)
  | Node _ _ k v Empty => Some (k, v)
  | Node _ _ _ _ right => tryLast right
};

let rec trySearch (predicate: 'k => ordering) (tree: avlTreeMap 'k 'v): (option ('k, 'v)) => switch tree {
  | Empty => None
  | Leaf k v =>
      let result = predicate v;
      if (result === Equal) (Some (k, v)) else None
  | Node _ left k v right =>
      let result = predicate k;
      if (result === LessThan) (trySearch predicate left)
      else if (result === GreaterThan) (trySearch predicate right)
      else Some (k, v)
};

let rec values (tree: avlTreeMap 'k 'v): (seq 'v) => switch tree {
  | Empty => Seq.empty
  | Leaf _ v => Seq.return v
  | Node _ left _ v right => Seq.concat [
      Seq.defer(fun () => values left),
      Seq.return v,
      Seq.defer(fun () => values right),
    ]
};

let maxHeightDiff = 2;

let rebalance (left: avlTreeMap 'k 'v) (k: 'k) (v: 'v) (right: avlTreeMap 'k 'v): (avlTreeMap 'k 'v) => {
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

let rec removeFirst (tree: avlTreeMap 'k 'v): (avlTreeMap 'k 'v) => switch tree {
  | Empty => Empty
  | Leaf _ _ => Empty
  | Node _ Empty k v right => right
  | Node _ left k v right => rebalance (removeFirst left) k v right;
};

let rec removeLast (tree: avlTreeMap 'k 'v): (avlTreeMap 'k 'v) => switch tree {
  | Empty => Empty
  | Leaf _ _ => Empty
  | Node _ left k v Empty => left
  | Node _ left k v right => rebalance left k v (removeLast right);
};

type alterResult =
  | Added
  | NoChange
  | Removed
  | Replace;

let rec alter
    (comparator: comparator 'k)
    (result: ref alterResult)
    (xK: 'k)
    (f: (option 'v) => (option 'v))
    (tree: avlTreeMap 'k 'v): (avlTreeMap 'k 'v) => switch tree {
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
      if (cmp === LessThan) (switch (f None) {
        | None =>
            result := NoChange;
            tree;
        | Some xV =>
            result := Added;
            Node 2 Empty xK xV tree
      })
      else if (cmp === GreaterThan) (switch (f None) {
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
      if (cmp === LessThan) {
        let newLeft = alter comparator result xK f left;
        switch !result {
          | Added => rebalance newLeft k v right
          | NoChange => tree
          | Removed => rebalance newLeft k v right
          | Replace => Node height newLeft k v right
        }
      }
      else if (cmp === GreaterThan) {
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
              let (k, v) = first right;
              rebalance left k v (removeFirst right);
          }
        | Some xV =>
            result := Replace;
            Node height left xK xV right;
      })
};

let rec put
    (comparator: comparator 'k)
    (xK: 'k)
    (xV: 'v)
    (tree: avlTreeMap 'k 'v): (avlTreeMap 'k 'v) => switch tree {
  | Empty => Leaf xK xV
  | Leaf k v =>
      let cmp = comparator xK k;

      if (cmp === LessThan) (Node 2 Empty xK xV tree)
      else if (cmp === GreaterThan) (Node 2 tree xK xV Empty)
      else if (xV === v) tree
      else (Leaf xK xV)
  | Node height left k v right =>
      let cmp = comparator xK k;

      if (cmp === LessThan) {
        let newLeft = put comparator xK xV left;
        if (newLeft === left) tree else rebalance newLeft k v right
      } else if (cmp === GreaterThan) {
        let newRight = put comparator xK xV right;
        if (newRight === right) tree else rebalance left k v newRight
      } else if (xV === v) tree
      else (Node height left xK xV right)
};

let rec putWithResult
    (comparator: comparator 'k)
    (result: ref alterResult)
    (xK: 'k)
    (xV: 'v)
    (tree: avlTreeMap 'k 'v): (avlTreeMap 'k 'v) => switch tree {
  | Empty =>
      result := Added;
      Leaf xK xV
  | Leaf k v =>
      let cmp = comparator xK k;

      if (cmp === LessThan) {
        result := Added;
        Node 2 Empty xK xV tree
      }
      else if (cmp === GreaterThan) {
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

      if (cmp === LessThan) {
        let newLeft = putWithResult comparator result xK xV left;
        if (newLeft === left) tree else rebalance newLeft k v right
      } else if (cmp === GreaterThan) {
        let newRight = putWithResult comparator result xK xV right;
        if (newRight === right) tree else rebalance left k v newRight
      } else if (xV === v) tree
      else {
        result := Replace;
        Node height left xK xV right
      }
};
