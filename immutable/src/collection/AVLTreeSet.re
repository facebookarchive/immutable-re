open Comparator;
open Ordering;
open Seq;

type avlTreeSet 'a =
  | Empty
  | Leaf 'a
  | Node int (avlTreeSet 'a) 'a (avlTreeSet 'a);

let height (tree: avlTreeSet 'a): int => switch tree {
  | Empty => 0
  | Leaf _ => 1
  | Node h _ _ _=> h
};

let rec validate (tree: avlTreeSet 'a): (avlTreeSet 'a) => switch tree {
  | Empty => tree
  | Leaf _ => tree
  | Node h left v right =>
      let lh = height left;
      let rh = height right;
      (lh - rh) > 2 || (lh - rh) < -2 ? failwith "invalid" : {
        validate left |> ignore;
        validate right |> ignore;
        tree
      };
};

let makeTree (left: avlTreeSet 'a) (v: 'a) (right: avlTreeSet 'a): (avlTreeSet 'a) => {
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

let rebalance (left: avlTreeSet 'a) (v: 'a) (right: avlTreeSet 'a): (avlTreeSet 'a) => {
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

let rec add (comparator: comparator 'a) (x: 'a) (tree: avlTreeSet 'a): (avlTreeSet 'a) => switch tree {
  | Empty => Leaf x
  | Leaf v =>
      let cmp = comparator x v;
      if (cmp === LessThan) {
        Node 2 Empty x tree
      } else if (cmp === GreaterThan) {
        Node 2 tree x Empty
      } else tree
  | Node height left v right =>
      let cmp = comparator x v;
      if (cmp === LessThan) {
        let newLeft = add comparator x left;
        if (newLeft === left) tree else rebalance newLeft v right
      } else if (cmp === GreaterThan) {
        let newRight = add comparator x right;
        if (newRight === right) tree else rebalance left v newRight
      } else tree
};

let rec contains (comparator: comparator 'a) (x: 'a) (tree: avlTreeSet 'a): bool => switch tree {
  | Empty => false
  | Leaf v => if (x === v) true else {
      let cmp = comparator x v;
      cmp === Equal
    }
  | Node _ left v right => if (x === v) true else {
      let cmp = comparator x v;
      if (cmp === LessThan) (contains comparator x left)
      else if (cmp === GreaterThan) (contains comparator x right)
      else true
    }
};

let rec first (tree: avlTreeSet 'a): 'a => switch tree {
  | Leaf v => v
  | Node _ Empty v _ => v
  | Node _ left _ _ => first left
};

let rec forEach (f: 'a => unit) (tree: avlTreeSet 'a) => switch tree {
  | Empty  => ()
  | Leaf v => f v
  | Node _ left v right =>
     forEach f left;
     f v;
     forEach f right;
};

let rec last (tree: avlTreeSet 'a): 'a => switch tree {
  | Leaf v => v
  | Node _ _ v Empty => v
  | Node _ _ _ right => last right
};

let rec reduce (f: 'acc => 'a => 'acc) (acc: 'acc) (tree: avlTreeSet 'a): 'acc => switch tree {
  | Empty  => acc
  | Leaf v => f acc v
  | Node _ left v right =>
     let acc = reduce f acc left;
     let acc = f acc v;
     let acc = reduce f acc right;
     acc
};

let rec reduceRight (f: 'acc => 'a => 'acc) (acc: 'acc) (tree: avlTreeSet 'a): 'acc => switch tree {
  | Empty => acc
  | Leaf v => f acc v
  | Node _ left v right =>
     let acc = reduceRight f acc right;
     let acc = f acc v;
     let acc = reduceRight f acc left;
     acc
};

let rec removeFirst (tree: avlTreeSet 'a): (avlTreeSet 'a) => switch tree {
  | Empty => Empty
  | Leaf v => Empty
  | Node _ Empty v right => right
  | Node _ left v right => rebalance (removeFirst left) v right;
};

let rec removeFirstWithValue (first: ref 'a) (tree: avlTreeSet 'a): (avlTreeSet 'a) => switch tree {
  | Empty => Empty
  | Leaf v =>
      first := v;
      Empty
  | Node _ Empty v right =>
      first := v;
      right
  | Node _ left v right => rebalance (removeFirstWithValue first left) v right;
};

let rec removeLast (tree: avlTreeSet 'a): (avlTreeSet 'a) => switch tree {
  | Empty => Empty
  | Leaf v => Empty
  | Node _ left v Empty => left
  | Node _ left v right => rebalance left v (removeLast right);
};

let rec remove (comparator: comparator 'a) (x: 'a) (tree: avlTreeSet 'a): (avlTreeSet 'a) => switch tree {
  | Empty => Empty
  | Leaf v => if (x === v) Empty else {
      let cmp = comparator x v;
      if (cmp === Equal) Empty else tree
    }
  | Node height left v right => if (x === v) (switch (left, right) {
      | (Empty, _) => right
      | (_, Empty) => left
      | _ => rebalance left (first right) (removeFirst right)
    }) else {
      let cmp = comparator x v;
      if (cmp === LessThan) {
        let newLeft = remove comparator x left;
        if (newLeft === left) tree else rebalance newLeft v right
      } else if (cmp === GreaterThan) {
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

let rec toSeq (tree: avlTreeSet 'a): (seq 'a) => switch tree {
  | Empty => Seq.empty
  | Leaf v => Seq.return v
  | Node _ left v right => Seq.concat [
      Seq.defer(fun () => toSeq left),
      Seq.return v,
      Seq.defer(fun () => toSeq right),
    ]
};

let rec tryFirst (tree: avlTreeSet 'a): (option 'a) => switch tree {
  | Empty => None
  | Leaf v => Some v
  | Node _ Empty v _ => Some v
  | Node _ left _ _ => tryFirst left
};

let rec tryLast (tree: avlTreeSet 'a): (option 'a) => switch tree {
  | Empty => None
  | Leaf v => Some v
  | Node _ _ v Empty => Some v
  | Node _ _ _ right => tryLast right
};