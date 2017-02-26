open Collection;
open Comparator;
open Equality;
open Functions;
open Functions.Operators;
open Hash;
open Keyed;
open Option.Operators;
open Ordering;
open Seq;

type avlTreeSet 'a =
  | Empty
  | Leaf 'a
  | Node int (avlTreeSet 'a) 'a (avlTreeSet 'a);

let module AVLTreeSet = {
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

  let rec forEach (f: 'a => unit) (tree: avlTreeSet 'a): 'acc => switch tree {
    | Empty  => ()
    | Leaf v => f v
    | Node _ left v right =>
       forEach f left;
       f v;
       forEach f right;
  };

  let rec maxValue (tree: avlTreeSet 'a): 'a => switch tree {
    | Leaf v => v
    | Node _ _ v Empty => v
    | Node _ _ _ right => maxValue right
  };

  let rec minValue (tree: avlTreeSet 'a): 'a => switch tree {
    | Leaf v => v
    | Node _ Empty v _ => v
    | Node _ left _ _ => minValue left
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

  let rec removeMax (tree: avlTreeSet 'a): (avlTreeSet 'a) => switch tree {
    | Empty => Empty
    | Leaf v => Empty
    | Node _ left v Empty => left
    | Node _ left v right => rebalance left v (removeMax right);
  };

  let rec removeMin (tree: avlTreeSet 'a): (avlTreeSet 'a) => switch tree {
    | Empty => Empty
    | Leaf v => Empty
    | Node _ Empty v right => right
    | Node _ left v right => rebalance (removeMin left) v right;
  };

  let rec remove (comparator: comparator 'a) (x: 'a) (tree: avlTreeSet 'a): (avlTreeSet 'a) => switch tree {
    | Empty => Empty
    | Leaf v => if (x === v) Empty else {
        let cmp = comparator x v;
        if (cmp === Equal) Empty else tree
      }
    | Node _ left v right => if (x === v) (switch (left, right) {
        | (Empty, _) => right
        | (_, Empty) => left
        | _ => rebalance left (minValue right) (removeMin right)
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
          | _ => rebalance left (minValue right) (removeMin right)
        }
      }
  };

  let rec search (predicate: 'a => ordering) (tree: avlTreeSet 'a): 'a => switch tree {
    | Empty => failwith "not found"
    | Leaf v =>
        let result = predicate v;
        if (result === Equal) v
        else (failwith "not found")
    | Node _ left v right =>
        let result = predicate v;
        if (result === LessThan) (search predicate left)
        else if (result === GreaterThan) (search predicate right)
        else  v
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

  let rec tryMaxValue (tree: avlTreeSet 'a): (option 'a) => switch tree {
    | Empty => None
    | Leaf v => Some v
    | Node _ _ v Empty => Some v
    | Node _ _ _ right => tryMaxValue right
  };

  let rec tryMinValue (tree: avlTreeSet 'a): (option 'a) => switch tree {
    | Empty => None
    | Leaf v => Some v
    | Node _ Empty v _ => Some v
    | Node _ left _ _ => tryMinValue left
  };

  let rec trySearch (predicate: 'a => ordering) (tree: avlTreeSet 'a): (option 'a) => switch tree {
    | Empty => None
    | Leaf v =>
        let result = predicate v;
        if (result === Equal) (Some v) else None
    | Node _ left v right =>
        let result = predicate v;
        if (result === LessThan) (trySearch predicate left)
        else if (result === GreaterThan) (trySearch predicate right)
        else Some v
  };
};

type sortedSet 'a = {
  comparator: comparator 'a,
  count: int,
  tree: avlTreeSet 'a,
};

let add (x: 'a) ({ comparator, count, tree } as sortedSet: sortedSet 'a): (sortedSet 'a) => {
  let newTree = tree |> AVLTreeSet.add comparator x;
  if (newTree === tree) sortedSet else { comparator, count: count + 1, tree: newTree }
};

let addAll (seq: seq 'a) (sortedSet: sortedSet 'a): (sortedSet 'a) => seq
  |> Seq.reduce (fun acc next => acc |> add next) sortedSet;

let contains (x: 'a) ({ comparator, tree }: sortedSet 'a): bool =>
  AVLTreeSet.contains comparator x tree;

let count ({ count }: sortedSet 'a): int => count;

let empty: sortedSet 'a = { comparator: Comparator.structural, count: 0, tree: Empty };

let emptyWith (comparator: comparator 'a): (sortedSet 'a) => ({ comparator, count: 0, tree: Empty });

let isEmpty ({ count }: sortedSet 'a): bool => count == 0;

let isNotEmpty ({ count }: sortedSet 'a): bool => count != 0;

let fromSeq (seq: seq 'a): (sortedSet 'a) =>
  empty |> addAll seq;

let fromSeqWith (comparator: comparator 'a) (seq: seq 'a): (sortedSet 'a) =>
  emptyWith comparator |> addAll seq;

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) ({ tree }: sortedSet 'a): 'acc =>
  tree |> AVLTreeSet.reduce f acc;

let reduceRight (f: 'acc => 'a => 'acc) (acc: 'acc) ({ tree }: sortedSet 'a): 'acc =>
  tree |> AVLTreeSet.reduceRight f acc;

let remove (x: 'a) ({ comparator, count, tree } as sortedSet: sortedSet 'a): (sortedSet 'a) => {
  let newTree = AVLTreeSet.remove comparator x tree;
  if (newTree === tree) sortedSet else { comparator, count: count - 1, tree: newTree }
};

let removeAll ({ comparator }: sortedSet 'a): (sortedSet 'a) =>
  emptyWith comparator;

let removeMax ({ comparator, count, tree } as sortedSet: sortedSet 'a): (sortedSet 'a) => {
  let newTree = AVLTreeSet.removeMax tree;
  if (newTree === tree) sortedSet else { comparator, count: count - 1, tree: newTree }
};

let removeMin ({ comparator, count, tree } as sortedSet: sortedSet 'a): (sortedSet 'a) => {
  let newTree = AVLTreeSet.removeMin tree;
  if (newTree === tree) sortedSet else { comparator, count: count - 1, tree: newTree }
};

let search (predicate: 'a => ordering) ({ tree }: sortedSet 'a): 'a =>
  tree |> AVLTreeSet.search predicate;

let toSeq ({ tree }: sortedSet 'a): (seq 'a) =>
  tree |> AVLTreeSet.toSeq;

let compareWith (comparator: comparator 'a) (this: sortedSet 'a) (that: sortedSet 'a): ordering =>
  Seq.compareWith comparator (toSeq this) (toSeq that);

let compare (this: sortedSet 'a) (that: sortedSet 'a): ordering =>
  compareWith Comparator.structural this that;

let equalsWith (equals: equality 'a) (this: sortedSet 'a) (that: sortedSet 'a): bool =>
  Seq.equalsWith equals (toSeq this) (toSeq that);

let equals (this: sortedSet 'a) (that: sortedSet 'a): bool =>
  equalsWith Equality.structural this that;

let every (f: 'a => bool) (set: sortedSet 'a): bool =>
  set |> toSeq |> Seq.every f;

let find (f: 'a => bool) (set: sortedSet 'a): 'a =>
  set |> toSeq |> Seq.find f;

let forEach (f: 'a => unit) ({ tree }: sortedSet 'a) =>
  tree |> AVLTreeSet.forEach f;

let hashWith (hash: (hash 'a)) (set: sortedSet 'a): int => set
  |> reduce (Hash.reducer hash) Hash.initialValue;

let hash (set: sortedSet 'a): int =>
  hashWith Hash.structural set;

let maxValue ({ tree }: sortedSet 'a): 'a =>
  AVLTreeSet.maxValue tree;

let minValue ({ tree }: sortedSet 'a): 'a =>
  AVLTreeSet.minValue tree;

let none (f: 'a => bool) (set: sortedSet 'a): bool =>
  set |> toSeq |> Seq.none f;

let some (f: 'a => bool) (set: sortedSet 'a): bool =>
  set |> toSeq |> Seq.some f;

let tryFind (f: 'a => bool) (set: sortedSet 'a): (option 'a) =>
  set |> toSeq |> Seq.tryFind f;

let tryMaxValue ({ tree }: sortedSet 'a): (option 'a) =>
  AVLTreeSet.tryMaxValue tree;

let tryMinValue ({ tree }: sortedSet 'a): (option 'a) =>
  AVLTreeSet.tryMinValue tree;

let trySearch (predicate: 'a => ordering) ({ tree }: sortedSet 'a): (option 'a) =>
  tree |> AVLTreeSet.trySearch predicate;

let toCollection (set: sortedSet 'a): (collection 'a) => {
  contains: fun a => contains a set,
  count: count set,
  every: fun f => every f set,
  find: fun f => find f set,
  forEach: fun f => forEach f set,
  none: fun f => none f set,
  reduce: fun f acc => reduce f acc set,
  some: fun f => some f set,
  toSeq: toSeq set,
  tryFind: fun f => tryFind f set,
};

let toKeyed (set: sortedSet 'a): (keyed 'a 'a) => {
  containsWith: fun equals k v => set |> contains k ? equals k v : false,
  containsKey: fun k => set |> contains k,
  count: count set,
  every: fun f => set |> every (fun k => f k k),
  find: fun f => {
    let k = set |> find (fun k => f k k);
    (k, k)
  },
  forEach: fun f => set |> forEach (fun k => f k k),
  get: fun k => set |> contains k ? k : failwith "not found",
  none: fun f => set |> none (fun k => f k k),
  reduce: fun f acc => set |> reduce (fun acc k => f acc k k) acc,
  some: fun f => set |> some (fun k => f k k),
  toSeq: toSeq set |> Seq.map (fun k => (k, k)),
  tryFind: fun f => set |> tryFind (fun k => f k k) >>| (fun k => (k, k)),
  tryGet: fun k => set |> contains k ? Some k : None,
  values: toSeq set,
};

let intersect ({ comparator } as this: sortedSet 'a) (that: sortedSet 'a): (sortedSet 'a) =>
  Collection.intersect (toCollection this) (toCollection that) |> fromSeqWith comparator;

let subtract ({ comparator } as this: sortedSet 'a) (that: sortedSet 'a): (sortedSet 'a) =>
  Collection.subtract (toCollection this) (toCollection that) |> fromSeqWith comparator;

let union ({ comparator } as this: sortedSet 'a) (that: sortedSet 'a): (sortedSet 'a) =>
  Collection.union (toCollection this) (toCollection that) |> fromSeqWith comparator;
