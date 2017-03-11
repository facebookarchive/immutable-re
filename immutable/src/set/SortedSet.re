open Functions.Operators;
open Option.Operators;

type t 'a = {
  comparator: Comparator.t 'a,
  count: int,
  tree: AVLTreeSet.t 'a,
};

let add (x: 'a) ({ comparator, count, tree } as sortedSet: t 'a): (t 'a) => {
  let newTree = tree |> AVLTreeSet.add comparator x;
  if (newTree === tree) sortedSet else { comparator, count: count + 1, tree: newTree }
};

let addAll (seq: Seq.t 'a) (sortedSet: t 'a): (t 'a) => seq
  |> Seq.reduce (fun acc next => acc |> add next) sortedSet;

let contains (x: 'a) ({ comparator, tree }: t 'a): bool =>
  AVLTreeSet.contains comparator x tree;

let count ({ count }: t 'a): int => count;

let empty: t 'a = { comparator: Comparator.structural, count: 0, tree: Empty };

let emptyWith (comparator: Comparator.t 'a): (t 'a) => ({ comparator, count: 0, tree: Empty });

let isEmpty ({ count }: t 'a): bool => count == 0;

let isNotEmpty ({ count }: t 'a): bool => count != 0;

let fromSeq (seq: Seq.t 'a): (t 'a) =>
  empty |> addAll seq;

let fromSeqWith (comparator: Comparator.t 'a) (seq: Seq.t 'a): (t 'a) =>
  emptyWith comparator |> addAll seq;

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) ({ tree }: t 'a): 'acc =>
  tree |> AVLTreeSet.reduce f acc;

let reduceRight (f: 'acc => 'a => 'acc) (acc: 'acc) ({ tree }: t 'a): 'acc =>
  tree |> AVLTreeSet.reduceRight f acc;

let remove (x: 'a) ({ comparator, count, tree } as sortedSet: t 'a): (t 'a) => {
  let newTree = AVLTreeSet.remove comparator x tree;
  if (newTree === tree) sortedSet else { comparator, count: count - 1, tree: newTree }
};

let removeAll ({ comparator }: t 'a): (t 'a) =>
  emptyWith comparator;

let removeFirst ({ comparator, count, tree } as sortedSet: t 'a): (t 'a) => {
  let newTree = AVLTreeSet.removeFirst tree;
  if (newTree === tree) sortedSet else { comparator, count: count - 1, tree: newTree }
};

let removeLast ({ comparator, count, tree } as sortedSet: t 'a): (t 'a) => {
  let newTree = AVLTreeSet.removeLast tree;
  if (newTree === tree) sortedSet else { comparator, count: count - 1, tree: newTree }
};

let toSeq ({ tree }: t 'a): (Seq.t 'a) =>
  tree |> AVLTreeSet.toSeq;

let compare
    ({ comparator: thisCompare } as this: t 'a)
    ({ comparator: thatCompare } as that: t 'a): Ordering.t =>
  if (thisCompare !== thatCompare) { failwith "Sets must use the same comparator" }
  /* FIXME: Should be possible to make this more efficient
   * by recursively walking the tree.
   */
  else Seq.compareWith thisCompare (toSeq this) (toSeq that);

let equals
    ({ comparator: thisCompare } as this: t 'a)
    ({ comparator: thatCompare } as that: t 'a): bool =>
  (this === that) || (
    (thisCompare === thatCompare) &&

    /* FIXME: Should be possible to make this more efficient
     * by recursively walking the tree.
     */
    Seq.equalsWith (fun a b => (thisCompare a b) === Equal) (toSeq this) (toSeq that)
  );

let every (f: 'a => bool) (set: t 'a): bool =>
  set |> toSeq |> Seq.every f;

let find (f: 'a => bool) (set: t 'a): 'a =>
  set |> toSeq |> Seq.find f;

let first ({ tree }: t 'a): 'a =>
  AVLTreeSet.first tree;

let forEach (f: 'a => unit) ({ tree }: t 'a) =>
  tree |> AVLTreeSet.forEach f;

let hashWith (hash: (Hash.t 'a)) (set: t 'a): int => set
  |> reduce (Hash.reducer hash) Hash.initialValue;

let hash (set: t 'a): int =>
  hashWith Hash.structural set;

let last ({ tree }: t 'a): 'a =>
  AVLTreeSet.last tree;

let none (f: 'a => bool) (set: t 'a): bool =>
  set |> toSeq |> Seq.none f;

let some (f: 'a => bool) (set: t 'a): bool =>
  set |> toSeq |> Seq.some f;

let tryFind (f: 'a => bool) (set: t 'a): (option 'a) =>
  set |> toSeq |> Seq.tryFind f;

let tryFirst ({ tree }: t 'a): (option 'a) =>
  AVLTreeSet.tryFirst tree;

let tryLast ({ tree }: t 'a): (option 'a) =>
  AVLTreeSet.tryLast tree;

let toSet (set: t 'a): (ImmSet.t 'a) => {
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

let toMap (set: t 'a): (ImmMap.t 'a 'a) => {
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

let intersect ({ comparator } as this: t 'a) (that: t 'a): (t 'a) =>
  ImmSet.intersect (toSet this) (toSet that) |> fromSeqWith comparator;

let subtract ({ comparator } as this: t 'a) (that: t 'a): (t 'a) =>
  ImmSet.subtract (toSet this) (toSet that) |> fromSeqWith comparator;

let union ({ comparator } as this: t 'a) (that: t 'a): (t 'a) =>
  ImmSet.union (toSet this) (toSet that) |> fromSeqWith comparator;
