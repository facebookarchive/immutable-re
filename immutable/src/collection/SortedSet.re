open AVLTreeSet;
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

let removeFirst ({ comparator, count, tree } as sortedSet: sortedSet 'a): (sortedSet 'a) => {
  let newTree = AVLTreeSet.removeFirst tree;
  if (newTree === tree) sortedSet else { comparator, count: count - 1, tree: newTree }
};

let removeLast ({ comparator, count, tree } as sortedSet: sortedSet 'a): (sortedSet 'a) => {
  let newTree = AVLTreeSet.removeLast tree;
  if (newTree === tree) sortedSet else { comparator, count: count - 1, tree: newTree }
};

let search (predicate: 'a => ordering) ({ tree }: sortedSet 'a): 'a =>
  tree |> AVLTreeSet.search predicate;

let toSeq ({ tree }: sortedSet 'a): (seq 'a) =>
  tree |> AVLTreeSet.toSeq;

let compare ({ comparator } as this: sortedSet 'a) (that: sortedSet 'a): ordering =>
   Seq.compareWith comparator (toSeq this) (toSeq that);

let equals ({ comparator } as this: sortedSet 'a) (that: sortedSet 'a): bool =>
  Seq.equalsWith (fun a b => (comparator a b) === Equal) (toSeq this) (toSeq that);

let every (f: 'a => bool) (set: sortedSet 'a): bool =>
  set |> toSeq |> Seq.every f;

let find (f: 'a => bool) (set: sortedSet 'a): 'a =>
  set |> toSeq |> Seq.find f;

let first ({ tree }: sortedSet 'a): 'a =>
  AVLTreeSet.first tree;

let forEach (f: 'a => unit) ({ tree }: sortedSet 'a) =>
  tree |> AVLTreeSet.forEach f;

let hashWith (hash: (hash 'a)) (set: sortedSet 'a): int => set
  |> reduce (Hash.reducer hash) Hash.initialValue;

let hash (set: sortedSet 'a): int =>
  hashWith Hash.structural set;

let last ({ tree }: sortedSet 'a): 'a =>
  AVLTreeSet.last tree;

let none (f: 'a => bool) (set: sortedSet 'a): bool =>
  set |> toSeq |> Seq.none f;

let some (f: 'a => bool) (set: sortedSet 'a): bool =>
  set |> toSeq |> Seq.some f;

let tryFind (f: 'a => bool) (set: sortedSet 'a): (option 'a) =>
  set |> toSeq |> Seq.tryFind f;

let tryFirst ({ tree }: sortedSet 'a): (option 'a) =>
  AVLTreeSet.tryFirst tree;

let tryLast ({ tree }: sortedSet 'a): (option 'a) =>
  AVLTreeSet.tryLast tree;

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
