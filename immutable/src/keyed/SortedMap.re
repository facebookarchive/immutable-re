open AVLTreeMap;
open Collection;
open Comparator;
open Equality;
open Hash;
open Keyed;
open Ordering;
open Seq;

type sortedMap 'k 'v = {
  comparator: comparator 'k,
  count: int,
  tree: avlTreeMap 'k 'v,
};

let empty: (sortedMap 'k 'v) = {
  comparator: Comparator.structural,
  count: 0,
  tree: Empty,
};

let emptyWith (comparator: comparator 'k): (sortedMap 'k 'v) => {
  comparator,
  count: 0,
  tree: Empty,
};

let alter
    (key: 'k)
    (f: option 'v => option 'v)
    ({ comparator, count, tree } as map: sortedMap 'k 'v): (sortedMap 'k 'v) => {
  let alterResult = ref NoChange;
  let newTree = tree |> AVLTreeMap.alter comparator alterResult key f;
  switch !alterResult {
    | Added => { comparator, count: count + 1, tree: newTree }
    | NoChange => map
    | Replace => { comparator, count, tree: newTree }
    | Removed => { comparator, count: count - 1, tree: newTree }
  };
};

let containsWith
    (valueEquals: equality 'v )
    (key: 'k)
    (value: 'v)
    ({ comparator, tree }: sortedMap 'k 'v): bool =>
  tree |> AVLTreeMap.contains comparator valueEquals key value;

let contains (key: 'k) (value: 'v) (map: sortedMap 'k 'v): bool =>
  map |> containsWith Equality.structural key value;

let containsKey (key: 'k) ({ comparator, tree }: sortedMap 'k 'v): bool =>
  tree |> AVLTreeMap.containsKey comparator key;

let count ({ count }: sortedMap 'k 'v): int => count;

let every (f: 'k => 'v => bool) ({ tree }: sortedMap 'k 'v): bool =>
  tree |> AVLTreeMap.every f;

let first ({ tree }: sortedMap 'k 'v): ('k, 'v) =>
  tree |> AVLTreeMap.first;

let forEach (f: 'k => 'v => unit) ({ tree }: sortedMap 'k 'v): unit =>
  tree |> AVLTreeMap.forEach f;

let isEmpty ({ count }: sortedMap 'k 'v): bool =>
  count == 0;

let isNotEmpty ({ count }: sortedMap 'k 'v): bool =>
  count != 0;

let last ({ tree }: sortedMap 'k 'v): ('k, 'v) =>
  tree |> AVLTreeMap.last;

let none (f: 'k => 'v => bool) ({ tree }: sortedMap 'k 'v): bool =>
  tree |> AVLTreeMap.none f;

let put (key: 'k) (value: 'v) ({ comparator, count, tree } as map: sortedMap 'k 'v): (sortedMap 'k 'v) => {
  let alterResult = ref AVLTreeMap.NoChange;
  let newTree = tree |> AVLTreeMap.putWithResult comparator alterResult key value;
  switch !alterResult {
    | Added => { comparator, count: count + 1, tree: newTree }
    | NoChange => map
    | Replace => { comparator, count, tree: newTree }
  };
};

let putAll (seq: seq ('k, 'v)) (map: sortedMap 'k 'v): (sortedMap 'k 'v) =>
  seq |> Seq.reduce (fun acc (k, v) => acc |> put k v) map;

let fromSeqWith (comparator: comparator 'k) (seq: seq ('k, 'v)): (sortedMap 'k 'v) =>
  emptyWith comparator |> putAll seq;

let fromSeq (seq: seq ('k, 'v)): (sortedMap 'k 'v) =>
  fromSeqWith (Comparator.structural) seq;

let fromKeyedWith (comparator: comparator 'k) (keyed: keyed 'k 'v): (sortedMap 'k 'v) =>
  keyed |> Keyed.reduce (fun acc k v => acc |> put k v) (emptyWith comparator);

let fromKeyed (keyed: keyed 'k 'v): (sortedMap 'k 'v) =>
  fromKeyedWith Comparator.structural keyed;

let get (key: 'k) ({ comparator, tree }: sortedMap 'k 'v): 'v =>
  tree |> AVLTreeMap.get comparator key;

let reduce (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) ({ tree }: sortedMap 'k 'v): 'acc =>
  tree |> AVLTreeMap.reduce f acc;

let reduceRight (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) ({ tree }: sortedMap 'k 'v): 'acc =>
  tree |> AVLTreeMap.reduceRight f acc;

let map (f: 'k => 'a => 'b) ({ comparator } as map: sortedMap 'k 'a): (sortedMap 'k 'b) =>
  map |> reduce (fun acc k v => acc |> put k (f k v)) (emptyWith comparator);

let remove (key: 'k) (map: sortedMap 'k 'v): (sortedMap 'k 'v) =>
  map |> alter key Functions.alwaysNone;

let removeAll ({ comparator }: sortedMap 'k 'v): (sortedMap 'k 'v) =>
  emptyWith comparator;

let removeFirst ({ comparator, count, tree } as map: sortedMap 'k 'v): (sortedMap 'k 'v) => {
  let newTree = tree |> AVLTreeMap.removeFirst;

  if (tree === newTree) map
  else { comparator, count: count - 1, tree: newTree }
};

let removeLast ({ comparator, count, tree } as map: sortedMap 'k 'v): (sortedMap 'k 'v) => {
  let newTree = tree |> AVLTreeMap.removeLast;

  if (tree === newTree) map
  else { comparator, count: count - 1, tree: newTree }
};

let some (f: 'k => 'v => bool) ({ tree }: sortedMap 'k 'v): bool =>
  tree |> AVLTreeMap.none f;

let toSeq ({ tree }: sortedMap 'k 'v): (seq ('k, 'v)) =>
  tree |> AVLTreeMap.toSeq;

let tryFind (f: 'k => 'v => bool) ({ comparator, tree }: sortedMap 'k 'v): (option ('k, 'v)) =>
  tree |> AVLTreeMap.tryFind f;

let find (f: 'k => 'v => bool) ({ comparator, tree }: sortedMap 'k 'v): ('k, 'v) =>
  tree |> AVLTreeMap.tryFind f |> Option.first;

let tryFirst ({ tree }: sortedMap 'k 'v): (option ('k, 'v)) =>
  tree |> AVLTreeMap.tryFirst;

let tryGet (key: 'k) ({ comparator, tree }: sortedMap 'k 'v): (option 'v) =>
  tree |> AVLTreeMap.tryGet comparator key;

let tryLast ({ tree }: sortedMap 'k 'v): (option ('k, 'v)) =>
  tree |> AVLTreeMap.tryLast;

let values ({ tree }: sortedMap 'k 'v): (seq 'v) =>
  tree |> AVLTreeMap.values;

let toKeyed (map: sortedMap 'k 'v): (keyed 'k 'v) => {
  containsWith: fun eq k v => map |> containsWith eq k v,
  containsKey: fun k => containsKey k map,
  count: (count map),
  every: fun f => every f map,
  find: fun f => find f map,
  forEach: fun f => forEach f map,
  get: fun i => get i map,
  none: fun f => none f map,
  reduce: fun f acc => map |> reduce f acc,
  some: fun f => map |> some f,
  toSeq: (toSeq map),
  tryFind: fun f => tryFind f map,
  tryGet: fun i => tryGet i map,
  values: (values map),
};

let compareWith
    (compareValue: comparator 'v)
    ({ comparator } as this: sortedMap 'k 'v)
    (that: sortedMap 'k 'v): ordering =>
  Seq.compareWith (fun (k1, v1) (k2, v2) => {
    let cmp = comparator k1 k2;
    if (cmp === Equal) (compareValue v1 v2)
    else cmp
  }) (toSeq this) (toSeq that);

let compare (this: sortedMap 'k 'v) (that: sortedMap 'k 'v): ordering =>
  compareWith Comparator.structural this that;

let equalsWith
    (valueEquals: equality 'v)
    ({ comparator } as this: sortedMap 'k 'v)
    (that: sortedMap 'k 'v): bool =>
  Seq.equalsWith (fun (k1, v1) (k2, v2) =>
    if (k1 === k2) true
    else if (comparator k1 k2 === Equal) (valueEquals v1 v2)
    else false
  ) (toSeq this) (toSeq that);

let equals (this: sortedMap 'k 'v) (that: sortedMap 'k 'v): bool =>
  equalsWith Equality.structural this that;

let hash (map: sortedMap 'k 'v): int =>
  map |> toKeyed |> Keyed.hash;

let hashWith (keyHash: hash 'k) (valueHash: hash 'v) (map: sortedMap 'k 'v): int =>
  map |> toKeyed |> Keyed.hashWith keyHash valueHash;

let keys (map: sortedMap 'k 'v): (collection 'k) =>
  map |> toKeyed |> Keyed.keys;

let merge
    (f: 'k => (option 'vAcc) => (option 'v) => (option 'vAcc))
    (next: keyed 'k 'v)
    (map: sortedMap 'k 'vAcc): (sortedMap 'k 'vAcc) =>
  Collection.union (map |> toKeyed |> Keyed.keys) (next |> Keyed.keys) |> Seq.reduce (
    fun acc key => {
      let result = f key (map |> tryGet key) (next |> Keyed.tryGet key);
      switch result {
        | None => acc |> remove key
        | Some value => acc |> put key value
      }
    }
  )
  map;

let toCollectionWith (equality: equality 'v) (map: sortedMap 'k 'v): (collection ('k, 'v)) =>
  map |> toKeyed |> Keyed.toCollectionWith equality;

let toCollection (map: sortedMap 'k 'v): (collection ('k, 'v)) =>
  map |> toKeyed |> Keyed.toCollection;
