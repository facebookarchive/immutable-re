type t 'k 'v = {
  comparator: Comparator.t 'k,
  count: int,
  tree: AVLTreeMap.t 'k 'v,
};

let empty: (t 'k 'v) = {
  comparator: Comparator.structural,
  count: 0,
  tree: AVLTreeMap.Empty,
};

let emptyWith (comparator: Comparator.t 'k): (t 'k 'v) => {
  comparator,
  count: 0,
  tree: AVLTreeMap.Empty,
};

let alter
    (key: 'k)
    (f: option 'v => option 'v)
    ({ comparator, count, tree } as map: t 'k 'v): (t 'k 'v) => {
  let alterResult = ref AVLTreeMap.NoChange;
  let newTree = tree |> AVLTreeMap.alter comparator alterResult key f;
  switch !alterResult {
    | AVLTreeMap.Added => { comparator, count: count + 1, tree: newTree }
    | AVLTreeMap.NoChange => map
    | AVLTreeMap.Replace => { comparator, count, tree: newTree }
    | AVLTreeMap.Removed => { comparator, count: count - 1, tree: newTree }
  };
};

let containsWith
    (valueEquals: Equality.t 'v )
    (key: 'k)
    (value: 'v)
    ({ comparator, tree }: t 'k 'v): bool =>
  tree |> AVLTreeMap.contains comparator valueEquals key value;

let contains (key: 'k) (value: 'v) (map: t 'k 'v): bool =>
  map |> containsWith Equality.structural key value;

let containsKey (key: 'k) ({ comparator, tree }: t 'k 'v): bool =>
  tree |> AVLTreeMap.containsKey comparator key;

let count ({ count }: t 'k 'v): int => count;

let every (f: 'k => 'v => bool) ({ tree }: t 'k 'v): bool =>
  tree |> AVLTreeMap.every f;

let first ({ tree }: t 'k 'v): ('k, 'v) =>
  tree |> AVLTreeMap.first;

let forEach (f: 'k => 'v => unit) ({ tree }: t 'k 'v): unit =>
  tree |> AVLTreeMap.forEach f;

let isEmpty ({ count }: t 'k 'v): bool =>
  count == 0;

let isNotEmpty ({ count }: t 'k 'v): bool =>
  count != 0;

let last ({ tree }: t 'k 'v): ('k, 'v) =>
  tree |> AVLTreeMap.last;

let none (f: 'k => 'v => bool) ({ tree }: t 'k 'v): bool =>
  tree |> AVLTreeMap.none f;

let put (key: 'k) (value: 'v) ({ comparator, count, tree } as map: t 'k 'v): (t 'k 'v) => {
  let alterResult = ref AVLTreeMap.NoChange;
  let newTree = tree |> AVLTreeMap.putWithResult comparator alterResult key value;
  switch !alterResult {
    | AVLTreeMap.Added => { comparator, count: count + 1, tree: newTree }
    | AVLTreeMap.NoChange => map
    | AVLTreeMap.Replace => { comparator, count, tree: newTree }
    | AVLTreeMap.Removed => failwith "invalid state"
  };
};

let putAll (seq: Seq.t ('k, 'v)) (map: t 'k 'v): (t 'k 'v) =>
  seq |> Seq.reduce (fun acc (k, v) => acc |> put k v) map;

let fromSeqWith (comparator: Comparator.t 'k) (seq: Seq.t ('k, 'v)): (t 'k 'v) =>
  emptyWith comparator |> putAll seq;

let fromSeq (seq: Seq.t ('k, 'v)): (t 'k 'v) =>
  fromSeqWith (Comparator.structural) seq;

let fromMapWith (comparator: Comparator.t 'k) (map: ImmMap.t 'k 'v): (t 'k 'v) =>
  map |> ImmMap.reduce (fun acc k v => acc |> put k v) (emptyWith comparator);

let fromMap (map: ImmMap.t 'k 'v): (t 'k 'v) =>
  fromMapWith Comparator.structural map;

let get (key: 'k) ({ comparator, tree }: t 'k 'v): 'v =>
  tree |> AVLTreeMap.get comparator key;

let reduce (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) ({ tree }: t 'k 'v): 'acc =>
  tree |> AVLTreeMap.reduce f acc;

let reduceRight (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) ({ tree }: t 'k 'v): 'acc =>
  tree |> AVLTreeMap.reduceRight f acc;

let map (f: 'k => 'a => 'b) ({ comparator } as map: t 'k 'a): (t 'k 'b) =>
  map |> reduce (fun acc k v => acc |> put k (f k v)) (emptyWith comparator);

let remove (key: 'k) (map: t 'k 'v): (t 'k 'v) =>
  map |> alter key Functions.alwaysNone;

let removeAll ({ comparator }: t 'k 'v): (t 'k 'v) =>
  emptyWith comparator;

let removeFirst ({ comparator, count, tree } as map: t 'k 'v): (t 'k 'v) => {
  let newTree = tree |> AVLTreeMap.removeFirst;

  if (tree === newTree) map
  else { comparator, count: count - 1, tree: newTree }
};

let removeLast ({ comparator, count, tree } as map: t 'k 'v): (t 'k 'v) => {
  let newTree = tree |> AVLTreeMap.removeLast;

  if (tree === newTree) map
  else { comparator, count: count - 1, tree: newTree }
};

let some (f: 'k => 'v => bool) ({ tree }: t 'k 'v): bool =>
  tree |> AVLTreeMap.none f;

let toSeq ({ tree }: t 'k 'v): (Seq.t ('k, 'v)) =>
  tree |> AVLTreeMap.toSeq;

let tryFind (f: 'k => 'v => bool) ({ tree }: t 'k 'v): (option ('k, 'v)) =>
  tree |> AVLTreeMap.tryFind f;

let find (f: 'k => 'v => bool) ({ tree }: t 'k 'v): ('k, 'v) =>
  tree |> AVLTreeMap.tryFind f |> Option.first;

let tryFirst ({ tree }: t 'k 'v): (option ('k, 'v)) =>
  tree |> AVLTreeMap.tryFirst;

let tryGet (key: 'k) ({ comparator, tree }: t 'k 'v): (option 'v) =>
  tree |> AVLTreeMap.tryGet comparator key;

let tryLast ({ tree }: t 'k 'v): (option ('k, 'v)) =>
  tree |> AVLTreeMap.tryLast;

let values ({ tree }: t 'k 'v): (Seq.t 'v) =>
  tree |> AVLTreeMap.values;

let toIterable (map: t 'k 'v): (Iterable.t ('k, 'v)) =>
  if (isEmpty map) Iterable.empty
  else {
    reduce: fun f acc => map |> reduce
      (fun acc k v => f acc (k, v))
      acc
  };

let toKeyedIterable (map: t 'k 'v): (KeyedIterable.t 'k 'v) =>
  if (isEmpty map) KeyedIterable.empty
  else {
    reduce: fun f acc => map |> reduce f acc
  };

let toMap (map: t 'k 'v): (ImmMap.t 'k 'v) => {
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
    (compareValue: Comparator.t 'v)
    ({ comparator: thisComparator } as this: t 'k 'v)
    ({ comparator: thatComparator } as that: t 'k 'v): Ordering.t =>
  if (thisComparator !== thatComparator) { failwith "Maps must use the same comparator" }
  /* FIXME: Should be possible to make this more efficient
   * by recursively walking the tree.
   */
  else Seq.compareWith (fun (k1, v1) (k2, v2) => {
    let cmp = thisComparator k1 k2;
    if (cmp === Ordering.equal) (compareValue v1 v2)
    else cmp
  }) (toSeq this) (toSeq that);

let compare (this: t 'k 'v) (that: t 'k 'v): Ordering.t =>
  compareWith Comparator.structural this that;

let equalsWith
    (valueEquals: Equality.t 'v)
    ({ comparator } as this: t 'k 'v)
    (that: t 'k 'v): bool =>
  Seq.equalsWith (fun (k1, v1) (k2, v2) =>
    if (k1 === k2) true
    else if (comparator k1 k2 === Ordering.equal) (valueEquals v1 v2)
    else false
  ) (toSeq this) (toSeq that);

let equals (this: t 'k 'v) (that: t 'k 'v): bool =>
  equalsWith Equality.structural this that;

let hash (map: t 'k 'v): int =>
  map |> toMap |> ImmMap.hash;

let hashWith (keyHash: Hash.t 'k) (valueHash: Hash.t 'v) (map: t 'k 'v): int =>
  map |> toMap |> ImmMap.hashWith keyHash valueHash;

let keys (map: t 'k 'v): (ImmSet.t 'k) =>
  map |> toMap |> ImmMap.keys;

let merge
    (f: 'k => (option 'vAcc) => (option 'v) => (option 'vAcc))
    (next: t 'k 'v)
    (map: t 'k 'vAcc): (t 'k 'vAcc) =>
  ImmSet.union (keys map) (keys next) |> Seq.reduce (
    fun acc key => {
      let result = f key (map |> tryGet key) (next |> tryGet key);
      switch result {
        | None => acc |> remove key
        | Some value => acc |> put key value
      }
    }
  )
  map;

let toSetWith (equality: Equality.t 'v) (map: t 'k 'v): (ImmSet.t ('k, 'v)) =>
  map |> toMap |> ImmMap.toSetWith equality;

let toSet (map: t 'k 'v): (ImmSet.t ('k, 'v)) =>
  map |> toMap |> ImmMap.toSet;
