open Option.Operators;

type t 'k 'v = {
  containsWith: (Equality.t 'v) => 'k => 'v => bool,
  containsKey: 'k => bool,
  count: int,
  every: ('k => 'v => bool) => bool,
  find: ('k => 'v => bool) => ('k, 'v),
  forEach: ('k => 'v => unit) => unit,
  get: 'k => 'v,
  none: ('k => 'v => bool) => bool,
  reduce: 'acc . ('acc => 'k => 'v => 'acc) => 'acc => 'acc,
  some: ('k => 'v => bool) => bool,
  toSeq: (Seq.t ('k, 'v)),
  tryFind: ('k => 'v => bool) => (option ('k, 'v)),
  tryGet: 'k => (option 'v),
  values: (Seq.t 'v),
};

let containsWith
    (valueEquality: Equality.t 'v)
    (key: 'k)
    (value: 'v)
    ({ containsWith }: t 'k 'v): bool =>
  containsWith valueEquality key value;

let contains
    (key: 'k)
    (value: 'v)
    ({ containsWith }: t 'k 'v): bool =>
  containsWith Equality.structural key value;

let containsKey (key: 'k) ({ containsKey }: t 'k 'v): bool =>
  containsKey key;

let count ({ count }: t 'k 'v) => count;

let empty: (t 'k 'v) = {
  containsWith: fun _ _ _ => false,
  containsKey: fun _ => false,
  count: 0,
  every: fun _ => false,
  find: fun _ => failwith "empty",
  forEach: fun _ => (),
  get: fun _ => failwith "empty",
  none: fun _ => true,
  reduce: fun _ acc => acc,
  some: fun _ => false,
  toSeq: Seq.empty,
  tryFind: fun _ => None,
  tryGet: fun _ => None,
  values: Seq.empty,
};

let equalsWith (valueEquality: Equality.t 'v) (that: t 'k 'v) (this: t 'k 'v): bool =>
  (this === that) ? true :
  (this.count != that.count) ? false :
  this.every (
    fun key thisValue => that.tryGet key
    >>| (fun thatValue => thisValue |> valueEquality thatValue)
    |? false
  );

let equals (that: t 'k 'v) (this: t 'k 'v): bool =>
  equalsWith Equality.structural that this;

let every (f: 'k => 'v => bool) ({ every }: t 'k 'v): bool =>
  every f;

let find (f: 'k => 'v => bool) ({ find }: t 'k 'v): ('k, 'v) =>
  find f;

let forEach (f: 'k => 'v => unit) ({ forEach }: t 'k 'v): unit =>
  forEach f;

let get (key: 'k) ({ get }: t 'k 'v): 'v =>
  get key;

let hashWith (keyHash: Hash.t 'k) (valueHash: Hash.t 'v) ({ reduce }: t 'k 'v): int =>
  reduce (MapEntry.hashReducer keyHash valueHash) Hash.initialValue;

let hash (map: t 'k 'v): int =>
  hashWith Hash.structural Hash.structural map;

let isEmpty ({ count }: t 'k 'v): bool =>
  count == 0;

let isNotEmpty ({ count }: t 'k 'v): bool =>
  count != 0;

let keys (map: t 'k 'v): (ImmSet.t 'k) => {
  contains: map.containsKey,
  count: map.count,
  every: fun f => map.every (fun k _ => f k),
  find: fun f => {
    let (key, _) = map.find (fun k _ => f k);
    key
  },
  forEach: fun f => map.forEach (fun k _ => f k),
  none: fun f => map.none (fun k _ => f k),
  reduce: fun f acc => map.reduce (fun acc k _ => f acc k) acc,
  some: fun f => map.some (fun k _ => f k),
  toSeq: map.toSeq |> Seq.map (fun (k, _) => k),
  tryFind: fun f => map.tryFind (fun k _ => f k) >>| (fun (k, v) => k),
};

let map (m: 'k => 'a => 'b) (map: t 'k 'a): (t 'k 'b) => {
  containsWith: fun equals k b => map.tryGet k >>| (fun a => m k a |> equals b) |? false,
  containsKey: map.containsKey,
  count: map.count,
  every: fun f => map.every (fun k a => f k (m k a)),
  find: fun f => {
    let (k, v) = map.find (fun k a => f k (m k a));
    (k, m k v)
  },
  forEach: fun f => map.forEach (fun k a => f k (m k a)),
  get: fun k => {
    let v = map.get k;
    m k v;
  },
  none: fun f => map.none (fun k a => f k (m k a)),
  reduce: fun f acc => map.reduce (fun acc k a => f acc k (m k a)) acc,
  some: fun f => map.some (fun k a => f k (m k a)),
  toSeq: map.toSeq |> Seq.map (fun (k, v) => (k, m k v)),
  tryFind: fun f => map.tryFind (fun k a => f k (m k a)) >>| (fun (k, v) => (k, m k v)),
  tryGet: fun k => map.tryGet k >>| m k,
  values: map.toSeq |> Seq.map (fun (k, v) => m k v),
};

let none (f: 'k => 'v => bool) ({ none }: t 'k 'v): bool =>
  none f;

let ofSet (set: ImmSet.t 'a): (t 'a 'a) => {
  containsWith: fun equals k v => set |> ImmSet.contains k ? equals k v : false,
  containsKey: fun k => set |> ImmSet.contains k,
  count: ImmSet.count set,
  every: fun f => set |> ImmSet.every (fun k => f k k),
  find: fun f => {
    let k = set |> ImmSet.find (fun k => f k k);
    (k, k)
  },
  forEach: fun f => set |> ImmSet.forEach (fun k => f k k),
  get: fun k => set |> ImmSet.contains k ? k : failwith "not found",
  none: fun f => set |> ImmSet.none (fun k => f k k),
  reduce: fun f acc => set |> ImmSet.reduce (fun acc k => f acc k k) acc,
  some: fun f => set |> ImmSet.some (fun k => f k k),
  toSeq: ImmSet.toSeq set |> Seq.map (fun k => (k, k)),
  tryFind: fun f => set |> ImmSet.tryFind (fun k => f k k) >>| (fun k => (k, k)),
  tryGet: fun k => set |> ImmSet.contains k ? Some k : None,
  values: ImmSet.toSeq set,
};

let reduce (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) ({ reduce }: t 'k 'v): 'acc =>
  reduce f acc;

let some (f: 'k => 'v => bool) ({ some }: t 'k 'v): bool =>
  some f;

let toSetWith
    (equals: Equality.t 'v)
    (map: t 'k 'v): (ImmSet.t ('k, 'v)) => {
  contains: fun (k, v) => map.tryGet k >>| equals v |? false,
  count: map.count,
  every: fun f => map.every (fun k v => f (k, v)),
  find: fun f => map.find (fun k v => f (k, v)),
  forEach: fun f => map.forEach (fun k v => f (k, v)),
  none: fun f => map.none (fun k v => f (k, v)),
  reduce: fun f acc => map.reduce (fun acc k v => f acc (k, v)) acc,
  some: fun f => map.some (fun k v => f (k, v)),
  toSeq: map.toSeq,
  tryFind: fun f => map.tryFind (fun k v => f (k, v)),
};

let toSet (map: t 'k 'v): (ImmSet.t ('k, 'v)) =>
  toSetWith Equality.structural map;

let toSeq ({ toSeq }: t 'k 'v): (Seq.t ('k, 'v)) => toSeq;

let tryFind (f: 'k => 'v => bool) ({ tryFind }: t 'k 'v): (option ('k, 'v)) =>
  tryFind f;

let tryGet (key: 'k) ({ tryGet }: t 'k 'v): (option 'v) =>
  tryGet key;

let values ({ values }: t 'k 'v): (Seq.t 'v) => values;
