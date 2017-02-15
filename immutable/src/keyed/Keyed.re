open Collection;
open Comparator;
open Equality;
open Functions;
open Hash;
open Option.Operators;
open Ordering;
open Pair;
open Seq;

type keyed 'k 'v = {
  containsWith: (equality 'v) => 'k => 'v => bool,
  containsKey: 'k => bool,
  count: int,
  every: ('k => 'v => bool) => bool,
  find: ('k => 'v => bool) => ('k, 'v),
  forEach: ('k => 'v => unit) => unit,
  get: 'k => 'v,
  none: ('k => 'v => bool) => bool,
  reduce: 'acc . ('acc => 'k => 'v => 'acc) => 'acc => 'acc,
  some: ('k => 'v => bool) => bool,
  toSeq: (seq ('k, 'v)),
  tryFind: ('k => 'v => bool) => (option ('k, 'v)),
  tryGet: 'k => (option 'v),
  values: (seq 'v),
};

let containsWith
    (valueEquality: equality 'v)
    (key: 'k)
    (value: 'v)
    ({ containsWith }: keyed 'k 'v): bool =>
  containsWith valueEquality key value;

let contains
    (key: 'k)
    (value: 'v)
    ({ containsWith }: keyed 'k 'v): bool =>
  containsWith Equality.structural key value;

let containsKey (key: 'k) ({ containsKey }: keyed 'k 'v): bool =>
  containsKey key;

let count ({ count }: keyed 'k 'v) => count;

let empty: (keyed 'k 'v) = {
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

let equalsWith (valueEquality: equality 'v) (that: keyed 'k 'v) (this: keyed 'k 'v): bool =>
  (this === that) ? true :
  (this.count != that.count) ? false :
  this.every (
    fun key thisValue => that.tryGet key
    >>| (fun thatValue => thisValue |> valueEquality thatValue)
    |? false
  );

let equals (that: keyed 'k 'v) (this: keyed 'k 'v): bool =>
  equalsWith Equality.structural that this;

let every (f: 'k => 'v => bool) ({ every }: keyed 'k 'v): bool =>
  every f;

let find (f: 'k => 'v => bool) ({ find }: keyed 'k 'v): ('k, 'v) =>
  find f;

let forEach (f: 'k => 'v => unit) ({ forEach }: keyed 'k 'v): unit =>
  forEach f;

let get (key: 'k) ({ get }: keyed 'k 'v): 'v =>
  get key;

let hashWith (keyHash: hash 'k) (valueHash: hash 'v) ({ reduce }: keyed 'k 'v): int =>
  reduce (KeyedEntry.hashReducer keyHash valueHash) Hash.initialValue;

let hash (keyed: keyed 'k 'v): int =>
  hashWith Hash.structural Hash.structural keyed;

let isEmpty ({ count }: keyed 'k 'v): bool =>
  count == 0;

let isNotEmpty ({ count }: keyed 'k 'v): bool =>
  count != 0;

let keys (keyed: keyed 'k 'v): (collection 'k) => {
  contains: keyed.containsKey,
  count: keyed.count,
  every: fun f => keyed.every (fun k _ => f k),
  find: fun f => {
    let (key, _) = keyed.find (fun k _ => f k);
    key
  },
  forEach: fun f => keyed.forEach (fun k _ => f k),
  none: fun f => keyed.none (fun k _ => f k),
  reduce: fun f acc => keyed.reduce (fun acc k _ => f acc k) acc,
  some: fun f => keyed.some (fun k _ => f k),
  toSeq: keyed.toSeq |> Seq.map (fun (k, _) => k),
  tryFind: fun f => keyed.tryFind (fun k _ => f k) >>| (fun (k, v) => k),
};

let map (m: 'k => 'a => 'b) (keyed: keyed 'k 'a): (keyed 'k 'b) => {
  containsWith: fun equals k b => keyed.tryGet k >>| (fun a => m k a |> equals b) |? false,
  containsKey: keyed.containsKey,
  count: keyed.count,
  every: fun f => keyed.every (fun k a => f k (m k a)),
  find: fun f => {
    let (k, v) = keyed.find (fun k a => f k (m k a));
    (k, m k v)
  },
  forEach: fun f => keyed.forEach (fun k a => f k (m k a)),
  get: fun k => {
    let v = keyed.get k;
    m k v;
  },
  none: fun f => keyed.none (fun k a => f k (m k a)),
  reduce: fun f acc => keyed.reduce (fun acc k a => f acc k (m k a)) acc,
  some: fun f => keyed.some (fun k a => f k (m k a)),
  toSeq: keyed.toSeq |> Seq.map (fun (k, v) => (k, m k v)),
  tryFind: fun f => keyed.tryFind (fun k a => f k (m k a)) >>| (fun (k, v) => (k, m k v)),
  tryGet: fun k => keyed.tryGet k >>| m k,
  values: keyed.toSeq |> Seq.map (fun (k, v) => m k v),
};

let none (f: 'k => 'v => bool) ({ none }: keyed 'k 'v): bool =>
  none f;

let reduce (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) ({ reduce }: keyed 'k 'v): 'acc =>
  reduce f acc;

let some (f: 'k => 'v => bool) ({ some }: keyed 'k 'v): bool =>
  some f;

let toCollectionWith
    (equals: equality 'v)
    (keyed: keyed 'k 'v): (collection ('k, 'v)) => {
  contains: fun (k, v) => keyed.tryGet k >>| equals v |? false,
  count: keyed.count,
  every: fun f => keyed.every (fun k v => f (k, v)),
  find: fun f => keyed.find (fun k v => f (k, v)),
  forEach: fun f => keyed.forEach (fun k v => f (k, v)),
  none: fun f => keyed.none (fun k v => f (k, v)),
  reduce: fun f acc => keyed.reduce (fun acc k v => f acc (k, v)) acc,
  some: fun f => keyed.some (fun k v => f (k, v)),
  toSeq: keyed.toSeq,
  tryFind: fun f => keyed.tryFind (fun k v => f (k, v)),
};

let toCollection (keyed: keyed 'k 'v): (collection ('k, 'v)) =>
  toCollectionWith Equality.structural keyed;

let toSeq ({ toSeq }: keyed 'k 'v): (seq ('k, 'v)) => toSeq;

let tryFind (f: 'k => 'v => bool) ({ tryFind }: keyed 'k 'v): (option ('k, 'v)) =>
  tryFind f;

let tryGet (key: 'k) ({ tryGet }: keyed 'k 'v): (option 'v) =>
  tryGet key;

let values ({ values }: keyed 'k 'v): (seq 'v) => values;
