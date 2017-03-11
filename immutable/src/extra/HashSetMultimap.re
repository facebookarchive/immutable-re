open Option.Operators;

type t 'k 'v = {
  count: int,
  map: (HashMap.t 'k (HashSet.t 'v)),
  valueStrategy: HashStrategy.t 'v,
};

let contains (key: 'k) (value: 'v) ({ map }: t 'k 'v): bool =>
  map |> HashMap.tryGet key >>| HashSet.contains value |? false;

let containsKey (key: 'k) ({ map }: t 'k 'v): bool =>
  map |> HashMap.containsKey key;

let count ({ count }: t 'k 'v): int => count;

let empty: (t 'k 'v) = {
  count: 0,
  map: HashMap.empty,
  valueStrategy: HashStrategy.structuralCompare,
};

let emptyWith
    (keyStrategy: HashStrategy.t 'k)
    (valueStrategy: HashStrategy.t 'v): (t 'k 'v) => ({
  count: 0,
  map: (HashMap.emptyWith keyStrategy),
  valueStrategy,
});

let equals (this: t 'k 'v) (that: t 'k 'v): bool =>
  HashMap.equalsWith HashSet.equals this.map that.map;

let every (f: 'k => 'v => bool) ({ map }: t 'k 'v): bool => {
  let f' k set =>
    set |> HashSet.every (fun v => f k v);
  map |> HashMap.every f';
};

let forEach (f: 'k => 'v => unit) ({ map }: t 'k 'v): unit => {
  let f' k set =>
    set |> HashSet.forEach (fun v => f k v);
  map |> HashMap.forEach f';
};

let get (key: 'k) ({ map, valueStrategy }: t 'k 'v): (HashSet.t 'v) =>
  map |> HashMap.tryGet key |? HashSet.emptyWith valueStrategy;

let hash ({ map }: t 'k 'v): int =>
  map |> HashMap.hashWith HashSet.hash;

let isEmpty ({ map }: t 'k 'v): bool =>
  map |> HashMap.isEmpty;

let isNotEmpty ({ map }: t 'k 'v): bool =>
  map |> HashMap.isNotEmpty;

let keys ({ map }: t 'k 'v): (ImmSet.t 'k) =>
  map |> HashMap.keys;

let none (f: 'k => 'v => bool) ({ map }: t 'k 'v): bool => {
  let f' k set =>
    set |> HashSet.none (fun v => f k v);
  map |> HashMap.every f';
};

let put
    (key: 'k)
    (value: 'v)
    ({ count, map, valueStrategy } as multimap: t 'k 'v): (t 'k 'v) => {
  let increment = ref 0;
  let newMap = map |> HashMap.alter key (fun oldSet => switch oldSet {
    | Some oldSet =>
        let newSet = HashSet.add value oldSet;
        increment := (HashSet.count newSet) - (HashSet.count oldSet);
        newSet;
    | None =>
        increment := 1;
        HashSet.emptyWith valueStrategy |> HashSet.add value;
  } |> Option.return);

  if (newMap === map) multimap
  else {
    count: count + !increment,
    map: newMap,
    valueStrategy,
  };
};

let putAllValues
    (key: 'k)
    (values: Seq.t 'v)
    ({ count, map, valueStrategy } as multimap: t 'k 'v): (t 'k 'v) => {
  let increment = ref 0;
  let newMap = map |> HashMap.alter key (fun oldSet => switch oldSet {
    | Some oldSet =>
        let newSet = HashSet.addAll values oldSet;
        increment := (HashSet.count newSet) - (HashSet.count oldSet);
        newSet;
    | None =>
        let newSet = HashSet.fromSeqWith valueStrategy values;
        increment := HashSet.count newSet;
        newSet;
  } |> Option.return);

  if (newMap === map) multimap
  else {
    count: count + !increment,
    map: newMap,
    valueStrategy,
  };
};

let reduce (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) ({ map }: t 'k 'v): 'acc => {
  let reducer acc key values =>
    values |> HashSet.reduce (fun acc v => f acc key v) acc;
  map |> HashMap.reduce reducer acc;
};

let remove
    (key: 'k)
    ({ map, count, valueStrategy } as multimap: t 'k 'v): (t 'k 'v) =>
  map |> HashMap.tryGet key >>| (fun set => ({
    count: count - (HashSet.count set),
    map: map |> HashMap.remove key,
    valueStrategy,
  })) |? multimap;

let removeAll ({ map, valueStrategy }: t 'k 'v): (t 'k 'v) =>
  { count: 0, map: map |> HashMap.removeAll, valueStrategy };

let some (f: 'k => 'v => bool) ({ map }: t 'k 'v): bool => {
  let f' k set =>
    set |> HashSet.some (fun v => f k v);
  map |> HashMap.some f';
};

let toSeq ({ map }: t 'k 'v): (Seq.t ('k, 'v)) =>
  map |> HashMap.toSeq |> Seq.flatMap (
    fun (k, set) => set |> HashSet.toSeq |> Seq.map (Pair.create k)
  );

let tryFind (f: 'k => 'v => bool) ({ map }: t 'k 'v): (option ('k, 'v)) => {
  let result = ref None;
  let f' k set => set |> HashSet.tryFind (
    fun v => if (f k v) { result := Some (k, v); true } else false
  ) |> Option.isNotEmpty;
  map |> HashMap.tryFind f' |> ignore;
  !result
};

let find (f: 'k => 'v => bool) (multimap: t 'k 'v): ('k, 'v) =>
  multimap |> tryFind f |> Option.first;

let values ({ map }: t 'k 'v): (Seq.t 'v) =>
  map |> HashMap.values |> Seq.flatMap HashSet.toSeq;

let toSet
    ({ count } as multimap: t 'k 'v): (ImmSet.t ('k, 'v)) => {
  contains: fun (k, v) => multimap |> contains k v,
  count,
  every: fun f => multimap |> every (fun k v => f (k, v)),
  find: fun f => multimap |> find (fun k v => f (k, v)),
  forEach: fun f => multimap |> forEach (fun k v => f (k, v)),
  none: fun f => multimap |> none (fun k v => f (k, v)),
  reduce: fun f acc => multimap |> reduce (fun acc k v => f acc (k, v)) acc,
  some: fun f => multimap |> some (fun k v => f (k, v)),
  toSeq: (toSeq multimap),
  tryFind: fun f => multimap |> tryFind (fun k v => f (k, v)),
};
