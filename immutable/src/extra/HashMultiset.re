open Option.Operators;

type t 'a = {
  count: int,
  map: (HashMap.t 'a int),
};

let add (value: 'a) ({ count, map }: t 'a): (t 'a) => {
  count: count + 1,
  map: map |> HashMap.alter value (fun
    | Some valueCount => Some (valueCount + 1)
    | _ => Some 1
  ),
};

let contains (value: 'a) ({ count, map }: t 'a): bool =>
  map |> HashMap.containsKey value;

let count ({ count }: t 'a): int => count;

let empty: (t 'a) = {
  count: 0,
  map: HashMap.empty,
};

let emptyWith (keyStrategy: HashStrategy.t 'a): (t 'a) => {
  count: 0,
  map: HashMap.emptyWith keyStrategy,
};

let equals (this: t 'a) (that: t 'a) =>
  HashMap.equals this.map that.map;

let every (f: 'a => int => bool) ({ map }: t 'a): bool =>
  map |> HashMap.every f;

let find (f: 'a => int => bool) ({ map }: t 'a): ('a, int) =>
  map |> HashMap.find f;

let forEach (f: 'a => int => unit) ({ map }: t 'a) =>
  map |> HashMap.forEach f;

let get (value: 'a) ({ map }: t 'a): int =>
  map |> HashMap.tryGet value |? 0;

let hash ({ map }: t 'a): int =>
  map |> HashMap.hash;

let isEmpty ({ count }: t 'a): bool =>
  count == 0;

let isNotEmpty ({ count }: t 'a): bool =>
  count != 0;

let none (f: 'a => int => bool) ({ map }: t 'a): bool =>
  map |> HashMap.none f;

let reduce (f: 'acc => 'a => int => 'acc) (acc: 'acc) ({ map }: t 'a): 'acc =>
  map |> HashMap.reduce f acc;

let remove (value: 'a) ({ count, map } as hashMultiset: t 'a): (t 'a) => {
  let newCount = ref count;
  let newMap = map |> HashMap.alter value (fun currentValue => switch currentValue {
    | Some oldValueCount =>
        newCount := count - oldValueCount;
        None
    | None => None;
  });

  if (newMap === map) hashMultiset
  else { count: !newCount, map: newMap };
};

let removeAll ({ count, map }: t 'a): (t 'a) =>
  { count: 0, map: map |> HashMap.removeAll };

let set (value: 'a) (valueCount: int) ({ count, map } as multiset: t 'a): (t 'a) => {
  Preconditions.failIf "count must be greater than 0" (valueCount < 0);

  let newCount = ref count;
  let newMap = map |> HashMap.alter value (fun currentValue => switch currentValue {
    | Some oldValueCount when valueCount == 0 =>
        newCount := count - oldValueCount;
        None
    | Some oldValueCount =>
        newCount := count - oldValueCount + valueCount;
        Some valueCount;
    | None when valueCount == 0 =>
        None
    | None =>
        newCount := count + valueCount;
        Some valueCount
  });

  if (newMap === map) multiset
  else { count: !newCount, map: newMap };
};

let some (f: 'a => int => bool) ({ map }: t 'a): bool =>
  map |> HashMap.some f;

let toMap ({ map }: t 'a): (ImmMap.t 'a int) => map |> HashMap.toMap;

let toSeq ({ map }: t 'a): (Seq.t 'a) =>
  map |> HashMap.toSeq |> Seq.flatMap (fun (v, i) => Seq.repeat v (Some i));

let tryFind (f: 'a => int => bool) ({ map }: t 'a): (option ('a, int)) =>
  map |> HashMap.tryFind f;

let values ({ map }: t 'a): (ImmSet.t 'a) =>
  map |> HashMap.keys;

let module TransientHashMultiset = {
  let module TransientHashMap = HashMap.TransientHashMap;

  type hashMultiset 'a = t 'a;

  type t 'a = {
    mutable count: int,
    map: (TransientHashMap.t 'a int),
  };

  let mutate ({ count, map}: hashMultiset 'a): (t 'a) => {
    count: count,
    map: map |> HashMap.mutate,
  };

  let add
      (value: 'a)
      ({ count, map } as transient: t 'a): (t 'a) => {
    transient.count = count + 1;
    map |> TransientHashMap.put value (
      (map |> TransientHashMap.tryGet value >>| succ) |? 1
    ) |> ignore;

    transient;
  };

  let addAll
      (seq: Seq.t 'a)
      (transient: t 'a): (t 'a) => seq
    |> Seq.reduce (fun acc next => transient |> add next) transient;

  let contains (value: 'a) ({ count, map }: t 'a): bool =>
    map |> TransientHashMap.tryGet value >>| Functions.alwaysTrue |? false;

  let count ({ count }: t 'a): int => count;

  let empty (): (t 'a)  =>
    empty |> mutate;

  let emptyWith (strategy: HashStrategy.t 'a): (t 'a) =>
    emptyWith strategy |> mutate;

  let get (value: 'a) ({ map }: t 'a): int =>
    (map |> TransientHashMap.tryGet value) |? 0;

  let isEmpty ({ map }: t 'a): bool =>
    map |> TransientHashMap.isEmpty;

  let isNotEmpty ({ map }: t 'a): bool =>
    map |> TransientHashMap.isNotEmpty;

  let persist ({ count, map }: t 'a): (hashMultiset 'a) => ({
    count,
    map: map |> TransientHashMap.persist,
  });

  let remove
      (value: 'a)
      ({ count, map } as transient: t 'a): (t 'a) => {
    map |> TransientHashMap.tryGet value >>| (fun valueCount => {
      transient.count = count - valueCount;
      map |> TransientHashMap.remove value |> ignore;
    }) |? ();

    transient;
  };

  let removeAll ({ map } as transient: t 'a): (t 'a) => {
    transient.count = 0;
    map |> TransientHashMap.removeAll |> ignore;
    transient;
  };

  let set
      (value: 'a)
      (valueCount: int)
      ({ count, map } as transient: t 'a): (t 'a) => {
    transient.count = count + valueCount - (map |> TransientHashMap.tryGet value |? 0);
    map |> TransientHashMap.put value valueCount |> ignore;
    transient
  };
};

let mutate = TransientHashMultiset.mutate;

let addAll
    (seq: Seq.t 'a)
    (hashMultiset: t 'a): (t 'a) => hashMultiset
  |> mutate
  |> TransientHashMultiset.addAll seq
  |> TransientHashMultiset.persist;

let fromSeq (seq: Seq.t 'a): (t 'a) =>
  empty |> addAll seq;

let fromSeqWith (strategy: HashStrategy.t 'a) (seq: Seq.t 'a): (t 'a) =>
  (emptyWith strategy) |> addAll seq;
