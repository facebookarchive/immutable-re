open Collection;
open Equality;
open Functions;
open Keyed;
open HashMap;
open HashStrategy;
open Option.Operators;
open Seq;

type hashMultiset 'a = {
  count: int,
  map: (hashMap 'a int),
};

let add (value: 'a) ({ count, map }: hashMultiset 'a): (hashMultiset 'a) => {
  count: count + 1,
  map: map |> HashMap.alter value (fun
    | Some valueCount => Some (valueCount + 1)
    | _ => Some 1
  ),
};

let contains (value: 'a) ({ count, map }: hashMultiset 'a): bool =>
  map |> HashMap.containsKey value;

let count ({ count }: hashMultiset 'a): int => count;

let empty: (hashMultiset 'a) = {
  count: 0,
  map: HashMap.empty,
};

let emptyWith (keyStrategy: hashStrategy 'a): (hashMultiset 'a) => {
  count: 0,
  map: HashMap.emptyWith keyStrategy,
};

let equals (this: hashMultiset 'a) (that: hashMultiset 'a) =>
  HashMap.equals this.map that.map;

let every (f: 'a => int => bool) ({ map }: hashMultiset 'a): bool =>
  map |> HashMap.every f;

let find (f: 'a => int => bool) ({ map }: hashMultiset 'a): ('a, int) =>
  map |> HashMap.find f;

let forEach (f: 'a => int => unit) ({ map }: hashMultiset 'a) =>
  map |> HashMap.forEach f;

let get (value: 'a) ({ map }: hashMultiset 'a): int =>
  map |> HashMap.tryGet value |? 0;

let hash ({ map }: hashMultiset 'a): int =>
  map |> HashMap.hash;

let isEmpty ({ count }: hashMultiset 'a): bool =>
  count == 0;

let isNotEmpty ({ count }: hashMultiset 'a): bool =>
  count != 0;

let none (f: 'a => int => bool) ({ map }: hashMultiset 'a): bool =>
  map |> HashMap.none f;

let reduce (f: 'acc => 'a => int => 'acc) (acc: 'acc) ({ map }: hashMultiset 'a): 'acc =>
  map |> HashMap.reduce f acc;

let remove (value: 'a) ({ count, map } as hashMultiset: hashMultiset 'a): (hashMultiset 'a) => {
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

let removeAll ({ count, map }: hashMultiset 'a): (hashMultiset 'a) =>
  { count: 0, map: map |> HashMap.removeAll };

let set (value: 'a) (valueCount: int) ({ count, map } as multiset: hashMultiset 'a): (hashMultiset 'a) => {
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

let some (f: 'a => int => bool) ({ map }: hashMultiset 'a): bool =>
  map |> HashMap.some f;

let toKeyed ({ map }: hashMultiset 'a): (keyed 'a int) => map |> HashMap.toKeyed;

let toSeq ({ map }: hashMultiset 'a): (seq ('a, int)) =>
  map |> HashMap.toSeq;

let tryFind (f: 'a => int => bool) ({ map }: hashMultiset 'a): (option ('a, int)) =>
  map |> HashMap.tryFind f;

let values ({ map }: hashMultiset 'a): (collection 'a) =>
  map |> HashMap.keys;

type transientHashMultiset 'a = {
  mutable count: int,
  map: (transientHashMap 'a int),
};

let mutate ({ count, map}: hashMultiset 'a): (transientHashMultiset 'a) => ({
  count: count,
  map: map |> HashMap.mutate,
});

let module TransientHashMultiset = {
  let add
      (value: 'a)
      ({ count, map } as transient: transientHashMultiset 'a): (transientHashMultiset 'a) => {
    transient.count = count + 1;
    map |> TransientHashMap.put value (
      (map |> TransientHashMap.tryGet value >>| succ) |? 1
    ) |> ignore;

    transient;
  };

  let addAll
      (seq: seq 'a)
      (transient: transientHashMultiset 'a): (transientHashMultiset 'a) => seq
    |> Seq.reduce (fun acc next => transient |> add next) transient;

  let contains (value: 'a) ({ count, map }: transientHashMultiset 'a): bool =>
    map |> TransientHashMap.tryGet value >>| alwaysTrue |? false;

  let count ({ count }: transientHashMultiset 'a): int => count;

  let empty (): (transientHashMultiset 'a)  =>
    empty |> mutate;

  let emptyWith (strategy: hashStrategy 'a) =>
    emptyWith strategy |> mutate;

  let get (value: 'a) ({ map }: transientHashMultiset 'a): int =>
    (map |> TransientHashMap.tryGet value) |? 0;

  let isEmpty ({ map }: transientHashMultiset 'a): bool =>
    map |> TransientHashMap.isEmpty;

  let isNotEmpty ({ map }: transientHashMultiset 'a): bool =>
    map |> TransientHashMap.isNotEmpty;

  let persist ({ count, map }: transientHashMultiset 'a): (hashMultiset 'a) => ({
    count,
    map: map |> TransientHashMap.persist,
  });

  let remove
      (value: 'a)
      ({ count, map } as transient: transientHashMultiset 'a): (transientHashMultiset 'a) => {
    map |> TransientHashMap.tryGet value >>| (fun valueCount => {
      transient.count = count - valueCount;
      map |> TransientHashMap.remove value |> ignore;
    }) |? ();

    transient;
  };

  let removeAll ({ map } as transient: transientHashMultiset 'a): (transientHashMultiset 'a) => {
    transient.count = 0;
    map |> TransientHashMap.removeAll |> ignore;
    transient;
  };

  let set
      (value: 'a)
      (valueCount: int)
      ({ count, map } as transient: transientHashMultiset 'a): (transientHashMultiset 'a) => {
    transient.count = count + valueCount - (map |> TransientHashMap.tryGet value |? 0);
    map |> TransientHashMap.put value valueCount |> ignore;
    transient
  };
};

let addAll
    (seq: seq 'a)
    (hashMultiset: hashMultiset 'a): (hashMultiset 'a) => hashMultiset
  |> mutate
  |> TransientHashMultiset.addAll seq
  |> TransientHashMultiset.persist;

let fromSeq (seq: seq 'a): (hashMultiset 'a) =>
  empty |> addAll seq;

let fromSeqWith (strategy: hashStrategy 'a) (seq: seq 'a): (hashMultiset 'a) =>
  (emptyWith strategy) |> addAll seq;
