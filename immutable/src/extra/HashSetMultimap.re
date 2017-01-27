/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Collection;
open Equality;
open Keyed;
open HashMap;
open HashStrategy;
open Option.Operators;
open Pair;
open Seq;
open HashSet;

type hashSetMultimap 'k 'v = {
  count: int,
  map: (hashMap 'k (hashSet 'v)),
  valueStrategy: hashStrategy 'v,
};

let count ({ count }: hashSetMultimap 'k 'v): int => count;

let empty (): (hashSetMultimap 'k 'v) => ({
  count: 0,
  map: (HashMap.empty ()),
  valueStrategy: HashStrategy.structuralCompare (),
});

let emptyWith
    (keyStrategy: hashStrategy 'k)
    (valueStrategy: hashStrategy 'v): (hashSetMultimap 'k 'v) => ({
  count: 0,
  map: (HashMap.emptyWith keyStrategy),
  valueStrategy,
});

let get (key: 'k) ({ map, valueStrategy }: hashSetMultimap 'k 'v): (hashSet 'v) =>
  map |> HashMap.tryGet key |? HashSet.emptyWith valueStrategy;

let put
    (key: 'k)
    (value: 'v)
    ({ count, map, valueStrategy }: hashSetMultimap 'k 'v): (hashSetMultimap 'k 'v) => {
  let oldSet = map |> HashMap.tryGet key |? HashSet.emptyWith valueStrategy;
  let oldCount = oldSet |> HashSet.count;
  let set = oldSet |> HashSet.put value;

  {
    count: count - oldCount + (set |> HashSet.count),
    map: map |> HashMap.put key set,
    valueStrategy,
  }
};

let reduce (f: 'acc => 'v => 'acc) (acc: 'acc) ({ map }: hashSetMultimap 'k 'v): 'acc => {
  let rec reducer acc _ values =>
    values |> HashSet.reduce (fun acc v => f acc v) acc;

  map |> HashMap.reduceWithKey reducer acc;
};

let reduceWithKey (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) ({ map }: hashSetMultimap 'k 'v): 'acc => {
  let rec reducer acc key values =>
    values |> HashSet.reduce (fun acc v => f acc key v) acc;

  map |> HashMap.reduceWithKey reducer acc;
};

let remove
    (key: 'k)
    ({ map, count, valueStrategy } as multimap: hashSetMultimap 'k 'v): (hashSetMultimap 'k 'v) =>
  map |> HashMap.tryGet key >>| (fun set => ({
    count: count - (HashSet.count set),
    map: map |> HashMap.remove key,
    valueStrategy,
  })) |? multimap;

let removeAll ({ map, valueStrategy }: hashSetMultimap 'k 'v): (hashSetMultimap 'k 'v) =>
  { count: 0, map: map |> HashMap.removeAll, valueStrategy };

let toKeyed ({ map }: hashSetMultimap 'k 'v): (keyed 'k (collection 'v)) => Keyed.create
  count::(map |> HashMap.count)
  seq::(map |> HashMap.toSeq |> Seq.map (Pair.mapSnd HashSet.toCollection))
  tryGet::(fun k => map |> HashMap.tryGet k >>| HashSet.toCollection);

let toSeq ({ map }: hashSetMultimap 'k 'v): (seq ('k, 'v)) =>
  map |> HashMap.toSeq |> Seq.flatMap (
    fun (k, set) => set |> HashSet.toSeq |> Seq.map (Pair.create k)
  );
