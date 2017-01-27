/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

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

type transientHashMultiset 'a = {
  mutable count: int,
  map: (transientHashMap 'a int),
};

let add (value: 'a) ({ count, map }: hashMultiset 'a): (hashMultiset 'a) => ({
  count: count + 1,
  map: map |> HashMap.put value (
    (map |> HashMap.tryGet value >>| succ) |? 1
  ),
});

let contains (value: 'a) ({ count, map }: hashMultiset 'a): bool =>
  map |> HashMap.tryGet value >>| alwaysTrue |? false;

let count ({ count }: hashMultiset 'a): int => count;

let empty (): (hashMultiset 'a) => ({
  count: 0,
  map: HashMap.empty (),
});

let emptyWith (keyStrategy: hashStrategy 'a): (hashMultiset 'a) => {
  count: 0,
  map: HashMap.emptyWith keyStrategy,
};

let get (value: 'a) ({ map }: hashMultiset 'a): int => (map |> HashMap.tryGet value) |? 0;

let mutate ({ count, map}: hashMultiset 'a): (transientHashMultiset 'a) => ({
  count: count,
  map: map |> HashMap.mutate,
});

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) ({ map }: hashMultiset 'a): 'acc => {
  let rec reducer acc key value => value == 0 ? acc : {
    let acc = f acc key;
    reducer acc key (value - 1)
  };

  map |> HashMap.reduceWithKey reducer acc;
};

let remove (value: 'a) ({ count, map } as hashMultiset: hashMultiset 'a): (hashMultiset 'a) => map
  |> HashMap.tryGet value
  >>| (fun valueCount => ({
      count: count - valueCount,
      map: map |> HashMap.remove value
    }: (hashMultiset 'a)))
  |? hashMultiset;

let removeAll ({ count, map }: hashMultiset 'a): (hashMultiset 'a) =>
  { count: 0, map: map |> HashMap.removeAll };

let set (value: 'a) (valueCount: int) ({ count, map }: hashMultiset 'a): (hashMultiset 'a) => ({
  count: count + valueCount - (map |> HashMap.tryGet value |? 0),
  map: map |> HashMap.put value valueCount,
});

let toKeyed ({ map }: hashMultiset 'a): (keyed 'a int) => map |> HashMap.toKeyed;

let toSeq ({ map }: hashMultiset 'a): (seq 'a) => map
  |> HashMap.toSeq
  |> Seq.flatMap(fun (v, count) => Seq.repeat v (Some count));

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

  let get (value: 'a) ({ map }: transientHashMultiset 'a): int =>
    (map |> TransientHashMap.tryGet value) |? 0;

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
  (empty ()) |> addAll seq;

let fromSeqWith (strategy: hashStrategy 'a) (seq: seq 'a): (hashMultiset 'a) =>
  (emptyWith strategy) |> addAll seq;
