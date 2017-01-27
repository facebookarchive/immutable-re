/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Equality;
open Collection;
open Keyed;
open HashMap;
open HashStrategy;
open Option.Operators;
open Seq;

type biMap 'k 'v = {
  map: hashMap 'k 'v,
  inverse: hashMap 'v 'k
};

type transientBiMap 'k 'v = {
  map: transientHashMap 'k 'v,
  inverse: transientHashMap 'v 'k,
};

let count ({ map }: biMap 'k 'v) => map |> HashMap.count;

let empty (): (biMap 'k 'v) => ({
  map: HashMap.empty (),
  inverse: HashMap.empty ()
});

let emptyWith
    (keyStrategy: hashStrategy 'k)
    (valueStrategy: hashStrategy 'v): (biMap 'k 'v) => ({
  map: HashMap.emptyWith keyStrategy,
  inverse: HashMap.emptyWith valueStrategy,
});

let inverse ({ map, inverse }: biMap 'k 'v): biMap 'v 'k => ({
  map: inverse,
  inverse: map,
});

let mutate ({ map, inverse }: biMap 'k 'v): (transientBiMap 'k 'v) => ({
  map: map |> HashMap.mutate,
  inverse: inverse |> HashMap.mutate,
});

let put
    (key: 'k)
    (value: 'v)
    ({ map, inverse } as biMap: biMap 'k 'v): (biMap 'k 'v) =>
  switch (map |> HashMap.tryGet key, inverse |> HashMap.tryGet value) {
    | (Some oldValue, _) when oldValue === value => biMap
    | (_, Some oldKey) => ({
        map: map |> HashMap.remove oldKey |> HashMap.put key value,
        inverse: inverse |> HashMap.remove value |> HashMap.put value key,
      })
    | _ => ({
        map: map |> HashMap.put key value,
        inverse: inverse |> HashMap.put value key,
      })
  };

let reduce (f: 'acc => 'v => 'acc) (acc: 'acc) ({ map }: biMap 'k 'v): 'acc =>
  map |> HashMap.reduce f acc;

let reduceWithKey (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) ({ map }: biMap 'k 'v): 'acc =>
  map |> HashMap.reduceWithKey f acc;

let remove
    (key: 'k)
    ({ map, inverse } as biMap: biMap 'k 'v): (biMap 'k 'v) => {
  let updated = map |> HashMap.tryGet key >>| fun value => ({
    map: map |> HashMap.remove key,
    inverse: inverse |> HashMap.remove value,
  }: (biMap 'k 'v));
  updated |? biMap;
};

let removeAll ({ map, inverse }: biMap 'k 'v): (biMap 'k 'v) =>
  { map: map |> HashMap.removeAll, inverse: inverse |> HashMap.removeAll };

let toInverseMap ({ inverse }: biMap 'k 'v): (hashMap 'k 'k) => inverse;

let toKeyed ({ map }: biMap 'k 'v): (keyed 'k 'v) => map |> HashMap.toKeyed;

let toMap ({ map }: biMap 'k 'v): (hashMap 'k 'v) => map;

let toSeq ({ map }: biMap 'k 'v): (seq ('k, 'v)) => map |> HashMap.toSeq;

let tryGet (key: 'k) ({ map }: biMap 'k 'v): (option 'v) => map |> HashMap.tryGet key;

let tryPut
    (key: 'k)
    (value: 'v)
    ({ map, inverse } as biMap: biMap 'k 'v): (biMap 'k 'v) =>
  switch (map |> HashMap.tryGet key, inverse |> HashMap.tryGet value) {
    | (Some oldValue, _) when oldValue === value => biMap
    | (_, Some oldKey) => biMap
    | _ => ({
        map: map |> HashMap.put key value,
        inverse: inverse |> HashMap.put value key,
      })
  };

let module TransientBiMap = {
  let count ({ map }: transientBiMap 'k 'v) => map |> TransientHashMap.count;

  let persist ({ map, inverse }: transientBiMap 'k 'v): (biMap 'k 'v) => ({
    map: map |> TransientHashMap.persist,
    inverse: inverse |> TransientHashMap.persist,
  });

  let put
      (key: 'k)
      (value: 'v)
      ({ map, inverse } as transient: transientBiMap 'k 'v): (transientBiMap 'k 'v) => {
    switch (map |> TransientHashMap.tryGet key, inverse |> TransientHashMap.tryGet value) {
      | (Some oldValue, _) when oldValue === value => ()
      | (_, Some oldKey) =>
          map |> TransientHashMap.remove oldKey |> TransientHashMap.put key value |> ignore;
          inverse |> TransientHashMap.remove value |> TransientHashMap.put value key |> ignore;
      | _ =>
          map |> TransientHashMap.put key value |> ignore;
          inverse |> TransientHashMap.put value key |> ignore;
    };

    transient
  };

  let putAll
      (seq: seq ('k, 'v))
      (map: transientBiMap 'k 'v): (transientBiMap 'k 'v) => seq
    |> Seq.reduce (fun acc (k, v) => acc |> put k v) map;

  let remove
      (key: 'k)
      ({ map, inverse } as transient: transientBiMap 'k 'v): (transientBiMap 'k 'v) => {
    map |> TransientHashMap.tryGet key >>| (fun value => {
      map |> TransientHashMap.remove key |> ignore;
      inverse |> TransientHashMap.remove value |> ignore;
    }) |? ();

    transient
  };

  let removeAll ({ map, inverse } as transient: transientBiMap 'k 'v): (transientBiMap 'k 'v) => {
    map |> TransientHashMap.removeAll |> ignore;
    inverse |> TransientHashMap.removeAll |> ignore;
    transient
  };

  let tryGet (key: 'k) ({ map }: transientBiMap 'k 'v): (option 'v) =>
    map |> TransientHashMap.tryGet key;

  let tryPut
      (key: 'k)
      (value: 'v)
      ({ map, inverse } as transient: transientBiMap 'k 'v): (transientBiMap 'k 'v) => {
    switch (map |> TransientHashMap.tryGet key, inverse |> TransientHashMap.tryGet value) {
      | (Some oldValue, _) when oldValue === value => ()
      | (_, Some oldKey) => ()
      | _ => {
          map |> TransientHashMap.put key value |> ignore;
          inverse |> TransientHashMap.put value key |> ignore;
        }
    };

    transient
  };
};

let putAll (seq: seq ('k, 'v)) (map: biMap 'k 'v): (biMap 'k 'v) => map
  |> mutate
  |> TransientBiMap.putAll seq
  |> TransientBiMap.persist;

let fromSeq (seq: seq ('k, 'v)): (biMap 'k 'v) =>
  empty () |> putAll seq;

let fromSeqWith
    (keyStrategy: hashStrategy 'k)
    (valueStrategy: hashStrategy 'v)
    (seq: seq ('k, 'v)): (biMap 'k 'v) =>
  emptyWith keyStrategy valueStrategy |> putAll seq;
