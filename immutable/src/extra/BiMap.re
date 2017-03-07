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

let contains (key: 'k) (value: 'v) ({ map, inverse }: biMap 'k 'v): bool =>
  map |> HashMap.containsWith (HashStrategy.equals inverse.strategy) key value;

let containsKey (key: 'k) ({ map }: biMap 'k 'v): bool =>
  map |> HashMap.containsKey key;

let count ({ map }: biMap 'k 'v) => map |> HashMap.count;

let empty: (biMap 'k 'v) = {
  map: HashMap.empty,
  inverse: HashMap.empty,
};

let emptyWith
    (keyStrategy: hashStrategy 'k)
    (valueStrategy: hashStrategy 'v): (biMap 'k 'v) => {
  map: HashMap.emptyWith keyStrategy,
  inverse: HashMap.emptyWith valueStrategy,
};

let equals ({ map: thisMap, inverse: thisInverse }: biMap 'k 'v) ({ map: thatMap }: biMap 'k 'v): bool => {
  let { strategy: valueStrategy } = thisInverse;
  HashMap.equalsWith (HashStrategy.equals valueStrategy) thisMap thatMap;
};

let every (f: 'k => 'v => bool) ({ map }: biMap 'k 'v): bool =>
  map |> HashMap.every f;

let find (f: 'k => 'v => bool) ({ map }: biMap 'k 'v): ('k, 'v) =>
  map |> HashMap.find f;

let forEach (f: 'k => 'v => unit) ({ map }: biMap 'k 'v) =>
  map |> HashMap.forEach f;

let get (key: 'k) ({ map }: biMap 'k 'v): 'v =>
  map |> HashMap.get key;

let hash ({ map, inverse }: biMap 'k 'v): int =>
  map |> HashMap.hashWith (HashStrategy.hash inverse.strategy);

let keys ({ map }: biMap 'k 'v): (collection 'k) =>
  map |> HashMap.keys;

let inverse ({ map, inverse }: biMap 'k 'v): biMap 'v 'k => {
  map: inverse,
  inverse: map,
};

let isEmpty ({ map }: biMap 'k 'v): bool =>
  map |> HashMap.isEmpty;

let isNotEmpty ({ map }: biMap 'k 'v): bool =>
  map |> HashMap.isNotEmpty;

let none (f: 'k => 'v => bool) ({ map }: biMap 'k 'v): bool =>
  map |> HashMap.none f;

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

let reduce (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) ({ map }: biMap 'k 'v): 'acc =>
  map |> HashMap.reduce f acc;

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

let removeValue (value: 'v) ({ map, inverse } as biMap: biMap 'k 'v): (biMap 'k 'v) => {
  let updated = inverse |> HashMap.tryGet value >>| fun key => ({
    map: map |> HashMap.remove key,
    inverse: inverse |> HashMap.remove value,
  }: (biMap 'k 'v));

  updated |? biMap;
};

let some (f: 'k => 'v => bool) ({ map }: biMap 'k 'v): bool =>
  map |> HashMap.some f;

let toCollection ({ map, inverse }: biMap 'k 'v): (collection ('k, 'v)) =>
  /* Kind of cheating */
  map |> HashMap.toCollectionWith (HashStrategy.equals inverse.strategy);

let toInverseMap ({ inverse }: biMap 'k 'v): (hashMap 'k 'k) => inverse;

let toKeyed ({ map }: biMap 'k 'v): (keyed 'k 'v) => map |> HashMap.toKeyed;

let toSeq ({ map }: biMap 'k 'v): (seq ('k, 'v)) => map |> HashMap.toSeq;

let tryFind (f: 'k => 'v => bool) ({ map }: biMap 'k 'v): (option ('k, 'v)) =>
  map |> HashMap.tryFind f;

let tryGet (key: 'k) ({ map }: biMap 'k 'v): (option 'v) =>
  map |> HashMap.tryGet key;

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

let values ({ inverse }: biMap 'k 'v): (collection 'v) =>
  inverse |> HashMap.keys;

type transientBiMap 'k 'v = {
  map: transientHashMap 'k 'v,
  inverse: transientHashMap 'v 'k,
};

let mutate ({ map, inverse }: biMap 'k 'v): (transientBiMap 'k 'v) => ({
  map: map |> HashMap.mutate,
  inverse: inverse |> HashMap.mutate,
});

let module TransientBiMap = {
  let count ({ map }: transientBiMap 'k 'v) => map |> TransientHashMap.count;

  let empty (): (transientBiMap 'k 'v) =>
    empty |> mutate;

  let emptyWith
      (keyStrategy: hashStrategy 'k)
      (valueStrategy: hashStrategy 'v): (transientBiMap 'k 'v) =>
    emptyWith keyStrategy valueStrategy |> mutate;

  let isEmpty (transient: transientBiMap 'k 'v): bool =>
    transient |> count == 0;

  let isNotEmpty (transient: transientBiMap 'k 'v): bool =>
    transient |> count != 0;

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
  empty |> putAll seq;

let fromSeqWith
    (keyStrategy: hashStrategy 'k)
    (valueStrategy: hashStrategy 'v)
    (seq: seq ('k, 'v)): (biMap 'k 'v) =>
  emptyWith keyStrategy valueStrategy |> putAll seq;
