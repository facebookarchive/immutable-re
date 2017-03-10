open Equality;
open Set;
open Keyed;
open HashMap;
open HashStrategy;
open Option.Operators;
open Seq;

type hashBiMap 'k 'v = {
  map: hashMap 'k 'v,
  inverse: hashMap 'v 'k
};

let contains (key: 'k) (value: 'v) ({ map, inverse }: hashBiMap 'k 'v): bool =>
  map |> HashMap.containsWith (HashStrategy.equals inverse.strategy) key value;

let containsKey (key: 'k) ({ map }: hashBiMap 'k 'v): bool =>
  map |> HashMap.containsKey key;

let count ({ map }: hashBiMap 'k 'v) => map |> HashMap.count;

let empty: (hashBiMap 'k 'v) = {
  map: HashMap.empty,
  inverse: HashMap.empty,
};

let emptyWith
    (keyStrategy: hashStrategy 'k)
    (valueStrategy: hashStrategy 'v): (hashBiMap 'k 'v) => {
  map: HashMap.emptyWith keyStrategy,
  inverse: HashMap.emptyWith valueStrategy,
};

let equals ({ map: thisMap, inverse: thisInverse }: hashBiMap 'k 'v) ({ map: thatMap }: hashBiMap 'k 'v): bool => {
  let { strategy: valueStrategy } = thisInverse;
  HashMap.equalsWith (HashStrategy.equals valueStrategy) thisMap thatMap;
};

let every (f: 'k => 'v => bool) ({ map }: hashBiMap 'k 'v): bool =>
  map |> HashMap.every f;

let find (f: 'k => 'v => bool) ({ map }: hashBiMap 'k 'v): ('k, 'v) =>
  map |> HashMap.find f;

let forEach (f: 'k => 'v => unit) ({ map }: hashBiMap 'k 'v) =>
  map |> HashMap.forEach f;

let get (key: 'k) ({ map }: hashBiMap 'k 'v): 'v =>
  map |> HashMap.get key;

let hash ({ map, inverse }: hashBiMap 'k 'v): int =>
  map |> HashMap.hashWith (HashStrategy.hash inverse.strategy);

let keys ({ map }: hashBiMap 'k 'v): (set 'k) =>
  map |> HashMap.keys;

let inverse ({ map, inverse }: hashBiMap 'k 'v): hashBiMap 'v 'k => {
  map: inverse,
  inverse: map,
};

let isEmpty ({ map }: hashBiMap 'k 'v): bool =>
  map |> HashMap.isEmpty;

let isNotEmpty ({ map }: hashBiMap 'k 'v): bool =>
  map |> HashMap.isNotEmpty;

let none (f: 'k => 'v => bool) ({ map }: hashBiMap 'k 'v): bool =>
  map |> HashMap.none f;

let put
    (key: 'k)
    (value: 'v)
    ({ map, inverse } as hashBiMap: hashBiMap 'k 'v): (hashBiMap 'k 'v) =>
  switch (map |> HashMap.tryGet key, inverse |> HashMap.tryGet value) {
    | (Some oldValue, _) when oldValue === value => hashBiMap
    | (_, Some oldKey) => ({
        map: map |> HashMap.remove oldKey |> HashMap.put key value,
        inverse: inverse |> HashMap.remove value |> HashMap.put value key,
      })
    | _ => ({
        map: map |> HashMap.put key value,
        inverse: inverse |> HashMap.put value key,
      })
  };

let reduce (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) ({ map }: hashBiMap 'k 'v): 'acc =>
  map |> HashMap.reduce f acc;

let remove
    (key: 'k)
    ({ map, inverse } as hashBiMap: hashBiMap 'k 'v): (hashBiMap 'k 'v) => {
  let updated = map |> HashMap.tryGet key >>| fun value => ({
    map: map |> HashMap.remove key,
    inverse: inverse |> HashMap.remove value,
  }: (hashBiMap 'k 'v));
  updated |? hashBiMap;
};

let removeAll ({ map, inverse }: hashBiMap 'k 'v): (hashBiMap 'k 'v) =>
  { map: map |> HashMap.removeAll, inverse: inverse |> HashMap.removeAll };

let removeValue (value: 'v) ({ map, inverse } as hashBiMap: hashBiMap 'k 'v): (hashBiMap 'k 'v) => {
  let updated = inverse |> HashMap.tryGet value >>| fun key => ({
    map: map |> HashMap.remove key,
    inverse: inverse |> HashMap.remove value,
  }: (hashBiMap 'k 'v));

  updated |? hashBiMap;
};

let some (f: 'k => 'v => bool) ({ map }: hashBiMap 'k 'v): bool =>
  map |> HashMap.some f;

let toSet ({ map, inverse }: hashBiMap 'k 'v): (set ('k, 'v)) =>
  /* Kind of cheating */
  map |> HashMap.toSetWith (HashStrategy.equals inverse.strategy);

let toInverseMap ({ inverse }: hashBiMap 'k 'v): (hashMap 'k 'k) => inverse;

let toKeyed ({ map }: hashBiMap 'k 'v): (keyed 'k 'v) => map |> HashMap.toKeyed;

let toSeq ({ map }: hashBiMap 'k 'v): (seq ('k, 'v)) => map |> HashMap.toSeq;

let tryFind (f: 'k => 'v => bool) ({ map }: hashBiMap 'k 'v): (option ('k, 'v)) =>
  map |> HashMap.tryFind f;

let tryGet (key: 'k) ({ map }: hashBiMap 'k 'v): (option 'v) =>
  map |> HashMap.tryGet key;

let tryPut
    (key: 'k)
    (value: 'v)
    ({ map, inverse } as hashBiMap: hashBiMap 'k 'v): (hashBiMap 'k 'v) =>
  switch (map |> HashMap.tryGet key, inverse |> HashMap.tryGet value) {
    | (Some oldValue, _) when oldValue === value => hashBiMap
    | (_, Some oldKey) => hashBiMap
    | _ => ({
        map: map |> HashMap.put key value,
        inverse: inverse |> HashMap.put value key,
      })
  };

let values ({ inverse }: hashBiMap 'k 'v): (set 'v) =>
  inverse |> HashMap.keys;

type transientHashBiMap 'k 'v = {
  map: transientHashMap 'k 'v,
  inverse: transientHashMap 'v 'k,
};

let mutate ({ map, inverse }: hashBiMap 'k 'v): (transientHashBiMap 'k 'v) => ({
  map: map |> HashMap.mutate,
  inverse: inverse |> HashMap.mutate,
});

let module TransientHashBiMap = {
  let count ({ map }: transientHashBiMap 'k 'v) => map |> TransientHashMap.count;

  let empty (): (transientHashBiMap 'k 'v) =>
    empty |> mutate;

  let emptyWith
      (keyStrategy: hashStrategy 'k)
      (valueStrategy: hashStrategy 'v): (transientHashBiMap 'k 'v) =>
    emptyWith keyStrategy valueStrategy |> mutate;

  let isEmpty (transient: transientHashBiMap 'k 'v): bool =>
    transient |> count == 0;

  let isNotEmpty (transient: transientHashBiMap 'k 'v): bool =>
    transient |> count != 0;

  let persist ({ map, inverse }: transientHashBiMap 'k 'v): (hashBiMap 'k 'v) => ({
    map: map |> TransientHashMap.persist,
    inverse: inverse |> TransientHashMap.persist,
  });

  let put
      (key: 'k)
      (value: 'v)
      ({ map, inverse } as transient: transientHashBiMap 'k 'v): (transientHashBiMap 'k 'v) => {
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
      (map: transientHashBiMap 'k 'v): (transientHashBiMap 'k 'v) => seq
    |> Seq.reduce (fun acc (k, v) => acc |> put k v) map;

  let remove
      (key: 'k)
      ({ map, inverse } as transient: transientHashBiMap 'k 'v): (transientHashBiMap 'k 'v) => {
    map |> TransientHashMap.tryGet key >>| (fun value => {
      map |> TransientHashMap.remove key |> ignore;
      inverse |> TransientHashMap.remove value |> ignore;
    }) |? ();

    transient
  };

  let removeAll ({ map, inverse } as transient: transientHashBiMap 'k 'v): (transientHashBiMap 'k 'v) => {
    map |> TransientHashMap.removeAll |> ignore;
    inverse |> TransientHashMap.removeAll |> ignore;
    transient
  };

  let removeValue (value: 'v) ({ map, inverse } as transient: transientHashBiMap 'k 'v): (transientHashBiMap 'k 'v) => {
    inverse |> TransientHashMap.tryGet value >>| (fun key => {
      map |> TransientHashMap.remove key |> ignore;
      inverse |> TransientHashMap.remove value |> ignore;
    }) |? ();

    transient
  };

  let tryGet (key: 'k) ({ map }: transientHashBiMap 'k 'v): (option 'v) =>
    map |> TransientHashMap.tryGet key;

  let tryPut
      (key: 'k)
      (value: 'v)
      ({ map, inverse } as transient: transientHashBiMap 'k 'v): (transientHashBiMap 'k 'v) => {
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

let putAll (seq: seq ('k, 'v)) (map: hashBiMap 'k 'v): (hashBiMap 'k 'v) => map
  |> mutate
  |> TransientHashBiMap.putAll seq
  |> TransientHashBiMap.persist;

let fromSeq (seq: seq ('k, 'v)): (hashBiMap 'k 'v) =>
  empty |> putAll seq;

let fromSeqWith
    (keyStrategy: hashStrategy 'k)
    (valueStrategy: hashStrategy 'v)
    (seq: seq ('k, 'v)): (hashBiMap 'k 'v) =>
  emptyWith keyStrategy valueStrategy |> putAll seq;
