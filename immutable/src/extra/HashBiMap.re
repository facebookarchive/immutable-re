open Option.Operators;

type t 'k 'v = {
  map: HashMap.t 'k 'v,
  inverse: HashMap.t 'v 'k
};

let contains (key: 'k) (value: 'v) ({ map, inverse }: t 'k 'v): bool =>
  map |> HashMap.containsWith (HashStrategy.equals {
    /* To get the access to internals of HashMap.t */
    open HashMap;

    inverse.strategy
  }) key value;

let containsKey (key: 'k) ({ map }: t 'k 'v): bool =>
  map |> HashMap.containsKey key;

let count ({ map }: t 'k 'v) => map |> HashMap.count;

let empty: (t 'k 'v) = {
  map: HashMap.empty,
  inverse: HashMap.empty,
};

let emptyWith
    (keyStrategy: HashStrategy.t 'k)
    (valueStrategy: HashStrategy.t 'v): (t 'k 'v) => {
  map: HashMap.emptyWith keyStrategy,
  inverse: HashMap.emptyWith valueStrategy,
};

let equals ({ map: thisMap, inverse: thisInverse }: t 'k 'v) ({ map: thatMap }: t 'k 'v): bool => {
  /* To get the access to internals of HashMap.t */
  open HashMap;

  let { strategy: valueStrategy } = thisInverse;
  HashMap.equalsWith (HashStrategy.equals valueStrategy) thisMap thatMap;
};

let every (f: 'k => 'v => bool) ({ map }: t 'k 'v): bool =>
  map |> HashMap.every f;

let find (f: 'k => 'v => bool) ({ map }: t 'k 'v): ('k, 'v) =>
  map |> HashMap.find f;

let forEach (f: 'k => 'v => unit) ({ map }: t 'k 'v) =>
  map |> HashMap.forEach f;

let get (key: 'k) ({ map }: t 'k 'v): 'v =>
  map |> HashMap.get key;

let hash ({ map, inverse }: t 'k 'v): int =>
  map |> HashMap.hashWith (HashStrategy.hash {
    /* To get the access to internals of HashMap.t */
    open HashMap;
    
    inverse.strategy
  });

let keys ({ map }: t 'k 'v): (ImmSet.t 'k) =>
  map |> HashMap.keys;

let inverse ({ map, inverse }: t 'k 'v): t 'v 'k => {
  map: inverse,
  inverse: map,
};

let isEmpty ({ map }: t 'k 'v): bool =>
  map |> HashMap.isEmpty;

let isNotEmpty ({ map }: t 'k 'v): bool =>
  map |> HashMap.isNotEmpty;

let none (f: 'k => 'v => bool) ({ map }: t 'k 'v): bool =>
  map |> HashMap.none f;

let put
    (key: 'k)
    (value: 'v)
    ({ map, inverse } as hashBiMap: t 'k 'v): (t 'k 'v) =>
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

let reduce (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) ({ map }: t 'k 'v): 'acc =>
  map |> HashMap.reduce f acc;

let remove
    (key: 'k)
    ({ map, inverse } as hashBiMap: t 'k 'v): (t 'k 'v) => {
  let updated = map |> HashMap.tryGet key >>| fun value => ({
    map: map |> HashMap.remove key,
    inverse: inverse |> HashMap.remove value,
  }: (t 'k 'v));
  updated |? hashBiMap;
};

let removeAll ({ map, inverse }: t 'k 'v): (t 'k 'v) =>
  { map: map |> HashMap.removeAll, inverse: inverse |> HashMap.removeAll };

let removeValue (value: 'v) ({ map, inverse } as hashBiMap: t 'k 'v): (t 'k 'v) => {
  let updated = inverse |> HashMap.tryGet value >>| fun key => ({
    map: map |> HashMap.remove key,
    inverse: inverse |> HashMap.remove value,
  }: (t 'k 'v));

  updated |? hashBiMap;
};

let some (f: 'k => 'v => bool) ({ map }: t 'k 'v): bool =>
  map |> HashMap.some f;

let toSet ({ map, inverse }: t 'k 'v): (ImmSet.t ('k, 'v)) =>
  /* Kind of cheating */
  map |> HashMap.toSetWith (HashStrategy.equals {
    /* To get the access to internals of HashMap.t */
    open HashMap;
    inverse.strategy
  });

let toMap ({ map }: t 'k 'v): (ImmMap.t 'k 'v) => map |> HashMap.toMap;

let toSeq ({ map }: t 'k 'v): (Seq.t ('k, 'v)) => map |> HashMap.toSeq;

let tryFind (f: 'k => 'v => bool) ({ map }: t 'k 'v): (option ('k, 'v)) =>
  map |> HashMap.tryFind f;

let tryGet (key: 'k) ({ map }: t 'k 'v): (option 'v) =>
  map |> HashMap.tryGet key;

let tryPut
    (key: 'k)
    (value: 'v)
    ({ map, inverse } as hashBiMap: t 'k 'v): (t 'k 'v) =>
  switch (map |> HashMap.tryGet key, inverse |> HashMap.tryGet value) {
    | (Some oldValue, _) when oldValue === value => hashBiMap
    | (_, Some _) => hashBiMap
    | _ => ({
        map: map |> HashMap.put key value,
        inverse: inverse |> HashMap.put value key,
      })
  };

let values ({ inverse }: t 'k 'v): (ImmSet.t 'v) =>
  inverse |> HashMap.keys;

let module TransientHashBiMap = {
  type hashBiMap 'k 'v = t 'k 'v;

  let module TransientHashMap = HashMap.TransientHashMap;

  type t 'k 'v = {
    map: TransientHashMap.t 'k 'v,
    inverse: TransientHashMap.t 'v 'k,
  };

  let mutate ({ map, inverse }: hashBiMap 'k 'v): (t 'k 'v) => ({
    map: map |> HashMap.mutate,
    inverse: inverse |> HashMap.mutate,
  });

  let count ({ map }: t 'k 'v) => map |> TransientHashMap.count;

  let empty (): (t 'k 'v) =>
    empty |> mutate;

  let emptyWith
      (keyStrategy: HashStrategy.t 'k)
      (valueStrategy: HashStrategy.t 'v): (t 'k 'v) =>
    emptyWith keyStrategy valueStrategy |> mutate;

  let isEmpty (transient: t 'k 'v): bool =>
    transient |> count == 0;

  let isNotEmpty (transient: t 'k 'v): bool =>
    transient |> count != 0;

  let persist ({ map, inverse }: t 'k 'v): (hashBiMap 'k 'v) => ({
    map: map |> TransientHashMap.persist,
    inverse: inverse |> TransientHashMap.persist,
  });

  let put
      (key: 'k)
      (value: 'v)
      ({ map, inverse } as transient: t 'k 'v): (t 'k 'v) => {
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
      (seq: Seq.t ('k, 'v))
      (map: t 'k 'v): (t 'k 'v) => seq
    |> Seq.reduce (fun acc (k, v) => acc |> put k v) map;

  let remove
      (key: 'k)
      ({ map, inverse } as transient: t 'k 'v): (t 'k 'v) => {
    map |> TransientHashMap.tryGet key >>| (fun value => {
      map |> TransientHashMap.remove key |> ignore;
      inverse |> TransientHashMap.remove value |> ignore;
    }) |? ();

    transient
  };

  let removeAll ({ map, inverse } as transient: t 'k 'v): (t 'k 'v) => {
    map |> TransientHashMap.removeAll |> ignore;
    inverse |> TransientHashMap.removeAll |> ignore;
    transient
  };

  let removeValue (value: 'v) ({ map, inverse } as transient: t 'k 'v): (t 'k 'v) => {
    inverse |> TransientHashMap.tryGet value >>| (fun key => {
      map |> TransientHashMap.remove key |> ignore;
      inverse |> TransientHashMap.remove value |> ignore;
    }) |? ();

    transient
  };

  let tryGet (key: 'k) ({ map }: t 'k 'v): (option 'v) =>
    map |> TransientHashMap.tryGet key;

  let tryPut
      (key: 'k)
      (value: 'v)
      ({ map, inverse } as transient: t 'k 'v): (t 'k 'v) => {
    switch (map |> TransientHashMap.tryGet key, inverse |> TransientHashMap.tryGet value) {
      | (Some oldValue, _) when oldValue === value => ()
      | (_, Some _) => ()
      | _ => {
          map |> TransientHashMap.put key value |> ignore;
          inverse |> TransientHashMap.put value key |> ignore;
        }
    };

    transient
  };
};

let mutate = TransientHashBiMap.mutate;

let putAll (seq: Seq.t ('k, 'v)) (map: t 'k 'v): (t 'k 'v) => map
  |> mutate
  |> TransientHashBiMap.putAll seq
  |> TransientHashBiMap.persist;

let fromSeq (seq: Seq.t ('k, 'v)): (t 'k 'v) =>
  empty |> putAll seq;

let fromSeqWith
    (keyStrategy: HashStrategy.t 'k)
    (valueStrategy: HashStrategy.t 'v)
    (seq: Seq.t ('k, 'v)): (t 'k 'v) =>
  emptyWith keyStrategy valueStrategy |> putAll seq;
