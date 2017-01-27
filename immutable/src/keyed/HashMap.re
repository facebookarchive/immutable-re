/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Equality;
open Functions;
open HashSet;
open HashStrategy;
open Keyed;
open Option;
open Option.Operators;
open Pair;
open Seq;

type hashMap 'k 'v = {
  set: hashSet ('k, 'v),
  strategy: hashStrategy 'k,
};

type transientHashMap 'k 'v = {
  set: transientHashSet ('k, 'v),
  strategy: hashStrategy 'k,
};

let alter
    (key: 'k)
    (f: option 'v => option 'v)
    ({ set, strategy } as map: hashMap 'k 'v): (hashMap 'k 'v) => {
  let hash = strategy |> HashStrategy.hash key;
  let keyFinder = KeyedEntry.keyComparatorFinder (HashStrategy.comparator strategy) key;
  let newSet = set |> HashSet.alter hash keyFinder (KeyedEntry.alter key f);
  set === newSet ? map : ({ set: newSet, strategy });
};

let count ({ set }: hashMap 'k 'v): int => set |> HashSet.count;

let makeKeyedEntryStrategy (strategy: hashStrategy 'k): (hashStrategy ('k, 'v)) => switch strategy {
  | Comparator hash comparator => Comparator
      (KeyedEntry.keyHash hash)
      (KeyedEntry.keyComparator comparator)
  | Equality hash equality => Equality
      (KeyedEntry.keyHash hash)
      (KeyedEntry.keyEquality equality)
};

let emptyWith (strategy: hashStrategy 'k): hashMap 'k 'v => {
  let hashSetStrategy = makeKeyedEntryStrategy strategy;
  { set: HashSet.emptyWith hashSetStrategy, strategy }
};

let empty (): hashMap 'k 'v =>
  emptyWith (HashStrategy.structuralCompare ());

let mutate ({ set, strategy }: hashMap 'k 'v): (transientHashMap 'k 'v) =>
  { set: set |> HashSet.mutate, strategy };

let put (key: 'k) (value: 'v) ({ set, strategy } as map: hashMap 'k 'v): (hashMap 'k 'v) => {
  let newSet = set |> HashSet.put (key, value);
  set === newSet ? map : ({ set: newSet, strategy });
};

let remove (key: 'k) (map: hashMap 'k 'v): (hashMap 'k 'v) =>
  map |> alter key alwaysNone;

let removeAll ({ strategy }: hashMap 'k 'v): (hashMap 'k 'v) =>
  emptyWith strategy;

let toSeq ({ set }: hashMap 'k 'v): (seq ('k, 'v)) => set |> HashSet.toSeq;

let tryGet
    (key: 'k)
    ({ set, strategy }: hashMap 'k 'v): (option 'v) => set
  |> HashSet.find
    (strategy |> HashStrategy.hash key)
    (KeyedEntry.keyComparatorFinder (HashStrategy.comparator strategy) key)
  >>| snd;

let toKeyed (map: hashMap 'k 'v): (keyed 'k 'v) => Keyed.create
  count::(map |> count)
  seq::(map |> toSeq)
  tryGet::(fun k => map |> tryGet k);

let module TransientHashMap = {
  let count ({ set }: transientHashMap 'k 'v): int => set |> TransientHashSet.count;

  let alter
      (key: 'k)
      (f: option 'v => option 'v)
      ({ set, strategy } as transient: transientHashMap 'k 'v): (transientHashMap 'k 'v) => {
    let hash = strategy |> HashStrategy.hash key;
    let keyFinder = KeyedEntry.keyComparatorFinder (HashStrategy.comparator strategy) key;

    set |> TransientHashSet.alter hash keyFinder (KeyedEntry.alter key f) |> ignore;
    transient
  };

  let persist ({ set, strategy }: transientHashMap 'k 'v): (hashMap 'k 'v) =>
    { set: set |> TransientHashSet.persist, strategy };

  let put
      (key: 'k)
      (value: 'v)
      ({ set, strategy } as transient: transientHashMap 'k 'v): (transientHashMap 'k 'v) => {
    set |> TransientHashSet.put (key, value) |> ignore;
    transient
  };

  let putAll (seq: seq ('k, 'v)) (transient: transientHashMap 'k 'v): (transientHashMap 'k 'v) => seq
    |> Seq.reduce (fun acc (k, v) => acc |> put k v) transient;

  let remove (key: 'k) (map: transientHashMap 'k 'v): (transientHashMap 'k 'v) =>
    map |> alter key alwaysNone;

  let removeAll ({ set } as transient: transientHashMap 'k 'v): (transientHashMap 'k 'v) => {
    set |> TransientHashSet.removeAll |> ignore;
    transient
  };

  let tryGet
      (key: 'k)
      ({ set, strategy }: transientHashMap 'k 'v): (option 'v) => set
    |> TransientHashSet.find
      (strategy |> HashStrategy.hash key)
      (KeyedEntry.keyComparatorFinder (HashStrategy.comparator strategy) key)
    >>| snd;
};

let putAll (seq: seq ('k, 'v)) (map: hashMap 'k 'v): (hashMap 'k 'v) =>
  map |> mutate |> TransientHashMap.putAll seq |> TransientHashMap.persist;

let fromSeqWith (strategy: hashStrategy 'k) (seq: seq ('k, 'v)): (hashMap 'k 'v) =>
  emptyWith strategy |> putAll seq;

let fromSeq (seq: seq ('k, 'v)): (hashMap 'k 'v) => fromSeqWith (HashStrategy.structuralCompare ()) seq;

let fromKeyedWith (strategy: hashStrategy 'k) (keyed: keyed 'k 'v): (hashMap 'k 'v) =>
  keyed |> Keyed.toSeq |> fromSeqWith strategy;

let fromKeyed (keyed: keyed 'k 'v): (hashMap 'k 'v) => keyed |> Keyed.toSeq |> fromSeq;

let map (f: 'a => 'b) ({ strategy } as map: hashMap 'k 'a): (hashMap 'k 'b) => {
  let empty = emptyWith strategy;
  empty |> putAll (map |> toSeq |> Seq.map (Pair.mapSnd f));
};

let mapWithKey (f: 'k => 'a => 'b) ({ strategy } as map: hashMap 'k 'a): (hashMap 'k 'b) => {
  let empty = emptyWith strategy;
  empty |> putAll (map |> toSeq |> Seq.map @@ Pair.mapSndWithFst @@ f);
};

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) ({ set }: hashMap 'k 'a): 'acc => {
  let reducer acc (_, value) => f acc value;
  set |> HashSet.reduce reducer acc
};

let reduceWithKey (f: 'acc => 'k => 'a => 'acc) (acc: 'acc) ({ set }: hashMap 'k 'a): 'acc => {
  let reducer acc (key, value) => f acc key value;
  set |> HashSet.reduce reducer acc
};

let merge
    (f: 'k => (option 'vAcc) => (option 'v) => (option 'vAcc))
    (next: keyed 'k 'v)
    (map: hashMap 'k 'vAcc): (hashMap 'k 'vAcc) =>
  Collection.union (map |> toKeyed |> Keyed.keys) (next |> Keyed.keys)
    |> Seq.reduce (
        fun acc key => {
          let result = f key (map |> tryGet key) (next |> Keyed.tryGet key);
          switch result {
            | None => acc |> TransientHashMap.remove key
            | Some value => acc |> TransientHashMap.put key value
          }
        }
      )
      (mutate map)
    |> TransientHashMap.persist;
