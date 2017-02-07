/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Collection;
open Comparator;
open Equality;
open Functions;
open Hash;
open Option.Operators;
open Ordering;
open Pair;
open Seq;

type keyed 'k 'v = {
  count: int,
  seq: (seq (('k, 'v))),
  tryGet: 'k => (option 'v),
};

type keyComparatorFinder 'k 'v = 'k => ('k, 'v) => ordering;
type keyEqualityFinder 'k 'v = 'k => ('k, 'v) => bool;

let module KeyedEntry = {
  let alter
      (key: 'k)
      (f: option 'v => option 'v)
      (entry: option ('k, 'v)): (option ('k, 'v)) => switch entry {
    | Some ((k, v) as entry) =>
        f (Some v) >>| (fun newV => newV === v ? entry : (key, newV))
    | None => f None >>| (fun v => (key, v))
  };

  let keyComparator
      (keyComparator: comparator 'k)
      ((thatKey, _): ('k, 'v))
      ((thisKey, _): ('k, 'v)): ordering =>
    keyComparator thatKey thisKey;

  let keyEquality
      (keyEquality: equality 'k)
      ((thatKey, _): ('k, 'v))
      ((thisKey, _): ('k, 'v)): bool =>
    keyEquality thatKey thisKey;

  let keyComparatorFinder (keyCompare: comparator 'k) (key: 'k) ((entryKey, _): ('k, 'v)): ordering =>
    keyCompare key entryKey;

  let keyEqualityFinder (keyEquality: equality 'k) (key: 'k) ((entryKey, _): ('k, 'v)): bool =>
    keyEquality key entryKey;

  let keyHash (keyHash: hash 'k) ((key, _): ('k, 'v)): int => keyHash key;

  let hash (keyHash: hash 'k) (valueHash: hash 'v) ((key, value): ('k, 'v)) =>
    (keyHash key) lxor (valueHash value);
};

let contains (valueEquality: equality 'v) (key: 'k) (value: 'v) (keyed: keyed 'k 'v): bool =>
  keyed.tryGet key >>| (valueEquality value) |? false;

let containsKey (key: 'k) (keyed: keyed 'k 'v): bool =>
  keyed.tryGet key >>| alwaysTrue |? false;

let count (keyed: keyed 'k 'v) => keyed.count;

let create
    count::(count: int)
    seq::(seq: seq ('k, 'v))
    tryGet::(tryGet: 'k => (option 'v)): (keyed 'k 'v) =>
  ({ count, seq, tryGet });

let empty: (keyed 'k 'v) = {
  count: 0,
  seq: Seq.empty,
  tryGet: alwaysNone,
};

let equalsWith (valueEquality: equality 'v) (that: keyed 'k 'v) (this: keyed 'k 'v): bool => (this === that) || (
  (this.count == that.count) &&
  this.seq |> Seq.every (
    fun (key, thisValue) => that.tryGet key
    >>| (fun thatValue => thisValue |> valueEquality thatValue)
    |? false
  )
);

let equals (that: keyed 'k 'v) (this: keyed 'k 'v): bool =>
  equalsWith Equality.structural that this;

let hash (keyHash: hash 'k) (valueHash: hash 'v) ({ seq }: keyed 'k 'v): int =>
  seq |> Seq.hash (KeyedEntry.hash keyHash valueHash);

let keys ({ count, seq } as keyed: keyed 'k 'v): (collection 'k) => Collection.create
  contains::(fun key => keyed |> containsKey key)
  count::count
  seq::(seq |> Seq.map fst);

let map (f: 'v1 => 'v2) ({ count, seq, tryGet }: keyed 'k 'v1): (keyed 'k 'v2) => create
  count::count
  seq::(seq |> Seq.map (Pair.mapSnd f))
  tryGet::(fun k => tryGet k >>| f);

let mapWithKey (f: 'k => 'v1 => 'v2) ({ count, seq, tryGet }: keyed 'k 'v1): (keyed 'k 'v2) => create
  count::count
  seq::(seq |> Seq.map (Pair.mapSndWithFst f))
  tryGet::(fun k => tryGet k >>| f k);

let ofCollection (col: collection 'a): (keyed 'a 'a) => create
  count::(col |> Collection.count)
  seq::(col |> Collection.toSeq |> Seq.map (Pair.pairify))
  tryGet::(fun a => (col |> Collection.contains a) ? Some a : None);

let toCollection
    (valueEquality: equality 'v)
    ({ count, seq } as keyed: keyed 'k 'v): (collection ('k, 'v)) => Collection.create
  contains::(fun (k, v) => keyed |> contains valueEquality k v)
  count::count
  seq::seq;

let toSeq (keyed: keyed 'k 'v): (seq ('k, 'v)) => keyed.seq;

let tryGet (key: 'k) (keyed: keyed 'k 'v): (option 'v) => keyed.tryGet key;
