open Comparator;
open Equality;
open Hash;
open Option.Operators;
open Ordering;

type keyComparatorFinder 'k 'v = 'k => ('k, 'v) => ordering;
type keyEqualityFinder 'k 'v = 'k => ('k, 'v) => bool;

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

let hash (keyHash: hash 'k) (valueHash: hash 'v) (key: 'k) (value: 'v): int =>
  (keyHash key) lxor (valueHash value);

let hashReducer
    (keyHash: hash 'k)
    (valueHash: hash 'v)
    (acc: int)
    (key: 'k)
    (value: 'v): int =>
  acc + (hash keyHash valueHash key value);
