/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Equality;
open Functions;
open Functions.Operators;
open Hash;
open Seq;

type collection 'a = {
  contains: 'a => bool,
  count: int,
  seq: (seq 'a),
};

let contains (value: 'a) (collection: collection 'a): bool =>
  collection.contains value;

let count (collection: collection 'a): int => collection.count;

let create
  contains::(contains: 'a => bool)
  count::(count: int)
  seq::(seq: seq 'a): (collection 'a) => ({ contains, count, seq });

let empty: (collection 'a) = {
  contains: alwaysFalse,
  count: 0,
  seq: Seq.empty,
};

let equals (that: collection 'a) (this: collection 'a): bool => (this === that) || (
  (this.count == that.count) &&
  this.seq |> Seq.every that.contains
);

let hashWith (hash: hash 'a) (collection: collection 'a): int =>
  collection.seq |> Seq.hashWith hash;

let hash (collection: collection 'a): int =>
  collection.seq |> Seq.hash;

let intersect (that: collection 'a) (this: collection 'a): (seq 'a) =>
  this.seq |> Seq.filter (that.contains);

let subtractFrom (that: collection 'a) (this: collection 'a): (seq 'a) =>
  that.seq |> Seq.filter (this.contains >> not);

let toSeq (collection: collection 'a): (seq 'a) => collection.seq;

let union (that: collection 'a) (this: collection 'a): (seq 'a) => Seq.concat [
  this.seq,
  that.seq |> Seq.filter (this.contains >> not)
];
