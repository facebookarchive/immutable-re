open Equality;
open Functions;
open Functions.Operators;
open Hash;
open Seq;

type collection 'a = {
  contains: 'a => bool,
  count: int,
  toSeq: (seq 'a),
};

let contains (value: 'a) (collection: collection 'a): bool =>
  collection.contains value;

let count (collection: collection 'a): int => collection.count;

let empty: (collection 'a) = {
  contains: alwaysFalse,
  count: 0,
  toSeq: Seq.empty,
};

let every (f: 'a => bool) ({ toSeq }: collection 'a): bool =>
  toSeq |> Seq.every f;

let equals (that: collection 'a) (this: collection 'a): bool =>
  (this === that) ? true :
  (this.count != that.count) ? false :
  this |> every that.contains;

let forEach (f: 'a => unit) ({ toSeq }: collection 'a): unit =>
  toSeq |> Seq.forEach f;

let find (f: 'a => bool) ({ toSeq }: collection 'a): 'a =>
  toSeq |> Seq.find f;

let hashWith (hash: hash 'a) ({ toSeq }: collection 'a): int =>
  toSeq |> Seq.hashWith hash;

let hash ({ toSeq }: collection 'a): int =>
  toSeq |> Seq.hash;

let isEmpty ({ count }: collection 'a): bool =>
  count == 0;

let isNotEmpty ({ count }: collection 'a): bool =>
  count != 0;

let intersect (that: collection 'a) (this: collection 'a): (seq 'a) =>
  this.toSeq |> Seq.filter (that.contains);

let none (f: 'a => bool) ({ toSeq }: collection 'a): bool =>
  toSeq |> Seq.none f;

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) ({ toSeq }: collection 'a): 'acc =>
  toSeq |> Seq.reduce f acc;

let some (f: 'a => bool) ({ toSeq }: collection 'a): bool =>
  toSeq |> Seq.some f;

let subtractFrom (that: collection 'a) (this: collection 'a): (seq 'a) =>
  that.toSeq |> Seq.filter (this.contains >> not);

let tryFind (f: 'a => bool) ({ toSeq }: collection 'a): (option 'a) =>
  toSeq |> Seq.tryFind f;

let toSeq ({ toSeq }: collection 'a): (seq 'a) => toSeq;

let union (that: collection 'a) (this: collection 'a): (seq 'a) => Seq.concat [
  this.toSeq,
  that.toSeq |> Seq.filter (this.contains >> not)
];
