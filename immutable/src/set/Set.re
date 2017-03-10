open Equality;
open Functions;
open Functions.Operators;
open Hash;
open Seq;

type set 'a = {
  contains: 'a => bool,
  count: int,
  every: ('a => bool) => bool,
  find: ('a => bool) => 'a,
  forEach: ('a => unit) => unit,
  none: ('a => bool) =>  bool,
  reduce: 'acc . ('acc => 'a => 'acc) => 'acc => 'acc,
  some: ('a => bool) => bool,
  toSeq: (seq 'a),
  tryFind: ('a => bool) => (option 'a),
};

let contains (value: 'a) ({ contains }: set 'a): bool =>
  contains value;

let count ({ count }: set 'a): int => count;

let empty: (set 'a) = {
  contains: fun _ => false,
  count: 0,
  every: fun f => false,
  find: fun _ => failwith "set is empty",
  forEach: fun _ => (),
  none: fun _ => true,
  reduce: fun _ acc => acc,
  some: fun _ => false,
  toSeq: Seq.empty,
  tryFind: Functions.alwaysNone,
};

let equals (that: set 'a) (this: set 'a): bool =>
  (this === that) ? true :
  (this.count != that.count) ? false :
  this.every that.contains;

let every (f: 'a => bool) ({ every }: set 'a): bool =>
  every f;

let find (f: 'a => bool) ({ find }: set 'a): 'a =>
  find f;

let forEach (f: 'a => unit) ({ forEach }: set 'a): unit =>
  forEach f;

let hashWith (hash: hash 'a) ({ reduce }: set 'a): int =>
  reduce (fun acc next => acc + hash next) 0;

let hash (set: set 'a): int =>
  hashWith Hash.structural set;

let inRange (start: int) (count: int) (step: int): (set int) => {
  Preconditions.failIf "step must be greater or less than 0" (step == 0);

  let toSeq = Seq.inRange start (Some count) step;
  let max = start + (count * step);

  {
    contains: fun index =>
      (step > 0) && (index > start) && (index < max) ? ({
        let effectiveIndex = index - start;
        (effectiveIndex mod step) == 0
      }) :
      (step < 0) && (index < start) && (index > max) ? ({
        let effectiveIndex = index + start;
        (effectiveIndex mod step) != 0
      }) :
      (index == start) && (count != 0),
    count,
    every: fun f => toSeq |> Seq.every f,
    find: fun f => toSeq |> Seq.find f,
    forEach: fun f => toSeq |> Seq.forEach f,
    none: fun f => toSeq |> Seq.none f,
    reduce: fun f acc => toSeq |> Seq.reduce f acc,
    some: fun f => toSeq |> Seq.some f,
    toSeq,
    tryFind: fun f => toSeq |> Seq.tryFind f,
  }
};

let intersect (this: set 'a) (that: set 'a): (seq 'a) =>
  this.toSeq |> Seq.filter (that.contains);

let isEmpty ({ count }: set 'a): bool =>
  count == 0;

let isNotEmpty ({ count }: set 'a): bool =>
  count != 0;

let none (f: 'a => bool) ({ none }: set 'a): bool =>
  none f;

let ofOptionWith (equals: equality 'a) (opt: option 'a): (set 'a) => {
  contains: fun v => Option.containsWith equals v opt,
  count: Option.count opt,
  every: fun f => Option.every f opt,
  find: fun f => Option.find f opt,
  forEach: fun f => Option.forEach f opt,
  none: fun f => Option.none f opt,
  reduce: fun f acc => Option.reduce f acc opt,
  some: fun f => Option.some f opt,
  toSeq: Seq.ofOption opt,
  tryFind: fun f => Option.tryFind f opt,
};

let ofOption (opt: option 'a): (set 'a) =>
  ofOptionWith Equality.structural opt;

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) ({ reduce }: set 'a): 'acc =>
  reduce f acc;

let some (f: 'a => bool) ({ some }: set 'a): bool =>
  some f;

let subtract (this: set 'a) (that: set 'a): (seq 'a) =>
  this.toSeq |> Seq.filter (that.contains >> not);

let toSeq ({ toSeq }: set 'a): (seq 'a) => toSeq;

let tryFind (f: 'a => bool) ({ tryFind }: set 'a): (option 'a) =>
  tryFind f;

let union (this: set 'a) (that: set 'a): (seq 'a) => Seq.concat [
  this.toSeq,
  subtract that this,
];
