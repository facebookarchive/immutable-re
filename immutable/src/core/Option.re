open Comparator;
open Equality;
open Hash;
open Ordering;

let compareWith
    (compare: comparator 'a)
    (this: option 'a)
    (that: option 'a): ordering => switch (this, that) {
  | (Some x, Some y) => compare x y
  | (Some _, _) => Ordering.greaterThan
  | (_, Some _) => Ordering.lessThan
  | (None, None) => Ordering.equal
};

let compare (this: option 'a) (that: option 'a): ordering =>
  compareWith Comparator.structural this that;

let containsWith (equals: equality 'a) (value: 'a) (opt: option 'a): bool => switch opt {
  | Some x => equals x value
  | None => false
};

let contains (value: 'a) (opt: option 'a): bool =>
  containsWith Equality.structural value opt;

let count (opt: option 'a): int => switch opt {
  | Some _ => 1
  | None => 0
};

let empty: (option 'a) = None;

let equalsWith (equals: equality 'a) (this: option 'a) (that: option 'a): bool => switch (this, that) {
  | (Some x, Some y) => equals x y
  | (None, None) => true
  | _ => false
};

let equals (this: option 'a) (that: option 'a): bool =>
  equalsWith Equality.structural this that;

let every (f: 'a => bool) (opt: option 'a): bool => switch opt {
  | None => false
  | Some x => f x
};

let filter (f: 'a => bool) (opt: option 'a): (option 'a) => switch opt {
  | Some x => f x ? opt : None
  | _ => None
};

let find (f: 'a => bool) (opt: option 'a): 'a => switch opt {
  | None => failwith "empty"
  | Some a when f a => a
  | _ => failwith "not found"
};

let first (opt: option 'a): 'a => switch opt {
  | Some x => x
  | None => failwith "option is none"
};

let flatMap (f: 'a => option 'b) (opt: option 'a): option 'b => switch opt {
  | Some a => f a
  | _ => None
};

let flatten (opt: option (option 'a)): (option 'a) => switch opt {
  | Some (Some a) => Some a
  | _ => None
};

let forEach (f: 'a => unit) (opt: option 'a): unit => switch opt {
  | Some a => f a
  | _ => ()
};

let last = first;

let hashWith (hash: hash 'a) (opt: option 'a): int => switch opt {
  | None => 0
  | Some x => hash x
};

let hash (opt: option 'a): int => hashWith Hash.structural opt;

let isEmpty (opt: option _): bool => switch opt {
  | Some _ => false
  | None => true
};

let isNotEmpty (opt: option _): bool => switch opt {
  | Some _ => true
  | None => false
};

let map (f: 'a => 'b) (opt: option 'a): option 'b => switch opt {
  | Some a => Some (f a)
  | _ => None
};

let none (f: 'a => bool) (opt: option 'a): bool => switch opt {
  | Some a => not @@ f @@ a
  | _ => true
};

let orCompute (compute: unit => 'a) (opt: option 'a): 'a => switch (opt) {
  | Some a => a
  | _ => compute ()
};

let orDefault (defaultValue: 'a) (opt: option 'a): 'a => switch (opt) {
  | Some a => a
  | _ => defaultValue
};

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) (opt: option 'a): 'acc => switch opt {
  | Some a => f acc a
  | _ => acc
};

let return (a: 'a): (option 'a) => Some a;

let some (f: 'a => bool) (opt: option 'a): bool => switch opt {
  | Some a => f a
  | _ => false
};

let tryFind (f: 'a => bool) (opt: option 'a): (option 'a) => switch opt {
  | None => None
  | Some a when f a => Some a
  | _ =>  None
};

let tryFirst = Functions.identity;
let tryLast = tryFirst;

let module Operators = {
  let (>>=) (opt: option 'a) (f: 'a => option 'b): option 'b => flatMap f opt;
  let (>>|) (opt: option 'a) (f:'a => 'b): option 'b => map f opt;
  let (|?) (opt: option 'a) (defaultValue: 'a): 'a => orDefault defaultValue opt;
};
