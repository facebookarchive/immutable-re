open Collection;
open Comparator;
open Equality;
open Functions;
open Functions.Operators;
open Hash;
open Keyed;
open Option.Operators;
open Ordering;
open RedBlackTree;
open Seq;

type sortedSet 'a = {
  comparator: comparator 'a,
  count: int,
  tree: redBlackTree 'a,
};

type alterResult 'a =
  | Added (redBlackTree 'a)
  | NoChange
  | Removed (redBlackTree 'a)
  | Replace (redBlackTree 'a);

let t
  (color: color)
  (left: redBlackTree 'a)
  (value: 'a)
  (right: redBlackTree 'a): (redBlackTree 'a) => T color left value right;

let app l r => app t l r;

let balLeft l v r => balLeft t l v r;

let balRight l v r => balRight t l v r;

let balance l v r => balance t l v r;

let add (x: 'a) ({ comparator, count, tree } as sortedSet: sortedSet 'a): (sortedSet 'a) => {
  let rec recurse (tree: redBlackTree 'a): (redBlackTree 'a) => switch tree {
    | E => T R E x E
    | T color a y b => x === y ? tree : switch (comparator x y) {
      | LessThan =>
          let insA = recurse a;
          color == B ? balance insA y b : T R insA y b
      | GreaterThan =>
          let insB = recurse b;
          color === B ? balance a y insB : T R a y insB
      | Equal => T color a x b
    };
  };

  let newTree = tree |> recurse;
  newTree === tree ? sortedSet : { comparator, count: count + 1, tree: blacken newTree }
};

let addAll (seq: seq 'a) (sortedSet: sortedSet 'a): (sortedSet 'a) => seq
  |> Seq.reduce (fun acc next => acc |> add next) sortedSet;

let alter
    (predicate: 'a => ordering)
    (f: option 'a => option 'a)
    ({ comparator, count, tree } as sortedSet: sortedSet 'a): (sortedSet 'a) => {
  let rec recurse (tree: redBlackTree 'a): (alterResult 'a) => switch tree {
    | E => switch (f None) {
        | Some x => Added (T R E x E)
        | None => NoChange
      }
    | T color a y b => switch (predicate y) {
      | LessThan => switch (a |> recurse) {
          | Added insA => Added (switch color {
              | B => balance insA y b
              | R => T R insA y b
            })
          | NoChange => NoChange
          | Removed delA => Removed (switch a {
              | T B _ _ _ => balLeft delA y b
              | _ => T R delA y b
            })
          | Replace insA => Replace (T color insA y b)
        }
      | GreaterThan => switch (b |> recurse) {
          | Added insB => Added (switch color {
              | B => (balance a y insB)
              | R => T R a y insB
            })
          | NoChange => NoChange
          | Removed delB => Removed (switch b {
              | T B _ _ _ => balRight a y delB
              | _ => T R a y delB
            })
          | Replace insB => Replace (T color a y insB)
        }
      | Equal => switch (f @@ Option.return @@ y) {
          | Some x => x === y ? NoChange : Replace (T color a x b)
          | None => Removed (app a b)
        }
    }
  };

  switch (tree |> recurse) {
    | Added tree => ({ comparator, count: count + 1, tree: blacken tree })
    | NoChange => sortedSet
    | Removed tree => ({ comparator, count: count - 1, tree: blacken tree })
    | Replace tree => ({ comparator, count, tree })
  };
};

let contains (x: 'a) ({ comparator, tree }: sortedSet 'a): bool => {
  let rec recurse (tree: redBlackTree 'a): bool => switch tree {
    | E => false
    | T _ left y right => switch (comparator x y) {
      | LessThan => recurse left
      | GreaterThan => recurse right
      | Equal => true
    }
  };

  recurse tree;
};

let count ({ count }: sortedSet 'a): int => count;

let empty: sortedSet 'a = { comparator: Comparator.structural, count: 0, tree: E };

let emptyWith (comparator: comparator 'a): (sortedSet 'a) => ({ comparator, count: 0, tree: E });

let isEmpty ({ count }: sortedSet 'a): bool => count == 0;

let isNotEmpty ({ count }: sortedSet 'a): bool => count != 0;

let search (predicate: 'a => ordering) ({ tree }: sortedSet 'a): 'a => {
  let rec recurse (tree: redBlackTree 'a): 'a => switch tree {
    | E => failwith "not found"
    | T _ left value right => switch (predicate value) {
        | LessThan => recurse left
        | GreaterThan => recurse right
        | _ => value
      }
  };

  tree |> recurse;
};

let fromSeq (seq: seq 'a): (sortedSet 'a) =>
  empty |> addAll seq;

let fromSeqWith (comparator: comparator 'a) (seq: seq 'a): (sortedSet 'a) =>
  emptyWith comparator |> addAll seq;

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) ({ tree }: sortedSet 'a): 'acc =>
  tree |> RedBlackTree.reduce f acc;

let reduceRight (f: 'acc => 'a => 'acc) (acc: 'acc) ({ tree }: sortedSet 'a): 'acc =>
  tree |> RedBlackTree.reduceRight f acc;

let remove (x: 'a) ({ comparator, count, tree } as sortedSet: sortedSet 'a): (sortedSet 'a) => {
  let rec recurse (tree: redBlackTree 'a): (redBlackTree 'a) => switch tree {
    | E => E
    | T color a y b => switch (comparator x y) {
      | LessThan =>
          let delA = a |> recurse;
          switch a {
            | T B _ _ _ => balLeft delA y b
            | _ => T R delA y b
          }
      | GreaterThan =>
          let delB = b |> recurse;
          switch b {
            | T B _ _ _ => balRight a y delB
            | _ => T R a y delB
          }
      | Equal => app a b
    }
  };

  let newTree = tree |> recurse;
  newTree === tree ? sortedSet : { comparator, count: count - 1, tree: blacken newTree }
};

let removeAll ({ comparator }: sortedSet 'a): (sortedSet 'a) =>
  emptyWith comparator;

let search (predicate: 'a => ordering) ({ tree }: sortedSet 'a): 'a => {
  let rec recurse (tree: redBlackTree 'a): 'a => switch tree {
    | E => failwith "not found"
    | T _ left value right => switch (predicate value) {
        | LessThan => recurse left
        | GreaterThan => recurse right
        | _ => value
      }
  };

  tree |> recurse;
};

let toSeq ({ tree }: sortedSet 'a): (seq 'a) => tree |> RedBlackTree.toSeq;

let compareWith (comparator: comparator 'a) (this: sortedSet 'a) (that: sortedSet 'a): ordering =>
  Seq.compareWith comparator (toSeq this) (toSeq that);

let compare (this: sortedSet 'a) (that: sortedSet 'a): ordering =>
  compareWith Comparator.structural this that;

let equalsWith (equals: equality 'a) (this: sortedSet 'a) (that: sortedSet 'a): bool =>
  Seq.equalsWith equals (toSeq this) (toSeq that);

let equals (this: sortedSet 'a) (that: sortedSet 'a): bool =>
  equalsWith Equality.structural this that;

let every (f: 'a => bool) (set: sortedSet 'a): bool =>
  set |> toSeq |> Seq.every f;

let find (f: 'a => bool) (set: sortedSet 'a): 'a =>
  set |> toSeq |> Seq.find f;

let forEach (f: 'a => unit) (set: sortedSet 'a) =>
  set |> toSeq |> Seq.forEach f;

let hash (set: sortedSet 'a): int =>
  set |> toSeq |> Seq.hash;

let hashWith (hash: hash 'a) (set: sortedSet 'a): int =>
  set |> toSeq |> Seq.hashWith hash;

let none (f: 'a => bool) (set: sortedSet 'a): bool =>
  set |> toSeq |> Seq.none f;

let some (f: 'a => bool) (set: sortedSet 'a): bool =>
  set |> toSeq |> Seq.some f;

let tryFind (f: 'a => bool) (set: sortedSet 'a): (option 'a) =>
  set |> toSeq |> Seq.tryFind f;

let trySearch (predicate: 'a => ordering) ({ tree }: sortedSet 'a): (option 'a) => {
  let rec recurse (tree: redBlackTree 'a): (option 'a) => switch tree {
    | E => None
    | T _ left value right => switch (predicate value) {
        | LessThan => recurse left
        | GreaterThan => recurse right
        | _ => Some value
      }
  };

  tree |> recurse;
};

let toCollection (set: sortedSet 'a): (collection 'a) => {
  contains: fun a => contains a set,
  count: count set,
  every: fun f => every f set,
  find: fun f => find f set,
  forEach: fun f => forEach f set,
  none: fun f => none f set,
  reduce: fun f acc => reduce f acc set,
  some: fun f => some f set,
  toSeq: toSeq set,
  tryFind: fun f => tryFind f set,
};

let toKeyed (set: sortedSet 'a): (keyed 'a 'a) => {
  containsWith: fun equals k v => set |> contains k ? equals k v : false,
  containsKey: fun k => set |> contains k,
  count: count set,
  every: fun f => set |> every (fun k => f k k),
  find: fun f => {
    let k = set |> find (fun k => f k k);
    (k, k)
  },
  forEach: fun f => set |> forEach (fun k => f k k),
  get: fun k => set |> contains k ? k : failwith "not found",
  none: fun f => set |> none (fun k => f k k),
  reduce: fun f acc => set |> reduce (fun acc k => f acc k k) acc,
  some: fun f => set |> some (fun k => f k k),
  toSeq: toSeq set |> Seq.map (fun k => (k, k)),
  tryFind: fun f => set |> tryFind (fun k => f k k) >>| (fun k => (k, k)),
  tryGet: fun k => set |> contains k ? Some k : None,
  values: toSeq set,
};

/* FIXME: Unimplemented functions */
let intersect (this: sortedSet 'a) (that: sortedSet 'a): (sortedSet 'a) =>
  failwith "Not Implemented";

let subtract (this: sortedSet 'a) (that: sortedSet 'a): (sortedSet 'a) =>
  failwith "Not Implemented";

let union (this: sortedSet 'a) (that: sortedSet 'a): (sortedSet 'a) =>
  failwith "Not Implemented";
