/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Collection;
open Comparator;
open Equality;
open Functions;
open Functions.Operators;
open Keyed;
open Option.Operators;
open Ordering;
open RedBlackTree;
open Seq;
open SetImpl;

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

let count ({ count }: sortedSet 'a) => count;

let empty: sortedSet 'a = { comparator: Comparator.structural, count: 0, tree: E };

let emptyWith (comparator: comparator 'a): (sortedSet 'a) => ({ comparator, count: 0, tree: E });

let find (predicate: 'a => ordering) ({ tree }: sortedSet 'a): (option 'a) => {
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

let put (x: 'a) ({ comparator, count, tree } as sortedSet: sortedSet 'a): (sortedSet 'a) => {
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

let putAll (seq: seq 'a) (sortedSet: sortedSet 'a): (sortedSet 'a) => seq
  |> Seq.reduce (fun acc next => acc |> put next) sortedSet;

let fromSeq (seq: seq 'a): (sortedSet 'a) =>
  empty |> putAll seq;

let fromSeqWith (comparator: comparator 'a) (seq: seq 'a): (sortedSet 'a) =>
  emptyWith comparator |> putAll seq;

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

let toSeq ({ tree }: sortedSet 'a): (seq 'a) => tree |> RedBlackTree.toSeq;

let module SortedSetSetImpl = SetImpl.Make {
  type t 'a = sortedSet 'a;
  let contains = contains;
  let count = count;
  let toSeq = toSeq;
};

let toCollection = SortedSetSetImpl.toCollection;
let toKeyed = SortedSetSetImpl.toKeyed;
