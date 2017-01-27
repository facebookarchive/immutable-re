/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Equality;
open Functions;
open Keyed;
open Option.Operators;
open Preconditions;
open RedBlackTree;
open Seq;

type treeList 'a = redBlackTree ('a, int);

let count (treeList: treeList 'a): int => switch treeList {
  | E => 0
  | T _ _ (_, count) _ => count
};

let t
    (color: color)
    (left: treeList 'a)
    ((value, _): ('a, int))
    (right: treeList 'a): (treeList 'a) => {
  let value = (value, 1 + (count left) + (count right));
  T color left value right
};

let balLeft l v r => balLeft t l v r;

let balRight l v r => balRight t l v r;

let balance l v r => balance t l v r;

let rec insertAtUnsafe
    (index: int)
    (key: 'a)
    (treeList: treeList 'a): (treeList 'a) => switch treeList {
  | E => T R E (key, 1) E
  | T color left nodeValue right =>
    if (index < (count left)) {
      let insLeft = left |> insertAtUnsafe index key;

      switch color {
        | B => (balance insLeft nodeValue right)
        | R => t R insLeft nodeValue right
      }
    } else {
      let insRight = right |> insertAtUnsafe (index - (count left) - 1) key;

      switch color {
          | B => balance left nodeValue insRight
          | R => t R left nodeValue insRight
        }
    };
};

let insertAt
    (index: int)
    (x: 'a)
    (treeList: treeList 'a): (treeList 'a) => {
  Preconditions.failIfOutOfRange (count treeList + 1) index;
  treeList |> insertAtUnsafe index x |> blacken
};

let add (value: 'a) (treeList: treeList 'a): (treeList 'a) =>
  treeList |> insertAt (treeList |> count) value;

let empty: (treeList 'a) = E;

let rec getUnsafe
  (index: int)
  (treeList: treeList 'a): 'a => switch treeList {
  | E => failwith "Invalid state"
  | T color left (key, nodeCount) right =>
      index < (count left) ? left |> getUnsafe index :
      index > (count left) ? right |> getUnsafe (index - (count left) - 1) :
      key;
};

let tryGet
    (index: int)
    (treeList: treeList 'a): (option 'a) =>
  Preconditions.noneIfIndexOutOfRange (count treeList) index (flip getUnsafe treeList);

let first (treeList: treeList 'a): option 'a =>
  treeList |> tryGet 0;

let last (treeList: treeList 'a): option 'a =>
  treeList |> tryGet ((count treeList) - 1);

let rec removeAtUnsafe
    (index: int)
    (treeList: treeList 'a): (option (treeList 'a)) => switch treeList {
  | E => failwith "Invalid state"
  | T _ E (nodeKey, _) E when index == 0 => Some E
  | T _ left (nodeKey, _) E when index == (count left) => Some left
  | T _ E (nodeKey, _) right when index == 0 => Some right
  | T _ left (nodeKey, _) right when index == (count left) =>
      app t left right |> Option.return

  | T _ ((T B _ _ _) as left) value right when index < (count left) =>
      left |> removeAtUnsafe index >>| (fun delLeft =>
        balLeft delLeft value right
      )
  | T _ left value right when index < (count left) =>
      left |> removeAtUnsafe index >>| (fun delLeft =>
        t R delLeft value right
      )

  | T _ left value ((T B _ _ _) as right) =>
      right |> removeAtUnsafe (index - (count left) - 1) >>| (fun delRight =>
        balRight left value delRight
      )
  | T _ left value right =>
      right |> removeAtUnsafe (index - (count left) - 1) >>| (fun delRight =>
        t R left value delRight
      )
};

let removeAt (index: int) (treeList: treeList 'a): (treeList 'a) => {
  Preconditions.failIfOutOfRange (count treeList) index;
  removeAtUnsafe index treeList >>| blacken |? E
};

let poll (treeList: treeList 'a): (treeList 'a) =>
  treeList |> removeAt 0;

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) (treeList: treeList 'a): 'acc => {
  let reducer acc (a, _) => f acc a;
  treeList |> RedBlackTree.reduce reducer acc;
};

let reduceRight (f: 'acc => 'a => 'acc) (acc: 'acc) (treeList: treeList 'a): 'acc => {
  let reducer acc (a, _) => f acc a;
  treeList |> RedBlackTree.reduce reducer acc;
};

let removeLast (treeList: treeList 'a): (treeList 'a) =>
  treeList |> removeAt ((count treeList) - 1);

let toSeq (tree: treeList 'a): (seq 'a) =>
  tree |> RedBlackTree.toSeq |> Seq.map (fun (a, _) => a);

let toSeqReversed (tree: treeList 'a): (seq 'a) =>
  tree |> RedBlackTree.toSeqReversed |> Seq.map (fun (a, _) => a);

let rec updateUnsafe
    (index: int)
    (key: 'a)
    (treeList: treeList 'a): (treeList 'a) => switch treeList {
  | E => failwith "invalid state"
  | T color left (nodeKey, nodeCount) right when index == (count left) =>
      T color left (key, nodeCount) right
  | T color left nodeValue right when index < (count left) =>
      let newLeft = left |> updateUnsafe index key;
      T color newLeft nodeValue right
  | T color left nodeValue right =>
      let newRight = right |> updateUnsafe (index - (count left) - 1) key;
      T color left nodeValue newRight
};

let update (index: int) (value: 'a) (treeList: treeList 'a): (treeList 'a) => {
  Preconditions.failIfOutOfRange (count treeList) index;
  treeList |> updateUnsafe index value;
};
