
/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open CopyOnWriteArray;
open Functions;
open Indexed;
open Vector;
open Option.Operators;
open Preconditions;
open Seq;
open Transient;

type tailNode 'a = {
  owner: option owner,
  values: array 'a,
};

let module TailNode = {
  let bits = 5;
  let width = 1 lsl 5;

  let count ({ values }: tailNode 'a): int =>
    CopyOnWriteArray.count values;

  let empty: tailNode 'a = {
    owner: None,
    values: [||],
  };

  let get (index: int) ({ values }: tailNode 'a) =>
    values |> CopyOnWriteArray.get index;

  let isEmpty ({ values }: tailNode 'a): bool =>
    (CopyOnWriteArray.count values) == 0;

  let isFull ({ values }: tailNode 'a): bool =>
    (CopyOnWriteArray.count values) == width;

  let isNotEmpty ({ values }: tailNode 'a): bool =>
    (CopyOnWriteArray.count values) != 0;

  let isNotFull ({ values }: tailNode 'a): bool =>
    (CopyOnWriteArray.count values) != width;

  let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) ({ values }: tailNode 'a): 'acc =>
    values |> CopyOnWriteArray.reduce f acc;

  let reduceRight (f: 'acc => 'a => 'acc) (acc: 'acc) ({ values }: tailNode 'a): 'acc =>
    values |> CopyOnWriteArray.reduceRight f acc;

  let toSeq ({ values }: tailNode 'a): (seq 'a) =>
    CopyOnWriteArray.toSeq values;

  let toSeqReversed ({ values }: tailNode 'a): (seq 'a) =>
    CopyOnWriteArray.toSeqReversed values;

  let tryGet (index: int) ({ values }: tailNode 'a) =>
    values |> CopyOnWriteArray.tryGet index;
};

let module DequeImpl = {
  type direction = Ascending | Descending;

  module type DequeBase = {
    type t 'a;
    type vector 'a;
    type node 'a;
    type tailNode 'a;

    let create:
      direction::(direction) =>
      right::(tailNode 'a) =>
      middle::(vector 'a) =>
      left::(tailNode 'a) =>
      (t 'a);

    let trieAddFirst: (node 'a) => (vector 'a) => (vector 'a);
    let trieAddLast: (node 'a) => (vector 'a) => (vector 'a);
    let trieAlter: int => (node 'a => node 'a) => (vector 'a) => (vector 'a);
    let trieCount: (vector 'a) => int;
    let trieFirst: (vector 'a) => (node 'a);
    let trieGet: int => (vector 'a) => (node 'a);
    let trieIsEmpty: (vector 'a) => bool;
    let trieLast: (vector 'a) => (node 'a);
    let trieRemoveFirst: (vector 'a) => (vector 'a);
    let trieRemoveLast: (vector 'a) => (vector 'a);
    let trieTryGet: int => (vector 'a) => (option (node 'a));

    let tailNodeAddFirst: (option owner) => 'a => (tailNode 'a) => (tailNode 'a);
    let tailNodeAddLast: (option owner) => 'a => (tailNode 'a) => (tailNode 'a);
    let tailNodeCount: (tailNode 'a) => int;
    let tailNodeEmpty: unit => (tailNode 'a);
    let tailNodeGet: int => (tailNode 'a) => 'a;
    let tailNodeIsEmpty: (tailNode 'a) => bool;
    let tailNodeIsFull: (tailNode 'a) => bool;
    let tailNodeIsNotFull: (tailNode 'a) => bool;
    let tailNodeRemoveFirst: (option owner) => (tailNode 'a) => (tailNode 'a);
    let tailNodeRemoveLast: (option owner) => (tailNode 'a) => (tailNode 'a);
    let tailNodeTryGet: int => (tailNode 'a) => (option 'a);
    let tailNodeUpdate: (option owner) => int => 'a => (tailNode 'a) => (tailNode 'a);

    let nodeGet: int => (node 'a) => 'a;
    let nodeTryGet: int => (node 'a) => (option 'a);
    let nodeUpdate: (option owner) => int => 'a => (node 'a) => (node 'a);

    let nodeToTailNode: (node 'a) => (tailNode 'a);
    let tailNodeToNode: (tailNode 'a) => (node 'a);

    let direction: (t 'a) => direction;
    let left: (t 'a) => (tailNode 'a);
    let middle: (t 'a) => (vector 'a);
    let right: (t 'a) => (tailNode 'a);
  };

  module type S = {
    type t 'a;

    let addFirst: (option owner) => 'a => (t 'a) => (t 'a);
    let addLast: (option owner) => 'a => (t 'a) => (t 'a);
    let count: (t 'a) => int;
    let first: (t 'a) => 'a;
    let get: int => (t 'a) => 'a;
    let last: (t 'a) => 'a;
    let removeFirst: (option owner) => (t 'a) => (t 'a);
    let removeLast: (option owner) => (t 'a) => (t 'a);
    let reverse: (option owner) => (t 'a) => (t 'a);
    let tryFirst: (t 'a) => (option 'a);
    let tryGet: int => (t 'a) => (option 'a);
    let tryLast: (t 'a) => (option 'a);
    let update: (option owner) => int => 'a => (t 'a) => (t 'a);
  };

  let module Make = fun (X: DequeBase) => {
    type t 'a = X.t 'a;
    type creator 'a = right::(X.node 'a) => middle::(X.vector 'a) => left::(X.node 'a) => (t 'a);

    let computeMiddleCount (trieCount: int): int => trieCount lsl TailNode.bits;
    let computeMiddleIndex (absIndex: int): int => absIndex asr TailNode.bits;
    let computeNodeIndex (index: int): int => index land (TailNode.width - 1);

    let addFirstImpl
        (owner: option owner)
        (value: 'a)
        (deque: t 'a): (t 'a) => {
      let right = X.right deque;
      let middle = X.middle deque;
      let left = X.left deque;

      (X.tailNodeIsNotFull right) ? X.create
        direction::Ascending
        right::(right |> X.tailNodeAddFirst owner value)
        middle::middle
        left::right :

      (X.tailNodeIsFull right) && (X.tailNodeIsEmpty left) ? X.create
        direction::Ascending
        right::(X.tailNodeEmpty () |> X.tailNodeAddFirst owner value)
        middle::middle
        left::right :

      (X.tailNodeIsFull right) ? X.create
        direction::Ascending
        right::(X.tailNodeEmpty () |> X.tailNodeAddFirst owner value)
        middle::(middle |> X.trieAddFirst (X.tailNodeToNode right))
        left::left :

      X.create
        direction::Ascending
        right::(right |> X.tailNodeAddFirst owner value)
        middle::middle
        left::left
    };

    let addLastImpl
        (owner: option owner)
        (value: 'a)
        (deque: t 'a): (t 'a) => {
      let left = X.left deque;
      let middle = X.middle deque;
      let right = X.right deque;

      /* If the right node is empty, the whole queue is empty */
      (X.tailNodeIsNotFull right) && (X.tailNodeIsEmpty left) ? X.create
        direction::Ascending
        right::(right |> X.tailNodeAddLast owner value)
        middle::middle
        left::left :

      (X.tailNodeIsFull left) ? X.create
        direction::Ascending
        right::right
        middle::(middle |> X.trieAddLast (X.tailNodeToNode left))
        left:: (X.tailNodeEmpty () |> X.tailNodeAddLast owner value) :

      X.create
        direction::Ascending
        right::right
        middle::middle
        left::(left |> X.tailNodeAddLast owner value)
    };

    let addFirst
        (owner: option owner)
        (value: 'a)
        (deque: t 'a): (t 'a) => switch (X.direction deque) {
      | Ascending => addFirstImpl owner value deque
      | Descending => addLastImpl owner value deque
    };

    let addLast
        (owner: option owner)
        (value: 'a)
        (deque: t 'a): (t 'a) => switch (X.direction deque) {
      | Ascending => addLastImpl owner value deque
      | Descending => addFirstImpl owner value deque
    };

    let count (deque: t 'a): int => {
      let right = X.right deque;
      let middle = X.middle deque;
      let left = X.left deque;

      (X.tailNodeCount right) + (computeMiddleCount (X.trieCount middle)) + (X.tailNodeCount left);
    };

    let getUnsafe (index: int) (deque: t 'a): 'a => {
      let index = switch (X.direction deque) {
        | Ascending => index
        | Descending => (count deque) - index - 1;
      };

      let right = X.right deque;
      let middle = X.middle deque;
      let left = X.left deque;

      let rightCount = X.tailNodeCount right;
      let trieCount = X.trieCount middle;

      let leftIndex = index - (computeMiddleCount trieCount) - rightCount;

      (index < rightCount) ? right |> X.tailNodeGet index :

      (leftIndex >= 0) ? left |> X.tailNodeGet leftIndex :

      {
        let index = index - rightCount;
        let middleIndex = computeMiddleIndex index;
        let nodeIndex = computeNodeIndex index;
        middle |> X.trieGet middleIndex |> X.nodeGet nodeIndex;
      }
    };

    let get (index: int) (deque: t 'a): 'a => {
      Preconditions.failIfOutOfRange (count deque) index;
      getUnsafe index deque;
    };

    let first (deque: t 'a): 'a =>
      deque |> get 0;

    let last (deque: t 'a): 'a =>
      deque |> get ((count deque) - 1);

    let removeFirstImpl
        (owner: option owner)
        (deque: t 'a): (t 'a) =>  {
      let right = X.right deque;
      let middle = X.middle deque;
      let left = X.left deque;

      let rightCount = X.tailNodeCount right;
      let trieCount = X.trieCount middle;
      let leftCount = X.tailNodeCount left;

      rightCount > 1 ? X.create
        direction::Ascending
        right::(X.tailNodeRemoveFirst owner right)
        middle::middle
        left::left :

      trieCount > 1 ? X.create
        direction::Ascending
        right::(X.trieFirst middle |> X.nodeToTailNode)
        middle::(X.trieRemoveFirst middle)
        left::left :

      leftCount > 1 ? X.create
        direction::Ascending
        right::left
        middle::middle
        left::(X.tailNodeEmpty ()) :

      failwith "deque is empty";
    };

    let removeLastImpl (owner: option owner) (deque: t 'a): (t 'a) => {
      let right = X.right deque;
      let middle = X.middle deque;
      let left = X.left deque;

      let rightCount = X.tailNodeCount right;
      let trieCount = X.trieCount middle;
      let leftCount = X.tailNodeCount left;

      leftCount > 1 ? X.create
        direction::Ascending
        right::right
        middle::middle
        left::(X.tailNodeRemoveLast owner left) :

      trieCount > 1 ? X.create
        direction::Ascending
        right::right
        middle::(X.trieRemoveLast middle)
        left::(X.trieLast middle |> X.nodeToTailNode) :

      rightCount > 1 ? X.create
        direction::Ascending
        right::(X.tailNodeRemoveLast owner right)
        middle::middle
        left::left :

      failwith "deque is empty";
    };

    let removeFirst (owner: option owner) (deque: t 'a): (t 'a) => switch (X.direction deque) {
      | Ascending => removeFirstImpl owner deque
      | Descending => removeLastImpl owner deque
    };

    let removeLast (owner: option owner) (deque: t 'a): (t 'a) => switch (X.direction deque) {
      | Ascending => removeLastImpl owner deque
      | Descending => removeFirstImpl owner deque
    };

    let reverse (owner: option owner) (deque: t 'a): (t 'a) => {
      let right = X.right deque;
      let middle = X.middle deque;
      let left = X.left deque;

      let direction = switch (X.direction deque) {
        | Ascending => Descending
        | Descending => Ascending
      };

      X.create
        direction::direction
        right::right
        middle::middle
        left::left;
    };

    let tryGet (index: int) (deque: t 'a): (option 'a) =>
      Preconditions.noneIfIndexOutOfRange (count deque) index (flip getUnsafe deque);

    let tryFirst (deque: t 'a): option 'a =>
      deque |> tryGet 0;

    let tryLast (deque: t 'a): option 'a =>
      deque |> tryGet ((count deque) - 1);

    let update (owner: option owner) (index: int) (value: 'a) (deque: t 'a): (t 'a) => {
      Preconditions.failIfOutOfRange (count deque) index;

      let direction = X.direction deque;
      let right = X.right deque;
      let middle = X.middle deque;
      let left = X.left deque;

      let index = switch (direction) {
        | Ascending => index
        | Descending => (count deque) - index - 1;
      };

      let rightCount = X.tailNodeCount right;
      let trieCount = X.trieCount middle;

      let leftIndex = index - (computeMiddleCount trieCount) - rightCount;

      /* FIXME: Add checks for reference equality between the old and new value to avoid
       * unnecessary allocations.
       */
      (index < rightCount) ? X.create
        direction::direction
        right::(right |> X.tailNodeUpdate owner index value)
        middle::middle
        left::left :

      (leftIndex >= 0) ? X.create
        direction::direction
        right::right
        middle::middle
        left::(left |> X.tailNodeUpdate owner leftIndex value) :

      {
        let index = (index - rightCount);
        let middleIndex = computeMiddleIndex index;
        let nodeIndex = computeNodeIndex index;
        let newMiddle = middle |> X.trieAlter middleIndex (X.nodeUpdate owner nodeIndex value);

        X.create
          direction::direction
          right::right
          middle::newMiddle
          left::left;
      };
    };
  };
};

type deque 'a =
  | Ascending (tailNode 'a) (vector (tailNode 'a)) (tailNode 'a)
  | Descending (tailNode 'a) (vector (tailNode 'a)) (tailNode 'a);

let empty: (deque 'a) = Ascending TailNode.empty Vector.empty TailNode.empty;

type persistentDequeNode 'a = tailNode 'a;
let module PersistentDequeImpl = DequeImpl.Make {
  type t 'a = deque 'a;
  type vector 'a = Vector.vector (tailNode 'a);
  type node 'a = persistentDequeNode 'a;
  type tailNode 'a = persistentDequeNode 'a;

  let create
      direction::(direction: DequeImpl.direction)
      right::(right: tailNode 'a)
      middle::(middle: vector 'a)
      left::(left: tailNode 'a): (t 'a) => switch direction {
    | DequeImpl.Ascending => Ascending left middle right
    | DequeImpl.Descending => Descending left middle right
  };

  let trieAddFirst = Vector.addFirst;
  let trieAddLast = Vector.addLast;
  let trieAlter = Vector.alter;
  let trieCount = Vector.count;
  let trieFirst = Vector.first;
  let trieGet = Vector.get;
  let trieIsEmpty = Vector.isEmpty;
  let trieLast = Vector.last;
  let trieRemoveFirst = Vector.removeFirst;
  let trieRemoveLast = Vector.removeLast;
  let trieTryGet = Vector.tryGet;

  let tailNodeAddFirst (_: option owner) (value: 'a) ({ values }: tailNode 'a) =>
    ({ owner: None, values: values |> CopyOnWriteArray.addFirst value });

  let tailNodeAddLast (_: option owner) (value: 'a) ({ values }: tailNode 'a) =>
    ({ owner: None, values: values |> CopyOnWriteArray.addLast value });

  let tailNodeCount = TailNode.count;
  let tailNodeEmpty (): tailNode 'a => TailNode.empty;

  let tailNodeGet = TailNode.get;
  let tailNodeIsEmpty = TailNode.isEmpty;
  let tailNodeIsFull = TailNode.isFull;
  let tailNodeIsNotFull = TailNode.isNotFull;

  let tailNodeRemoveFirst (_: option owner) ({ values }: tailNode 'a) =>
    ({ owner: None, values: CopyOnWriteArray.removeFirst values });

  let tailNodeRemoveLast (_: option owner) ({ values }: tailNode 'a) =>
    ({ owner: None, values: CopyOnWriteArray.removeLast values });

  let tailNodeTryGet = TailNode.tryGet;

  let tailNodeUpdate (_: option owner) (index: int) (value: 'a) ({ values }: tailNode 'a) =>
    ({ owner: None, values: values |> CopyOnWriteArray.update index value });

  let nodeGet = TailNode.get;
  let nodeTryGet = TailNode.tryGet;
  let nodeUpdate owner value => tailNodeUpdate None value;

  let nodeToTailNode node => node;
  let tailNodeToNode node => node;

  let direction (deque: t 'a): DequeImpl.direction => switch deque {
    | Ascending _ _ _ => DequeImpl.Ascending
    | Descending _ _ _ => DequeImpl.Descending
  };

  let right (deque: t 'a): (tailNode 'a) => switch deque {
    | Ascending _ _ right
    | Descending _ _ right => right
  };

  let middle (deque: t 'a): (vector 'a) => switch deque {
    | Ascending _ middle _
    | Descending _ middle _ => middle
  };

  let left (deque: t 'a): (tailNode 'a) => switch deque {
    | Ascending left _ _
    | Descending left _ _ => left
  };
};

type tailNodeWithCount 'a = {
  mutable count: int,
  owner: option owner,
  values: array 'a,
};

type transientDequeImpl 'a =
  | Ascending (tailNodeWithCount 'a) (transientVector (tailNode 'a)) (tailNodeWithCount 'a)
  | Descending (tailNodeWithCount 'a) (transientVector (tailNode 'a)) (tailNodeWithCount 'a);

let module TransientDequeImpl = DequeImpl.Make {
  type t 'a = transientDequeImpl 'a;
  type vector 'a = transientVector (tailNode 'a);
  type node 'a = tailNode 'a;
  type tailNode 'a = tailNodeWithCount 'a;

  let create
      direction::(direction: DequeImpl.direction)
      right::(right: tailNodeWithCount 'a)
      middle::(middle: vector 'a)
      left::(left: tailNodeWithCount 'a): (transientDequeImpl 'a) => switch direction {
    | DequeImpl.Ascending => Ascending left middle right
    | DequeImpl.Descending => Descending left middle right
  };

  let trieAddFirst = TransientVector.addFirst;
  let trieAddLast = TransientVector.addLast;
  let trieAlter = TransientVector.alter;
  let trieCount = TransientVector.count;
  let trieFirst = TransientVector.first;
  let trieGet = TransientVector.get;
  let trieIsEmpty = TransientVector.isEmpty;
  let trieLast = TransientVector.last;
  let trieRemoveFirst = TransientVector.removeFirst;
  let trieRemoveLast = TransientVector.removeLast;
  let trieTryGet = TransientVector.tryGet;

  let tailNodeAddFirst
      (owner: option owner)
      (value: 'a)
      ({ count, owner: nodeOwner, values: nodeValues } as tailNode: tailNode 'a): (tailNode 'a) => switch (owner, nodeOwner) {
    | (Some owner, Some nodeOwner) when owner === nodeOwner =>
        nodeValues.(count) = value;
        tailNode.count = count + 1;
        tailNode
    | _ =>
        let values = Array.make (TailNode.width) value;
        Array.blit nodeValues 0 values 1 count;
        let count = count + 1;

        { count, owner, values };
  };

  let tailNodeAddLast
      (owner: option owner)
      (value: 'a)
      ({ count, owner: nodeOwner, values: nodeValues } as tailNode: tailNode 'a): (tailNode 'a) => switch (owner, nodeOwner) {
    | (Some owner, Some nodeOwner) when owner === nodeOwner =>
        nodeValues.(count) = value;
        tailNode.count = count + 1;
        tailNode
    | _ =>
        let values = Array.make (TailNode.width) value;
        Array.blit nodeValues 0 values 0 count;
        let count = count + 1;

        { count, owner, values };
  };

  let tailNodeCount ({ count }: tailNode 'a): int => count;

  let tailNodeEmpty (): tailNode 'a => { count: 0, owner: None, values: [||] };

  let tailNodeGet (index: int) ({ values }: tailNode 'a) =>
    values |> CopyOnWriteArray.get index;

  let tailNodeIsEmpty (tailNode: tailNode 'a): bool =>
    (tailNodeCount tailNode) == 0;

  let tailNodeIsFull (tailNode: tailNode 'a): bool =>
    (tailNodeCount tailNode) == TailNode.width;

  let tailNodeIsNotFull (tailNode: tailNode 'a): bool =>
    (tailNodeCount tailNode) != TailNode.width;

  let tailNodeRemoveFirst
      (owner: option owner)
      ({ count, owner: nodeOwner, values: nodeValues } as tailNode: tailNode 'a): (tailNode 'a) => switch (owner, nodeOwner) {
    | (Some owner, Some nodeOwner) when owner === nodeOwner =>
        for i in 0 to (count - 1) {
          nodeValues.(i) = nodeValues.(i + 1);
        };
        tailNode.count = count - 1;
        tailNode
    | _ when count == 1 =>
        tailNodeEmpty ()
    | _ =>
        let initialValue = nodeValues.(1);
        let count = count - 1;
        let values = Array.make (TailNode.width) initialValue;
        Array.blit nodeValues 0 nodeValues 1 count;

        { count, owner, values };
  };

  let tailNodeRemoveLast
      (owner: option owner)
      ({ count, owner: nodeOwner, values: nodeValues } as tailNode: tailNode 'a): (tailNode 'a) => switch (owner, nodeOwner) {
    | (Some owner, Some nodeOwner) when owner === nodeOwner =>
        tailNode.count = count - 1;
        tailNode
    | _ when count == 1 =>
        tailNodeEmpty ()
    | _ =>
        let initialValue = nodeValues.(0);
        let count = count - 1;
        let values = Array.make (TailNode.width) initialValue;
        Array.blit nodeValues 0 nodeValues 0 count;

        { count, owner, values };
  };

  let tailNodeTryGet (index: int) ({ values }: tailNode 'a) => {
    values |> CopyOnWriteArray.tryGet index;
  };

  let tailNodeUpdate
      (owner: option owner)
      (index: int)
      (value: 'a)
      ({ count, owner: nodeOwner, values } as tailNode: tailNode 'a): (tailNode 'a) => switch (owner, nodeOwner) {
    | (Some owner, Some nodeOwner) when owner === nodeOwner =>
        tailNode.values.(index) = value;
        tailNode
    | _ =>  { count, owner, values: values |> CopyOnWriteArray.update index value };
  };

  let nodeGet = TailNode.get;
  let nodeTryGet = TailNode.tryGet;
  let nodeUpdate
      (owner: option owner)
      (index: int)
      (value: 'a)
      (node: node 'a): (node 'a) => switch (owner, node.owner) {
    | (Some owner, Some nodeOwner) when owner === nodeOwner =>
        node.values.(index) = value;
        node
    | _ => ({ owner, values: node.values |> CopyOnWriteArray.update index value })
  };

  let nodeToTailNode ({ owner, values }: node 'a): (tailNode 'a) => { count: TailNode.width, owner, values };
  let tailNodeToNode ({ count, owner, values }: tailNode 'a): (node 'a) => { owner, values };

  let direction (deque: t 'a): DequeImpl.direction => switch deque {
    | Ascending _ _ _ => DequeImpl.Ascending
    | Descending _ _ _ => DequeImpl.Descending
  };

  let right (deque: t 'a): (tailNode 'a) => switch deque {
    | Ascending _ _ right
    | Descending _ _ right => right
  };

  let middle (deque: t 'a): (vector 'a) => switch deque {
    | Ascending _ middle _
    | Descending _ middle _ => middle
  };

  let left (deque: t 'a): (tailNode 'a) => switch deque {
    | Ascending left _ _
    | Descending left _ _ => left
  };
};

type transientDeque 'a = transient (transientDequeImpl 'a);

let mutate (deque: deque 'a): (transientDeque 'a) => switch deque {
  | Ascending left middle right =>
      let left = { count: TailNode.count left, owner: None, values: left.values };
      let right = { count: TailNode.count right, owner: None, values: right.values };
      Transient.create (Ascending left (Vector.mutate middle) right)
  | Descending left middle right  =>
      let left = { count: TailNode.count left, owner: None, values: left.values };
      let right = { count: TailNode.count right, owner: None, values: right.values };
      Transient.create (Descending left (Vector.mutate middle) right)
};

let addFirst value trie => PersistentDequeImpl.addFirst None value trie;
let addLast value trie => PersistentDequeImpl.addLast None value trie;
let add = addLast;
let count = PersistentDequeImpl.count;
let first = PersistentDequeImpl.first;
let get = PersistentDequeImpl.get;
let last = PersistentDequeImpl.last;
let removeFirst trie => PersistentDequeImpl.removeFirst None trie;
let removeLast trie => PersistentDequeImpl.removeLast None trie;
let reverse trie => PersistentDequeImpl.reverse None trie;
let tryFirst = PersistentDequeImpl.tryFirst;
let tryGet = PersistentDequeImpl.tryGet;
let tryLast = PersistentDequeImpl.tryLast;
let update index value trie => PersistentDequeImpl.update None index value trie;

let rec reduce (f: 'acc => 'a => 'acc) (acc: 'acc) (deque: deque 'a): 'acc => switch deque {
  | Ascending left middle right =>
      let acc = right |> TailNode.reduce f acc;
      let acc = middle |> Vector.reduce (TailNode.reduce f) acc;
      let acc = left |> TailNode.reduce f acc;
      acc
  | Descending left middle right =>
    reduceRight f acc ((Ascending left middle right): deque 'a)
}

and reduceRight (f: 'acc => 'a => 'acc) (acc: 'acc) (deque: deque 'a): 'acc => switch deque {
  | Ascending left middle right =>
      let acc = left |> TailNode.reduceRight f acc;
      let acc = middle |> Vector.reduceRight (TailNode.reduceRight f) acc;
      let acc = right |> TailNode.reduceRight f acc;
      acc
  | Descending left middle right =>
      reduce f acc (Ascending left middle right)
};

let removeAll (_: deque 'a): (deque 'a) => empty;

let rec toSeq (deque: deque 'a): (seq 'a) => switch deque {
  | Ascending left middle right => Seq.concat [
      TailNode.toSeq right,
      middle |> Vector.toSeq |> Seq.flatMap TailNode.toSeq,
      TailNode.toSeq left,
    ]
  | Descending left middle right =>
      toSeqReversed ((Ascending left middle right): deque 'a)
}

and toSeqReversed (deque: deque 'a): (seq 'a) => switch deque {
  | Ascending left middle right => Seq.concat [
      TailNode.toSeqReversed left,
      middle |> Vector.toSeqReversed |> Seq.flatMap TailNode.toSeqReversed,
      TailNode.toSeqReversed right,
    ]
  | Descending left middle right =>
      toSeq ((Ascending left middle right): deque 'a)
};

let toIndexed (deque: deque 'a): (indexed 'a) => Indexed.create
  count::(count deque)
  rseq::(toSeqReversed deque)
  seq::(toSeq deque)
  tryGet::(fun i => deque |> tryGet i);

let module TransientDeque = {
  let addFirst value (transient: transientDeque 'a) => transient |> Transient.update (
    fun owner => TransientDequeImpl.addFirst (Some owner) value
  );

  let addLast value (transient: transientDeque 'a) => transient |> Transient.update (
    fun owner => TransientDequeImpl.addLast (Some owner) value
  );

  let add = addLast;
  let count (transient: transientDeque 'a) => transient |> Transient.get |> TransientDequeImpl.count;
  let first (transient: transientDeque 'a) => transient |> Transient.get |> TransientDequeImpl.first;
  let get (index: int) (transient: transientDeque 'a) => transient |> Transient.get |> TransientDequeImpl.get index;
  let last (transient: transientDeque 'a) => transient |> Transient.get |> TransientDequeImpl.last;
  let removeFirst (transient: transientDeque 'a) => transient |> Transient.update (
    fun owner => TransientDequeImpl.removeFirst (Some owner)
  );
  let removeLast (transient: transientDeque 'a) => transient |> Transient.update (
    fun owner => TransientDequeImpl.removeLast (Some owner)
  );
  let reverse(transient: transientDeque 'a) => transient |> Transient.update (
    fun owner => TransientDequeImpl.reverse (Some owner)
  );
  let tryFirst (transient: transientDeque 'a) => transient |> Transient.get |> TransientDequeImpl.tryFirst;
  let tryGet (index: int) (transient: transientDeque 'a) => transient |> Transient.get |> TransientDequeImpl.tryGet index;
  let tryLast (transient: transientDeque 'a) => transient |> Transient.get |> TransientDequeImpl.tryLast;
  let update index value (transient: transientDeque 'a) => transient |> Transient.update (
    fun owner => TransientDequeImpl.update (Some owner) index value
  );

  let addAll (values: seq 'a) (transient: transientDeque 'a): (transientDeque 'a) => values
    |> Seq.reduce (fun acc next => acc |> add next) transient;

  let persist (transient: transientDeque 'a): (deque 'a) => {
    let deque = transient |> Transient.persist;

    switch deque {
      | Ascending left middle right =>
          let rightValues = CopyOnWriteArray.range 0 (Some right.count) right.values;
          let leftValues = CopyOnWriteArray.range 0 (Some left.count) left.values;
          Ascending
            { owner: None, values: leftValues }
            (TransientVector.persist middle)
            { owner: None, values: rightValues }
      | Descending left middle right =>
          let rightValues = CopyOnWriteArray.range 0 (Some right.count) right.values;
          let leftValues = CopyOnWriteArray.range 0 (Some left.count) left.values;
          Descending
            { owner: None, values: leftValues }
            (TransientVector.persist middle)
            { owner: None, values: rightValues }
    };
  };

  let removeAll (transient: transientDeque 'a): (transientDeque 'a) =>
    transient |> Transient.update (fun owner deque => {
      let emptyTailNode = {
        count: 0,
        owner: None,
        values: [||],
      };

      switch deque {
        | Ascending left middle right => Ascending
            emptyTailNode
            (TransientVector.removeAll middle)
            emptyTailNode
        | Descending left middle right => Ascending
            emptyTailNode
            (TransientVector.removeAll middle)
            emptyTailNode
      };
    });
};

let addAll (values: seq 'a) (deque: deque 'a): (deque 'a) => deque
  |> mutate
  |> TransientDeque.addAll values
  |> TransientDeque.persist;

let fromSeq (src: seq 'a): (deque 'a) =>
  empty |> addAll src;

let fromIndexed (indexed: indexed 'a): (deque 'a) =>
  (indexed |> Indexed.toSeq) |> fromSeq;
