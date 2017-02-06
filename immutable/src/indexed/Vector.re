open CopyOnWriteArray;
open Functions;
open Indexed;
open Option;
open Option.Operators;
open Preconditions;
open Seq;
open Transient;

type trie 'a =
  | Empty
  | Leaf (option owner) (array 'a)
  | Level int (ref int) (option owner) (array (trie 'a));

let module Trie = {
  let bits = 5;
  let width = 1 lsl 5;

  let count (trie: trie 'a): int => switch trie {
    | Empty => 0
    | Leaf _ values => CopyOnWriteArray.count values;
    | Level _ count _ _ => !count;
  };

  let rec every (f: 'a => bool) (trie: trie 'a): bool => switch trie {
    | Empty => true
    | Leaf _ values =>
        values |> CopyOnWriteArray.every f
    | Level _ _ _ nodes =>
        nodes |> CopyOnWriteArray.every (every f);
  };

  let rec none (f: 'a => bool) (trie: trie 'a): bool => switch trie {
    | Empty => true
    | Leaf _ values =>
        values |> CopyOnWriteArray.none f
    | Level _ _ _ nodes =>
        nodes |> CopyOnWriteArray.every (none f);
  };

  let rec some (f: 'a => bool) (trie: trie 'a): bool => switch trie {
    | Empty => false
    | Leaf _ values =>
        values |> CopyOnWriteArray.some f
    | Level _ _ _ nodes =>
        nodes |> CopyOnWriteArray.some (some f);
  };

  let rec reduce (f: 'acc => 'a => 'acc) (acc: 'acc) (trie: trie 'a): 'acc => switch trie {
    | Empty => acc
    | Leaf _ values => values |> CopyOnWriteArray.reduce f acc
    | Level _ _ _ nodes =>
        let reducer acc node => node |> reduce f acc;
        nodes |> CopyOnWriteArray.reduce reducer acc
  };

  let rec reduceRight (f: 'acc => 'a => 'acc) (acc: 'acc) (trie: trie 'a): 'acc => switch trie {
    | Empty => acc
    | Leaf _ values => values |> CopyOnWriteArray.reduceRight f acc
    | Level _ _ _ nodes =>
        let reducer acc node => node |> reduceRight f acc;
        nodes |> CopyOnWriteArray.reduceRight reducer acc
  };

  let rec toSeq (trie: trie 'a): (seq 'a) => switch trie {
    | Empty => Seq.empty
    | Leaf _ values => values |> CopyOnWriteArray.toSeq
    | Level _ _ _ nodes => nodes |> CopyOnWriteArray.toSeq |> Seq.flatMap toSeq
  };

  let rec toSeqReversed (trie: trie 'a): (seq 'a) => switch trie {
    | Empty => Seq.empty
    | Leaf _ values => values |> CopyOnWriteArray.toSeqReversed
    | Level _ _ _ nodes => nodes |> CopyOnWriteArray.toSeqReversed |> Seq.flatMap toSeqReversed
  };

  let rec tryFind (f: 'a => bool) (trie: trie 'a): (option 'a) => switch trie {
    | Empty => None
    | Leaf _ values =>
        values |> CopyOnWriteArray.tryFind f
    | Level _ _ _ nodes =>
        let nodesCount = CopyOnWriteArray.count nodes;
        let rec loop index => index < nodesCount
          ? switch (tryFind f nodes.(index)) {
              | Some _ as result => result
              | _ => loop (index + 1)
            }
          : None;
        loop 0
  };

  let depth (trie: trie 'a): int => switch trie {
    | Empty => failwith "invalid state"
    | Leaf _ values => 0
    | Level depth _ _ _ => depth
  };

  let depthCapacity (depth: int): int =>
    width lsl (depth * bits);

  let capacity (trie: trie 'a): int =>
    depthCapacity (depth trie);

  let childCapacity (trie: trie 'a): int =>
    depthCapacity (depth trie - 1);

  let empty = Empty;

  let isEmpty (trie: trie 'a) =>
    (count trie) == 0;

  let rec tryAddFirstLeafToTrieWithMutator
      (updateLevel: int => int => (trie 'a) => (trie 'a) => (trie 'a))
      (owner: option owner)
      (values: array 'a)
      (Level depth levelCount owner tries as trie: trie 'a): (option (trie 'a)) => {
    let firstIndex = 0;
    let firstChild = tries |> CopyOnWriteArray.first;
    let firstChildCount = count firstChild;
    let triesWidth = CopyOnWriteArray.count tries;
    let valuesCount = CopyOnWriteArray.count values;

    switch firstChild {
      | Level _ _ _ _ => switch (firstChild |> tryAddFirstLeafToTrieWithMutator updateLevel owner values) {
          | Some newFirstChild =>
              let newLevelCount = !levelCount - firstChildCount + (count newFirstChild);
              updateLevel newLevelCount firstIndex newFirstChild trie |> Option.return;
          | None when triesWidth < width =>
              let newLevelCount = !levelCount + valuesCount;
              Level depth (ref newLevelCount) owner (tries |> CopyOnWriteArray.addFirst (Leaf owner values)) |> Option.return;
          | _ => None
        }
      | Leaf _ _ when depth > 1 =>
          let newFirstChildLevelCount = firstChildCount + valuesCount;
          let newFirstChild = Level 1 (ref newFirstChildLevelCount) owner [| Leaf owner values, firstChild |];
          let newLevelCount = !levelCount - firstChildCount + newFirstChildLevelCount;
          updateLevel newLevelCount firstIndex newFirstChild trie |> Option.return;
      | Leaf _ _ when triesWidth < width =>
          let newLevelCount = !levelCount + valuesCount;
          Level depth (ref newLevelCount) owner (tries |> CopyOnWriteArray.addFirst (Leaf owner values)) |> Option.return;
      | _ => None
    };
  };

  let addFirstLeafWithMutator
      (updateLevel: int => int => (trie 'a) => (trie 'a) => (trie 'a))
      (owner: option owner)
      (values: array 'a)
      (trie: trie 'a): (trie 'a) => switch trie {
    | Empty =>
        Leaf owner values
    | Leaf _ _ =>
        let levelCount = count trie + CopyOnWriteArray.count values;
        Level 1 (ref levelCount) owner [| Leaf owner values, trie |];
    | Level levelDepth _ _ _ => switch (trie |> tryAddFirstLeafToTrieWithMutator updateLevel owner values) {
      | Some trie => trie
      | None =>
          let levelCount = count trie + CopyOnWriteArray.count values;
          let levelDepth = levelDepth + 1;
          Level levelDepth (ref levelCount) owner [| Leaf owner values, trie |];
    }
  };

  let rec tryAddLastLeafToTrieWithMutator
      (updateLevel: int => int => (trie 'a) => (trie 'a) => (trie 'a))
      (owner: option owner)
      (values: array 'a)
      (Level depth levelCount owner tries as trie: trie 'a): (option (trie 'a)) => {
    let lastIndex = tries |> CopyOnWriteArray.lastIndex;
    let lastChild = tries |> CopyOnWriteArray.last;
    let lastChildCount = count lastChild;
    let triesWidth = CopyOnWriteArray.count tries;
    let valuesCount = CopyOnWriteArray.count values;

    switch lastChild {
      | Level _ _ _ _ => switch (lastChild |> tryAddLastLeafToTrieWithMutator updateLevel owner values) {
          | Some newLastChild =>
              let newLevelCount = !levelCount - lastChildCount + (count newLastChild);
              updateLevel newLevelCount lastIndex newLastChild trie |> Option.return;
          | None when triesWidth < width =>
              let newLevelCount = !levelCount + valuesCount;
              Level depth (ref newLevelCount) owner (tries |> CopyOnWriteArray.addLast (Leaf owner values)) |> Option.return;
          | _ => None
        }
      | Leaf _ _ when depth > 1 =>
          let newLastChildLevelCount = lastChildCount + valuesCount;
          let newLastChild = Level 1 (ref newLastChildLevelCount) owner [| lastChild, Leaf owner values |];
          let newLevelCount = !levelCount - lastChildCount + newLastChildLevelCount;
          updateLevel newLevelCount lastIndex newLastChild trie |> Option.return;
      | Leaf _ _ when triesWidth < width =>
          let newLevelCount = !levelCount + valuesCount;
          Level depth (ref newLevelCount) owner (tries |> CopyOnWriteArray.addLast (Leaf owner values)) |> Option.return;
      | _ => None
    }
  };

  let addLastLeafWithMutator
      (updateLevel: int => int => (trie 'a) => (trie 'a) => (trie 'a))
      (owner: option owner)
      (values: array 'a)
      (trie: trie 'a): (trie 'a) => switch trie {
    | Empty =>
        Leaf owner values
    | Leaf _ _ =>
        let levelCount = count trie + CopyOnWriteArray.count values;
        Level 1 (ref levelCount) owner [| trie, Leaf owner values |];
    | Level levelDepth _ _ _ => switch (trie |> tryAddLastLeafToTrieWithMutator updateLevel owner values) {
      | Some trie => trie
      | None =>
          let levelCount = count trie + CopyOnWriteArray.count values;
          let levelDepth = levelDepth + 1;
          Level levelDepth (ref levelCount) owner [| trie, Leaf owner values |];
    }
  };

  let rec removeFirstLeafWithMutator
      (updateLevel: int => int => (trie 'a) => (trie 'a) => (trie 'a))
      (owner: option owner)
      (trie: trie 'a): (trie 'a, trie 'a) => switch trie {
    | Leaf _ _ => (trie, Empty);
    | Level levelDepth levelCount _ tries when levelDepth > 1 =>
        let triesWidth = CopyOnWriteArray.count tries;
        let firstChildIndex = 0;
        let firstChild = tries |> CopyOnWriteArray.first;
        let (firstLeaf, newFirstChild) = firstChild |> removeFirstLeafWithMutator updateLevel owner;

        switch newFirstChild {
          | Empty when triesWidth > 2 =>
              let newLevelCount = !levelCount - (count firstLeaf);
              let newTrie = Level levelDepth (ref newLevelCount) owner (CopyOnWriteArray.removeFirst tries);
              (firstLeaf, newTrie)
          | Empty =>
              (firstLeaf, CopyOnWriteArray.last tries)
          | _ =>
              let newLevelCount = !levelCount - (count firstLeaf);
              (firstLeaf, updateLevel newLevelCount firstChildIndex newFirstChild trie)
        };
    | Level 1 levelCount _ tries =>
        let triesWidth = CopyOnWriteArray.count tries;
        let firstChild = tries |> CopyOnWriteArray.first;

        triesWidth > 2 ? {
          let newLevelCount = !levelCount - (count firstChild);
          (firstChild, Level 1 (ref newLevelCount) owner (CopyOnWriteArray.removeFirst tries))
        } : {
          (firstChild, CopyOnWriteArray.last tries)
        };
    | _ => failwith "invalid state"
  };

  let rec removeLastLeafWithMutator
      (updateLevel: int => int => (trie 'a) => (trie 'a) => (trie 'a))
      (owner: option owner)
      (trie: trie 'a): (trie 'a, trie 'a) => switch trie {
    | Leaf _ _ => (Empty, trie);
    | Level levelDepth levelCount _ tries when levelDepth > 1 =>
        let triesWidth = CopyOnWriteArray.count tries;
        let lastChildIndex = tries |> CopyOnWriteArray.lastIndex;
        let lastChild = tries |> CopyOnWriteArray.last;
        let (newLastChild, lastLeaf) = lastChild |> removeLastLeafWithMutator updateLevel owner;

        switch newLastChild {
          | Empty when triesWidth > 2 =>
              let newLevelCount = !levelCount - (count lastLeaf);
              let newTrie = Level levelDepth (ref newLevelCount) owner (CopyOnWriteArray.removeLast tries);
              (newTrie, lastLeaf)
          | Empty =>
              (CopyOnWriteArray.first tries, lastLeaf)
          | _ =>
              let newLevelCount = !levelCount - (count lastLeaf);
              (updateLevel newLevelCount lastChildIndex newLastChild trie, lastLeaf)
        };
    | Level 1 levelCount _ tries =>
        let triesWidth = CopyOnWriteArray.count tries;
        let lastChild = tries |> CopyOnWriteArray.last;

        triesWidth > 2 ? {
          let newLevelCount = !levelCount - (count lastChild);
          (Level 1 (ref newLevelCount) owner (CopyOnWriteArray.removeLast tries), lastChild)
        } : {
          (CopyOnWriteArray.first tries, lastChild)
        };
    | _ => failwith "invalid state"
  };

  let computeIndexUsingRadixSearch (depth: int) (index: int): int => {
    let mask = width - 1;
    let level = depth * bits;
    (index lsr level) land mask;
  };

  let canRadixSearch (trie: trie 'a): bool => switch trie {
    | Empty => failwith "invalid state"
    | Leaf _ _ => true
    | Level depth count _ tries =>
        let childCapacity = depthCapacity (depth - 1);
        let triesCount = CopyOnWriteArray.count tries;
        !count == (triesCount * childCapacity)
  };

  let computeLevelIndexUsingRadixSearch (index: int) (Level depth _ _ tries: trie 'a): int =>
    computeIndexUsingRadixSearch depth index;

  let computeLevelIndexUsingCountSearch (index: int) (Level depth _ _ tries: trie 'a): int => {
    let rec loop index childIndex => {
      let childNode = tries.(childIndex);
      let childCount = count childNode;

      index < childCount
        ? childIndex
        : loop (index - childCount) (childIndex + 1);
    };

    loop index 0;
  };

  let getImpl
      (computeLevelIndex: int => (trie 'a) => int)
      (get: int => (trie 'a) => 'a)
      (index: int)
      (trie: trie 'a): 'a => switch trie {
    | Leaf _ values =>
        values.(computeIndexUsingRadixSearch 0 index)
    | Level depth _ _ tries =>
        let child = tries.(computeLevelIndex index trie);
        child |> get index
    | Empty => failwith "invalid state"
  };

  let rec getUsingRadixSearch (index: int) (trie: trie 'a): 'a => getImpl
    computeLevelIndexUsingRadixSearch
    getUsingRadixSearch
    index
    trie;

  let rec get (index: int) (trie: trie 'a): 'a => switch trie {
    | Level _ _ _ tries when not @@ canRadixSearch @@ trie => getImpl
        computeLevelIndexUsingCountSearch
        get
        index
        trie
    | _ => trie |> getUsingRadixSearch index
  };

  let skipImpl
      (computeLevelIndex: int => (trie 'a) => int)
      (skip:
        (option owner) =>
        int =>
        (trie 'a) =>
        (array 'a, trie 'a)
      )
      (owner: option owner)
      (skipCount: int)
      (trie: trie 'a): (array 'a, trie 'a) => switch trie {
    | Leaf _ nodes =>
      let skipCount = computeIndexUsingRadixSearch 0 (skipCount - 1) + 1;
      let result = CopyOnWriteArray.skip skipCount nodes;
      (result, empty)
    | Level depth levelCount _ tries =>
      let triesLastIndex = CopyOnWriteArray.lastIndex tries;
      let childIndex = computeLevelIndex (skipCount - 1) trie;
      let childNode = tries.(childIndex);
      let (tail, newChildNode) as childResult = childNode |> skip owner skipCount;

      switch newChildNode {
        | Empty when childIndex == triesLastIndex => childResult
        | Empty when childIndex == (triesLastIndex - 1) =>
          (tail, tries |> CopyOnWriteArray.last)
        | Empty =>
          let newTries = tries |> CopyOnWriteArray.skip (childIndex + 1);
          let levelCount = newTries |> CopyOnWriteArray.reduce (fun acc next => count next) 0;
          (tail, Level depth (ref levelCount) owner newTries)
        | _ =>
          let newTries = tries |> CopyOnWriteArray.skip childIndex;
          newTries.(0) = newChildNode;
          let levelCount = newTries |> CopyOnWriteArray.reduce (fun acc next => count next) 0;
          (tail, Level depth (ref levelCount) owner newTries)
      }
    | Empty => failwith "invalid state"
  };

  let rec skipUsingRadixSearch
      (owner: option owner)
      (skipCount: int)
      (trie: trie 'a): (array 'a, trie 'a) => skipImpl
    computeLevelIndexUsingRadixSearch
    skipUsingRadixSearch
    owner
    skipCount
    trie;

  let rec skip
      (owner: option owner)
      (count: int)
      (trie: trie 'a): (array 'a, trie 'a) => switch trie {
    | Level _ _ _ tries when not @@ canRadixSearch @@ trie => skipImpl
       computeLevelIndexUsingCountSearch
       skip
       owner
       count
       trie
    | _ => skipUsingRadixSearch
       owner
       count
       trie
  };

  let takeImpl
      (computeLevelIndex: int => (trie 'a) => int)
      (take:
        (option owner) =>
        int =>
        (trie 'a) =>
        (trie 'a, array 'a)
      )
      (owner: option owner)
      (takeCount: int)
      (trie: trie 'a): (trie 'a, array 'a) => switch trie {
    | Leaf _ nodes =>
      let takeCount = computeIndexUsingRadixSearch 0 (takeCount - 1) + 1;
      let result = CopyOnWriteArray.take takeCount nodes;
      (empty, result)
    | Level depth levelCount _ tries =>
      let childIndex = computeLevelIndex (takeCount - 1) trie;
      let childNode = tries.(childIndex);
      let (newChildNode, tail) as childResult = childNode |> take owner takeCount;

      switch newChildNode {
        | Empty when childIndex == 0 => childResult
        | Empty when childIndex == 1 => (tries.(0), tail)
        | Empty =>
          let newTries = tries |> CopyOnWriteArray.take childIndex;
          let levelCount = newTries |> CopyOnWriteArray.reduce (fun acc next => count next) 0;
          (Level depth (ref levelCount) owner newTries, tail)
        | _ =>
          let newTries = tries |> CopyOnWriteArray.take (childIndex + 1);
          newTries.(childIndex) = newChildNode;
          let levelCount = newTries |> CopyOnWriteArray.reduce (fun acc next => count next) 0;
          (Level depth (ref levelCount) owner newTries, tail)
      };
    | Empty => failwith "invalid state"
  };

  let rec takeUsingRadixSearch
      (owner: option owner)
      (takeCount: int)
      (trie: trie 'a): (trie 'a, array 'a) => takeImpl
    computeLevelIndexUsingRadixSearch
    takeUsingRadixSearch
    owner
    takeCount
    trie;

  let rec take
      (owner: option owner)
      (count: int)
      (trie: trie 'a): (trie 'a, array 'a) => switch trie {
    | Level _ _ _ tries when not @@ canRadixSearch @@ trie => takeImpl
       computeLevelIndexUsingCountSearch
       take
       owner
       count
       trie
    | _ => takeUsingRadixSearch
       owner
       count
       trie
  };

  let updateWithMutatorImpl
      (computeLevelIndex: int => (trie 'a) => int)
      (updateWithMutator:
        (int => int => (trie 'a) => (trie 'a) => (trie 'a)) =>
        (int => 'a => (trie 'a) => (trie 'a)) =>
        int =>
        'a =>
        (trie 'a) =>
        (trie 'a)
      )
      (updateLevel: int => int => (trie 'a) => (trie 'a) => (trie 'a))
      (updateLeaf: int => 'a => (trie 'a) => (trie 'a))
      (index: int)
      (value: 'a)
      (trie: trie 'a): (trie 'a) => switch trie {
    | Empty => failwith "invalid state"
    | Leaf _ _ => trie |> updateLeaf (computeIndexUsingRadixSearch 0 index) value;
    | Level depth count _ tries =>
        let childIndex = computeLevelIndex index trie;
        let childNode = tries.(childIndex);
        let newChildNode = childNode |> updateWithMutator updateLevel updateLeaf index value;
        childNode === newChildNode
          ? trie
          : trie |> updateLevel !count childIndex newChildNode;
  };

  let rec updateUsingRadixSearchWithMutator
      (updateLevel: int => int => (trie 'a) => (trie 'a) => (trie 'a))
      (updateLeaf: int => 'a => (trie 'a) => (trie 'a))
      (index: int)
      (value: 'a)
      (trie: trie 'a): (trie 'a) => updateWithMutatorImpl
    computeLevelIndexUsingRadixSearch
    updateUsingRadixSearchWithMutator
    updateLevel
    updateLeaf
    index
    value
    trie;

  let rec updateWithMutator
      (updateLevel: int => int => (trie 'a) => (trie 'a) => (trie 'a))
      (updateLeaf: int => 'a => (trie 'a) => (trie 'a))
      (index: int)
      (value: 'a)
      (trie: trie 'a): (trie 'a) => switch trie {
    | Level _ _ _ tries when not @@ canRadixSearch @@ trie => updateWithMutatorImpl
       computeLevelIndexUsingCountSearch
       updateWithMutator
       updateLevel
       updateLeaf
       index
       value
       trie;
    | _ => updateUsingRadixSearchWithMutator
        updateLevel
        updateLeaf
        index
        value
        trie;
  };
};

let module VectorImpl = {
  module type VectorBase = {
    type t 'a;

    let addFirst: (option owner) => 'a => (t 'a) => (t 'a);
    let addLast: (option owner) => 'a => (t 'a) => (t 'a);
    let count: (t 'a) => int;
    let empty: (t 'a);
    let getUnsafe: int => (t 'a) => 'a;
    let removeFirst: (option owner) => (t 'a) => (t 'a);
    let removeLast: (option owner) => (t 'a) => (t 'a);
    let update: (option owner) => int => 'a => (t 'a) => (t 'a);
  };

  module type S = {
    type t 'a;

    let add: (option owner) => 'a => (t 'a) => (t 'a);
    let addAll: (option owner) => (seq 'a) => (t 'a) => (t 'a);
    let addFirst: (option owner) => 'a => (t 'a) => (t 'a);
    let addLast: (option owner) => 'a => (t 'a) => (t 'a);
    let count: (t 'a) => int;
    let empty: (t 'a);
    let first: (t 'a) => 'a;
    let get: int => (t 'a) => 'a;
    let isEmpty: (t 'a) => bool;
    let isNotEmpty: (t 'a) => bool;
    let last: (t 'a) => 'a;
    let removeAll: (t 'a) => (t 'a);
    let removeFirst: (option owner) => (t 'a) => (t 'a);
    let removeLast: (option owner) => (t 'a) => (t 'a);
    let tryFirst: (t 'a) => (option 'a);
    let tryGet: int => (t 'a) => (option 'a);
    let tryLast: (t 'a) => (option 'a);
    let update: (option owner) => int => 'a => (t 'a) => (t 'a);
  };

  let module Make = fun (X: VectorBase) => {
    type t 'a = X.t 'a;

    let add = X.addLast;

    let addAll (owner: option owner) (seq: seq 'a) (vector: t 'a): (t 'a) => seq
      |> Seq.reduce (fun acc next => acc |> X.addLast owner next) vector;

    let addFirst = X.addFirst;
    let addLast = X.addLast;
    let count = X.count;

    let get (index: int) (vector: t 'a): 'a => {
      Preconditions.failIfOutOfRange (X.count vector) index;
      X.getUnsafe index vector;
    };

    let empty: (t 'a) = X.empty;

    let first (vector: t 'a): 'a => get 0 vector;

    let isEmpty (vector: t 'a): bool =>
      (X.count vector) == 0;

    let isNotEmpty (vector: t 'a): bool =>
      (X.count vector) != 0;

    let last (vector: t 'a): 'a => get ((X.count vector) - 1) vector;

    let removeAll (vector: t 'a): (t 'a) => X.empty;

    let removeFirst = X.removeFirst;

    let removeLast = X.removeLast;

    let tryGet (index: int) (vector: t 'a): (option 'a) => {
      let trieCount = count vector;
      Preconditions.noneIfIndexOutOfRange trieCount index (flip X.getUnsafe vector);
    };

    let tryFirst (vector: t 'a): (option 'a) => tryGet 0 vector;

    let tryLast (vector: t 'a): (option 'a) => tryGet ((X.count vector) - 1) vector;

    let update (owner: option owner) (index: int) (value: 'a) (vector: t 'a): (t 'a) => {
      Preconditions.failIfOutOfRange (X.count vector) index;
      X.update owner index value vector;
    };
  };
};

type vector 'a = {
  left: array 'a,
  middle: trie 'a,
  right: array 'a,
};

let updateLevelPersistent
    (count: int)
    (index: int)
    (child: trie 'a)
    (Level depth _ _ tries: trie 'a): (trie 'a) =>
  Level depth (ref count) None (CopyOnWriteArray.update index child tries);

let module PersistentVector = VectorImpl.Make {
  type t 'a = vector 'a;

  let tailIsFull (arr: array 'a): bool => (CopyOnWriteArray.count arr) == Trie.width;
  let tailIsNotFull (arr: array 'a): bool => (CopyOnWriteArray.count arr) != Trie.width;

  let count ({ left, middle, right }: vector 'a): int => {
    let leftCount = CopyOnWriteArray.count left;
    let middleCount = Trie.count middle;
    let rightCount = CopyOnWriteArray.count right;

    leftCount + middleCount + rightCount;
  };

  let empty = {
    left: [||],
    middle: Trie.empty,
    right: [||],
  };

  let updateLeaf
      (index: int)
      (value: 'a)
      (Leaf _ values: trie 'a): (trie 'a) =>
    Leaf None (values |> CopyOnWriteArray.update index value);

  let addFirst (owner: option owner) (value: 'a) ({ left, middle, right }: vector 'a): (vector 'a) =>
    (tailIsFull left) && (CopyOnWriteArray.isNotEmpty right) ? {
      left: [| value |],
      middle: Trie.addFirstLeafWithMutator updateLevelPersistent None left middle,
      right,
    } :

    (tailIsFull left) && (CopyOnWriteArray.isEmpty right) ? {
      left: [| value |],
      middle,
      right: left,
    } :

    {
      left: left |> CopyOnWriteArray.addFirst value,
      middle,
      right,
    };

  let addLast (owner: option owner) (value: 'a) ({ left, middle, right }: vector 'a): (vector 'a) =>
    /* If right is empty, then middle is also empty */
    (tailIsNotFull left) && (CopyOnWriteArray.isEmpty right) ? {
      left: left |> CopyOnWriteArray.addLast value,
      middle,
      right,
    } :

    (tailIsNotFull right) ? {
      left,
      middle,
      right: right |> CopyOnWriteArray.addLast value,
    } :

    {
      left,
      middle: Trie.addLastLeafWithMutator updateLevelPersistent None right middle,
      right: [| value |],
    };

  let removeFirst (owner: option owner) ({ left, middle, right }: vector 'a): (vector 'a) => {
    let leftCount = CopyOnWriteArray.count left;
    let middleCount = Trie.count middle;
    let rightCount = CopyOnWriteArray.count right;

    leftCount > 1 ? {
      left: CopyOnWriteArray.removeFirst left,
      middle,
      right,
    } :

    middleCount > 0 ? {
      let (Leaf _ left, middle) = Trie.removeFirstLeafWithMutator updateLevelPersistent None middle;
      { left, middle, right };
    } :

    rightCount > 0 ? {
      left: right,
      middle,
      right: [||],
    } :

    leftCount == 1 ? empty :

    failwith "vector is empty";
  };

  let removeLast (owner: option owner) ({ left, middle, right }: vector 'a): (vector 'a) => {
    let leftCount = CopyOnWriteArray.count left;
    let middleCount = Trie.count middle;
    let rightCount = CopyOnWriteArray.count right;

    rightCount > 1 ? {
      left,
      middle,
      right: CopyOnWriteArray.removeLast right,
    } :

    middleCount > 0 ? {
      let (middle, Leaf _ right) = Trie.removeLastLeafWithMutator updateLevelPersistent None middle;
      { left, middle, right };
    } :

    rightCount == 1 ? {
      left,
      middle,
      right: [||],
    } :

    leftCount > 0 ? {
      left: CopyOnWriteArray.removeLast left,
      middle,
      right,
    } :

    failwith "vector is empty";
  };

  let getUnsafe (index: int) ({ left, middle, right }: vector 'a): 'a => {
    let leftCount = CopyOnWriteArray.count left;
    let middleCount = Trie.count middle;

    let rightIndex = index - middleCount - leftCount;

    index < leftCount ? left.(index) :
    rightIndex >= 0 ? right.(rightIndex) :
    {
      let index = index - leftCount;
      middle |> Trie.get index;
    }
  };

  let update
      (owner: option owner)
      (index: int)
      (value: 'a)
      ({ left, middle, right } as vector: vector 'a): (vector 'a) => {
    Preconditions.failIfOutOfRange (count vector) index;

    let leftCount = CopyOnWriteArray.count left;
    let middleCount = Trie.count middle;

    let rightIndex = index - middleCount - leftCount;

    index < leftCount ? {
      left: left |>  CopyOnWriteArray.update index value,
      middle,
      right,
    } :

    rightIndex >= 0 ? {
      left,
      middle,
      right: right |> CopyOnWriteArray.update rightIndex value,
    } :

    {
      let index = (index - leftCount);
      let middle = middle |> Trie.updateWithMutator updateLevelPersistent updateLeaf index value;
      { left, middle, right }
    };
  };
};

type transientVectorImpl 'a = {
  left: array 'a,
  leftCount: int,
  middle: trie 'a,
  right: array 'a,
  rightCount: int,
};

let tailCopyAndExpand (arr: array 'a): (array 'a) => {
  let arrCount = CopyOnWriteArray.count arr;
  let retval = Array.make Trie.width arr.(0);
  Array.blit arr 0 retval 0 (min arrCount Trie.width);
  retval;
};

let module TransientVectorImpl = VectorImpl.Make {
  type t 'a = transientVectorImpl 'a;

  let tailIsEmpty (count: int): bool => count == 0;
  let tailIsFull (count: int): bool => count == Trie.width;
  let tailIsNotEmpty (count: int): bool => count != 0;
  let tailIsNotFull (count: int): bool => count != Trie.width;

  let tailAddFirst (value: 'a) (arr: array 'a): (array 'a) => {
    let arr = (CopyOnWriteArray.count arr) == 0
      ? Array.make Trie.width value
      : arr;

    let rec loop index => index > 0 ? {
      arr.(index) = arr.(index - 1);
      loop (index - 1);
    } : ();

    loop (CopyOnWriteArray.lastIndex arr);
    arr.(0) = value;
    arr;
  };

  let tailRemoveFirst (arr: array 'a): (array 'a) => {
    let countArr = CopyOnWriteArray.count arr;
    let rec loop index => index < countArr ? {
      arr.(index - 1) = arr.(index);
      loop (index + 1);
    } : arr;

    loop 1;
  };

  let tailUpdate (index: int) (value: 'a) (arr: array 'a): (array 'a) => {
    let arr = (CopyOnWriteArray.count arr) == 0
      ? Array.make Trie.width value
      : arr;

    arr.(index) = value;
    arr;
  };

  let count ({ leftCount, middle, rightCount }: transientVectorImpl 'a): int => {
    let middleCount = Trie.count middle;
    leftCount + middleCount + rightCount;
  };

  let empty = {
    left: [||],
    leftCount: 0,
    middle: Trie.empty,
    right: [||],
    rightCount: 0,
  };

  let updateLevel
      (owner: owner)
      (count: int)
      (index: int)
      (child: trie 'a)
      (trie: trie 'a): (trie 'a) => switch trie {
    | Level depth trieCount (Some trieOwner) tries when trieOwner === owner =>
        tries.(index) = child;
        trieCount := count;
        trie
    | Level depth _ _ tries =>
        Level depth (ref count) (Some owner) (CopyOnWriteArray.update index child tries)
    | _ => failwith "Invalid state"
  };

  let updateLeaf
      (owner: owner)
      (index: int)
      (value: 'a)
      (trie: trie 'a): (trie 'a) => switch trie {
    | Leaf (Some trieOwner) values when trieOwner === owner =>
        values.(index) = value;
        trie
    | Leaf _ values =>
        Leaf (Some owner) (values |> CopyOnWriteArray.update index value)
    | _ => failwith "Invalid state"
  };

  let addFirst
      (owner: option owner)
      (value: 'a)
      ({
        left,
        leftCount,
        middle,
        right,
        rightCount,
      }: transientVectorImpl 'a): (transientVectorImpl 'a) =>
    (tailIsFull leftCount) && (tailIsNotEmpty rightCount) ? {
      left: Array.make Trie.width value,
      leftCount: 1,
      middle: Trie.addFirstLeafWithMutator (updateLevel @@ Option.get @@ owner) owner left middle,
      right,
      rightCount,
    } :

    (tailIsFull leftCount) && (tailIsEmpty rightCount) ? {
      left: Array.make Trie.width value,
      leftCount: 1,
      middle,
      right: left,
      rightCount: leftCount,
    } :

    {
      left: left |> tailAddFirst value,
      leftCount: leftCount + 1,
      middle,
      right,
      rightCount,
    };

  let addLast
      (owner: option owner)
      (value: 'a)
      ({
        left,
        leftCount,
        middle,
        right,
        rightCount,
      }: transientVectorImpl 'a): (transientVectorImpl 'a) =>
    /* If right is empty, then middle is also empty */
    (tailIsNotFull leftCount) && (tailIsEmpty rightCount) ? {
      left: left |> tailUpdate leftCount value,
      leftCount: leftCount + 1,
      middle,
      right,
      rightCount,
    } :

    (tailIsNotFull rightCount) ? {
      left,
      leftCount,
      middle,
      right: right |> tailUpdate rightCount value,
      rightCount: rightCount + 1,
    } :

    {
      left,
      leftCount,
      middle: Trie.addLastLeafWithMutator (updateLevel @@ Option.get @@ owner) owner right middle,
      right: Array.make Trie.width value,
      rightCount: 1,
    };

  let removeFirst
      (owner: option owner)
      ({
        left,
        leftCount,
        middle,
        right,
        rightCount,
      }: transientVectorImpl 'a): (transientVectorImpl 'a) =>
    leftCount > 1 ? {
      left: tailRemoveFirst left,
      leftCount: leftCount - 1,
      middle,
      right,
      rightCount,
    } :

    (Trie.count middle) > 0 ? {
      let (Leaf leftOwner left, middle) = middle
        |> Trie.removeFirstLeafWithMutator (updateLevel @@ Option.get @@ owner) owner;
      let leftCount = CopyOnWriteArray.count left;

      let owner = Option.get owner;
      let left = switch leftOwner {
        | Some leftOwner when leftOwner === owner && leftCount == Trie.width => left
        | _ => tailCopyAndExpand left
      };

      {
        left,
        leftCount,
        middle,
        right,
        rightCount,
      };
    } :

    rightCount > 0 ? {
      left: right,
      leftCount: rightCount,
      middle,
      right: Array.make Trie.width right.(0),
      rightCount: 0,
    } :

    leftCount == 1 ? {
      left,
      leftCount: 0,
      middle,
      right,
      rightCount,
    } :

    failwith "vector is empty";

  let removeLast
      (owner: option owner)
      ({
        left,
        leftCount,
        middle,
        right,
        rightCount,
      }: transientVectorImpl 'a): (transientVectorImpl 'a) =>
    rightCount > 1 ? {
      left,
      leftCount,
      middle,
      right,
      rightCount: rightCount - 1,
    } :

    (Trie.count middle) > 0 ? {
      let (middle, Leaf rightOwner right) = middle
        |> Trie.removeLastLeafWithMutator (updateLevel @@ Option.get @@ owner) owner;
      let rightCount = CopyOnWriteArray.count right;

      let owner = Option.get owner;
      let right = switch rightOwner {
        | Some rightOwner when rightOwner === owner && rightCount == Trie.width => right
        | _ => tailCopyAndExpand right
      };

      { left, leftCount, middle, right, rightCount };
    } :

    rightCount == 1 ? {
      left,
      leftCount,
      middle,
      right,
      rightCount: 0,
    } :

    leftCount > 0 ? {
      left,
      leftCount: leftCount - 1,
      middle,
      right,
      rightCount,
    } :

    failwith "vector is empty";

  let getUnsafe
      (index: int)
      ({
        left,
        leftCount,
        middle,
        right,
        rightCount,
      }: transientVectorImpl 'a): 'a => {
    let middleCount = Trie.count middle;
    let rightIndex = index - middleCount - leftCount;

    index < leftCount ? left.(index) :

    rightIndex >= 0 ? right.(rightIndex) :

    {
      let index = index - leftCount;
      middle |> Trie.get index;
    }
  };

  let update
      (owner: option owner)
      (index: int)
      (value: 'a)
      ({
        left,
        leftCount,
        middle,
        right,
        rightCount,
      } as vector: transientVectorImpl 'a): (transientVectorImpl 'a) => {
    Preconditions.failIfOutOfRange (count vector) index;

    let middleCount = Trie.count middle;

    let rightIndex = index - middleCount - leftCount;

    index < leftCount ? {
      left: left |> tailUpdate index value,
      leftCount,
      middle,
      right,
      rightCount,
    } :

    rightIndex >= 0 ? {
      left,
      leftCount,
      middle,
      right: right |> tailUpdate rightIndex value,
      rightCount,
    } :

    {
      let index = (index - leftCount);
      let middle = middle |> Trie.updateWithMutator
        (updateLevel @@ Option.get @@ owner)
        (updateLeaf @@ Option.get @@ owner)
        index
        value;

      { left, leftCount, middle, right, rightCount }
    };
  };
};

let add value => PersistentVector.add None value;
let addFirst value => PersistentVector.addFirst None value;
let addLast value => PersistentVector.addLast None value;
let count = PersistentVector.count;
let empty = PersistentVector.empty;
let first = PersistentVector.first;
let get = PersistentVector.get;
let isEmpty = PersistentVector.isEmpty;
let isNotEmpty = PersistentVector.isNotEmpty;
let last = PersistentVector.last;
let removeAll = PersistentVector.removeAll;
let removeFirst vector => PersistentVector.removeFirst None vector;
let removeLast vector => PersistentVector.removeLast None vector;
let tryFirst = PersistentVector.tryFirst;
let tryGet = PersistentVector.tryGet;
let tryLast = PersistentVector.tryLast;
let update index => PersistentVector.update None index;

type transientVector 'a = transient (transientVectorImpl 'a);

let mutate ({ left, middle, right }: vector 'a): (transientVector 'a) => Transient.create {
  left: (CopyOnWriteArray.count left > 0) ? tailCopyAndExpand left : [||],
  leftCount: CopyOnWriteArray.count left,
  middle,
  right: (CopyOnWriteArray.count right > 0) ? tailCopyAndExpand right : [||],
  rightCount: CopyOnWriteArray.count right,
};

module TransientVector = {
  let addFirst (value: 'a) (transient: transientVector 'a): (transientVector 'a) =>
    transient |> Transient.update (fun owner => TransientVectorImpl.addFirst (Some owner) value);

  let addLast (value: 'a) (transient: transientVector 'a): (transientVector 'a) =>
    transient |> Transient.update (fun owner => TransientVectorImpl.addLast (Some owner) value);

  let add = addLast;

  let addAll (seq: seq 'a) (transient: transientVector 'a): (transientVector 'a) =>
    transient |> Transient.update (fun owner => TransientVectorImpl.addAll (Some owner) seq);

  let count (transient: transientVector 'a): int =>
    transient |> Transient.get |> TransientVectorImpl.count;

  let empty () => empty |> mutate;

  let isEmpty (transient: transientVector 'a): bool =>
    transient |> Transient.get |> TransientVectorImpl.isEmpty;

  let isNotEmpty (transient: transientVector 'a): bool =>
    transient |> Transient.get |> TransientVectorImpl.isNotEmpty;

  let tailCompress (count: int) (arr: array 'a): (array 'a) => {
    let arrCount = CopyOnWriteArray.count arr;

    arrCount == count ? arr :
    arrCount > 0 ? {
      let retval = Array.make count arr.(0);
      Array.blit arr 0 retval 0 count;
      retval;
    } : [||];
  };

  let persist (transient: transientVector 'a): (vector 'a) => {
    let {
      left,
      leftCount,
      middle,
      right,
      rightCount,
    } = transient |> Transient.persist;

    {
      left: left |> tailCompress leftCount,
      middle,
      right: right |> tailCompress rightCount,
    }
  };

  let removeAll (transient: transientVector 'a): (transientVector 'a) =>
      transient |> Transient.update (fun owner => TransientVectorImpl.removeAll);

  let removeFirst (transient: transientVector 'a): (transientVector 'a) =>
    transient |> Transient.update (fun owner => TransientVectorImpl.removeFirst (Some owner));

  let removeLast (transient: transientVector 'a): (transientVector 'a) =>
    transient |> Transient.update (fun owner => TransientVectorImpl.removeLast (Some owner));

  let get (index: int) (transient: transientVector 'a): 'a =>
    transient |> Transient.get |> TransientVectorImpl.get index;

  let first (transient: transientVector 'a): 'a =>
    transient |> Transient.get |> TransientVectorImpl.first;

  let last (transient: transientVector 'a): 'a =>
    transient |> Transient.get |> TransientVectorImpl.last;

  let reverse (transient: transientVector 'a): (transientVector 'a) =>
    transient |> Transient.update (fun owner vector => {
      let count = TransientVectorImpl.count vector;
      let lastIndex = count - 1;

      let rec loop indexFirst indexLast => indexFirst < indexLast ? {
        let first = vector |> TransientVectorImpl.get indexFirst;
        let last = vector |> TransientVectorImpl.get indexLast;

        vector
          |> TransientVectorImpl.update (Some owner) indexFirst first
          |> TransientVectorImpl.update (Some owner) indexLast last;
      }: vector;

      loop 0 lastIndex;
    });

  let tryGet (index: int) (transient: transientVector 'a): (option 'a) =>
    transient |> Transient.get |> TransientVectorImpl.tryGet index;

  let tryFirst (transient: transientVector 'a): (option 'a) =>
    transient |> Transient.get |> TransientVectorImpl.tryFirst;

  let tryLast (transient: transientVector 'a): (option 'a) =>
    transient |> Transient.get |> TransientVectorImpl.tryLast;

  let update (index: int) (value: 'a) (transient: transientVector 'a): (transientVector 'a) =>
    transient |> Transient.update (fun owner => TransientVectorImpl.update (Some owner) index value);
};

let addAll (seq: seq 'a) (trie: vector 'a): (vector 'a) => trie
  |> mutate
  |> TransientVector.addAll seq
  |> TransientVector.persist;

let every (f: 'a => bool) ({ left, middle, right }: vector 'a): bool =>
  (CopyOnWriteArray.every f left) && (Trie.every f middle) && (CopyOnWriteArray.every f right);

let find (f: 'a => bool) ({ left, middle, right }: vector 'a): 'a =>
  /* FIXME: Add an operator to Option for this use case */
  switch (left |> CopyOnWriteArray.tryFind f) {
    | Some v => v
    | _ => switch (middle |> Trie.tryFind f) {
      | Some v => v
      | _ => right |> CopyOnWriteArray.find f
    }
  };

let none (f: 'a => bool) ({ left, middle, right }: vector 'a): bool =>
  (CopyOnWriteArray.none f left) && (Trie.none f middle) && (CopyOnWriteArray.none f right);

let some (f: 'a => bool) ({ left, middle, right }: vector 'a): bool =>
  (CopyOnWriteArray.some f left) || (Trie.some f middle) || (CopyOnWriteArray.some f right);

let fromSeq (seq: seq 'a): (vector 'a) =>
  empty |> addAll seq;

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) ({ left, middle, right }: vector 'a): 'acc => {
  let acc = left |> CopyOnWriteArray.reduce f acc;
  let acc = middle |> Trie.reduce f acc;
  let acc = right |> CopyOnWriteArray.reduce f acc;
  acc;
};

let reduceRight (f: 'acc => 'a => 'acc) (acc: 'acc) ({ left, middle, right }: vector 'a): 'acc => {
  let acc = right |> CopyOnWriteArray.reduceRight f acc;
  let acc = middle |> Trie.reduceRight f acc;
  let acc = left |> CopyOnWriteArray.reduceRight f acc;
  acc;
};

let map (f: 'a => 'b) (vector: vector 'a): (vector 'b) => vector
  |> reduce
    (fun acc next => acc |> TransientVector.add @@ f @@ next)
    (mutate empty)
  |> TransientVector.persist;

let mapReverse (f: 'a => 'b) (vector: vector 'a): (vector 'b) => vector
  |> reduceRight
    (fun acc next => acc |> TransientVector.add @@ f @@ next)
    (mutate empty)
  |> TransientVector.persist;

let reverse (vector: vector 'a): (vector 'a) => vector
  |> reduceRight
    (fun acc next => acc |> TransientVector.add next)
    (mutate empty)
  |> TransientVector.persist;

let skip (skipCount: int) ({ left, middle, right } as vec: vector 'a): (vector 'a) => {
  let vectorCount = count vec;
  let leftCount = CopyOnWriteArray.count left;
  let middleCount = Trie.count middle;

  skipCount >= vectorCount ? empty :

  skipCount < leftCount ? {
    left: left |> CopyOnWriteArray.skip skipCount,
    middle,
    right,
  } :

  skipCount == leftCount ? {
    let (Leaf _ left, middle) = Trie.removeFirstLeafWithMutator updateLevelPersistent None middle;
    { left, middle, right }
  } :

  skipCount - leftCount < middleCount ? {
    let skipCount = skipCount - leftCount;
    let (left, middle) = Trie.skip None skipCount middle;
    { left, middle, right }
  } :

  {
    let skipCount = skipCount - leftCount - middleCount;
    {
      left:  right |> CopyOnWriteArray.skip skipCount,
      middle: Trie.empty,
      right: [||],
    }
  }
};

let take (takeCount: int) ({ left, middle, right } as vec: vector 'a): (vector 'a) => {
  let vectorCount = count vec;
  let leftCount = CopyOnWriteArray.count left;
  let middleCount = Trie.count middle;

  takeCount >= vectorCount ? vec :
  takeCount <= leftCount ? {
    left: left |> CopyOnWriteArray.take takeCount,
    middle: Trie.empty,
    right: [||],
  } :

  takeCount - leftCount < middleCount ? {
    let takeCount = takeCount - leftCount;
    let (middle, right) = Trie.take None takeCount middle;
    { left, middle, right }
  } :

  takeCount - leftCount == middleCount ? {
    let (middle, Leaf _ right) = Trie.removeLastLeafWithMutator updateLevelPersistent None middle;
    { left, middle, right }
  } :

  {
    let takeCount = takeCount - leftCount - middleCount;
    { left, middle, right: right |> CopyOnWriteArray.take takeCount }
  }
};

let toSeq ({ left, middle, right }: vector 'a): (seq 'a) => Seq.concat [
  CopyOnWriteArray.toSeq left,
  Trie.toSeq middle,
  CopyOnWriteArray.toSeq right,
];

let toSeqReversed ({ left, middle, right }: vector 'a): (seq 'a) => Seq.concat [
  CopyOnWriteArray.toSeqReversed right,
  Trie.toSeqReversed middle,
  CopyOnWriteArray.toSeqReversed left,
];

let toIndexed (vector: vector 'a): (indexed 'a) => Indexed.create
  count::(count vector)
  rseq::(toSeqReversed vector)
  seq::(toSeq vector)
  tryGet::(fun i => vector |> tryGet i);

let tryFind (f: 'a => bool) ({ left, middle, right }: vector 'a): (option 'a) =>
  /* FIXME: Add an operator to Option for this use case */
  switch (left |> CopyOnWriteArray.tryFind f) {
    | Some _ as v => v
    | _ => switch (middle |> Trie.tryFind f) {
      | Some _ as v => v
      | _ => right |> CopyOnWriteArray.tryFind f
    }
  };
