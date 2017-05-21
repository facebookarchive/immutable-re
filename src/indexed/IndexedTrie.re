/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'a =
  | Empty
  | Leaf Transient.Owner.t (array 'a)
  | Level int (ref int) Transient.Owner.t (array (t 'a));

let count (trie: t 'a): int => switch trie {
  | Empty => 0
  | Leaf _ values => CopyOnWriteArray.count values;
  | Level _ count _ _ => !count;
};

let reduce
    triePredicate::(triePredicate: 'acc => t 'a => bool)
    trieReducer::(trieReducer: 'acc => t 'a => 'acc)
    while_::(predicate: 'acc => 'a => bool)
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (trie: t 'a): 'acc => switch trie {
  | Empty => acc
  | Leaf _ values =>
      values |> CopyOnWriteArray.reduce while_::predicate f acc
  | Level _ _ _ nodes =>
      nodes |> CopyOnWriteArray.reduce while_::triePredicate trieReducer acc
};

let reduceReversed
    triePredicate::(triePredicate: 'acc => t 'a => bool)
    trieReducer::(trieReducer: 'acc => t 'a => 'acc)
    while_::(predicate: 'acc => 'a => bool)
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (trie: t 'a): 'acc => switch trie {
  | Empty => acc
  | Leaf _ values =>
      values |> CopyOnWriteArray.reduceReversed while_::predicate f acc
  | Level _ _ _ nodes =>
      nodes |> CopyOnWriteArray.reduceReversed while_::triePredicate trieReducer acc
};

let rec toSequence (trie: t 'a): (Sequence.t 'a) => switch trie {
  | Empty => Sequence.empty ()
  | Leaf _ values => values |> CopyOnWriteArray.toSequence
  | Level _ _ _ nodes => nodes |> CopyOnWriteArray.toSequence |> Sequence.flatMap toSequence
};

let rec toSequenceReversed (trie: t 'a): (Sequence.t 'a) => switch trie {
  | Empty => Sequence.empty ()
  | Leaf _ values => values |> CopyOnWriteArray.toSequenceReversed
  | Level _ _ _ nodes => nodes |> CopyOnWriteArray.toSequenceReversed |> Sequence.flatMap toSequenceReversed
};

let depth (trie: t 'a): int => switch trie {
  | Empty => failwith "invalid state"
  | Leaf _ _ => 0
  | Level depth _ _ _ => depth
};

let bits = 5;
let width = 1 lsl 5;

let depthCapacity (depth: int): int =>
  width lsl (depth * bits);

let capacity (trie: t 'a): int =>
  depthCapacity (depth trie);

let childCapacity (trie: t 'a): int =>
  depthCapacity (depth trie - 1);

let empty = Empty;

let isEmpty (trie: t 'a) =>
  (count trie) === 0;

let module Mutator = {
  type level 'a =
    owner::Transient.Owner.t =>
    levelCount::int =>
    index::int =>
    child::(t 'a) =>
    (t 'a) =>
    (t 'a);

  type leaf 'a =
    owner::Transient.Owner.t =>
    index::int =>
    value::'a =>
    (t 'a) =>
    (t 'a);

  let updateLeafPersistent
      owner::(_: Transient.Owner.t)
      index::(index: int)
      value::(value: 'a)
      (Leaf _ values as trie: t 'a): (t 'a) => {
    let oldValue = values.(index);

    if (oldValue === value) trie
    else Leaf Transient.Owner.none (values |> CopyOnWriteArray.update index value);
  };

  let updateLeafTransient
      owner::(owner: Transient.Owner.t)
      index::(index: int)
      value::(value: 'a)
      (trie: t 'a): (t 'a) => switch trie {
    | Leaf trieOwner values when trieOwner === owner =>
        values.(index) = value;
        trie
    | Leaf _ values =>
        let oldValue = values.(index);

        if (oldValue === value) trie
        else Leaf Transient.Owner.none (values |> CopyOnWriteArray.update index value);
    | _ => failwith "Invalid state"
  };

  let updateLevelPersistent
      owner::(_: Transient.Owner.t)
      levelCount::(count: int)
      index::(index: int)
      child::(child: t 'a)
      (Level depth _ _ tries as trie: t 'a): (t 'a) => {
    let oldChild = tries.(index);
    if (oldChild === child) trie
    else Level depth (ref count) Transient.Owner.none (CopyOnWriteArray.update index child tries);
  };

  let updateLevelTransient
      owner::(owner: Transient.Owner.t)
      levelCount::(count: int)
      index::(index: int)
      child::(child: t 'a)
      (trie: t 'a): (t 'a) => switch trie {
    | Level _ trieCount trieOwner tries when trieOwner === owner =>
        tries.(index) = child;
        trieCount := count;
        trie
    | Level depth _ _ tries =>
        let oldChild = tries.(index);
        if (oldChild === child) trie
        else Level depth (ref count) owner (CopyOnWriteArray.update index child tries);
    | _ => failwith "Invalid state"
  };
};

let module Impl = {
  let rec addFirstLeafIfSpace
      (updateLevel: Mutator.level 'a)
      (owner: Transient.Owner.t)
      (values: array 'a)
      (trie: t 'a): (t 'a) => {
    let (Level levelDepth levelCount _ tries) = trie;

    let index = 0;
    let firstChild = tries |> CopyOnWriteArray.getOrRaise 0;
    let firstChildCount = count firstChild;
    let firstChildDepth = depth firstChild;
    let triesWidth = CopyOnWriteArray.count tries;
    let valuesCount = CopyOnWriteArray.count values;

    switch firstChild {
      | Leaf _ _ when firstChildDepth < (levelDepth - 1) =>
          let childLevelCount = firstChildCount + valuesCount;
          let child = Level 1 (ref childLevelCount) owner [| Leaf owner values, firstChild |];

          let levelCount = !levelCount - firstChildCount + childLevelCount;
          trie |> updateLevel ::owner ::levelCount ::index ::child;
      | Leaf _ _ when triesWidth < width =>
          let newLevelCount = !levelCount + valuesCount;
          Level levelDepth (ref newLevelCount) owner (tries |> CopyOnWriteArray.addFirst (Leaf owner values));
      | Level childLevelDepth _ _ _ =>
          let firstChild = firstChild |> addFirstLeafIfSpace updateLevel owner values;
          let newFirstChildCount = count firstChild;

          if (firstChildCount !== newFirstChildCount) {
            let levelCount = !levelCount - firstChildCount + newFirstChildCount;
            trie |> updateLevel ::owner ::levelCount ::index child::firstChild;
          } else if (firstChildDepth < (levelDepth - 1)) {
              let newFirstChildLevelCount = ref (firstChildCount + valuesCount);
              let child = Level (childLevelDepth + 1) newFirstChildLevelCount owner [| Leaf owner values, firstChild |];
              let levelCount = !levelCount + valuesCount;
              trie |> updateLevel ::owner ::levelCount ::index ::child;
          } else if (triesWidth < width) {
              let newLevelCount = !levelCount + valuesCount;
              Level levelDepth (ref newLevelCount) owner (tries |> CopyOnWriteArray.addFirst (Leaf owner values));
          } else trie
      | _ => trie
    }
  };

  let addFirstLeaf
      (updateLevel: Mutator.level 'a)
      (owner: Transient.Owner.t)
      (values: array 'a)
      (trie: t 'a): (t 'a) => switch trie {
    | Empty => Leaf owner values
    | Leaf _ _ =>
        let levelCount = count trie + CopyOnWriteArray.count values;
        Level 1 (ref levelCount) owner [| Leaf owner values, trie |];
    | Level levelDepth _ _ _ =>
        let prevTrieCount = count trie;
        let trie = trie |> addFirstLeafIfSpace updateLevel owner values;
        let newTrieCount = count trie;

        if (newTrieCount !== prevTrieCount) trie
        else {
          let levelCount = newTrieCount + CopyOnWriteArray.count values;
          let levelDepth = levelDepth + 1;
          Level levelDepth (ref levelCount) owner [| Leaf owner values, trie |];
        }
  };

  let rec addLastLeafIfSpace
      (updateLevel: Mutator.level 'a)
      (owner: Transient.Owner.t)
      (values: array 'a)
      (trie: t 'a): (t 'a) => {
    let (Level levelDepth levelCount _ tries) = trie;

    let index = tries |> CopyOnWriteArray.lastIndexOrRaise;
    let lastChild = tries |> CopyOnWriteArray.getOrRaise index;
    let lastChildCount = count lastChild;
    let lastChildDepth = depth lastChild;
    let triesWidth = CopyOnWriteArray.count tries;
    let valuesCount = CopyOnWriteArray.count values;

    switch lastChild {
      | Leaf _ _ when lastChildDepth < (levelDepth - 1) =>
          let newLastChildLevelCount = lastChildCount + valuesCount;
          let child = Level 1 (ref newLastChildLevelCount) owner [| lastChild, Leaf owner values |];
          let levelCount = !levelCount - lastChildCount + newLastChildLevelCount;

          trie |> updateLevel ::owner ::levelCount ::index ::child;
      | Leaf _ when triesWidth < width =>
          let newLevelCount = ref (!levelCount + valuesCount);
          Level levelDepth newLevelCount owner (tries |> CopyOnWriteArray.addLast (Leaf owner values))
      | Level childLevelDepth _ _ _ =>
          let lastChild = lastChild |> addLastLeafIfSpace updateLevel owner values;
          let newLastChildCount = count lastChild;

          if (newLastChildCount !== lastChildCount) {
            let levelCount = !levelCount - lastChildCount + newLastChildCount;
            trie |> updateLevel ::owner ::levelCount ::index child::lastChild;
          }
          else if (lastChildDepth < (levelDepth - 1)) {
            let newLastChildLevelCount = ref (lastChildCount + valuesCount);
            let child = Level (childLevelDepth + 1) newLastChildLevelCount owner [| lastChild, Leaf owner values |];
            let levelCount = !levelCount + valuesCount;
            trie |> updateLevel ::owner ::levelCount ::index ::child;
          }
          else if (triesWidth < width) {
            let newLevelCount = ref (!levelCount + valuesCount);
            Level levelDepth newLevelCount owner (tries |> CopyOnWriteArray.addLast (Leaf owner values));
          }
          else trie
      | _ => trie
    }
  };

  let addLastLeaf
      (updateLevel: Mutator.level 'a)
      (owner: Transient.Owner.t)
      (values: array 'a)
      (trie: t 'a): (t 'a) => switch trie {
    | Empty =>
        Leaf owner values
    | Leaf _ _ =>
        let levelCount = count trie + CopyOnWriteArray.count values;
        Level 1 (ref levelCount) owner [| trie, Leaf owner values |];
    | Level levelDepth _ _ _  =>
        let prevTrieCount = count trie;
        let trie = trie |> addLastLeafIfSpace updateLevel owner values;
        let newTrieCount = count trie;

        if (newTrieCount !== prevTrieCount) trie
        else {
          let levelCount = newTrieCount + CopyOnWriteArray.count values;
          let levelDepth = levelDepth + 1;
          Level levelDepth (ref levelCount) owner [| trie, Leaf owner values |];
        };
  };

  let canRadixSearch (depth: int) (count: int) (tries: array (t 'a)): bool => {
    let childCapacity = depthCapacity (depth - 1);
    let triesCount = CopyOnWriteArray.count tries;
    count === (triesCount * childCapacity)
  };

  let computeIndexUsingRadixSearch (depth: int) (index: int): int => {
    let mask = width - 1;
    let level = depth * bits;
    (index lsr level) land mask;
  };

  let rec getUsingRadixSearch (index: int) (trie: t 'a): 'a => switch trie {
    | Empty => failwith "empty"
    | Leaf _ values =>
        let valuesIndex = computeIndexUsingRadixSearch 0 index;
        values.(valuesIndex)
    | Level depth _ _ tries =>
        let triesIndex = computeIndexUsingRadixSearch depth index;
        getUsingRadixSearch index (tries.(triesIndex));
  };

  let rec get (index: int) (trie: t 'a): 'a => switch trie {
    | Level depth levelCount _ tries when (canRadixSearch depth !levelCount tries |> not) =>
        let rec loop index childIndex => {
          let childNode = tries.(childIndex);
          let childCount = count childNode;

          if (index >= childCount) (loop (index - childCount) (childIndex + 1))
          else {
            let trie = tries.(childIndex);
            get index trie
          };
        };

        loop index 0;
    | _ => getUsingRadixSearch index trie
  };

  let rec removeFirstLeaf
      (updateLevel: Mutator.level 'a)
      (owner: Transient.Owner.t)
      (firstLeaf: ref (t 'a))
      (trie: t 'a): t 'a => switch trie {
    | Leaf _ _ =>
        firstLeaf := trie;
        Empty;
    | Level levelDepth levelCount _ tries when levelDepth > 1 =>
        let triesWidth = CopyOnWriteArray.count tries;
        let firstChildIndex = 0;
        let lastIndex = tries |> CopyOnWriteArray.lastIndexOrRaise;
        let firstChild = tries |> CopyOnWriteArray.getOrRaise 0;
        let newFirstChild = firstChild |> removeFirstLeaf updateLevel owner firstLeaf;

        switch newFirstChild {
          | Empty when triesWidth > 2 =>
              let newLevelCount = !levelCount - (count !firstLeaf);
              Level levelDepth (ref newLevelCount) owner (CopyOnWriteArray.removeFirstOrRaise tries);
          | Empty =>
              CopyOnWriteArray.getOrRaise lastIndex tries
          | _ =>
              let newLevelCount = !levelCount - (count !firstLeaf);
              updateLevel ::owner levelCount::newLevelCount index::firstChildIndex child::newFirstChild trie;
        };
    | Level 1 levelCount _ tries =>
        let triesWidth = CopyOnWriteArray.count tries;
        firstLeaf := tries |> CopyOnWriteArray.getOrRaise 0;
        let lastIndex = tries |> CopyOnWriteArray.lastIndexOrRaise;

        if (triesWidth > 2) {
          let newLevelCount = !levelCount - (count !firstLeaf);
          Level 1 (ref newLevelCount) owner (CopyOnWriteArray.removeFirstOrRaise tries)
        }
        else CopyOnWriteArray.getOrRaise lastIndex tries;
    | _ => failwith "invalid state"
  };

  let rec removeLastLeaf
      (updateLevel: Mutator.level 'a)
      (owner: Transient.Owner.t)
      (lastLeaf: ref (t 'a))
      (trie: t 'a): t 'a => switch trie {
    | Leaf _ _ =>
        lastLeaf := trie;
        Empty;
    | Level levelDepth levelCount _ tries when levelDepth > 1 =>
        let triesWidth = CopyOnWriteArray.count tries;
        let lastChildIndex = tries |> CopyOnWriteArray.lastIndexOrRaise;
        let lastChild = tries |> CopyOnWriteArray.getOrRaise lastChildIndex;
        let newLastChild = lastChild |> removeLastLeaf updateLevel owner lastLeaf;

        switch newLastChild {
          | Empty when triesWidth > 2 =>
              let newLevelCount = !levelCount - (count !lastLeaf);
              Level levelDepth (ref newLevelCount) owner (CopyOnWriteArray.removeLastOrRaise tries);
          | Empty =>
              CopyOnWriteArray.getOrRaise 0 tries
          | _ =>
              let newLevelCount = !levelCount - (count !lastLeaf);
              updateLevel ::owner levelCount::newLevelCount index::lastChildIndex child::newLastChild trie;
        };
    | Level 1 levelCount _ tries =>
        let triesWidth = CopyOnWriteArray.count tries;
        let lastChildIndex = tries |> CopyOnWriteArray.lastIndexOrRaise;
        lastLeaf := tries |> CopyOnWriteArray.getOrRaise lastChildIndex;

        if (triesWidth > 2) {
          let newLevelCount = !levelCount - (count !lastLeaf);
          Level 1 (ref newLevelCount) owner (CopyOnWriteArray.removeLastOrRaise tries)
        }
        else CopyOnWriteArray.getOrRaise 0 tries;
    | _ => failwith "invalid state"
  };

  let rec updateUsingRadixSearch
      (updateLevel: Mutator.level 'a)
      (updateLeaf: Mutator.leaf 'a)
      (owner: Transient.Owner.t)
      (index: int)
      (value: 'a)
      (trie: t 'a): (t 'a) => switch trie {
    | Empty => failwith "empty"
    | Leaf _ values =>
        let valuesIndex = computeIndexUsingRadixSearch 0 index;
        trie |> updateLeaf ::owner index::valuesIndex ::value;
    | Level depth count _ tries =>
        let childIndex = computeIndexUsingRadixSearch depth index;
        let childNode = tries |> CopyOnWriteArray.getOrRaise childIndex;
        let newChildNode = childNode |> updateUsingRadixSearch updateLevel updateLeaf owner index value;
        updateLevel ::owner levelCount::!count index::childIndex child::newChildNode trie;
  };

  let rec update
      (updateLevel: Mutator.level 'a)
      (updateLeaf: Mutator.leaf 'a)
      (owner: Transient.Owner.t)
      (index: int)
      (value: 'a)
      (trie: t 'a): (t 'a) => switch trie {
    | Level depth levelCount _ tries when (canRadixSearch depth !levelCount tries |> not) =>
        let rec loop index childIndex => {
          let childNode = tries.(childIndex);
          let childCount = count childNode;

          if (index >= childCount) (loop (index - childCount) (childIndex + 1))
          else {
            let childNode = tries |> CopyOnWriteArray.getOrRaise childIndex;
            let newChildNode = childNode |> update updateLevel updateLeaf owner index value;
            updateLevel ::owner levelCount::!levelCount index::childIndex child::newChildNode trie;
          };
        };

        loop index 0;
    | _ => updateUsingRadixSearch updateLevel updateLeaf owner index value trie
  };

  let rec updateAll
      (updateLevel: Mutator.level 'a)
      (updateLeaf: Mutator.leaf 'a)
      (owner: Transient.Owner.t)
      (f: 'a => 'a)
      (trie: t 'a): (t 'a) => switch trie {
    | Empty => Empty
    | Leaf _ values =>
        let valuesCount = CopyOnWriteArray.count values;
        let rec loop index trie =>
          if (index < valuesCount) {
            let newValue = f values.(index);
            let newTrie = trie |> updateLeaf ::owner ::index value::newValue;
            loop (index + 1) newTrie;
          }
          else trie;

        loop 0 trie;
    | Level _ count _ tries =>
        let triesCount = CopyOnWriteArray.count tries;

        let rec loop index trie =>
          if (index < triesCount) {
            let childNode = tries.(index);
            let newChildNode = childNode |> updateAll updateLevel updateLeaf owner f;
            let newTrie =
              if (childNode === newChildNode) trie
              else (updateLevel ::owner levelCount::!count ::index child::newChildNode trie);
            loop (index + 1) newTrie;
          }
          else trie;

        loop 0 trie;
  };

  let rec updateWithUsingRadixSearch
      (updateLevel: Mutator.level 'a)
      (updateLeaf: Mutator.leaf 'a)
      (owner: Transient.Owner.t)
      (index: int)
      (f: 'a => 'a)
      (trie: t 'a): (t 'a) => switch trie {
    | Empty => failwith "empty"
    | Leaf _ values =>
        let valuesIndex = computeIndexUsingRadixSearch 0 index;
        let newValue = f values.(valuesIndex);
        trie |> updateLeaf ::owner index::valuesIndex value::newValue;
    | Level depth count _ tries =>
        let childIndex = computeIndexUsingRadixSearch depth index;
        let childNode = tries |> CopyOnWriteArray.getOrRaise childIndex;
        let newChildNode = childNode |> updateWithUsingRadixSearch updateLevel updateLeaf owner index f;
        updateLevel ::owner levelCount::!count index::childIndex child::newChildNode trie;
  };

  let rec updateWith
      (updateLevel: Mutator.level 'a)
      (updateLeaf: Mutator.leaf 'a)
      (owner: Transient.Owner.t)
      (index: int)
      (f: 'a => 'a)
      (trie: t 'a): (t 'a) => switch trie {
    | Level depth levelCount _ tries when (canRadixSearch depth !levelCount tries |> not) =>
        let rec loop index childIndex => {
          let childNode = tries.(childIndex);
          let childCount = count childNode;

          if (index >= childCount) (loop (index - childCount) (childIndex + 1))
          else {
            let childNode = tries |> CopyOnWriteArray.getOrRaise childIndex;
            let newChildNode = childNode |> updateWith updateLevel updateLeaf owner index f;
            updateLevel ::owner levelCount::!levelCount index::childIndex child::newChildNode trie;
          };
        };

        loop index 0;
    | _ => updateWithUsingRadixSearch updateLevel updateLeaf owner index f trie
  };
};

type levelMutator 'a = Transient.Owner.t => int => int => (t 'a) => (t 'a) => (t 'a);
type leafMutator 'a = Transient.Owner.t => int => 'a => (t 'a) => (t 'a);

let computeIndexUsingRadixSearch (depth: int) (index: int): int => {
  let mask = width - 1;
  let level = depth * bits;
  (index lsr level) land mask;
};

let canRadixSearch (trie: t 'a): bool => switch trie {
  | Empty => failwith "invalid state"
  | Leaf _ _ => true
  | Level depth count _ tries =>
      let childCapacity = depthCapacity (depth - 1);
      let triesCount = CopyOnWriteArray.count tries;
      !count === (triesCount * childCapacity)
};

type leveIndexContinuation 'a 'b =
  (/* parent */ t 'a) => (/* effective index */ int => (/* childIndex */ int) => 'b);

type computeLevelIndex 'a 'b = int => (leveIndexContinuation 'a 'b) => (t 'a) => ('b);

let computeLevelIndexUsingRadixSearch
    (index: int)
    (f: leveIndexContinuation 'a 'b)
    (Level depth _ _ _ as trie: t 'a): 'b => {
  let childIndex = computeIndexUsingRadixSearch depth index;
  f trie index childIndex;
};

let computeLevelIndexUsingCountSearch
    (index: int)
    (f: leveIndexContinuation 'a 'b)
    (Level _ _ _ tries as trie: t 'a): 'b => {
  let rec loop index childIndex => {
    let childNode = tries.(childIndex);
    let childCount = count childNode;

    if (index < childCount) (f trie index childIndex)
    else loop (index - childCount) (childIndex + 1);
  };

  loop index 0;
};

type skip 'a = Transient.Owner.t => int => (t 'a) => (array 'a, t 'a);

let skipImpl
    (computeLevelIndex: computeLevelIndex 'a (array 'a, t 'a))
    (skip: skip 'a)
    (owner: Transient.Owner.t)
    (skipCount: int)
    (trie: t 'a): (array 'a, t 'a) => switch trie {
  | Leaf _ nodes =>
    let skipCount = computeIndexUsingRadixSearch 0 (skipCount - 1) + 1;
    let result = CopyOnWriteArray.skip skipCount nodes;
    (result, empty)
  | Level _ _ _ _ =>
    trie |> computeLevelIndex (skipCount - 1) (fun (Level depth _ _ tries) effectiveIndex childIndex => {
      let childNode = tries.(childIndex);
      let lastChildIndex = tries |> CopyOnWriteArray.lastIndexOrRaise;
      let (tail, newChildNode) as childResult = childNode |> skip owner (effectiveIndex + 1);

      let triesLastIndex = CopyOnWriteArray.lastIndexOrRaise tries;

      switch newChildNode {
        | Empty when childIndex === triesLastIndex =>
            childResult
        | Empty when childIndex === (triesLastIndex - 1) =>
            (tail, tries |> CopyOnWriteArray.getOrRaise lastChildIndex)
        | Empty =>
            let newTries = tries |> CopyOnWriteArray.skip (childIndex + 1);
            let levelCount = newTries |> CopyOnWriteArray.reduce while_::Functions.alwaysTrue2 (fun acc next => acc + (count next)) 0;
            (tail, Level depth (ref levelCount) owner newTries)
        | Leaf _ _ =>
            let newTries = tries |> CopyOnWriteArray.skip childIndex;
            newTries.(0) = newChildNode;
            let levelCount = newTries |> CopyOnWriteArray.reduce while_::Functions.alwaysTrue2 (fun acc next => acc + (count next)) 0;
            (tail, Level depth (ref levelCount) owner newTries)
        | Level d1 _ _ _ =>
            let newTries = tries |> CopyOnWriteArray.skip childIndex;
            newTries.(0) = newChildNode;
            let levelCount = newTries |> CopyOnWriteArray.reduce while_::Functions.alwaysTrue2 (fun acc next => acc + (count next)) 0;
            (tail, Level depth (ref levelCount) owner newTries)
      }
    });
  | Empty => failwith "invalid state"
};

let rec skipUsingRadixSearch
    (owner: Transient.Owner.t)
    (skipCount: int)
    (trie: t 'a): (array 'a, t 'a) => skipImpl
  computeLevelIndexUsingRadixSearch
  skipUsingRadixSearch
  owner
  skipCount
  trie;

let rec skip
    (owner: Transient.Owner.t)
    (count: int)
    (trie: t 'a): (array 'a, t 'a) => switch trie {
  | Level _ _ _ _ when not @@ canRadixSearch @@ trie => skipImpl
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

type take 'a = Transient.Owner.t => int => (t 'a) => (t 'a, array 'a);

let takeImpl
    (computeLevelIndex: computeLevelIndex 'a (t 'a, array 'a))
    (take: take 'a)
    (owner: Transient.Owner.t)
    (takeCount: int)
    (trie: t 'a): (t 'a, array 'a) => switch trie {
  | Leaf _ nodes =>
    let takeCount = computeIndexUsingRadixSearch 0 (takeCount - 1) + 1;
    let result = CopyOnWriteArray.take takeCount nodes;
    (empty, result)
  | Level _ _ _ _ =>
    trie |> computeLevelIndex (takeCount - 1) (fun (Level depth _ _ tries) effectiveIndex childIndex => {
      let childNode = tries.(childIndex);
      let (newChildNode, tail) as childResult = childNode |> take owner (effectiveIndex + 1);

      switch newChildNode {
        | Empty when childIndex === 0 => childResult
        | Empty when childIndex === 1 => (tries.(0), tail)
        | Empty =>
          let newTries = tries |> CopyOnWriteArray.take childIndex;
          let levelCount = newTries |> CopyOnWriteArray.reduce while_::Functions.alwaysTrue2 (fun acc next => acc + (count next)) 0;
          (Level depth (ref levelCount) owner newTries, tail)
        | _ =>
          let newTries = tries |> CopyOnWriteArray.take (childIndex + 1);
          newTries.(childIndex) = newChildNode;
          let levelCount = newTries |> CopyOnWriteArray.reduce while_::Functions.alwaysTrue2 (fun acc next => acc + (count next)) 0;
          (Level depth (ref levelCount) owner newTries, tail)
      };
    });
  | Empty => failwith "invalid state"
};

let rec takeUsingRadixSearch
    (owner: Transient.Owner.t)
    (takeCount: int)
    (trie: t 'a): (t 'a, array 'a) => takeImpl
  computeLevelIndexUsingRadixSearch
  takeUsingRadixSearch
  owner
  takeCount
  trie;

let rec take
    (owner: Transient.Owner.t)
    (count: int)
    (trie: t 'a): (t 'a, array 'a) => switch trie {
 | Level _ _ _ _ when not @@ canRadixSearch @@ trie => takeImpl
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

let updateLevelPersistent
    (_: Transient.Owner.t)
    (count: int)
    (index: int)
    (child: t 'a)
    (Level depth _ _ tries: t 'a): (t 'a) =>
  Level depth (ref count) Transient.Owner.none (CopyOnWriteArray.update index child tries);

let updateLeafPersistent
    (_: Transient.Owner.t)
    (index: int)
    (value: 'a)
    (Leaf _ values: t 'a): (t 'a) =>
  Leaf Transient.Owner.none (values |> CopyOnWriteArray.update index value);

let updateLevelTransient
    (owner: Transient.Owner.t)
    (count: int)
    (index: int)
    (child: t 'a)
    (trie: t 'a): (t 'a) => switch trie {
  | Level _ trieCount trieOwner tries when trieOwner === owner =>
      tries.(index) = child;
      trieCount := count;
      trie
  | Level depth _ _ tries =>
      Level depth (ref count) owner (CopyOnWriteArray.update index child tries)
  | _ => failwith "Invalid state"
};

let updateLeafTransient
    (owner: Transient.Owner.t)
    (index: int)
    (value: 'a)
    (trie: t 'a): (t 'a) => switch trie {
  | Leaf trieOwner values when trieOwner === owner =>
      values.(index) = value;
      trie
  | Leaf _ values =>
      Leaf owner (values |> CopyOnWriteArray.update index value)
  | _ => failwith "Invalid state"
};
