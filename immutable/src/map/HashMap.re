/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

let module BitmapTrieMap = {
  type t 'k 'v =
    | Level int32 (array (t 'k 'v)) Transient.Owner.t
    | ComparatorCollision int (AVLTreeMap.t 'k 'v)
    | EqualityCollision int (EqualityMap.t 'k 'v)
    | Entry int 'k 'v
    | Empty;

  type updateLevelNode 'k 'v = Transient.Owner.t => int => (t 'k 'v) => (t 'k 'v) => (t 'k 'v);

  type alterResult =
    | Added
    | NoChange
    | Removed
    | Replace;

  let rec alter
      (hashStrategy: HashStrategy.t 'k)
      (updateLevelNode: updateLevelNode 'k 'v)
      (owner: Transient.Owner.t)
      (alterResult: ref alterResult)
      (depth: int)
      (hash: int)
      (key: 'k)
      (f: option 'v => option 'v)
      (map: t 'k 'v): (t 'k 'v) => switch map {
    | Entry entryHash entryKey entryValue when (hash == entryHash) =>
        if (HashStrategy.equals hashStrategy key entryKey) (
          switch (f @@ Option.return @@ entryValue) {
            | Some newEntryValue when newEntryValue === entryValue =>
                alterResult := NoChange;
                map;
            | Some newEntryValue =>
                alterResult := Replace;
                Entry entryHash key newEntryValue;
            | None =>
                alterResult := Removed;
                Empty
          })
        else (switch (f None) {
          | Some value => switch hashStrategy {
              | HashStrategy.Comparator _ comparator =>
                  let map = AVLTreeMap.Empty
                    |> AVLTreeMap.put comparator entryKey entryValue
                    |> AVLTreeMap.put comparator key value;
                  alterResult := Added;
                  ComparatorCollision entryHash map;
              | HashStrategy.Equality _ equals =>
                  /* FIXME: Could improve this to avoid collisions. */
                  let map = EqualityMap.empty
                    |> EqualityMap.put equals entryKey entryValue
                    |> EqualityMap.put equals key value;
                  alterResult := Added;
                  EqualityCollision entryHash map;
            }
          | None =>
              alterResult := NoChange;
              map;
        });
    | Entry entryHash _ _ => switch (f None) {
        | Some newEntryValue =>
            let bitmap = BitmapTrie.bitPos entryHash depth;
            Level bitmap [| map |] owner
              |> alter hashStrategy updateLevelNode owner alterResult depth hash key (Functions.return @@ Option.return @@ newEntryValue)
        | _ =>
            alterResult := NoChange;
            map
      }
    | EqualityCollision entryHash entryMap =>
        let entryMapCount = EqualityMap.count entryMap;
        let newEntryMap = entryMap |> EqualityMap.alter (HashStrategy.equals hashStrategy) key f;
        let newEntryMapCount = EqualityMap.count newEntryMap;

        if (newEntryMap === entryMap) {
            alterResult := NoChange;
            map
        } else if (entryMapCount == newEntryMapCount) {
          alterResult := Replace;
          EqualityCollision entryHash newEntryMap;
        } else if (newEntryMapCount > entryMapCount) {
          alterResult := Added;
          EqualityCollision entryHash newEntryMap;
        } else if (entryMapCount == 1) {
          alterResult := Removed;
          let (entrykey, entryValue) = newEntryMap |> EqualityMap.firstOrRaise;
          Entry entryHash entrykey entryValue;
        } else {
          alterResult := Removed;
          EqualityCollision entryHash newEntryMap;
        };
    | ComparatorCollision entryHash entryMap =>
        let collisionResult = ref AVLTreeMap.NoChange;
        let newEntryMap = entryMap |> AVLTreeMap.alter (HashStrategy.comparator hashStrategy) collisionResult key f;

        switch !collisionResult {
          | AVLTreeMap.Added =>
              alterResult := Added;
              ComparatorCollision entryHash newEntryMap;
          | AVLTreeMap.NoChange =>
              alterResult := NoChange;
              map
          | AVLTreeMap.Replace =>
              alterResult := Replace;
              ComparatorCollision entryHash newEntryMap;
          | AVLTreeMap.Removed => switch newEntryMap {
              | AVLTreeMap.Leaf k v =>
                  alterResult := Removed;
                  Entry entryHash k v;
              | _ =>
                alterResult := Removed;
                ComparatorCollision entryHash newEntryMap;
            }
        };
    | Level bitmap nodes _ =>
        let bit = BitmapTrie.bitPos hash depth;
        let index = BitmapTrie.index bitmap bit;

        if (BitmapTrie.containsNode bitmap bit) {
          let childNode = nodes.(index);
          let newChildNode = childNode |> alter hashStrategy updateLevelNode owner alterResult (depth + 1) hash key f;

          switch !alterResult {
            | Added => map |> updateLevelNode owner index newChildNode
            | NoChange => map
            | Removed =>
                if (newChildNode === Empty) {
                  let nodes = nodes |> CopyOnWriteArray.removeAt index;
                  if (CopyOnWriteArray.count nodes > 0) (Level (Int32.logxor bitmap bit) nodes owner)
                  else Empty
                } else (updateLevelNode owner index newChildNode map)
            | Replace => map |> updateLevelNode owner index newChildNode
          }
        } else (switch (f None) {
          | Some newEntryValue =>
              alterResult := Added;
              let node = Entry hash key newEntryValue;
              let nodes = nodes |> CopyOnWriteArray.insertAt index node;
              Level (Int32.logor bitmap bit) nodes owner
          | None =>
              alterResult := NoChange;
              map
        })
    | Empty => switch (f None) {
        | None =>
            alterResult := NoChange;
            map;
        | Some v =>
            alterResult := Added;
            Entry hash key v;
      }
  };

  let updateLevelNodePersistent
      (_: Transient.Owner.t)
      (index: int)
      (childNode: t 'k 'v)
      (Level bitmap nodes _: (t 'k 'v)): (t 'k 'v) =>
    Level bitmap (CopyOnWriteArray.update index childNode nodes) Transient.Owner.none;

  let updateLevelNodeTransient
      (owner: Transient.Owner.t)
      (index: int)
      (childNode: t 'k 'v)
      (Level bitmap nodes nodeOwner as node: (t 'k 'v)): (t 'k 'v) =>
    if (nodeOwner === owner) {
      nodes.(index) = childNode;
      node
    }
    else Level bitmap (CopyOnWriteArray.update index childNode nodes) owner;

  let rec containsKey
      (hashStrategy: HashStrategy.t 'k)
      (depth: int)
      (hash: int)
      (key: 'k)
      (set: t 'k 'v): bool => switch set {
    | Level bitmap nodes _ =>
        let bit = BitmapTrie.bitPos hash depth;
        let index = BitmapTrie.index bitmap bit;

        (BitmapTrie.containsNode bitmap bit) &&
        (containsKey hashStrategy (depth + 1) hash key nodes.(index));
    | EqualityCollision entryHash entryMap =>
        (hash == entryHash) && (EqualityMap.containsKey (HashStrategy.equals hashStrategy) key entryMap);
    | ComparatorCollision entryHash entryMap =>
        (hash == entryHash) && (AVLTreeMap.containsKey (HashStrategy.comparator hashStrategy) key entryMap);
    | Entry entryHash entryKey _ =>
        (hash == entryHash) && (HashStrategy.equals hashStrategy entryKey key);
    | Empty => false;
  };

  let rec get
      (hashStrategy: HashStrategy.t 'k)
      (depth: int)
      (hash: int)
      (key: 'k)
      (map: t 'k 'v): (option 'v) => switch map {
    | Level bitmap nodes _ =>
        let bit = BitmapTrie.bitPos hash depth;
        let index = BitmapTrie.index bitmap bit;

        if (BitmapTrie.containsNode bitmap bit) (get hashStrategy (depth + 1) hash key nodes.(index))
        else None
    | EqualityCollision entryHash entryMap =>
        if (hash == entryHash) (EqualityMap.get (HashStrategy.equals hashStrategy) key entryMap)
        else None
    | ComparatorCollision entryHash entryMap =>
        if (hash == entryHash) (AVLTreeMap.get (HashStrategy.comparator hashStrategy) key entryMap)
        else None
    | Entry entryHash entryKey entryValue =>
        if ((hash == entryHash) && (HashStrategy.equals hashStrategy entryKey key)) {
          Some entryValue
        } else None
    | Empty => None;
  };

  let rec getOrRaise
      (hashStrategy: HashStrategy.t 'k)
      (depth: int)
      (hash: int)
      (key: 'k)
      (map: t 'k 'v): 'v => switch map {
    | Level bitmap nodes _ =>
        let bit = BitmapTrie.bitPos hash depth;
        let index = BitmapTrie.index bitmap bit;

        if (BitmapTrie.containsNode bitmap bit) (getOrRaise hashStrategy (depth + 1) hash key nodes.(index))
        else (failwith "NotFound")
    | EqualityCollision entryHash entryMap =>
        if (hash == entryHash) (
          EqualityMap.getOrRaise (HashStrategy.equals hashStrategy) key entryMap
        )
        else (failwith "NotFound")
    | ComparatorCollision entryHash entryMap =>
        if (hash == entryHash) (AVLTreeMap.getOrRaise (HashStrategy.comparator hashStrategy) key entryMap)
        else (failwith "NotFound")
    | Entry entryHash entryKey entryValue =>
        if ((hash == entryHash) && (HashStrategy.equals hashStrategy entryKey key)) {
          entryValue
        } else (failwith "NotFound")
    | Empty => failwith "NotFound";
  };

  let rec reduce (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) (map: t 'k 'v): 'acc => switch map {
    | Level _ nodes _ =>
        let reducer acc node => node |> reduce f acc;
        nodes |> CopyOnWriteArray.reduce reducer acc
    | ComparatorCollision _ entryMap =>
        entryMap |> AVLTreeMap.reduce f acc
    | EqualityCollision _ entryMap =>
        entryMap |> EqualityMap.reduce f acc
    | Entry _ entryKey entryValue =>
        f acc entryKey entryValue
    | Empty => acc
  };

  let rec reduceWhileWithResult
      (shouldContinue: ref bool)
      (predicate: 'acc => 'k => 'v => bool)
      (f: 'acc => 'k => 'v => 'acc)
      (acc: 'acc)
      (map: t 'k 'v): 'acc => switch map {
    | Level _ nodes _ =>
        let reducer acc node => node
          |> reduceWhileWithResult shouldContinue predicate f acc;

        let predicate _ _ => !shouldContinue;

        nodes |> CopyOnWriteArray.reduceWhile predicate reducer acc
    | ComparatorCollision _ entryMap =>
        entryMap |> AVLTreeMap.reduceWhileWithResult shouldContinue predicate f acc
    | EqualityCollision _ entryMap =>
        entryMap |> EqualityMap.reduceWhile predicate f acc
    | Entry _ entryKey entryValue =>
        if (!shouldContinue && (predicate acc entryKey entryValue)) (f acc entryKey entryValue)
        else acc
    | Empty => acc
  };

  let reduceWhile
      (predicate: 'acc => 'k => 'v => bool)
      (f: 'acc => 'k => 'v => 'acc)
      (acc: 'acc)
      (map: t 'k 'v): 'acc => {
    let shouldContinue = ref true;
    let predicate acc k v => {
      let result = predicate acc k v;
      shouldContinue := result;
      result;
    };

    reduceWhileWithResult shouldContinue predicate f acc map;
  };

  let rec toSequence (map: t 'k 'v): (Sequence.t ('k, 'v)) => switch map {
    | Level _ nodes _ => nodes |> CopyOnWriteArray.toSequence |> Sequence.flatMap toSequence
    | ComparatorCollision _ entryMap => AVLTreeMap.toSequence entryMap
    | EqualityCollision _ entryMap => EqualityMap.toSequence entryMap;
    | Entry _ entryKey entryValue => Sequence.return (entryKey, entryValue);
    | Empty => Sequence.empty;
  };

  let rec values (map: t 'k 'v): (Iterator.t 'v) => switch map {
    | Level _ nodes _ => nodes |> CopyOnWriteArray.toIterator |> Iterator.flatMap values
    | ComparatorCollision _ entryMap => AVLTreeMap.values entryMap
    | EqualityCollision _ entryMap => EqualityMap.values entryMap;
    | Entry _ _ entryValue => Iterator.return entryValue;
    | Empty => Iterator.empty;
  };
};

type t 'k 'v = {
  count: int,
  root: BitmapTrieMap.t 'k 'v,
  strategy: HashStrategy.t 'k,
};

let emptyInstance = {
  count: 0,
  root: BitmapTrieMap.Empty,
  strategy: HashStrategy.structuralCompare,
};

let empty (): (t 'k 'v) => emptyInstance;

let emptyWith (strategy: HashStrategy.t 'k): (t 'k 'v) => {
  count: 0,
  root: BitmapTrieMap.Empty,
  strategy,
};

let alter
    (key: 'k)
    (f: option 'v => option 'v)
    ({ count, root, strategy } as map: t 'k 'v): (t 'k 'v) => {
  let hash = HashStrategy.hash strategy key;
  let alterResult = ref BitmapTrieMap.NoChange;
  let newRoot = root |> BitmapTrieMap.alter
    strategy
    BitmapTrieMap.updateLevelNodePersistent
    Transient.Owner.none
    alterResult
    0
    hash
    key
    f;

  switch !alterResult {
    | BitmapTrieMap.Added => { count: count + 1, root: newRoot, strategy }
    | BitmapTrieMap.NoChange => map
    | BitmapTrieMap.Replace => { count, root: newRoot, strategy }
    | BitmapTrieMap.Removed => { count: count - 1, root: newRoot, strategy }
  };
};

let containsKey (key: 'k) ({ root, strategy }: t 'k 'v): bool => {
  let hash = HashStrategy.hash strategy key;
  root |> BitmapTrieMap.containsKey strategy 0 hash key;
};

let count ({ count }: t 'k 'v): int => count;

let get (key: 'k) ({ strategy, root }: t 'k 'v): (option 'v) => {
  let hash = HashStrategy.hash strategy key;
  root |> BitmapTrieMap.get strategy 0 hash key;
};

let getOrRaise (key: 'k) ({ root, strategy }: t 'k 'v): 'v => {
  let hash = HashStrategy.hash strategy key;
  root |> BitmapTrieMap.getOrRaise strategy 0 hash key;
};

let isEmpty ({ count }: t 'k 'v): bool =>
  count == 0;

let isNotEmpty ({ count }: t 'k 'v): bool =>
  count != 0;

let put (key: 'k) (value: 'v) (map: t 'k 'v): (t 'k 'v) =>
  map |> alter key (Functions.return @@ Option.return @@ value);

let reduce (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) ({ root }: t 'k 'v): 'acc =>
  root |> BitmapTrieMap.reduce f acc;

let reduceWhile
    (predicate: 'acc => 'k => 'v => bool)
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    ({ root }: t 'k 'v): 'acc =>
  root |> BitmapTrieMap.reduceWhile predicate f acc;

let remove (key: 'k) (map: t 'k 'v): (t 'k 'v) =>
  map |> alter key Functions.alwaysNone;

let removeAll ({ strategy }: t 'k 'v): (t 'k 'v) =>
  emptyWith strategy;

let toIterator (map: t 'k 'v): (Iterator.t ('k, 'v)) =>
  if (isEmpty map) Iterator.empty
  else {
    reduceWhile: fun predicate f acc => map |> reduceWhile
      (fun acc k v => predicate acc (k, v))
      (fun acc k v => f acc (k, v))
      acc
  };

let toKeyedIterator (map: t 'k 'v): (KeyedIterator.t 'k 'v) =>
  if (isEmpty map) KeyedIterator.empty
  else {
    reduceWhile: fun predicate f acc => map |> reduceWhile predicate f acc
  };

let toSequence ({ root }: t 'k 'v): (Sequence.t ('k, 'v)) =>
  root |> BitmapTrieMap.toSequence;

let values ({ root }: t 'k 'v): (Iterator.t 'v) =>
  root |> BitmapTrieMap.values;

let toMap (map: t 'k 'v): (ImmMap.t 'k 'v) => {
  containsKey: fun k => containsKey k map,
  count: (count map),
  get: fun i => get i map,
  getOrRaise: fun i => getOrRaise i map,
  keyedIterator: toKeyedIterator map,
  sequence: toSequence map,
};

let keys (map: t 'k 'v): (ImmSet.t 'k) =>
  map |> toMap |> ImmMap.keys;

let module TransientHashMap = {
  type hashMap 'k 'v = t 'k 'v;

  type t 'k 'v = Transient.t (hashMap 'k 'v);

  let mutate (map: hashMap 'k 'v): (t 'k 'v) =>
    Transient.create map;

  let alterImpl
      (owner: Transient.Owner.t)
      (key: 'k)
      (f: option 'v => option 'v)
      ({ count, root, strategy } as map: hashMap 'k 'v): (hashMap 'k 'v) => {
    let hash = HashStrategy.hash strategy key;
    let alterResult = ref BitmapTrieMap.NoChange;
    let newRoot = root |> BitmapTrieMap.alter
      strategy
      BitmapTrieMap.updateLevelNodeTransient
      owner
      alterResult
      0
      hash
      key
      f;

    switch !alterResult {
      | BitmapTrieMap.Added => { count: count + 1, root: newRoot, strategy }
      | BitmapTrieMap.NoChange => map
      | BitmapTrieMap.Replace =>
          if (newRoot === root) map
          else { count, root: newRoot, strategy }
      | BitmapTrieMap.Removed => { count: count - 1, root: newRoot, strategy }
    };
  };

  let alter
      (key: 'k)
      (f: option 'v => option 'v)
      (transient: t 'k 'v): (t 'k 'v) =>
    transient |> Transient.update2 alterImpl key f;

  let containsKey (key: 'k) (transient: t 'k 'v): bool =>
    transient |> Transient.get |> containsKey key;

  let count (transient: t 'k 'v): int =>
    transient |> Transient.get |> count;

  let empty (): (t 'k 'v) =>
    empty () |> mutate;

  let get (key: 'k) (transient: t 'k 'v): (option 'v) =>
   transient |> Transient.get |> get key;

  let getOrRaise (key: 'k) (transient: t 'k 'v): 'v =>
    transient |> Transient.get |> getOrRaise key;

  let persistentEmptyWith = emptyWith;

  let emptyWith (strategy: HashStrategy.t 'k): (t 'k 'v) =>
    persistentEmptyWith strategy |> mutate;

  let isEmpty (transient: t 'k 'v): bool =>
    transient |> Transient.get |> isEmpty;

  let isNotEmpty (transient: t 'k 'v): bool =>
    transient |> Transient.get |> isNotEmpty;

  let persist (transient: t 'k 'v): (hashMap 'k 'v) =>
    transient |> Transient.persist;

  let put (key: 'k) (value: 'v) (transient: t 'k 'v): (t 'k 'v) =>
    transient |> alter key (Functions.return @@ Option.return @@ value);

  let putAll (iter: KeyedIterator.t 'k 'v) (map: t 'k 'v): (t 'k 'v) =>
    iter |> KeyedIterator.reduce (fun acc k v => acc |> put k v) map;

  let remove (key: 'k) (transient: t 'k 'v): (t 'k 'v) =>
    transient |> alter key Functions.alwaysNone;

  let removeAllImpl
      (_: Transient.Owner.t)
      ({ strategy }: hashMap 'k 'v): (hashMap 'k 'v) =>
    persistentEmptyWith strategy;

  let removeAll (transient: t 'k 'v): (t 'k 'v) =>
    transient |> Transient.update removeAllImpl;
};

let mutate = TransientHashMap.mutate;

let map (f: 'k => 'a => 'b) ({ strategy } as map: t 'k 'a): (t 'k 'b) => map
  |> reduce (fun acc k v => acc |> TransientHashMap.put k (f k v)) (emptyWith strategy |> mutate)
  |> TransientHashMap.persist;

let putAll (iter: KeyedIterator.t 'k 'v) (map: t 'k 'v): (t 'k 'v) =>
  map |> mutate |> TransientHashMap.putAll iter |> TransientHashMap.persist;

let fromWith (strategy: HashStrategy.t 'k) (iter: KeyedIterator.t 'k 'v): (t 'k 'v) =>
  emptyWith strategy |> putAll iter;

let from (iter: KeyedIterator.t 'k 'v): (t 'k 'v) =>
  fromWith (HashStrategy.structuralCompare) iter;

let merge
    (f: 'k => (option 'vAcc) => (option 'v) => (option 'vAcc))
    (next: t 'k 'v)
    (map: t 'k 'vAcc): (t 'k 'vAcc) =>
  ImmSet.union (keys map) (keys next)
    |> Iterator.reduce (
        fun acc key => {
          let result = f key (map |> get key) (next |> get key);
          switch result {
            | None => acc |> TransientHashMap.remove key
            | Some value => acc |> TransientHashMap.put key value
          }
        }
      )
      (mutate map)
    |> TransientHashMap.persist;
