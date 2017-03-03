open AVLTreeMap;
open Collection;
open Equality;
open EqualityMap;
open Hash;
open HashStrategy;
open Keyed;
open Ordering;
open Seq;
open SortedMap;
open Transient;

type bitmapTrieMap 'k 'v =
  | Level int32 (array (bitmapTrieMap 'k 'v)) (option owner)
  | ComparatorCollision int (avlTreeMap 'k 'v)
  | EqualityCollision int (equalityMap 'k 'v)
  | Entry int 'k 'v
  | Empty;

let module BitmapTrieMap = {
  type alterResult =
    | Added
    | NoChange
    | Removed
    | Replace;

  let rec alter
      (hashStrategy: hashStrategy 'k)
      (updateLevelNode: int => (bitmapTrieMap 'k 'v) => (bitmapTrieMap 'k 'v) => (bitmapTrieMap 'k 'v))
      (owner: option owner)
      (alterResult: ref alterResult)
      (depth: int)
      (hash: int)
      (key: 'k)
      (f: option 'v => option 'v)
      (map: bitmapTrieMap 'k 'v): (bitmapTrieMap 'k 'v) => switch map {
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
              | Comparator _ comparator =>
                  let map = AVLTreeMap.Empty
                    |> AVLTreeMap.put comparator entryKey entryValue
                    |> AVLTreeMap.put comparator key value;
                  alterResult := Added;
                  ComparatorCollision entryHash map;
              | Equality _ equals =>
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
    | Entry entryHash entryKey entryValue => switch (f None) {
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
          let (entrykey, entryValue) = newEntryMap |> EqualityMap.first;
          Entry entryHash entrykey entryValue;
        } else {
          alterResult := Removed;
          EqualityCollision entryHash newEntryMap;
        };
    | ComparatorCollision entryHash entryMap =>
        let collisionResult = ref AVLTreeMap.NoChange;
        let newEntryMap = entryMap |> AVLTreeMap.alter (HashStrategy.comparator hashStrategy) collisionResult key f;

        switch !collisionResult {
          | Added =>
              alterResult := Added;
              ComparatorCollision entryHash newEntryMap;
          | NoChange =>
              alterResult := NoChange;
              map
          | Replace =>
              alterResult := Replace;
              ComparatorCollision entryHash newEntryMap;
          | Removed => switch newEntryMap {
              | Leaf k v =>
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
            | Added => map |> updateLevelNode index newChildNode
            | NoChange => map
            | Removed =>
                if (newChildNode === Empty) {
                  let nodes = nodes |> CopyOnWriteArray.removeAt index;
                  if (CopyOnWriteArray.count nodes > 0) (Level (Int32.logxor bitmap bit) nodes owner)
                  else Empty
                } else (updateLevelNode index newChildNode map)
            | Replace => map |> updateLevelNode index newChildNode
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

  let rec containsKey
      (hashStrategy: hashStrategy 'k)
      (depth: int)
      (hash: int)
      (key: 'k)
      (set: bitmapTrieMap 'k 'v): bool => switch set {
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

  let rec containsWith
      (hashStrategy: hashStrategy 'k)
      (valueEquals: equality 'v)
      (depth: int)
      (hash: int)
      (key: 'k)
      (value: 'v)
      (set: bitmapTrieMap 'k 'v): bool => switch set {
    | Level bitmap nodes _ =>
        let bit = BitmapTrie.bitPos hash depth;
        let index = BitmapTrie.index bitmap bit;

        (BitmapTrie.containsNode bitmap bit) &&
        (containsWith hashStrategy valueEquals (depth + 1) hash key value nodes.(index));
    | EqualityCollision entryHash entryMap =>
        (hash == entryHash) &&
        (EqualityMap.contains (HashStrategy.equals hashStrategy) valueEquals key value entryMap);
    | ComparatorCollision entryHash entryMap =>
        (hash == entryHash) &&
        (AVLTreeMap.contains (HashStrategy.comparator hashStrategy) valueEquals key value entryMap);
    | Entry entryHash entryKey entryValue =>
        (hash == entryHash) &&
        (HashStrategy.equals hashStrategy entryKey key) &&
        (valueEquals entryValue value)
    | Empty => false;
  };

  let rec every (f: 'k => 'v => bool) (map: bitmapTrieMap 'k 'v): bool => switch map {
    | Level _ nodes _ => nodes |> CopyOnWriteArray.every (every f)
    | ComparatorCollision _ entryMap => entryMap |> AVLTreeMap.every f
    | EqualityCollision _ entryMap => entryMap |> EqualityMap.every f;
    | Entry _ entryKey entryValue => f entryKey entryValue
    | Empty => true
  };

  let rec forEach (f: 'k => 'v => unit) (map: bitmapTrieMap 'k 'v): unit => switch map {
    | Level _ nodes _ =>
        let iter node => node |> forEach f;
        nodes |> CopyOnWriteArray.forEach iter;
    | ComparatorCollision _ entryMap =>
        entryMap |> AVLTreeMap.forEach f
    | EqualityCollision _ entryMap =>
        entryMap |> EqualityMap.forEach f
    | Entry _ entryKey entryValue =>
        f entryKey entryValue
    | Empty => ()
  };

  let rec get
      (hashStrategy: hashStrategy 'k)
      (depth: int)
      (hash: int)
      (key: 'k)
      (map: bitmapTrieMap 'k 'v): 'v => switch map {
    | Level bitmap nodes _ =>
        let bit = BitmapTrie.bitPos hash depth;
        let index = BitmapTrie.index bitmap bit;

        if (BitmapTrie.containsNode bitmap bit) (get hashStrategy (depth + 1) hash key nodes.(index))
        else (failwith "NotFound")
    | EqualityCollision entryHash entryMap =>
        if (hash == entryHash) (EqualityMap.get (HashStrategy.equals hashStrategy) key entryMap)
        else (failwith "NotFound")
    | ComparatorCollision entryHash entryMap =>
        if (hash == entryHash) (AVLTreeMap.get (HashStrategy.comparator hashStrategy) key entryMap)
        else (failwith "NotFound")
    | Entry entryHash entryKey entryValue =>
        if ((hash == entryHash) && (HashStrategy.equals hashStrategy entryKey key)) {
          entryValue
        } else (failwith "NotFound")
    | Empty => failwith "NotFound";
  };

  let rec none (f: 'k => 'v => bool) (map: bitmapTrieMap 'k 'v): bool => switch map {
    | Level _ nodes _ => nodes |> CopyOnWriteArray.every (none f)
    | ComparatorCollision _ entryMap => entryMap |> AVLTreeMap.none f
    | EqualityCollision _ entryMap => entryMap |> EqualityMap.none f;
    | Entry _ entryKey entryValue => f entryKey entryValue |> not
    | Empty => true
  };

  let rec reduce (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) (map: bitmapTrieMap 'k 'v): 'acc => switch map {
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

  let rec some (f: 'k => 'v => bool) (map: bitmapTrieMap 'k 'v): bool => switch map {
    | Level _ nodes _ => nodes |> CopyOnWriteArray.some (some f)
    | ComparatorCollision _ entryMap => entryMap |> AVLTreeMap.some f
    | EqualityCollision _ entryMap => entryMap |> EqualityMap.some f;
    | Entry _ entryKey entryValue => f entryKey entryValue
    | Empty => false
  };

  let rec toSeq (map: bitmapTrieMap 'k 'v): (seq ('k, 'v)) => switch map {
    | Level _ nodes _ => nodes |> CopyOnWriteArray.toSeq |> Seq.flatMap toSeq
    | ComparatorCollision _ entryMap => AVLTreeMap.toSeq entryMap
    | EqualityCollision _ entryMap => EqualityMap.toSeq entryMap;
    | Entry _ entryKey entryValue => Seq.return (entryKey, entryValue);
    | Empty => Seq.empty;
  };

  let rec tryFind (f: 'k => 'v => bool) (map: bitmapTrieMap 'k 'v): (option ('k, 'v)) => switch map {
    | Level _ nodes _ =>
        let nodesCount = CopyOnWriteArray.count nodes;
        let rec loop index => index < nodesCount
          ? switch (tryFind f nodes.(index)) {
              | Some _ as result => result
              | _ => loop (index + 1)
            }
          : None;
        loop 0
    | ComparatorCollision _ entryMap => AVLTreeMap.tryFind f entryMap
    | EqualityCollision _ entryMap => EqualityMap.tryFind f entryMap;
    | Entry _ entryKey entryValue =>
        if (f entryKey entryValue) (Some (entryKey, entryValue))
        else None
    | Empty => None;
  };

  let rec tryGet
      (hashStrategy: hashStrategy 'k)
      (depth: int)
      (hash: int)
      (key: 'k)
      (map: bitmapTrieMap 'k 'v): (option 'v) => switch map {
    | Level bitmap nodes _ =>
        let bit = BitmapTrie.bitPos hash depth;
        let index = BitmapTrie.index bitmap bit;

        if (BitmapTrie.containsNode bitmap bit) (tryGet hashStrategy (depth + 1) hash key nodes.(index))
        else None
    | EqualityCollision entryHash entryMap =>
        if (hash == entryHash) (EqualityMap.tryGet (HashStrategy.equals hashStrategy) key entryMap)
        else None
    | ComparatorCollision entryHash entryMap =>
        if (hash == entryHash) (AVLTreeMap.tryGet (HashStrategy.comparator hashStrategy) key entryMap)
        else None
    | Entry entryHash entryKey entryValue =>
        if ((hash == entryHash) && (HashStrategy.equals hashStrategy entryKey key)) {
          Some entryValue
        } else None
    | Empty => None;
  };

  let rec values (map: bitmapTrieMap 'k 'v): (seq 'v) => switch map {
    | Level _ nodes _ => nodes |> CopyOnWriteArray.toSeq |> Seq.flatMap values
    | ComparatorCollision _ entryMap => AVLTreeMap.values entryMap
    | EqualityCollision _ entryMap => EqualityMap.values entryMap;
    | Entry _ entryKey entryValue => Seq.return entryValue;
    | Empty => Seq.empty;
  };
};

type hashMap 'k 'v = {
  count: int,
  root: bitmapTrieMap 'k 'v,
  strategy: hashStrategy 'k,
};

let empty: (hashMap 'k 'v) = {
  count: 0,
  root: Empty,
  strategy: HashStrategy.structuralCompare,
};

let emptyWith (strategy: hashStrategy 'k): (hashMap 'k 'v) => {
  count: 0,
  root: Empty,
  strategy,
};

let updateLevelNodePersistent
    (index: int)
    (childNode: bitmapTrieMap 'k 'v)
    ((Level bitmap nodes _): (bitmapTrieMap 'k 'v)): (bitmapTrieMap 'k 'v) =>
  Level bitmap (nodes |> CopyOnWriteArray.update index childNode) None;

let alter
    (key: 'k)
    (f: option 'v => option 'v)
    ({ count, root, strategy } as map: hashMap 'k 'v): (hashMap 'k 'v) => {
  let hash = HashStrategy.hash strategy key;
  let alterResult = ref BitmapTrieMap.NoChange;
  let newRoot = root |> BitmapTrieMap.alter
    strategy
    updateLevelNodePersistent
    None
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

let containsKey (key: 'k) ({ root, strategy }: hashMap 'k 'v): bool => {
  let hash = HashStrategy.hash strategy key;
  root |> BitmapTrieMap.containsKey strategy 0 hash key;
};

let containsWith (valueEquals: equality 'v) (key: 'k) (value: 'v) ({ root, strategy }: hashMap 'k 'v): bool => {
  let hash = HashStrategy.hash strategy key;
  root |> BitmapTrieMap.containsWith strategy valueEquals 0 hash key value;
};

let contains (key: 'k) (value: 'v) (map: hashMap 'k 'v): bool =>
  map |> containsWith Equality.structural key value;

let count ({ count }: hashMap 'k 'v): int => count;

let every (f: 'k => 'v => bool) ({ root }: hashMap 'k 'v): bool =>
  root |> BitmapTrieMap.every f;

let forEach (f: 'k => 'v => unit) ({ root }: hashMap 'k 'v) =>
  root |> BitmapTrieMap.forEach f;

let get (key: 'k) ({ root, strategy }: hashMap 'k 'v): 'v => {
  let hash = HashStrategy.hash strategy key;
  root |> BitmapTrieMap.get strategy 0 hash key;
};

let isEmpty ({ count }: hashMap 'k 'v): bool =>
  count == 0;

let isNotEmpty ({ count }: hashMap 'k 'v): bool =>
  count != 0;

let none (f: 'k => 'v => bool) ({ root }: hashMap 'k 'v): bool =>
  root |> BitmapTrieMap.none f;

let put (key: 'k) (value: 'v) (map: hashMap 'k 'v): (hashMap 'k 'v) =>
  map |> alter key (Functions.return @@ Option.return @@ value);

let reduce (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) ({ root }: hashMap 'k 'v): 'acc =>
  root |> BitmapTrieMap.reduce f acc;

let remove (key: 'k) (map: hashMap 'k 'v): (hashMap 'k 'v) =>
  map |> alter key Functions.alwaysNone;

let removeAll ({ strategy }: hashMap 'k 'v): (hashMap 'k 'v) =>
  emptyWith strategy;

let some (f: 'k => 'v => bool) ({ root }: hashMap 'k 'v): bool =>
  root |> BitmapTrieMap.some f;

let toSeq ({ root }: hashMap 'k 'v): (seq ('k, 'v)) =>
  root |> BitmapTrieMap.toSeq;

let tryFind (f: 'k => 'v => bool) ({ root }: hashMap 'k 'v): (option ('k, 'v)) =>
  root |> BitmapTrieMap.tryFind f;

let find (f: 'k => 'v => bool) (map: hashMap 'k 'v): ('k, 'v) =>
  map |> tryFind f |> Option.get;

let tryGet (key: 'k) ({ strategy, root }: hashMap 'k 'v): (option 'v) => {
  let hash = HashStrategy.hash strategy key;
  root |> BitmapTrieMap.tryGet strategy 0 hash key;
};

let values ({ root }: hashMap 'k 'v): (seq 'v) =>
  root |> BitmapTrieMap.values;

let toKeyed (map: hashMap 'k 'v): (keyed 'k 'v) => {
  containsWith: fun eq k v => map |> containsWith eq k v,
  containsKey: fun k => containsKey k map,
  count: (count map),
  every: fun f => every f map,
  find: fun f => find f map,
  forEach: fun f => forEach f map,
  get: fun i => get i map,
  none: fun f => none f map,
  reduce: fun f acc => map |> reduce f acc,
  some: fun f => map |> some f,
  toSeq: (toSeq map),
  tryFind: fun f => tryFind f map,
  tryGet: fun i => tryGet i map,
  values: (values map),
};

let equalsWith
    (valueEquals: equality 'v)
    ({ strategy } as this: hashMap 'k 'v)
    (that: hashMap 'k 'v): bool =>
  Seq.equalsWith (fun (k1, v1) (k2, v2) =>
    if (k1 === k2) true
    else if (HashStrategy.equals strategy k1 k2) (valueEquals v1 v2)
    else false
  ) (toSeq this) (toSeq that);

let equals (this: hashMap 'k 'v) (that: hashMap 'k 'v): bool =>
  equalsWith Equality.structural this that;

let hash (map: hashMap 'k 'v): int =>
  map |> toKeyed |> Keyed.hash;

let hashWith (valueHash: hash 'v) ({ strategy } as map: hashMap 'k 'v): int =>
  map |> toKeyed |> Keyed.hashWith (HashStrategy.hash strategy) valueHash;

let keys (map: hashMap 'k 'v): (collection 'k) =>
  map |> toKeyed |> Keyed.keys;

let toCollection (map: hashMap 'k 'v): (collection ('k, 'v)) =>
  map |> toKeyed |> Keyed.toCollection;

let toCollectionWith (equality: equality 'v) (map: hashMap 'k 'v): (collection ('k, 'v)) =>
  map |> toKeyed |> Keyed.toCollectionWith equality;

type transientHashMap 'k 'v = transient (hashMap 'k 'v);

let mutate (map: hashMap 'k 'v): (transientHashMap 'k 'v) =>
  Transient.create map;

let module TransientHashMap = {
  let updateLevelNodeTransient
      (owner: owner)
      (index: int)
      (childNode: bitmapTrieMap 'k 'v)
      ((Level bitmap nodes nodeOwner) as node: (bitmapTrieMap 'k 'v)): (bitmapTrieMap 'k 'v) => switch nodeOwner {
    | Some nodeOwner when nodeOwner === owner =>
        nodes.(index) = childNode;
        node
    | _ => Level bitmap (nodes |> CopyOnWriteArray.update index childNode) (Some owner)
  };

  let alter
      (key: 'k)
      (f: option 'v => option 'v)
      (transient: transientHashMap 'k 'v): (transientHashMap 'k 'v) =>
    transient |> Transient.update (fun owner ({ count, root, strategy } as map) => {
      let hash = HashStrategy.hash strategy key;
      let alterResult = ref BitmapTrieMap.NoChange;
      let newRoot = root |> BitmapTrieMap.alter
        strategy
        (updateLevelNodeTransient owner)
        None
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
    });

  let count (transient: transientHashMap 'k 'v): int =>
    transient |> Transient.get |> count;

  let empty (): (transientHashMap 'k 'v) =>
    empty |> mutate;

  let persistentEmptyWith = emptyWith;

  let emptyWith (strategy: hashStrategy 'k): (transientHashMap 'k 'v) =>
    persistentEmptyWith strategy |> mutate;

  let isEmpty (transient: transientHashMap 'k 'v): bool =>
    transient |> Transient.get |> isEmpty;

  let isNotEmpty (transient: transientHashMap 'k 'v): bool =>
    transient |> Transient.get |> isNotEmpty;

  let persist (transient: transientHashMap 'k 'v): (hashMap 'k 'v) =>
    transient |> Transient.persist;

  let put (key: 'k) (value: 'v) (transient: transientHashMap 'k 'v): (transientHashMap 'k 'v) =>
    transient |> alter key (Functions.return @@ Option.return @@ value);

  let putAll (seq: seq ('k, 'v)) (map: transientHashMap 'k 'v): (transientHashMap 'k 'v) =>
    seq |> Seq.reduce (fun acc (k, v) => acc |> put k v) map;

  let remove (key: 'k) (transient: transientHashMap 'k 'v): (transientHashMap 'k 'v) =>
    transient |> alter key Functions.alwaysNone;

  let removeAll (transient: transientHashMap 'k 'v): (transientHashMap 'k 'v) =>
    transient |> Transient.update (fun _ { strategy } => persistentEmptyWith strategy);

  let tryGet (key: 'k) (transient: transientHashMap 'k 'v): (option 'v) =>
    transient |> Transient.get |> tryGet key;
};

let map (f: 'k => 'a => 'b) ({ strategy } as map: hashMap 'k 'a): (hashMap 'k 'b) => map
  |> reduce (fun acc k v => acc |> TransientHashMap.put k (f k v)) (emptyWith strategy |> mutate)
  |> TransientHashMap.persist;

let putAll (seq: seq ('k, 'v)) (map: hashMap 'k 'v): (hashMap 'k 'v) =>
  map |> mutate |> TransientHashMap.putAll seq |> TransientHashMap.persist;

let fromSeqWith (strategy: hashStrategy 'k) (seq: seq ('k, 'v)): (hashMap 'k 'v) =>
  emptyWith strategy |> putAll seq;

let fromSeq (seq: seq ('k, 'v)): (hashMap 'k 'v) =>
  fromSeqWith (HashStrategy.structuralCompare) seq;

let fromKeyedWith (strategy: hashStrategy 'k) (keyed: keyed 'k 'v): (hashMap 'k 'v) =>
  Keyed.reduce (fun acc k v => acc |> TransientHashMap.put k v) (emptyWith strategy |> mutate) keyed
  |> TransientHashMap.persist;

let fromKeyed (keyed: keyed 'k 'v): (hashMap 'k 'v) =>
  fromKeyedWith HashStrategy.structuralCompare keyed;

let merge
    (f: 'k => (option 'vAcc) => (option 'v) => (option 'vAcc))
    (next: keyed 'k 'v)
    (map: hashMap 'k 'vAcc): (hashMap 'k 'vAcc) =>
  Collection.union (map |> toKeyed |> Keyed.keys) (next |> Keyed.keys)
    |> Seq.reduce (
        fun acc key => {
          let result = f key (map |> tryGet key) (next |> Keyed.tryGet key);
          switch result {
            | None => acc |> TransientHashMap.remove key
            | Some value => acc |> TransientHashMap.put key value
          }
        }
      )
      (mutate map)
    |> TransientHashMap.persist;
