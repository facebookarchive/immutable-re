open Collection;
open CopyOnWriteArray;
open Equality;
open Functions;
open Hash;
open Keyed;
open Option;
open Option.Operators;
open Seq;
open Transient;

type bitmapTrieIntMap 'a =
  | Level int32 (array (bitmapTrieIntMap 'a)) (option owner)
  | Entry int 'a
  | Empty;

let module BitmapTrieIntMap = {
  /* FIXME: Use the same alter pattern used in the other maps to avoid allocations */
  type alterResult =
    | Added
    | NoChange
    | Removed
    | Replace;

  let rec alter
      (updateLevelNode: int => (bitmapTrieIntMap 'a) => (bitmapTrieIntMap 'a) => (bitmapTrieIntMap 'a))
      (owner: option owner)
      (alterResult: ref alterResult)
      (depth: int)
      (key: int)
      (f: option 'a => option 'a)
      (map: bitmapTrieIntMap 'a): (bitmapTrieIntMap 'a) => switch map {
    | Entry entryKey entryValue when key == entryKey => switch (f @@ Option.return @@ entryValue) {
        | Some newEntryValue when newEntryValue === entryValue =>
            alterResult := NoChange;
            map;
        | Some newEntryValue =>
            alterResult := Replace;
            Entry key newEntryValue
        | None =>
            alterResult := Removed;
            Empty
      }
    | Entry entryKey _ => switch (f None) {
        | Some newEntryValue =>
            let bitmap = BitmapTrie.bitPos entryKey depth;
            Level bitmap [| map |] owner
              |> alter updateLevelNode owner alterResult depth key (Functions.return @@ Option.return @@ newEntryValue)
        | _ =>
            alterResult := NoChange;
            map;
      }
    | Level bitmap nodes _ =>
        let bit = BitmapTrie.bitPos key depth;
        let index = BitmapTrie.index bitmap bit;

        if (BitmapTrie.containsNode bitmap bit) {
          let childNode = nodes.(index);
          let newChildNode = childNode |> alter updateLevelNode owner alterResult (depth + 1) key f;

          switch !alterResult {
            | Added => map |> updateLevelNode index newChildNode;
            | NoChange => map
            | Replace => map |> updateLevelNode index newChildNode
            | Removed => switch newChildNode {
                | Empty =>
                    let nodes = nodes |> CopyOnWriteArray.removeAt index;
                    if (CopyOnWriteArray.count nodes > 0) (Level (Int32.logxor bitmap bit) nodes owner)
                    else Empty
                | _ => map |> updateLevelNode index newChildNode
              }
          }
        } else (switch (f None) {
          | Some newEntryValue =>
              alterResult := Added;
              let node = Entry key newEntryValue;
              let nodes = nodes |> CopyOnWriteArray.insertAt index node;
              Level (Int32.logor bitmap bit) nodes owner;
          | None =>
              alterResult := NoChange;
              map;
        })
    | Empty => switch (f None) {
        | None =>
            alterResult := NoChange;
            map
        | Some v =>
            alterResult := Added;
            Entry key v;
      }
  };

  let rec containsKey
      (depth: int)
      (key: int)
      (map: bitmapTrieIntMap 'a): bool => switch map {
    | Level bitmap nodes _ =>
        let bit = BitmapTrie.bitPos key depth;
        let index = BitmapTrie.index bitmap bit;

        (BitmapTrie.containsNode bitmap bit) &&
        (containsKey (depth + 1) key nodes.(index));
    | Entry entryKey entryValue => key == entryKey;
    | Empty => false;
  };

  let rec containsWith
      (equality: equality 'a)
      (depth: int)
      (key: int)
      (value: 'a)
      (map: bitmapTrieIntMap 'a): bool => switch map {
    | Level bitmap nodes _ =>
        let bit = BitmapTrie.bitPos key depth;
        let index = BitmapTrie.index bitmap bit;

        (BitmapTrie.containsNode bitmap bit) &&
        (containsWith equality (depth + 1) key value nodes.(index));
    | Entry entryKey entryValue =>
        key == entryKey && (equality value entryValue);
    | Empty => false;
  };

  let rec every (f: int => 'a => bool) (map: bitmapTrieIntMap 'a): bool => switch map {
    | Level _ nodes _ => nodes |> CopyOnWriteArray.every (fun node => every f node)
    | Entry key value => f key value
    | Empty => true
  };

  let rec forEach (f: int => 'a => unit) (map: bitmapTrieIntMap 'a): unit => switch map {
    | Level _ nodes _ =>
        let f map => forEach f map;
        nodes |> CopyOnWriteArray.forEach f;
    | Entry key value => f key value;
    | Empty => ();
  };

  let rec none (f: int => 'a => bool) (map: bitmapTrieIntMap 'a): bool => switch map {
    | Level _ nodes _ => nodes |> CopyOnWriteArray.every (fun node => none f node)
    | Entry key value => f key value |> not
    | Empty => true
  };

  let rec reduce (f: 'acc => int => 'a => 'acc) (acc: 'acc) (map: bitmapTrieIntMap 'a): 'acc => switch map {
    | Level _ nodes _ =>
        let reducer acc map => reduce f acc map;
        nodes |> CopyOnWriteArray.reduce reducer acc;
    | Entry key value => f acc key value;
    | Empty => acc;
  };

  let rec some (f: int => 'a => bool) (map: bitmapTrieIntMap 'a): bool => switch map {
    | Level _ nodes _ => nodes |> CopyOnWriteArray.some (fun node => some f node)
    | Entry key value => f key value
    | Empty => false
  };

  let rec toSeq (map: bitmapTrieIntMap 'a): (seq (int, 'a)) => switch map {
    | Entry key value => Seq.return (key, value)
    | Level _ nodes _ => nodes |> CopyOnWriteArray.toSeq |> Seq.flatMap toSeq
    | Empty => Seq.empty;
  };

  let rec tryFind (f: int => 'a => bool) (map: bitmapTrieIntMap 'a): (option (int, 'a)) => switch map {
    | Level _ nodes _ =>
        let nodesCount = CopyOnWriteArray.count nodes;
        let rec loop index =>
          if (index < nodesCount) {
            switch (tryFind f nodes.(index)) {
              | Some _ as result => result
              | _ => loop (index + 1)
            }
          } else None;
        loop 0
    | Entry key value => if (f key value) (Some (key, value)) else None;
    | Empty => None
  };

  let rec tryGet (depth: int) (key: int) (map: bitmapTrieIntMap 'a): (option 'a) => switch map {
    | Level bitmap nodes _ =>
        let bit = BitmapTrie.bitPos key depth;
        let index = BitmapTrie.index bitmap bit;

        if (BitmapTrie.containsNode bitmap bit) (tryGet (depth + 1) key nodes.(index))
        else None;
    | Entry entryKey entryValue when key == entryKey => Some entryValue
    | _ => None
  };

  let rec values (map: bitmapTrieIntMap 'a): (seq 'a) => switch map {
    | Entry key value => Seq.return value
    | Level _ nodes _ => nodes |> CopyOnWriteArray.toSeq |> Seq.flatMap values
    | Empty => Seq.empty;
  };
};

type intMap 'a = {
  count: int,
  root: (bitmapTrieIntMap 'a),
};

let updateLevelNodePersistent
    (index: int)
    (childNode: bitmapTrieIntMap 'a)
    ((Level bitmap nodes _): (bitmapTrieIntMap 'a)): (bitmapTrieIntMap 'a) =>
  Level bitmap (nodes |> CopyOnWriteArray.update index childNode) None;

let empty: intMap 'a = { count: 0, root: Empty };

let alter (key: int) (f: option 'a => option 'a) ({ count, root } as map: intMap 'a): (intMap 'a) => {
  let alterResult = ref BitmapTrieIntMap.NoChange;
  let newRoot = root |> BitmapTrieIntMap.alter updateLevelNodePersistent None alterResult 0 key f;

  switch !alterResult {
    | BitmapTrieIntMap.Added => { count: count + 1, root: newRoot }
    | BitmapTrieIntMap.NoChange => map
    | BitmapTrieIntMap.Replace => { count, root: newRoot }
    | BitmapTrieIntMap.Removed => { count: count - 1, root: newRoot }
  }
};

let containsKey (key: int) ({ root }: intMap 'a): bool =>
  root |> BitmapTrieIntMap.containsKey 0 key;

let containsWith (equality: equality 'a) (key: int) (value: 'a) ({ root }: intMap 'a): bool =>
  root |> BitmapTrieIntMap.containsWith equality 0 key value;

let contains (key: int) (value: 'a) (map: intMap 'a): bool =>
  map |> containsWith Equality.structural key value;

let count ({ count }: intMap 'a): int => count;

let every (f: int => 'a => bool) ({ root }: intMap 'a): bool =>
  root |> BitmapTrieIntMap.every f;

let find (f: int => 'a => bool) ({ root }: intMap 'a): (int, 'a) =>
  root |> BitmapTrieIntMap.tryFind f |> Option.get;

let forEach (f: int => 'a => unit) ({ root }: intMap 'a): unit =>
  root |> BitmapTrieIntMap.forEach f;

let get (key: int) ({ root }: intMap 'a): 'a =>
  root |> BitmapTrieIntMap.tryGet 0 key |> Option.get;

let isEmpty ({ count }: intMap 'a): bool => count == 0;

let isNotEmpty ({ count }: intMap 'a): bool => count != 0;

let none (f: int => 'a => bool) ({ root }: intMap 'a): bool =>
  root |> BitmapTrieIntMap.none f;

let put (key: int) (value: 'a) ({ count, root } as map: intMap 'a): (intMap 'a) =>
  map |> alter key (Functions.return @@ Option.return @@ value);

let reduce (f: 'acc => int => 'a => 'acc) (acc: 'acc) ({ root }: intMap 'a): 'acc =>
  root |> BitmapTrieIntMap.reduce f acc;

let remove (key: int) (map: intMap 'a): (intMap 'a) =>
  map |> alter key alwaysNone;

let removeAll (map: intMap 'a): (intMap 'a) => empty;

let some (f: int => 'a => bool) ({ root }: intMap 'a): bool =>
  root |> BitmapTrieIntMap.some f;

let toSeq ({ root }: intMap 'a): (seq ((int, 'a))) =>
  root |> BitmapTrieIntMap.toSeq;

let tryFind (f: int => 'a => bool) ({ root }: intMap 'a): (option (int, 'a)) =>
  root |> BitmapTrieIntMap.tryFind f;

let tryGet (key: int) ({ root }: intMap 'a): (option 'a) =>
  root |> BitmapTrieIntMap.tryGet 0 key;

let values ({ root }: intMap 'a): (seq 'a) =>
  root |> BitmapTrieIntMap.values;

let toKeyed (map: intMap 'a): (keyed int 'a) => {
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

let equals (this: intMap 'a) (that: intMap 'a): bool =>
  Keyed.equals (toKeyed this) (toKeyed that);

let equalsWith (equality: equality 'a) (this: intMap 'a) (that: intMap 'a): bool =>
  Keyed.equalsWith equality (toKeyed this) (toKeyed that);

let hash (map: intMap 'a): int =>
  map |> toKeyed |> Keyed.hash;

let hashWith (hash: hash 'a) (map: intMap 'a): int =>
  map |> toKeyed |> Keyed.hashWith Hash.structural hash;

let keys (map: intMap 'a): (collection int) =>
  map |> toKeyed |> Keyed.keys;

let toCollection (map: intMap 'a): (collection (int, 'a)) =>
  map |> toKeyed |> Keyed.toCollection;

let toCollectionWith (equality: equality 'a) (map: intMap 'a): (collection (int, 'a)) =>
  map |> toKeyed |> Keyed.toCollectionWith equality;

type transientIntMap 'a = transient (intMap 'a);

let mutate (map: intMap 'a): (transientIntMap 'a) => Transient.create map;

let module TransientIntMap = {
  let updateLevelNodeTransient
      (owner: owner)
      (index: int)
      (childNode: bitmapTrieIntMap 'a)
      ((Level bitmap nodes nodeOwner) as node: (bitmapTrieIntMap 'a)): (bitmapTrieIntMap 'a) => switch nodeOwner {
    | Some nodeOwner when nodeOwner === owner =>
        nodes.(index) = childNode;
        node
    | _ => Level bitmap (nodes |> CopyOnWriteArray.update index childNode) (Some owner)
  };

  let alter
      (key: int)
      (f: option 'a => option 'a)
      (transient: transientIntMap 'a): (transientIntMap 'a) =>
    transient |> Transient.update (fun owner ({ count, root } as map) => {
      let alterResult = ref BitmapTrieIntMap.NoChange;
      let newRoot = root
        |> BitmapTrieIntMap.alter (updateLevelNodeTransient owner) (Some owner) alterResult 0 key f;

      switch !alterResult {
        | BitmapTrieIntMap.Added => { count: count + 1, root: newRoot }
        | BitmapTrieIntMap.NoChange => map
        | BitmapTrieIntMap.Replace => if (root === newRoot) map else { count, root: newRoot }
        | BitmapTrieIntMap.Removed => { count: count - 1, root: newRoot }
      }
    });

  let count (transient: transientIntMap 'a): int =>
    transient |> Transient.get |> count;

  let persistentEmpty = empty;
  let empty (): transientIntMap 'a =>
    empty |> mutate;

  let isEmpty (transient: transientIntMap 'a): bool =>
    transient |> Transient.get |> isEmpty;

  let isNotEmpty (transient: transientIntMap 'a): bool =>
    transient |> Transient.get |> isNotEmpty;

  let persist (transient: transientIntMap 'a): (intMap 'a) =>
    transient |> Transient.persist;

  let put (key: int) (value: 'a) (transient: transientIntMap 'a): (transientIntMap 'a) =>
    transient |> alter key (Functions.return @@ Option.return @@ value);

  let putAll
      (seq: seq (int, 'a))
      (transient: transientIntMap 'a): (transientIntMap 'a) => seq
    |> Seq.reduce (fun acc (k, v) => acc |> put k v) transient;

  let remove (key: int) (transient: transientIntMap 'a): (transientIntMap 'a) =>
    transient |> alter key alwaysNone;

  let removeAll (transient: transientIntMap 'a): (transientIntMap 'a) =>
      transient |> Transient.update (fun owner map => persistentEmpty);

  let tryGet (key: int) (transient: transientIntMap 'a): (option 'a) =>
    transient |> Transient.get |> (tryGet key);
};

let putAll (seq: seq (int, 'a)) (map: intMap 'a): (intMap 'a) => map
  |> mutate
  |> TransientIntMap.putAll seq
  |> TransientIntMap.persist;

let map (f: int => 'a => 'b) (map: intMap 'a): (intMap 'b) => map
  |> reduce
    (fun acc key value => acc |> TransientIntMap.put key (f key value))
    (mutate empty)
  |> TransientIntMap.persist;

let fromSeq (seq: seq (int, 'a)): (intMap 'a) => putAll seq empty;

let fromKeyed (keyed: keyed int 'a): (intMap 'a) => keyed
  |> Keyed.reduce (fun acc k v => acc |> TransientIntMap.put k v) (mutate empty)
  |> TransientIntMap.persist;

let merge
    (f: int => (option 'vAcc) => (option 'v) => (option 'vAcc))
    (next: keyed int 'v)
    (map: intMap 'vAcc): (intMap 'vAcc) =>
  Collection.union (map |> toKeyed |> Keyed.keys) (next |> Keyed.keys)
    |> Seq.reduce (
        fun acc key => {
          let result = f key (map |> tryGet key) (next |> Keyed.tryGet key);
          switch result {
            | None => acc |> TransientIntMap.remove key
            | Some value => acc |> TransientIntMap.put key value
          }
        }
      )
      (mutate map)
    |> TransientIntMap.persist;
