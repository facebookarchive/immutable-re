let module BitmapTrieIntMap = {
  type t 'a =
    | Level int32 (array (t 'a)) Transient.Owner.t
    | Entry int 'a
    | Empty;

  type updateLevelNode 'a = Transient.Owner.t => int => (t 'a) => (t 'a) => (t 'a);

  type alterResult =
    | Added
    | NoChange
    | Removed
    | Replace;

  let rec alter
      (updateLevelNode: updateLevelNode 'a)
      (owner: Transient.Owner.t)
      (alterResult: ref alterResult)
      (depth: int)
      (key: int)
      (f: option 'a => option 'a)
      (map: t 'a): (t 'a) => switch map {
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
            | Added => map |> updateLevelNode owner index newChildNode;
            | NoChange => map
            | Replace => map |> updateLevelNode owner index newChildNode
            | Removed => switch newChildNode {
                | Empty =>
                    let nodes = nodes |> CopyOnWriteArray.removeAt index;
                    if (CopyOnWriteArray.count nodes > 0) (Level (Int32.logxor bitmap bit) nodes owner)
                    else Empty
                | _ => map |> updateLevelNode owner index newChildNode
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

  let updateLevelNodePersistent
      (_: Transient.Owner.t)
      (index: int)
      (childNode: t 'a)
      (Level bitmap nodes _: (t 'a)): (t 'a) =>
    Level bitmap (CopyOnWriteArray.update index childNode nodes) Transient.Owner.none;

  let rec containsKey
      (depth: int)
      (key: int)
      (map: t 'a): bool => switch map {
    | Level bitmap nodes _ =>
        let bit = BitmapTrie.bitPos key depth;
        let index = BitmapTrie.index bitmap bit;

        (BitmapTrie.containsNode bitmap bit) &&
        (containsKey (depth + 1) key nodes.(index));
    | Entry entryKey _ => key == entryKey;
    | Empty => false;
  };

  let updateLevelNodeTransient
      (owner: Transient.Owner.t)
      (index: int)
      (childNode: t 'a)
      (Level bitmap nodes nodeOwner as node: (t 'a)): (t 'a) =>
    if (nodeOwner === owner) {
        nodes.(index) = childNode;
        node
    }
    else Level bitmap (CopyOnWriteArray.update index childNode nodes) owner;

  let rec containsWith
      (equality: Equality.t 'a)
      (depth: int)
      (key: int)
      (value: 'a)
      (map: t 'a): bool => switch map {
    | Level bitmap nodes _ =>
        let bit = BitmapTrie.bitPos key depth;
        let index = BitmapTrie.index bitmap bit;

        (BitmapTrie.containsNode bitmap bit) &&
        (containsWith equality (depth + 1) key value nodes.(index));
    | Entry entryKey entryValue =>
        key == entryKey && (equality value entryValue);
    | Empty => false;
  };

  let rec every (f: int => 'a => bool) (map: t 'a): bool => switch map {
    | Level _ nodes _ => nodes |> CopyOnWriteArray.every (fun node => every f node)
    | Entry key value => f key value
    | Empty => true
  };

  let rec forEach (f: int => 'a => unit) (map: t 'a): unit => switch map {
    | Level _ nodes _ =>
        let f map => forEach f map;
        nodes |> CopyOnWriteArray.forEach f;
    | Entry key value => f key value;
    | Empty => ();
  };

  let rec none (f: int => 'a => bool) (map: t 'a): bool => switch map {
    | Level _ nodes _ => nodes |> CopyOnWriteArray.every (fun node => none f node)
    | Entry key value => f key value |> not
    | Empty => true
  };

  let rec reduce (f: 'acc => int => 'a => 'acc) (acc: 'acc) (map: t 'a): 'acc => switch map {
    | Level _ nodes _ =>
        let reducer acc map => reduce f acc map;
        nodes |> CopyOnWriteArray.reduce reducer acc;
    | Entry key value => f acc key value;
    | Empty => acc;
  };

  let rec some (f: int => 'a => bool) (map: t 'a): bool => switch map {
    | Level _ nodes _ => nodes |> CopyOnWriteArray.some (fun node => some f node)
    | Entry key value => f key value
    | Empty => false
  };

  let rec toSeq (map: t 'a): (Seq.t (int, 'a)) => switch map {
    | Entry key value => Seq.return (key, value)
    | Level _ nodes _ => nodes |> CopyOnWriteArray.toSeq |> Seq.flatMap toSeq
    | Empty => Seq.empty;
  };

  let rec tryFind (f: int => 'a => bool) (map: t 'a): (option (int, 'a)) => switch map {
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

  let rec tryGet (depth: int) (key: int) (map: t 'a): (option 'a) => switch map {
    | Level bitmap nodes _ =>
        let bit = BitmapTrie.bitPos key depth;
        let index = BitmapTrie.index bitmap bit;

        if (BitmapTrie.containsNode bitmap bit) (tryGet (depth + 1) key nodes.(index))
        else None;
    | Entry entryKey entryValue when key == entryKey => Some entryValue
    | _ => None
  };

  let rec values (map: t 'a): (Seq.t 'a) => switch map {
    | Entry _ value => Seq.return value
    | Level _ nodes _ => nodes |> CopyOnWriteArray.toSeq |> Seq.flatMap values
    | Empty => Seq.empty;
  };
};

type t 'a = {
  count: int,
  root: (BitmapTrieIntMap.t 'a),
};

let empty: t 'a = { count: 0, root: BitmapTrieIntMap.Empty };

let alter (key: int) (f: option 'a => option 'a) ({ count, root } as map: t 'a): (t 'a) => {
  let alterResult = ref BitmapTrieIntMap.NoChange;
  let newRoot = root |> BitmapTrieIntMap.alter
    BitmapTrieIntMap.updateLevelNodePersistent
    Transient.Owner.none
    alterResult
    0
    key
    f;

  switch !alterResult {
    | BitmapTrieIntMap.Added => { count: count + 1, root: newRoot }
    | BitmapTrieIntMap.NoChange => map
    | BitmapTrieIntMap.Replace => { count, root: newRoot }
    | BitmapTrieIntMap.Removed => { count: count - 1, root: newRoot }
  }
};

let containsKey (key: int) ({ root }: t 'a): bool =>
  root |> BitmapTrieIntMap.containsKey 0 key;

let containsWith (equality: Equality.t 'a) (key: int) (value: 'a) ({ root }: t 'a): bool =>
  root |> BitmapTrieIntMap.containsWith equality 0 key value;

let contains (key: int) (value: 'a) (map: t 'a): bool =>
  map |> containsWith Equality.structural key value;

let count ({ count }: t 'a): int => count;

let every (f: int => 'a => bool) ({ root }: t 'a): bool =>
  root |> BitmapTrieIntMap.every f;

let find (f: int => 'a => bool) ({ root }: t 'a): (int, 'a) =>
  root |> BitmapTrieIntMap.tryFind f |> Option.first;

let forEach (f: int => 'a => unit) ({ root }: t 'a): unit =>
  root |> BitmapTrieIntMap.forEach f;

let get (key: int) ({ root }: t 'a): 'a =>
  root |> BitmapTrieIntMap.tryGet 0 key |> Option.first;

let isEmpty ({ count }: t 'a): bool => count == 0;

let isNotEmpty ({ count }: t 'a): bool => count != 0;

let none (f: int => 'a => bool) ({ root }: t 'a): bool =>
  root |> BitmapTrieIntMap.none f;

let put (key: int) (value: 'a) (map: t 'a): (t 'a) =>
  map |> alter key (Functions.return @@ Option.return @@ value);

let reduce (f: 'acc => int => 'a => 'acc) (acc: 'acc) ({ root }: t 'a): 'acc =>
  root |> BitmapTrieIntMap.reduce f acc;

let remove (key: int) (map: t 'a): (t 'a) =>
  map |> alter key Functions.alwaysNone;

let removeAll (_: t 'a): (t 'a) => empty;

let some (f: int => 'a => bool) ({ root }: t 'a): bool =>
  root |> BitmapTrieIntMap.some f;

let toSeq ({ root }: t 'a): (Seq.t ((int, 'a))) =>
  root |> BitmapTrieIntMap.toSeq;

let tryFind (f: int => 'a => bool) ({ root }: t 'a): (option (int, 'a)) =>
  root |> BitmapTrieIntMap.tryFind f;

let tryGet (key: int) ({ root }: t 'a): (option 'a) =>
  root |> BitmapTrieIntMap.tryGet 0 key;

let values ({ root }: t 'a): (Seq.t 'a) =>
  root |> BitmapTrieIntMap.values;

let toMap (map: t 'a): (ImmMap.t int 'a) => {
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

let equals (this: t 'a) (that: t 'a): bool =>
  ImmMap.equals (toMap this) (toMap that);

let equalsWith (equality: Equality.t 'a) (this: t 'a) (that: t 'a): bool =>
  ImmMap.equalsWith equality (toMap this) (toMap that);

let hash (map: t 'a): int =>
  map |> toMap |> ImmMap.hash;

let hashWith (hash: Hash.t 'a) (map: t 'a): int =>
  map |> toMap |> ImmMap.hashWith Hash.structural hash;

let keys (map: t 'a): (ImmSet.t int) =>
  map |> toMap |> ImmMap.keys;

let toSet (map: t 'a): (ImmSet.t (int, 'a)) =>
  map |> toMap |> ImmMap.toSet;

let toSetWith (equality: Equality.t 'a) (map: t 'a): (ImmSet.t (int, 'a)) =>
  map |> toMap |> ImmMap.toSetWith equality;

let module TransientIntMap = {
  type intMap 'a = t 'a;
  type t 'a = Transient.t (intMap 'a);

  let mutate (map: intMap 'a): (t 'a) => Transient.create map;

  let alter
      (key: int)
      (f: option 'a => option 'a)
      (transient: t 'a): (t 'a) =>
    transient |> Transient.update (fun owner ({ count, root } as map) => {
      let alterResult = ref BitmapTrieIntMap.NoChange;
      let newRoot = root |> BitmapTrieIntMap.alter
        BitmapTrieIntMap.updateLevelNodeTransient
          owner
          alterResult
          0
          key
          f;

      switch !alterResult {
        | BitmapTrieIntMap.Added => { count: count + 1, root: newRoot }
        | BitmapTrieIntMap.NoChange => map
        | BitmapTrieIntMap.Replace => if (root === newRoot) map else { count, root: newRoot }
        | BitmapTrieIntMap.Removed => { count: count - 1, root: newRoot }
      }
    });

  let count (transient: t 'a): int =>
    transient |> Transient.get |> count;

  let persistentEmpty = empty;
  let empty (): t 'a =>
    empty |> mutate;

  let isEmpty (transient: t 'a): bool =>
    transient |> Transient.get |> isEmpty;

  let isNotEmpty (transient: t 'a): bool =>
    transient |> Transient.get |> isNotEmpty;

  let persist (transient: t 'a): (intMap 'a) =>
    transient |> Transient.persist;

  let put (key: int) (value: 'a) (transient: t 'a): (t 'a) =>
    transient |> alter key (Functions.return @@ Option.return @@ value);

  let putAll
      (seq: Seq.t (int, 'a))
      (transient: t 'a): (t 'a) => seq
    |> Seq.reduce (fun acc (k, v) => acc |> put k v) transient;

  let remove (key: int) (transient: t 'a): (t 'a) =>
    transient |> alter key Functions.alwaysNone;

  let removeAll (transient: t 'a): (t 'a) =>
      transient |> Transient.update (fun _ _ => persistentEmpty);

  let tryGet (key: int) (transient: t 'a): (option 'a) =>
    transient |> Transient.get |> (tryGet key);
};

let mutate = TransientIntMap.mutate;

let putAll (seq: Seq.t (int, 'a)) (map: t 'a): (t 'a) => map
  |> mutate
  |> TransientIntMap.putAll seq
  |> TransientIntMap.persist;

let map (f: int => 'a => 'b) (map: t 'a): (t 'b) => map
  |> reduce
    (fun acc key value => acc |> TransientIntMap.put key (f key value))
    (mutate empty)
  |> TransientIntMap.persist;

let fromSeq (seq: Seq.t (int, 'a)): (t 'a) => putAll seq empty;

let fromMap (map: ImmMap.t int 'a): (t 'a) => map
  |> ImmMap.reduce (fun acc k v => acc |> TransientIntMap.put k v) (mutate empty)
  |> TransientIntMap.persist;

let merge
    (f: int => (option 'vAcc) => (option 'v) => (option 'vAcc))
    (next: t 'v)
    (map: t 'vAcc): (t 'vAcc) =>
  ImmSet.union (keys map) (keys next)
    |> Seq.reduce (
        fun acc key => {
          let result = f key (map |> tryGet key) (next |> tryGet key);
          switch result {
            | None => acc |> TransientIntMap.remove key
            | Some value => acc |> TransientIntMap.put key value
          }
        }
      )
      (mutate map)
    |> TransientIntMap.persist;
