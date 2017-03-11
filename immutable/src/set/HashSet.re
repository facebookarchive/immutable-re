type bitmapTrieSet 'a =
  | Level int32 (array (bitmapTrieSet 'a)) (option Transient.Owner.t)
  | ComparatorCollision int (AVLTreeSet.t 'a)
  | EqualitySetCollision int (EqualitySet.t 'a)
  | Entry int 'a
  | Empty;

let module BitmapTrieSet = {
  let rec add
      (hashStrategy: HashStrategy.t 'a)
      (updateLevelNode: int => (bitmapTrieSet 'a) => (bitmapTrieSet 'a) => (bitmapTrieSet 'a))
      (owner: option Transient.Owner.t)
      (depth: int)
      (hash: int)
      (value: 'a)
      (set: bitmapTrieSet 'a): (bitmapTrieSet 'a) => switch set {
    | Level bitmap nodes _ =>
        let bit = BitmapTrie.bitPos hash depth;
        let index = BitmapTrie.index bitmap bit;

        if (BitmapTrie.containsNode bitmap bit) {
          let childNode = nodes.(index);
          let newChildNode = childNode |> add hashStrategy updateLevelNode owner (depth + 1) hash value;

          if (childNode === newChildNode) set
          else (updateLevelNode index newChildNode set)
        } else {
          let entry = Entry hash value;
          let nodes = nodes |> CopyOnWriteArray.insertAt index entry;
          Level (Int32.logor bitmap bit) nodes owner;
        }
    | EqualitySetCollision entryHash entrySet when hash == entryHash =>
        let newEntrySet = entrySet |> EqualitySet.add (HashStrategy.equals hashStrategy) value;
        if (newEntrySet === entrySet) set else (EqualitySetCollision entryHash newEntrySet);
    | EqualitySetCollision entryHash entrySet =>
        let bitmap = BitmapTrie.bitPos entryHash depth;
        Level bitmap [| set |] owner |> add hashStrategy updateLevelNode owner depth hash value;
    | ComparatorCollision entryHash entrySet when hash == entryHash =>
        let newEntrySet = entrySet |> AVLTreeSet.add (HashStrategy.comparator hashStrategy) value;
        if (newEntrySet === entrySet) set else (ComparatorCollision entryHash newEntrySet);
    | ComparatorCollision entryHash entrySet =>
        let bitmap = BitmapTrie.bitPos entryHash depth;
        Level bitmap [| set |] owner |> add hashStrategy updateLevelNode owner depth hash value;
    | Entry entryHash entryValue when hash == entryHash =>
        if ((HashStrategy.comparator hashStrategy value entryValue) === Equal) set
        else (switch hashStrategy {
          | Comparator _ comparator =>
              let set = AVLTreeSet.Empty
                |> AVLTreeSet.add (HashStrategy.comparator hashStrategy) entryValue
                |> AVLTreeSet.add (HashStrategy.comparator hashStrategy) value;
              ComparatorCollision entryHash set;
          | Equality _ equals =>
            let set = EqualitySet.empty
              |> EqualitySet.add (HashStrategy.equals hashStrategy) entryValue
              |> EqualitySet.add (HashStrategy.equals hashStrategy) value;
            EqualitySetCollision entryHash set;
        });
    | Entry entryHash entryValue =>
        let bitmap = BitmapTrie.bitPos entryHash depth;
        Level bitmap [| set |] owner |> add hashStrategy updateLevelNode owner depth hash value;
    | Empty => Entry hash value;
  };

  let rec contains
      (hashStrategy: HashStrategy.t 'a)
      (depth: int)
      (hash: int)
      (value: 'a)
      (set: bitmapTrieSet 'a): bool => switch set {
    | Level bitmap nodes _ =>
        let bit = BitmapTrie.bitPos hash depth;
        let index = BitmapTrie.index bitmap bit;

        (BitmapTrie.containsNode bitmap bit) &&
        (contains hashStrategy (depth + 1) hash value nodes.(index));
    | EqualitySetCollision entryHash entrySet =>
        (hash == entryHash) && (EqualitySet.contains (HashStrategy.equals hashStrategy) value entrySet);
    | ComparatorCollision entryHash entrySet =>
        (hash == entryHash) && (AVLTreeSet.contains (HashStrategy.comparator hashStrategy) value entrySet);
    | Entry entryHash entryValue =>
        (hash == entryHash) && ((HashStrategy.comparator hashStrategy entryValue value) === Equal);
    | Empty => false;
  };

  let rec remove
      (hashStrategy: HashStrategy.t 'a)
      (updateLevelNode: int => (bitmapTrieSet 'a) => (bitmapTrieSet 'a) => (bitmapTrieSet 'a))
      (owner: option Transient.Owner.t)
      (depth: int)
      (hash: int)
      (value: 'a)
      (set: bitmapTrieSet 'a): (bitmapTrieSet 'a) => switch set {
    | Level bitmap nodes _ =>
        let bit = BitmapTrie.bitPos hash depth;
        let index = BitmapTrie.index bitmap bit;

        if (BitmapTrie.containsNode bitmap bit) {
          let childNode = nodes.(index);
          let newChildNode = childNode |> remove hashStrategy updateLevelNode owner (depth + 1) hash value;

          if (newChildNode === childNode) set
          else if (newChildNode === Empty) {
            let nodes = nodes |> CopyOnWriteArray.removeAt index;

            if (CopyOnWriteArray.count nodes > 0) (Level (Int32.logxor bitmap bit) nodes owner)
            else Empty;
          } else (updateLevelNode index newChildNode set);
        } else set;
    | EqualitySetCollision entryHash entrySet when hash == entryHash =>
        let newEntrySet = entrySet |> EqualitySet.remove (HashStrategy.equals hashStrategy) value;

        if (newEntrySet === entrySet) set
        else if ((EqualitySet.count newEntrySet) == 1) {
          let entryValue = entrySet |> EqualitySet.toSeq |> Seq.first;
          (Entry entryHash entryValue)
        } else (EqualitySetCollision entryHash newEntrySet);
    | ComparatorCollision entryHash entrySet when hash == entryHash =>
        let newEntrySet = entrySet |> AVLTreeSet.remove (HashStrategy.comparator hashStrategy) value;

        if (newEntrySet === entrySet) set else (switch newEntrySet {
          | Leaf entryValue => (Entry entryHash entryValue)
          | _ => (ComparatorCollision entryHash newEntrySet)
        });
    | Entry entryHash entryValue when (hash == entryHash) && ((HashStrategy.comparator hashStrategy entryValue value) === Equal) =>
        Empty;
    | _ => set
  };

  let rec toSeq (set: bitmapTrieSet 'a): (Seq.t 'a) => switch set {
    | Level _ nodes _ => nodes |> CopyOnWriteArray.toSeq |> Seq.flatMap toSeq
    | ComparatorCollision _ entrySet => AVLTreeSet.toSeq entrySet;
    | EqualitySetCollision _ entrySet => EqualitySet.toSeq entrySet;
    | Entry _ entryValue => Seq.return entryValue;
    | Empty => Seq.empty;
  };
};

type hashSet 'a = {
  count: int,
  root: bitmapTrieSet 'a,
  strategy: HashStrategy.t 'a,
};

let updateLevelNodePersistent
    (index: int)
    (childNode: bitmapTrieSet 'a)
    ((Level bitmap nodes _): (bitmapTrieSet 'a)): (bitmapTrieSet 'a) =>
  Level bitmap (nodes |> CopyOnWriteArray.update index childNode) None;

let add (value: 'a) ({ count, root, strategy } as set: hashSet 'a): (hashSet 'a) => {
  let hash = HashStrategy.hash strategy value;
  let newRoot = root |> BitmapTrieSet.add strategy updateLevelNodePersistent None 0 hash value;
  if (newRoot === root) set
  else { count: count + 1, root: newRoot, strategy };
};

let contains (value: 'a) ({ root, strategy }: hashSet 'a): bool => {
  let hash = HashStrategy.hash strategy value;
  root |> BitmapTrieSet.contains strategy 0 hash value;
};

let count ({ count }: hashSet 'a): int => count;

let empty: (hashSet 'a) = {
  count: 0,
  root: Empty,
  strategy: HashStrategy.structuralCompare,
};

let emptyWith (strategy: HashStrategy.t 'a): (hashSet 'a) => {
  count: 0,
  root: Empty,
  strategy,
};

let isEmpty ({ count }: hashSet 'a): bool => count == 0;

let isNotEmpty ({ count }: hashSet 'a): bool => count != 0;

let remove (value: 'a) ({ count, root, strategy } as set: hashSet 'a): (hashSet 'a) => {
  let hash = HashStrategy.hash strategy value;
  let newRoot = root |> BitmapTrieSet.remove strategy updateLevelNodePersistent None 0 hash value;
  if (newRoot === root) set
  else { count: count - 1, root: newRoot, strategy };
};

let removeAll ({ strategy }: hashSet 'a): (hashSet 'a) =>
  emptyWith strategy;

let toSeq ({ root }: hashSet 'a): (Seq.t 'a) => root |> BitmapTrieSet.toSeq;

let every (f: 'a => bool) (set: hashSet 'a): bool =>
  set |> toSeq |> Seq.every f;

let find (f: 'a => bool) (set: hashSet 'a): 'a =>
  set |> toSeq |> Seq.find f;

let forEach (f: 'a => unit) (set: hashSet 'a): unit =>
  set |> toSeq |> Seq.forEach f;

let none (f: 'a => bool) (set: hashSet 'a): bool =>
  set |> toSeq |> Seq.none f;

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) (set: hashSet 'a): 'acc =>
  set |> toSeq |> Seq.reduce f acc;

let some (f: 'a => bool) (set: hashSet 'a): bool =>
  set |> toSeq |> Seq.some f;

let tryFind (f: 'a => bool) (set: hashSet 'a): (option 'a) =>
  set |> toSeq |> Seq.tryFind f;

let toSet (set: hashSet 'a): (ImmSet.t 'a) => {
  contains: fun v => contains v set,
  count: count set,
  every: fun f => set |> every f,
  find: fun f => set |> find f,
  forEach: fun f => set |> forEach f,
  none: fun f => set |> none f,
  reduce: fun f acc => set |> reduce f acc,
  some: fun f => set |> some f,
  toSeq: toSeq set,
  tryFind: fun f => set |> tryFind f,
};

let equals (this: hashSet 'a) (that: hashSet 'a): bool =>
  ImmSet.equals (toSet this) (toSet that);

let hash ({ strategy } as set: hashSet 'a): int =>
  set |> toSet |> ImmSet.hashWith (HashStrategy.hash strategy);

let toMap (set: hashSet 'a): (ImmMap.t 'a 'a) =>
  set |> toSet |> ImmMap.ofSet;

type transientHashSet 'a = Transient.t (hashSet 'a);

let mutate (set: hashSet 'a): (transientHashSet 'a) =>
  Transient.create set;

let module TransientHashSet = {
  let updateLevelNodeTransient
      (owner: Transient.Owner.t)
      (index: int)
      (childNode: bitmapTrieSet 'a)
      ((Level bitmap nodes nodeOwner) as node: (bitmapTrieSet 'a)): (bitmapTrieSet 'a) => switch nodeOwner {
    | Some nodeOwner when nodeOwner === owner =>
        nodes.(index) = childNode;
        node
    | _ => Level bitmap (nodes |> CopyOnWriteArray.update index childNode) (Some owner)
  };

  let add (value: 'a) (transient: transientHashSet 'a): (transientHashSet 'a) =>
    transient |> Transient.update (fun owner ({ count, root, strategy } as set) => {
      let hash = HashStrategy.hash strategy value;
      if (set |> contains value) set
      else {
        let newRoot = root |> BitmapTrieSet.add strategy (updateLevelNodeTransient owner) (Some owner) 0 hash value;
        { count: count + 1, root: newRoot, strategy };
      }
    });

  let addAll (seq: Seq.t 'a) (transient: transientHashSet 'a): (transientHashSet 'a) =>
    transient |> Transient.update (fun owner ({ count, root, strategy } as set) => {
      let newCount = ref count;

      let newRoot = seq |> Seq.reduce (fun acc value => {
        let hash = HashStrategy.hash strategy value;

        if (acc |> BitmapTrieSet.contains strategy 0 hash value) acc
        else  {
          let newRoot = acc
            |> BitmapTrieSet.add strategy (updateLevelNodeTransient owner) (Some owner) 0 hash value;
          newCount := !newCount + 1;
          newRoot
        }
      }) root;

      if (!newCount == count) set
      else { count: !newCount, root: newRoot, strategy };
    });

  let contains (value: 'a) (transient: transientHashSet 'a): bool =>
    transient |> Transient.get |> contains value;

  let count (transient: transientHashSet 'a): int =>
    transient |> Transient.get |> count;

  let empty (): (transientHashSet 'a) =>
    empty |> mutate;

  let persistentEmptyWith = emptyWith;

  let emptyWith (strategy: HashStrategy.t 'a): (transientHashSet 'a) =>
    emptyWith strategy |> mutate;

  let isEmpty (transient: transientHashSet 'a): bool =>
    transient |> Transient.get |> isEmpty;

  let isNotEmpty (transient: transientHashSet 'a): bool =>
    transient |> Transient.get |> isNotEmpty;

  let persist (transient: transientHashSet 'a): (hashSet 'a) =>
    transient |> Transient.persist;

  let remove (value: 'a) (transient: transientHashSet 'a): (transientHashSet 'a) =>
    transient |> Transient.update (fun owner ({ count, root, strategy } as set) => {
      let hash = HashStrategy.hash strategy value;
      let newRoot = root |> BitmapTrieSet.remove strategy (updateLevelNodeTransient owner) None 0 hash value;
      if (newRoot === root) set
      else { count: count - 1, root: newRoot, strategy };
    });

  let removeAll  (transient: transientHashSet 'a): (transientHashSet 'a) =>
    transient |> Transient.update (fun owner ({ strategy }) => persistentEmptyWith strategy);
};

let addAll (seq: Seq.t 'a) (set: hashSet 'a): (hashSet 'a) =>
  set |> mutate |> TransientHashSet.addAll seq |> TransientHashSet.persist;

let fromSeq (seq: Seq.t 'a): (hashSet 'a) =>
  empty |> addAll seq;

let fromSeqWith (strategy: HashStrategy.t 'a) (seq: Seq.t 'a): (hashSet 'a) =>
  emptyWith strategy |> addAll seq;

let intersect ({ strategy } as this: hashSet 'a) (that: hashSet 'a): (hashSet 'a) =>
  ImmSet.intersect (toSet this) (toSet that) |> fromSeqWith strategy;

let subtract ({ strategy } as this: hashSet 'a) (that: hashSet 'a): (hashSet 'a) =>
  ImmSet.subtract (toSet this) (toSet that) |> fromSeqWith strategy;

let union ({ strategy } as this: hashSet 'a) (that: hashSet 'a): (hashSet 'a) =>
  ImmSet.union (toSet this) (toSet that) |> fromSeqWith strategy;
