let module BitmapTrieSet = {
  type t 'a =
    | Level int32 (array (t 'a)) Transient.Owner.t
    | ComparatorCollision int (AVLTreeSet.t 'a)
    | EqualitySetCollision int (EqualitySet.t 'a)
    | Entry int 'a
    | Empty;

  type updateLevelNode 'a = Transient.Owner.t => int => (t 'a) => (t 'a) => (t 'a);

  let rec add
      (hashStrategy: HashStrategy.t 'a)
      (updateLevelNode: updateLevelNode 'a)
      (owner: Transient.Owner.t)
      (depth: int)
      (hash: int)
      (value: 'a)
      (set: t 'a): (t 'a) => switch set {
    | Level bitmap nodes _ =>
        let bit = BitmapTrie.bitPos hash depth;
        let index = BitmapTrie.index bitmap bit;

        if (BitmapTrie.containsNode bitmap bit) {
          let childNode = nodes.(index);
          let newChildNode = childNode |> add hashStrategy updateLevelNode owner (depth + 1) hash value;

          if (childNode === newChildNode) set
          else (updateLevelNode owner index newChildNode set)
        } else {
          let entry = Entry hash value;
          let nodes = nodes |> CopyOnWriteArray.insertAt index entry;
          Level (Int32.logor bitmap bit) nodes owner;
        }
    | EqualitySetCollision entryHash entrySet when hash == entryHash =>
        let newEntrySet = entrySet |> EqualitySet.add (HashStrategy.equals hashStrategy) value;
        if (newEntrySet === entrySet) set else (EqualitySetCollision entryHash newEntrySet);
    | EqualitySetCollision entryHash _ =>
        let bitmap = BitmapTrie.bitPos entryHash depth;
        Level bitmap [| set |] owner |> add hashStrategy updateLevelNode owner depth hash value;
    | ComparatorCollision entryHash entrySet when hash == entryHash =>
        let newEntrySet = entrySet |> AVLTreeSet.add (HashStrategy.comparator hashStrategy) value;
        if (newEntrySet === entrySet) set else (ComparatorCollision entryHash newEntrySet);
    | ComparatorCollision entryHash _ =>
        let bitmap = BitmapTrie.bitPos entryHash depth;
        Level bitmap [| set |] owner |> add hashStrategy updateLevelNode owner depth hash value;
    | Entry entryHash entryValue when hash == entryHash =>
        if ((HashStrategy.comparator hashStrategy value entryValue) === Ordering.equal) set
        else (switch hashStrategy {
          | HashStrategy.Comparator _ _ =>
              let set = AVLTreeSet.Empty
                |> AVLTreeSet.add (HashStrategy.comparator hashStrategy) entryValue
                |> AVLTreeSet.add (HashStrategy.comparator hashStrategy) value;
              ComparatorCollision entryHash set;
          | HashStrategy.Equality _ _ =>
            let set = EqualitySet.empty
              |> EqualitySet.add (HashStrategy.equals hashStrategy) entryValue
              |> EqualitySet.add (HashStrategy.equals hashStrategy) value;
            EqualitySetCollision entryHash set;
        });
    | Entry entryHash _ =>
        let bitmap = BitmapTrie.bitPos entryHash depth;
        Level bitmap [| set |] owner |> add hashStrategy updateLevelNode owner depth hash value;
    | Empty => Entry hash value;
  };

  let rec contains
      (hashStrategy: HashStrategy.t 'a)
      (depth: int)
      (hash: int)
      (value: 'a)
      (set: t 'a): bool => switch set {
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
        (hash == entryHash) && ((HashStrategy.comparator hashStrategy entryValue value) === Ordering.equal);
    | Empty => false;
  };

  let rec remove
      (hashStrategy: HashStrategy.t 'a)
      (updateLevelNode: updateLevelNode 'a)
      (owner: Transient.Owner.t)
      (depth: int)
      (hash: int)
      (value: 'a)
      (set: t 'a): (t 'a) => switch set {
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
          } else (updateLevelNode owner index newChildNode set);
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
          | AVLTreeSet.Leaf entryValue => (Entry entryHash entryValue)
          | _ => (ComparatorCollision entryHash newEntrySet)
        });
    | Entry entryHash entryValue when (hash == entryHash) && ((HashStrategy.comparator hashStrategy entryValue value) === Ordering.equal) =>
        Empty;
    | _ => set
  };

  let rec toSeq (set: t 'a): (Seq.t 'a) => switch set {
    | Level _ nodes _ => nodes |> CopyOnWriteArray.toSeq |> Seq.flatMap toSeq
    | ComparatorCollision _ entrySet => AVLTreeSet.toSeq entrySet;
    | EqualitySetCollision _ entrySet => EqualitySet.toSeq entrySet;
    | Entry _ entryValue => Seq.return entryValue;
    | Empty => Seq.empty;
  };

  let updateLevelNodePersistent
      (_: Transient.Owner.t)
      (index: int)
      (childNode: t 'a)
      (Level bitmap nodes _: (t 'a)): (t 'a) =>
    Level bitmap (CopyOnWriteArray.update index childNode nodes) Transient.Owner.none;

  let updateLevelNodeTransient
      (owner: Transient.Owner.t)
      (index: int)
      (childNode: t 'a)
      (Level bitmap nodes nodeOwner as node: (t 'a)): (t 'a) =>
    if (nodeOwner === owner) {
      nodes.(index) = childNode;
      node
    }
    else Level bitmap (CopyOnWriteArray.update index childNode nodes) owner
};

type t 'a = {
  count: int,
  root: BitmapTrieSet.t 'a,
  strategy: HashStrategy.t 'a,
};

let add (value: 'a) ({ count, root, strategy } as set: t 'a): (t 'a) => {
  let hash = HashStrategy.hash strategy value;
  let newRoot = root |> BitmapTrieSet.add
    strategy
    BitmapTrieSet.updateLevelNodePersistent
    Transient.Owner.none
    0
    hash
    value;

  if (newRoot === root) set
  else { count: count + 1, root: newRoot, strategy };
};

let contains (value: 'a) ({ root, strategy }: t 'a): bool => {
  let hash = HashStrategy.hash strategy value;
  root |> BitmapTrieSet.contains strategy 0 hash value;
};

let count ({ count }: t 'a): int => count;

let empty: (t 'a) = {
  count: 0,
  root: BitmapTrieSet.Empty,
  strategy: HashStrategy.structuralCompare,
};

let emptyWith (strategy: HashStrategy.t 'a): (t 'a) => {
  count: 0,
  root: BitmapTrieSet.Empty,
  strategy,
};

let isEmpty ({ count }: t 'a): bool => count == 0;

let isNotEmpty ({ count }: t 'a): bool => count != 0;

let remove (value: 'a) ({ count, root, strategy } as set: t 'a): (t 'a) => {
  let hash = HashStrategy.hash strategy value;
  let newRoot = root |> BitmapTrieSet.remove
    strategy
    BitmapTrieSet.updateLevelNodePersistent
    Transient.Owner.none
    0
    hash
    value;

  if (newRoot === root) set
  else { count: count - 1, root: newRoot, strategy };
};

let removeAll ({ strategy }: t 'a): (t 'a) =>
  emptyWith strategy;

let toSeq ({ root } as set: t 'a): (Seq.t 'a) =>
  if (isEmpty set) Seq.empty
  else root |> BitmapTrieSet.toSeq;

let every (f: 'a => bool) (set: t 'a): bool =>
  set |> toSeq |> Seq.every f;

let find (f: 'a => bool) (set: t 'a): 'a =>
  set |> toSeq |> Seq.find f;

let forEach (f: 'a => unit) (set: t 'a): unit =>
  set |> toSeq |> Seq.forEach f;

let none (f: 'a => bool) (set: t 'a): bool =>
  set |> toSeq |> Seq.none f;

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) (set: t 'a): 'acc =>
  set |> toSeq |> Seq.reduce f acc;

let some (f: 'a => bool) (set: t 'a): bool =>
  set |> toSeq |> Seq.some f;

let tryFind (f: 'a => bool) (set: t 'a): (option 'a) =>
  set |> toSeq |> Seq.tryFind f;

let toIterable (set: t 'a): (Iterable.t 'a) =>
  if (isEmpty set) Iterable.empty
  else { reduce: fun f acc => reduce f acc set };

let toKeyedIterable (set: t 'a): (KeyedIterable.t 'a 'a) =>
  if (isEmpty set) KeyedIterable.empty
  else { reduce: fun f acc => set |> reduce
    (fun acc next => f acc next next)
    acc
  };

let toSet (set: t 'a): (ImmSet.t 'a) =>
  if (isEmpty set) ImmSet.empty
  else {
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

let equals (this: t 'a) (that: t 'a): bool =>
  ImmSet.equals (toSet this) (toSet that);

let hash ({ strategy } as set: t 'a): int =>
  set |> toSet |> ImmSet.hashWith (HashStrategy.hash strategy);

let toMap (set: t 'a): (ImmMap.t 'a 'a) =>
  set |> toSet |> ImmMap.ofSet;

let module TransientHashSet = {
  type hashSet 'a = t 'a;

  type t 'a = Transient.t (hashSet 'a);

  let mutate (set: hashSet 'a): (t 'a) =>
    Transient.create set;

  let addImpl
      (owner: Transient.Owner.t)
      (value: 'a)
      ({ count, root, strategy } as set: hashSet 'a): (hashSet 'a) => {
    let hash = HashStrategy.hash strategy value;
    if (set |> contains value) set
    else {
      let newRoot = root |> BitmapTrieSet.add
        strategy
        BitmapTrieSet.updateLevelNodeTransient
        owner
        0
        hash
        value;

      { count: count + 1, root: newRoot, strategy };
    }
  };

  let add (value: 'a) (transient: t 'a): (t 'a) =>
    transient |> Transient.update1 addImpl value;

  let addAllImpl
      (owner: Transient.Owner.t)
      (iter: Iterable.t 'a)
      ({ count, root, strategy } as set: hashSet 'a): (hashSet 'a) => {
    let newCount = ref count;

    let newRoot = iter |> Iterable.reduce (fun acc value => {
      let hash = HashStrategy.hash strategy value;

      if (acc |> BitmapTrieSet.contains strategy 0 hash value) acc
      else  {
        let newRoot = acc |> BitmapTrieSet.add
          strategy
          BitmapTrieSet.updateLevelNodeTransient
          owner
          0
          hash
          value;

        newCount := !newCount + 1;
        newRoot
      }
    }) root;

    if (!newCount == count) set
    else { count: !newCount, root: newRoot, strategy };
  };

  let addAll (iter: Iterable.t 'a) (transient: t 'a): (t 'a) =>
    transient |> Transient.update1 addAllImpl iter;

  let contains (value: 'a) (transient: t 'a): bool =>
    transient |> Transient.get |> contains value;

  let count (transient: t 'a): int =>
    transient |> Transient.get |> count;

  let empty (): (t 'a) =>
    empty |> mutate;

  let persistentEmptyWith = emptyWith;

  let emptyWith (strategy: HashStrategy.t 'a): (t 'a) =>
    emptyWith strategy |> mutate;

  let isEmpty (transient: t 'a): bool =>
    transient |> Transient.get |> isEmpty;

  let isNotEmpty (transient: t 'a): bool =>
    transient |> Transient.get |> isNotEmpty;

  let persist (transient: t 'a): (hashSet 'a) =>
    transient |> Transient.persist;

  let removeImpl
      (owner: Transient.Owner.t)
      (value: 'a)
      ({ count, root, strategy } as set: hashSet 'a): (hashSet 'a) => {
    let hash = HashStrategy.hash strategy value;
    let newRoot = root |> BitmapTrieSet.remove
      strategy
      BitmapTrieSet.updateLevelNodeTransient
      owner
      0
      hash
      value;

    if (newRoot === root) set
    else { count: count - 1, root: newRoot, strategy };
  };

  let remove (value: 'a) (transient: t 'a): (t 'a) =>
    transient |> Transient.update1 removeImpl value;

  let removeAllImpl
      (_: Transient.Owner.t)
      ({ strategy }: hashSet 'a): (hashSet 'a) =>
    persistentEmptyWith strategy;

  let removeAll (transient: t 'a): (t 'a) =>
    transient |> Transient.update removeAllImpl;
};

let mutate = TransientHashSet.mutate;

let addAll (iter: Iterable.t 'a) (set: t 'a): (t 'a) =>
  set |> mutate |> TransientHashSet.addAll iter |> TransientHashSet.persist;

let from (iter: Iterable.t 'a): (t 'a) =>
  empty |> addAll iter;

let fromWith (strategy: HashStrategy.t 'a) (iter: Iterable.t 'a): (t 'a) =>
  emptyWith strategy |> addAll iter;

let intersect ({ strategy } as this: t 'a) (that: t 'a): (t 'a) =>
  /* FIXME: Makes this more efficient */
  ImmSet.intersect (toSet this) (toSet that) |> fromWith strategy;

let subtract ({ strategy } as this: t 'a) (that: t 'a): (t 'a) =>
  /* FIXME: Makes this more efficient */
  ImmSet.subtract (toSet this) (toSet that) |> fromWith strategy;

let union ({ strategy } as this: t 'a) (that: t 'a): (t 'a) =>
  /* FIXME: Makes this more efficient */
  ImmSet.union (toSet this) (toSet that) |> fromWith strategy;
