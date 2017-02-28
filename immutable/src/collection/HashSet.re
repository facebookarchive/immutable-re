open Collection;
open Comparator;
open EqualitySet;
open Hash;
open HashStrategy;
open Keyed;
open Ordering;
open Seq;
open SortedSet;
open Transient;

type bitmapTrieSet 'a =
  | Level int32 (array (bitmapTrieSet 'a)) (option owner)
  | SortedSetCollision int (sortedSet 'a)
  | EqualitySetCollision int (equalitySet 'a)
  | Entry int 'a
  | Empty;

let module BitmapTrieSet = {
  let rec add
      (hashStrategy: hashStrategy 'a)
      (updateLevelNode: int => (bitmapTrieSet 'a) => (bitmapTrieSet 'a) => (bitmapTrieSet 'a))
      (owner: option owner)
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
        let newEntrySet = entrySet |> EqualitySet.add value;
        if (newEntrySet === entrySet) set else (EqualitySetCollision entryHash newEntrySet);
    | EqualitySetCollision entryHash entrySet =>
        let bitmap = BitmapTrie.bitPos entryHash depth;
        Level bitmap [| set |] owner |> add hashStrategy updateLevelNode owner depth hash value;
    | SortedSetCollision entryHash entrySet when hash == entryHash =>
        let newEntrySet = entrySet |> SortedSet.add value;
        if (newEntrySet === entrySet) set else (SortedSetCollision entryHash newEntrySet);
    | SortedSetCollision entryHash entrySet =>
        let bitmap = BitmapTrie.bitPos entryHash depth;
        Level bitmap [| set |] owner |> add hashStrategy updateLevelNode owner depth hash value;
    | Entry entryHash entryValue when hash == entryHash =>
        if ((HashStrategy.comparator hashStrategy value entryValue) === Equal) set
        else (switch hashStrategy {
          | Comparator _ comparator =>
              let set = SortedSet.emptyWith comparator
                |> SortedSet.add entryValue
                |> SortedSet.add value;
              SortedSetCollision entryHash set;
          | Equality _ equals =>
            let set = EqualitySet.emptyWith equals
              |> EqualitySet.add entryValue
              |> EqualitySet.add value;
            EqualitySetCollision entryHash set;
        });
    | Entry entryHash entryValue =>
        let bitmap = BitmapTrie.bitPos entryHash depth;
        Level bitmap [| set |] owner |> add hashStrategy updateLevelNode owner depth hash value;
    | Empty => Entry hash value;
  };

  let rec contains
      (hashStrategy: hashStrategy 'a)
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
        (hash == entryHash) && (EqualitySet.contains value entrySet);
    | SortedSetCollision entryHash entrySet =>
        (hash == entryHash) && (SortedSet.contains value entrySet);
    | Entry entryHash entryValue =>
        (hash == entryHash) && ((HashStrategy.comparator hashStrategy entryValue value) === Equal);
    | Empty => false;
  };

  let rec remove
      (hashStrategy: hashStrategy 'a)
      (updateLevelNode: int => (bitmapTrieSet 'a) => (bitmapTrieSet 'a) => (bitmapTrieSet 'a))
      (owner: option owner)
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
        let newEntrySet = entrySet |> EqualitySet.remove value;

        if (newEntrySet === entrySet) set
        else if ((EqualitySet.count newEntrySet) == 1) {
          let entryValue = entrySet |> EqualitySet.toSeq |> Seq.first;
          (Entry entryHash entryValue)
        } else (EqualitySetCollision entryHash newEntrySet);
    | SortedSetCollision entryHash entrySet when hash == entryHash =>
        let newEntrySet = entrySet |> SortedSet.remove value;

        if (newEntrySet === entrySet) set
        else if ((SortedSet.count newEntrySet) == 1) {
          let entryValue = SortedSet.first newEntrySet;
          (Entry entryHash entryValue)
        } else (SortedSetCollision entryHash newEntrySet);
    | Entry entryHash entryValue when (hash == entryHash) && ((HashStrategy.comparator hashStrategy entryValue value) === Equal) =>
        Empty;
    | _ => set
  };

  let rec toSeq (set: bitmapTrieSet 'a): (seq 'a) => switch set {
    | Level _ nodes _ => nodes |> CopyOnWriteArray.toSeq |> Seq.flatMap toSeq
    | SortedSetCollision _ entrySet => SortedSet.toSeq entrySet;
    | EqualitySetCollision _ entrySet => EqualitySet.toSeq entrySet;
    | Entry _ entryValue => Seq.return entryValue;
    | Empty => Seq.empty;
  };
};

type hashSet 'a = {
  count: int,
  root: bitmapTrieSet 'a,
  strategy: hashStrategy 'a,
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

let emptyWith (strategy: hashStrategy 'a): (hashSet 'a) => {
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

let toSeq ({ root }: hashSet 'a): (seq 'a) => root |> BitmapTrieSet.toSeq;

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

let toCollection (set: hashSet 'a): (collection 'a) => {
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
  Collection.equals (toCollection this) (toCollection that);

let hash ({ strategy } as set: hashSet 'a): int =>
  set |> toCollection |> Collection.hashWith (HashStrategy.hash strategy);

let toKeyed (set: hashSet 'a): (keyed 'a 'a) =>
  set |> toCollection |> Keyed.ofCollection;

type transientHashSet 'a = transient (hashSet 'a);

let module TransientHashSet = {
  let updateLevelNodeTransient
      (owner: owner)
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

  let addAll (seq: seq 'a) (transient: transientHashSet 'a): (transientHashSet 'a) =>
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
    transient |> Transient.update (fun owner ({ strategy }) => emptyWith strategy);
};

let mutate (set: hashSet 'a): (transientHashSet 'a) =>
  Transient.create set;

let addAll (seq: seq 'a) (set: hashSet 'a): (hashSet 'a) =>
  set |> mutate |> TransientHashSet.addAll seq |> TransientHashSet.persist;

let fromSeq (seq: seq 'a): (hashSet 'a) =>
  empty |> addAll seq;

let fromSeqWith (strategy: hashStrategy 'a) (seq: seq 'a): (hashSet 'a) =>
  emptyWith strategy |> addAll seq;

let intersect ({ strategy } as this: hashSet 'a) (that: hashSet 'a): (hashSet 'a) =>
  Collection.intersect (toCollection this) (toCollection that) |> fromSeqWith strategy;

let subtract ({ strategy } as this: hashSet 'a) (that: hashSet 'a): (hashSet 'a) =>
  Collection.subtract (toCollection this) (toCollection that) |> fromSeqWith strategy;

let union ({ strategy } as this: hashSet 'a) (that: hashSet 'a): (hashSet 'a) =>
  Collection.union (toCollection this) (toCollection that) |> fromSeqWith strategy;
