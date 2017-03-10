open Set;
open Keyed;
open Seq;
open Transient;

type bitmapTrieIntSet =
  | Level int32 (array bitmapTrieIntSet) (option owner)
  | Entry int
  | Empty;

/* FIXME: I'm fairly certain the BitmapTrie functions can be changed to properly sort IntSet */

let module BitmapTrieIntSet = {
  let rec add
      (updateLevelNode: int => bitmapTrieIntSet => bitmapTrieIntSet => bitmapTrieIntSet)
      (owner: option owner)
      (depth: int)
      (value: int)
      (set: bitmapTrieIntSet): bitmapTrieIntSet => switch set {
    | Level bitmap nodes _ =>
        let bit = BitmapTrie.bitPos value depth;
        let index = BitmapTrie.index bitmap bit;

        if (BitmapTrie.containsNode bitmap bit) {
          let childNode = nodes.(index);
          let newChildNode = childNode |> add updateLevelNode owner (depth + 1) value;

          if (childNode === newChildNode) set
          else (updateLevelNode index newChildNode set)
        } else {
          let entry = Entry value;
          let nodes = nodes |> CopyOnWriteArray.insertAt index entry;
          Level (Int32.logor bitmap bit) nodes owner;
        }
    | Entry entryValue =>
      if (value == entryValue) set
      else {
        let bitmap = BitmapTrie.bitPos entryValue depth;
        Level bitmap [| set |] owner |> add updateLevelNode owner depth value;
      }
    | Empty => Entry value;
  };

  let rec contains
      (depth: int)
      (value: int)
      (set: bitmapTrieIntSet): bool => switch set {
    | Level bitmap nodes _ =>
        let bit = BitmapTrie.bitPos value depth;
        let index = BitmapTrie.index bitmap bit;

        (BitmapTrie.containsNode bitmap bit) &&
        (contains (depth + 1) value nodes.(index));
    | Entry entryValue => value == entryValue
    | Empty => false;
  };

  let rec remove
      (updateLevelNode: int => bitmapTrieIntSet => bitmapTrieIntSet => bitmapTrieIntSet)
      (owner: option owner)
      (depth: int)
      (value: int)
      (set: bitmapTrieIntSet): bitmapTrieIntSet => switch set {
    | Level bitmap nodes _ =>
        let bit = BitmapTrie.bitPos value depth;
        let index = BitmapTrie.index bitmap bit;

        if (BitmapTrie.containsNode bitmap bit) {
          let childNode = nodes.(index);
          let newChildNode = childNode |> remove updateLevelNode owner (depth + 1) value;

          if (newChildNode === childNode) set
          else if (newChildNode === Empty) {
            let nodes = nodes |> CopyOnWriteArray.removeAt index;

            if (CopyOnWriteArray.count nodes > 0) (Level (Int32.logxor bitmap bit) nodes owner)
            else Empty;
          } else (updateLevelNode index newChildNode set);
        } else set;
    | Entry entryValue when value == entryValue =>
        Empty;
    | _ => set
  };

  let rec toSeq (set: bitmapTrieIntSet): (seq int) => switch set {
    | Level _ nodes _ => nodes |> CopyOnWriteArray.toSeq |> Seq.flatMap toSeq
    | Entry entryValue => Seq.return entryValue;
    | Empty => Seq.empty;
  };
};

type intSet = {
  count: int,
  root: bitmapTrieIntSet,
};

let updateLevelNodePersistent
    (index: int)
    (childNode: bitmapTrieIntSet)
    ((Level bitmap nodes _): bitmapTrieIntSet): bitmapTrieIntSet =>
  Level bitmap (nodes |> CopyOnWriteArray.update index childNode) None;

let add (value: int) ({ count, root } as set: intSet): intSet => {
  let newRoot = root |> BitmapTrieIntSet.add updateLevelNodePersistent None 0 value;
  if (newRoot === root) set
  else { count: count + 1, root: newRoot };
};

let contains (value: int) ({ root }: intSet): bool =>
  root |> BitmapTrieIntSet.contains 0 value;

let count ({ count }: intSet): int => count;

let empty: intSet = { count: 0, root: Empty };

let isEmpty ({ count }: intSet): bool => count == 0;

let isNotEmpty ({ count }: intSet): bool => count != 0;

let remove (value: int) ({ count, root } as set: intSet): intSet => {
  let newRoot = root |> BitmapTrieIntSet.remove updateLevelNodePersistent None 0 value;
  if (newRoot === root) set
  else { count: count - 1, root: newRoot };
};

let removeAll (_: intSet): intSet =>
  empty;

let toSeq ({ root }: intSet): (seq int) =>
  root |> BitmapTrieIntSet.toSeq;

let every (f: int => bool) (set: intSet): bool =>
  set |> toSeq |> Seq.every f;

let find (f: int => bool) (set: intSet): int =>
  set |> toSeq |> Seq.find f;

let forEach (f: int => unit) (set: intSet): unit =>
  set |> toSeq |> Seq.forEach f;

let none (f: int => bool) (set: intSet): bool =>
  set |> toSeq |> Seq.none f;

let reduce (f: 'acc => int => 'acc) (acc: 'acc) (set: intSet): 'acc =>
  set |> toSeq |> Seq.reduce f acc;

let some (f: int => bool) (set: intSet): bool =>
  set |> toSeq |> Seq.some f;

let tryFind (f: int => bool) (set: intSet): (option int) =>
  set |> toSeq |> Seq.tryFind f;

let toSet (set: intSet): (set int) => {
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

let equals (this: intSet) (that: intSet): bool =>
  Set.equals (toSet this) (toSet that);

let hash (set: intSet): int =>
  set |> toSet |> Set.hash;

let toKeyed (set: intSet): (keyed int int) =>
  set |> toSet |> Keyed.ofSet;

type transientIntSet = transient intSet;

let mutate (set: intSet): (transientIntSet) =>
  Transient.create set;

let module TransientIntSet = {
  let updateLevelNodeTransient
      (owner: owner)
      (index: int)
      (childNode: bitmapTrieIntSet)
      ((Level bitmap nodes nodeOwner) as node: bitmapTrieIntSet): bitmapTrieIntSet => switch nodeOwner {
    | Some nodeOwner when nodeOwner === owner =>
        nodes.(index) = childNode;
        node
    | _ => Level bitmap (nodes |> CopyOnWriteArray.update index childNode) (Some owner)
  };

  let add (value: int) (transient: transientIntSet): (transientIntSet) =>
    transient |> Transient.update (fun owner ({ count, root } as set) => {
      if (set |> contains value) set
      else {
        let newRoot = root |> BitmapTrieIntSet.add (updateLevelNodeTransient owner) (Some owner) 0 value;
        { count: count + 1, root: newRoot };
      }
    });

  let addAll (seq: seq int) (transient: transientIntSet): (transientIntSet) =>
    transient |> Transient.update (fun owner ({ count, root } as set) => {
      let newCount = ref count;

      let newRoot = seq |> Seq.reduce (fun acc value => {
        if (acc |> BitmapTrieIntSet.contains 0 value) acc
        else  {
          let newRoot = acc
            |> BitmapTrieIntSet.add (updateLevelNodeTransient owner) (Some owner) 0 value;
          newCount := !newCount + 1;
          newRoot
        }
      }) root;

      if (!newCount == count) set
      else { count: !newCount, root: newRoot };
    });

  let contains (value: int) (transient: transientIntSet): bool =>
    transient |> Transient.get |> contains value;

  let count (transient: transientIntSet): int =>
    transient |> Transient.get |> count;

  let persistentEmpty = empty;

  let empty (): (transientIntSet) =>
    empty |> mutate;

  let isEmpty (transient: transientIntSet): bool =>
    transient |> Transient.get |> isEmpty;

  let isNotEmpty (transient: transientIntSet): bool =>
    transient |> Transient.get |> isNotEmpty;

  let persist (transient: transientIntSet): intSet =>
    transient |> Transient.persist;

  let remove (value: int) (transient: transientIntSet): (transientIntSet) =>
    transient |> Transient.update (fun owner ({ count, root } as set) => {
      let newRoot = root |> BitmapTrieIntSet.remove  (updateLevelNodeTransient owner) None 0 value;
      if (newRoot === root) set
      else { count: count - 1, root: newRoot };
    });

  let removeAll  (transient: transientIntSet): (transientIntSet) =>
    transient |> Transient.update (fun owner _ => persistentEmpty);
};

let addAll (seq: seq int) (set: intSet): intSet =>
  set |> mutate |> TransientIntSet.addAll seq |> TransientIntSet.persist;

let fromSeq (seq: seq int): intSet =>
  empty |> addAll seq;

let intersect (this: intSet) (that: intSet): intSet =>
  Set.intersect (toSet this) (toSet that) |> fromSeq;

let subtract (this: intSet) (that: intSet): intSet =>
  Set.subtract (toSet this) (toSet that) |> fromSeq;

let union (this: intSet) (that: intSet): intSet =>
  Set.union (toSet this) (toSet that) |> fromSeq;
