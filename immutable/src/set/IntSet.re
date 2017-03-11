/* FIXME: I'm fairly certain the BitmapTrie functions can be changed to properly sort IntSet */
let module BitmapTrieIntSet = {
  type t =
    | Level int32 (array t) (option Transient.Owner.t)
    | Entry int
    | Empty;

  let rec add
      (updateLevelNode: int => t => t => t)
      (owner: option Transient.Owner.t)
      (depth: int)
      (value: int)
      (set: t): t => switch set {
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
      (set: t): bool => switch set {
    | Level bitmap nodes _ =>
        let bit = BitmapTrie.bitPos value depth;
        let index = BitmapTrie.index bitmap bit;

        (BitmapTrie.containsNode bitmap bit) &&
        (contains (depth + 1) value nodes.(index));
    | Entry entryValue => value == entryValue
    | Empty => false;
  };

  let rec remove
      (updateLevelNode: int => t => t => t)
      (owner: option Transient.Owner.t)
      (depth: int)
      (value: int)
      (set: t): t => switch set {
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

  let rec toSeq (set: t): (Seq.t int) => switch set {
    | Level _ nodes _ => nodes |> CopyOnWriteArray.toSeq |> Seq.flatMap toSeq
    | Entry entryValue => Seq.return entryValue;
    | Empty => Seq.empty;
  };
};

type t = {
  count: int,
  root: BitmapTrieIntSet.t,
};

let updateLevelNodePersistent
    (index: int)
    (childNode: BitmapTrieIntSet.t)
    (BitmapTrieIntSet.Level bitmap nodes _: BitmapTrieIntSet.t): BitmapTrieIntSet.t =>
  BitmapTrieIntSet.Level bitmap (nodes |> CopyOnWriteArray.update index childNode) None;

let add (value: int) ({ count, root } as set: t): t => {
  let newRoot = root |> BitmapTrieIntSet.add updateLevelNodePersistent None 0 value;
  if (newRoot === root) set
  else { count: count + 1, root: newRoot };
};

let contains (value: int) ({ root }: t): bool =>
  root |> BitmapTrieIntSet.contains 0 value;

let count ({ count }: t): int => count;

let empty: t = { count: 0, root: BitmapTrieIntSet.Empty };

let isEmpty ({ count }: t): bool => count == 0;

let isNotEmpty ({ count }: t): bool => count != 0;

let remove (value: int) ({ count, root } as set: t): t => {
  let newRoot = root |> BitmapTrieIntSet.remove updateLevelNodePersistent None 0 value;
  if (newRoot === root) set
  else { count: count - 1, root: newRoot };
};

let removeAll (_: t): t =>
  empty;

let toSeq ({ root }: t): (Seq.t int) =>
  root |> BitmapTrieIntSet.toSeq;

let every (f: int => bool) (set: t): bool =>
  set |> toSeq |> Seq.every f;

let find (f: int => bool) (set: t): int =>
  set |> toSeq |> Seq.find f;

let forEach (f: int => unit) (set: t): unit =>
  set |> toSeq |> Seq.forEach f;

let none (f: int => bool) (set: t): bool =>
  set |> toSeq |> Seq.none f;

let reduce (f: 'acc => int => 'acc) (acc: 'acc) (set: t): 'acc =>
  set |> toSeq |> Seq.reduce f acc;

let some (f: int => bool) (set: t): bool =>
  set |> toSeq |> Seq.some f;

let tryFind (f: int => bool) (set: t): (option int) =>
  set |> toSeq |> Seq.tryFind f;

let toSet (set: t): (ImmSet.t int) => {
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

let equals (this: t) (that: t): bool =>
  ImmSet.equals (toSet this) (toSet that);

let hash (set: t): int =>
  set |> toSet |> ImmSet.hash;

let toMap (set: t): (ImmMap.t int int) =>
  set |> toSet |> ImmMap.ofSet;

let module TransientIntSet = {
  type intSet = t;

  type t = Transient.t intSet;

  let mutate (set: intSet): t =>
    Transient.create set;

  let updateLevelNodeTransient
      (owner: Transient.Owner.t)
      (index: int)
      (childNode: BitmapTrieIntSet.t)
      (BitmapTrieIntSet.Level bitmap nodes nodeOwner as node: BitmapTrieIntSet.t): BitmapTrieIntSet.t => switch nodeOwner {
    | Some nodeOwner when nodeOwner === owner =>
        nodes.(index) = childNode;
        node
    | _ => BitmapTrieIntSet.Level bitmap (nodes |> CopyOnWriteArray.update index childNode) (Some owner)
  };

  let add (value: int) (transient: t): t =>
    transient |> Transient.update (fun owner ({ count, root } as set) => {
      if (set |> contains value) set
      else {
        let newRoot = root |> BitmapTrieIntSet.add (updateLevelNodeTransient owner) (Some owner) 0 value;
        { count: count + 1, root: newRoot };
      }
    });

  let addAll (seq: Seq.t int) (transient: t): t =>
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

  let contains (value: int) (transient: t): bool =>
    transient |> Transient.get |> contains value;

  let count (transient: t): int =>
    transient |> Transient.get |> count;

  let persistentEmpty = empty;

  let empty (): t =>
    empty |> mutate;

  let isEmpty (transient: t): bool =>
    transient |> Transient.get |> isEmpty;

  let isNotEmpty (transient: t): bool =>
    transient |> Transient.get |> isNotEmpty;

  let persist (transient: t): intSet =>
    transient |> Transient.persist;

  let remove (value: int) (transient: t): t =>
    transient |> Transient.update (fun owner ({ count, root } as set) => {
      let newRoot = root |> BitmapTrieIntSet.remove  (updateLevelNodeTransient owner) None 0 value;
      if (newRoot === root) set
      else { count: count - 1, root: newRoot };
    });

  let removeAll (transient: t): t =>
    transient |> Transient.update (fun _ _ => persistentEmpty);
};

let mutate = TransientIntSet.mutate;

let addAll (seq: Seq.t int) (set: t): t =>
  set |> mutate |> TransientIntSet.addAll seq |> TransientIntSet.persist;

let fromSeq (seq: Seq.t int): t =>
  empty |> addAll seq;

let intersect (this: t) (that: t): t =>
  ImmSet.intersect (toSet this) (toSet that) |> fromSeq;

let subtract (this: t) (that: t): t =>
  ImmSet.subtract (toSet this) (toSet that) |> fromSeq;

let union (this: t) (that: t): t =>
  ImmSet.union (toSet this) (toSet that) |> fromSeq;
