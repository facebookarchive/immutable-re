/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open CopyOnWriteArray;
open Equality;
open Functions;
open Keyed;
open Option;
open Option.Operators;
open Seq;
open Transient;

/* FIXME: Consider making this a functor, that takes the bitcounting strategy and branch factor
 * as arguments so we can compare them for performance.
 */
let module BitmapTrie = {
  type node 'a =
    | Entry int 'a
    | Level int32 (array (node 'a)) (option owner);

  type bitmapTrie 'a = option (node 'a);

  let empty: bitmapTrie 'a = None;

  /* FIXME: It's not clear which bit counting strategy to use. The bitcount table
   * is likely faster but uses more memory.
   */
  let bitCountTable = {
    let table = Array.make 65536 0;
    let position1 = ref (-1);
    let position2 = ref (-1);
    for i in 1 to 65535 {
      if (!position1 == !position2) {
        position1 := 0;
        position2 := i;
      };

      table.(i) = table.(!position1) + 1;
      position1 := !position1 + 1;
    };

    table
  };

  let countBits (x: int32): int => {
    let intValue = Int32.to_int x;
    let intBits = bitCountTable.(intValue land 65535) + bitCountTable.((intValue asr 16) land 65535);
    intBits + (x < 0l ? 1 : 0);
  };
  /*
  let countBits (x: int32): int => {
    let intBits = {
      let x = Int32.to_int x;
      let x = x - ((x asr 1) land 0x55555555);
      let x = (x land 0x33333333) + ((x asr 2) land 0x33333333);
      let x = (x + (x asr 4)) land 0x0f0f0f0f;
      let x = x + (x asr 8);
      let x = x + (x asr 16);
      x land 0x7f;
    };
    intBits + (x < 0l ? 1 : 0);
  };*/

  let shift = 5;
  let width = 1 lsl shift;

  let bitPos (key: int) (depth: int): int32 => {
    let mask = (key lsr (depth * shift)) land 0x1F;
    Int32.shift_left 1l mask;
  };

  let index (bitmap: int32) (bit: int32): int  =>
    Int32.logand bitmap (Int32.sub bit 1l) |> countBits;

  let containsNode (bitmap: int32) (bit: int32): bool =>
    (Int32.logand bitmap bit) != 0l;

  let rec tryGetFromNode (depth: int) (key: int) (node: node 'a): (option 'a) => switch node {
    | Level bitmap nodes _ =>
        let bit = bitPos key depth;
        let index = index bitmap bit;

        containsNode bitmap bit
          ? nodes.(index) |> tryGetFromNode (depth + 1) key
          : None

    | Entry entryKey entryValue => key == entryKey ? Some entryValue : None;
  };

  let tryGet (key: int) (node: node 'a): (option 'a) =>
    tryGetFromNode 0 key node;

  type alterResult 'a =
    | Added 'a
    | Empty
    | NoChange
    | Removed 'a
    | Replace 'a;

  let rec alterNode
      (updateLevelNode: int => (node 'a) => (node 'a) => (node 'a))
      (owner: option owner)
      (f: option 'a => option 'a)
      (depth: int)
      (key: int)
      (node: node 'a): (alterResult (node 'a)) => switch node {
    | Entry entryKey entryValue when key == entryKey => switch (f @@ Option.return @@ entryValue) {
        | Some newEntryValue when newEntryValue === entryValue => NoChange
        | Some newEntryValue => Replace (Entry key newEntryValue)
        | None => Empty
      }
    | Entry entryKey entryValue => switch (f None) {
        | Some newEntryValue =>
            let bitmap = bitPos entryKey depth;
            Level bitmap [| node |] owner
              |> alterNode updateLevelNode owner (Functions.return @@ Option.return @@ newEntryValue) depth key
        | _ => NoChange
      }
    | Level bitmap nodes _ =>
        let bit = bitPos key depth;
        let index = index bitmap bit;

        (containsNode bitmap bit) ? {
          let childNode = nodes.(index);
          let childNodeAlterResult = childNode |> alterNode updateLevelNode owner f (depth + 1) key;

          switch childNodeAlterResult {
            | Added newChildNode => Added (node |> updateLevelNode index newChildNode)
            | NoChange => NoChange
            | Removed newChildNode => Removed (node |> updateLevelNode index newChildNode)
            | Replace newChildNode => Replace (node |> updateLevelNode index newChildNode)
            | Empty =>
                let nodes = nodes |> CopyOnWriteArray.removeAt index;
                CopyOnWriteArray.count nodes > 0
                  ? Removed (Level (Int32.logxor bitmap bit) nodes owner)
                  : Empty
          }
        } : switch (f None) {
          | Some newEntryValue =>
              let node = Entry key newEntryValue;
              let nodes = nodes |> CopyOnWriteArray.insertAt index node;
              Added (Level (Int32.logor bitmap bit) nodes owner)
          | None => NoChange
        }
  };

  let alter
      (updateLevelNode: int => (node 'a) => (node 'a) => (node 'a))
      (owner: option owner)
      (f: option 'a => option 'a)
      (key: int)
      (trie: bitmapTrie 'a): (alterResult (bitmapTrie 'a)) => trie
    >>| (alterNode updateLevelNode None f 0 key)
    >>| (fun
      | Added node => Added (Some node)
      | NoChange => NoChange
      | Removed node => Removed (Some node)
      | Replace node => Replace (Some node)
      | Empty => Empty
    )
    |> Option.orCompute (fun () => switch (f None) {
        | Some value => Added (Entry key value |> Option.return)
        | _ => NoChange
      }
    );

  let rec reduceWithKey (f: 'acc => int => 'a => 'acc) (acc: 'acc) (node: node 'a): 'acc => switch node {
    | Entry key value => f acc key value;
    | Level _ nodes _ =>
        let reducer acc node => reduceWithKey f acc node;
        nodes |> CopyOnWriteArray.reduce reducer acc;
  };

  let rec toSeq (node: node 'a): (seq ((int, 'a))) => switch node {
    | Entry key value => Seq.return (key, value)
    | Level _ nodes _ => nodes |> CopyOnWriteArray.toSeq |> Seq.flatMap toSeq
  };
};

open BitmapTrie;

type intMap 'a = {
  count: int,
  trie: (bitmapTrie 'a),
};

type transientIntMap 'a = transient (intMap 'a);

let count ({ count }: intMap 'a): int => count;

let empty: intMap 'a = { count: 0, trie: BitmapTrie.empty };

let mutate (map: intMap 'a): (transientIntMap 'a) => Transient.create map;

let updateLevelNodePersistent
    (index: int)
    (childNode: node 'a)
    (node: (node 'a)): (node 'a) => switch node {
  | Level bitmap nodes _ => Level bitmap (nodes |> CopyOnWriteArray.update index childNode) None
  | _ => failwith "Invalid state"
};

let alterWithOwnerAndMutator
    (owner: option owner)
    (updateLevelNode: int => (node 'a) => (node 'a) => (node 'a))
    (key: int)
    (f: option 'a => option 'a)
    ({ count, trie } as map: intMap 'a): (intMap 'a) => {
      let alterResult = trie |> BitmapTrie.alter
        updateLevelNode
        None
        f
        key;

      switch alterResult {
        | Added trie => { count: count + 1, trie }
        | NoChange => map
        | Replace trie => { count, trie }
        | Removed trie => { count: count - 1, trie }
        | Empty => empty
      }
};

let alter
    (key: int)
    (f: option 'a => option 'a)
    (map: intMap 'a): (intMap 'a) =>
  alterWithOwnerAndMutator None updateLevelNodePersistent key f map;

let put (key: int) (value: 'a) ({ count, trie } as map: intMap 'a): (intMap 'a) =>
  map |> alter key (Functions.return @@ Option.return @@ value);

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) ({ trie }: intMap 'a): 'acc =>
  trie >>| BitmapTrie.reduceWithKey (fun acc _ v => f acc v) acc |? acc;

let reduceWithKey (f: 'acc => int => 'a => 'acc) (acc: 'acc) ({ trie }: intMap 'a): 'acc =>
  trie >>| BitmapTrie.reduceWithKey f acc |? acc;

let remove (key: int) (map: intMap 'a): (intMap 'a) =>
  map |> alter key alwaysNone;

let removeAll (map: intMap 'a): (intMap 'a) => empty;

let toKeyed ({ count, trie }: intMap 'a): (keyed int 'a) => Keyed.create
  count::count
  seq::(trie >>| BitmapTrie.toSeq |? Seq.empty)
  tryGet::(fun key => trie >>= BitmapTrie.tryGet key);

let toSeq ({ trie }: intMap 'a): (seq ((int, 'a))) =>
  trie >>| BitmapTrie.toSeq |? Seq.empty;

let tryGet (key: int) ({ trie }: intMap 'a): (option 'a) =>
  trie >>= BitmapTrie.tryGet key;

let module TransientIntMap = {
  let count (transient: transientIntMap 'a): int =>
    transient |> Transient.get |> count;

  let persist (transient: transientIntMap 'a): (intMap 'a) =>
    transient |> Transient.persist;

  let updateLevelNodeTransient
      (owner: owner)
      (index: int)
      (childNode: node 'a)
      (node: (node 'a)): (node 'a) => switch node {
    | Level bitmap nodes nodeOwner => switch nodeOwner {
        | Some nodeOwner when nodeOwner === owner =>
            nodes.(index) = childNode;
            node
        | _ => Level bitmap (nodes |> CopyOnWriteArray.update index childNode) (Some owner)
      }
    | _ => failwith "invalid state"
  };

  let alter
      (key: int)
      (f: option 'a => option 'a)
      (transient: transientIntMap 'a): (transientIntMap 'a) =>
    transient |> Transient.update (fun owner map => alterWithOwnerAndMutator
      (Some owner)
      (updateLevelNodeTransient owner)
      key
      f
      map
    );

  let put (key: int) (value: 'a) (transient: transientIntMap 'a): (transientIntMap 'a) =>
    transient |> alter key (Functions.return @@ Option.return @@ value);

  let putAll
      (seq: seq (int, 'a))
      (transient: transientIntMap 'a): (transientIntMap 'a) => seq
    |> Seq.reduce (fun acc (k, v) => acc |> put k v) transient;

  let remove (key: int) (transient: transientIntMap 'a): (transientIntMap 'a) =>
    transient |> alter key alwaysNone;

  let removeAll (transient: transientIntMap 'a): (transientIntMap 'a) =>
      transient |> Transient.update (fun owner map => empty);

  let tryGet (key: int) (transient: transientIntMap 'a): (option 'a) =>
    transient |> Transient.get |> (tryGet key);
};

let putAll (seq: seq (int, 'a)) (map: intMap 'a): (intMap 'a) => map
  |> mutate
  |> TransientIntMap.putAll seq
  |> TransientIntMap.persist;

let map (f: 'a => 'b) (map: intMap 'a): (intMap 'b) =>
  empty |> putAll (map |> toSeq |> Seq.map (Pair.mapSnd f));

let mapWithKey (f: int => 'a => 'b) (map: intMap 'a): (intMap 'b) =>
  empty |> putAll (map |> toSeq |> Seq.map @@ Pair.mapSndWithFst @@ f);

let fromSeq (seq: seq (int, 'a)): (intMap 'a) => putAll seq empty;

let fromKeyed (keyed: keyed int 'a): (intMap 'a) => keyed |> Keyed.toSeq |> fromSeq;

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
