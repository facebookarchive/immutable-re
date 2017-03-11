type t 'a =
  | Empty
  | Leaf (option Transient.Owner.t) (array 'a)
  | Level int (ref int) (option Transient.Owner.t) (array (t 'a));

let bits = 5;
let width = 1 lsl 5;

let count (trie: t 'a): int => switch trie {
  | Empty => 0
  | Leaf _ values => CopyOnWriteArray.count values;
  | Level _ count _ _ => !count;
};

let rec every (f: 'a => bool) (trie: t 'a): bool => switch trie {
  | Empty => true
  | Leaf _ values =>
      values |> CopyOnWriteArray.every f
  | Level _ _ _ nodes =>
      nodes |> CopyOnWriteArray.every (every f);
};

let rec none (f: 'a => bool) (trie: t 'a): bool => switch trie {
  | Empty => true
  | Leaf _ values =>
      values |> CopyOnWriteArray.none f
  | Level _ _ _ nodes =>
      nodes |> CopyOnWriteArray.every (none f);
};

let rec some (f: 'a => bool) (trie: t 'a): bool => switch trie {
  | Empty => false
  | Leaf _ values =>
      values |> CopyOnWriteArray.some f
  | Level _ _ _ nodes =>
      nodes |> CopyOnWriteArray.some (some f);
};

let rec reduce (f: 'acc => 'a => 'acc) (acc: 'acc) (trie: t 'a): 'acc => switch trie {
  | Empty => acc
  | Leaf _ values => values |> CopyOnWriteArray.reduce f acc
  | Level _ _ _ nodes =>
      let reducer acc node => node |> reduce f acc;
      nodes |> CopyOnWriteArray.reduce reducer acc
};

let rec reduceRight (f: 'acc => 'a => 'acc) (acc: 'acc) (trie: t 'a): 'acc => switch trie {
  | Empty => acc
  | Leaf _ values => values |> CopyOnWriteArray.reduceRight f acc
  | Level _ _ _ nodes =>
      let reducer acc node => node |> reduceRight f acc;
      nodes |> CopyOnWriteArray.reduceRight reducer acc
};

let rec toSeq (trie: t 'a): (Seq.t 'a) => switch trie {
  | Empty => Seq.empty
  | Leaf _ values => values |> CopyOnWriteArray.toSeq
  | Level _ _ _ nodes => nodes |> CopyOnWriteArray.toSeq |> Seq.flatMap toSeq
};

let rec toSeqReversed (trie: t 'a): (Seq.t 'a) => switch trie {
  | Empty => Seq.empty
  | Leaf _ values => values |> CopyOnWriteArray.toSeqReversed
  | Level _ _ _ nodes => nodes |> CopyOnWriteArray.toSeqReversed |> Seq.flatMap toSeqReversed
};

let rec tryFind (f: 'a => bool) (trie: t 'a): (option 'a) => switch trie {
  | Empty => None
  | Leaf _ values =>
      values |> CopyOnWriteArray.tryFind f
  | Level _ _ _ nodes =>
      let nodesCount = CopyOnWriteArray.count nodes;
      let rec loop index => index < nodesCount
        ? switch (tryFind f nodes.(index)) {
            | Some _ as result => result
            | _ => loop (index + 1)
          }
        : None;
      loop 0
};

let depth (trie: t 'a): int => switch trie {
  | Empty => failwith "invalid state"
  | Leaf _ _ => 0
  | Level depth _ _ _ => depth
};

let depthCapacity (depth: int): int =>
  width lsl (depth * bits);

let capacity (trie: t 'a): int =>
  depthCapacity (depth trie);

let childCapacity (trie: t 'a): int =>
  depthCapacity (depth trie - 1);

let empty = Empty;

let isEmpty (trie: t 'a) =>
  (count trie) == 0;

let rec tryAddFirstLeafToTrieUsingMutator
    (updateLevel: int => int => (t 'a) => (t 'a) => (t 'a))
    (owner: option Transient.Owner.t)
    (values: array 'a)
    (Level levelDepth levelCount owner tries as trie: t 'a): (option (t 'a)) => {
  let firstIndex = 0;
  let firstChild = tries |> CopyOnWriteArray.first;
  let firstChildCount = count firstChild;
  let firstChildDepth = depth firstChild;
  let triesWidth = CopyOnWriteArray.count tries;
  let valuesCount = CopyOnWriteArray.count values;

  switch firstChild {
    | Leaf _ _ when firstChildDepth < (levelDepth - 1) =>
        let newFirstChildLevelCount = firstChildCount + valuesCount;
        let newFirstChild = Level 1 (ref newFirstChildLevelCount) owner [| Leaf owner values, firstChild |];
        let newLevelCount = !levelCount - firstChildCount + newFirstChildLevelCount;
        updateLevel newLevelCount firstIndex newFirstChild trie |> Option.return;
    | Leaf _ _ when triesWidth < width =>
        let newLevelCount = !levelCount + valuesCount;
        Level levelDepth (ref newLevelCount) owner (tries |> CopyOnWriteArray.addFirst (Leaf owner values)) |> Option.return;
    | Level childLevelDepth _ _ _ => switch (firstChild |> tryAddFirstLeafToTrieUsingMutator updateLevel owner values) {
        | Some newFirstChild =>
            let newLevelCount = !levelCount - firstChildCount + (count newFirstChild);
            updateLevel newLevelCount firstIndex newFirstChild trie |> Option.return;
        | None when firstChildDepth < (levelDepth - 1) =>
            let newFirstChildLevelCount = ref (firstChildCount + valuesCount);
            let newFirstChild = Level (childLevelDepth + 1) newFirstChildLevelCount owner [| Leaf owner values, firstChild |];
            let newLevelCount = !levelCount + valuesCount;
            updateLevel newLevelCount firstIndex newFirstChild trie |> Option.return;
        | None when triesWidth < width =>
            let newLevelCount = ref (!levelCount + valuesCount);
            Level levelDepth newLevelCount owner (tries |> CopyOnWriteArray.addFirst (Leaf owner values)) |> Option.return;
        | None => None
      }
    | _ => None
  };
};

let addFirstLeafUsingMutator
    (updateLevel: int => int => (t 'a) => (t 'a) => (t 'a))
    (owner: option Transient.Owner.t)
    (values: array 'a)
    (trie: t 'a): (t 'a) => switch trie {
  | Empty =>
      Leaf owner values
  | Leaf _ _ =>
      let levelCount = count trie + CopyOnWriteArray.count values;
      Level 1 (ref levelCount) owner [| Leaf owner values, trie |];
  | Level levelDepth _ _ _ => switch (trie |> tryAddFirstLeafToTrieUsingMutator updateLevel owner values) {
    | Some trie => trie
    | None =>
        let levelCount = count trie + CopyOnWriteArray.count values;
        let levelDepth = levelDepth + 1;
        Level levelDepth (ref levelCount) owner [| Leaf owner values, trie |];
  }
};

let rec tryAddLastLeafToTrieUsingMutator
    (updateLevel: int => int => (t 'a) => (t 'a) => (t 'a))
    (owner: option Transient.Owner.t)
    (values: array 'a)
    (Level levelDepth levelCount owner tries as trie: t 'a): (option (t 'a)) => {
  let lastIndex = tries |> CopyOnWriteArray.lastIndex;
  let lastChild = tries |> CopyOnWriteArray.last;
  let lastChildCount = count lastChild;
  let lastChildDepth = depth lastChild;
  let triesWidth = CopyOnWriteArray.count tries;
  let valuesCount = CopyOnWriteArray.count values;

  switch lastChild {
    | Leaf _ _ when lastChildDepth < (levelDepth - 1) =>
        let newLastChildLevelCount = lastChildCount + valuesCount;
        let newLastChild = Level 1 (ref newLastChildLevelCount) owner [| lastChild, Leaf owner values |];
        let newLevelCount = !levelCount - lastChildCount + newLastChildLevelCount;
        updateLevel newLevelCount lastIndex newLastChild trie |> Option.return;
    | Leaf _ when triesWidth < width =>
        let newLevelCount = ref (!levelCount + valuesCount);
        Level levelDepth newLevelCount owner (tries |> CopyOnWriteArray.addLast (Leaf owner values)) |> Option.return;
    | Level childLevelDepth _ _ _ => switch (lastChild |> tryAddLastLeafToTrieUsingMutator updateLevel owner values) {
        | Some newLastChild =>
            let newLevelCount = !levelCount - lastChildCount + (count newLastChild);
            updateLevel newLevelCount lastIndex newLastChild trie |> Option.return;
        | None when lastChildDepth < (levelDepth - 1) =>
            let newLastChildLevelCount = ref (lastChildCount + valuesCount);
            let newLastChild = Level (childLevelDepth + 1) newLastChildLevelCount owner [| lastChild, Leaf owner values |];
            let newLevelCount = !levelCount + valuesCount;
            updateLevel newLevelCount lastIndex newLastChild trie |> Option.return;
        | None when triesWidth < width =>
            let newLevelCount = ref (!levelCount + valuesCount);
            Level levelDepth newLevelCount owner (tries |> CopyOnWriteArray.addLast (Leaf owner values)) |> Option.return;
        | None => None
      }
    | _ => None
  }
};

let addLastLeafUsingMutator
    (updateLevel: int => int => (t 'a) => (t 'a) => (t 'a))
    (owner: option Transient.Owner.t)
    (values: array 'a)
    (trie: t 'a): (t 'a) => switch trie {
  | Empty =>
      Leaf owner values
  | Leaf _ _ =>
      let levelCount = count trie + CopyOnWriteArray.count values;
      Level 1 (ref levelCount) owner [| trie, Leaf owner values |];
  | Level levelDepth _ _ _ => switch (trie |> tryAddLastLeafToTrieUsingMutator updateLevel owner values) {
    | Some trie => trie
    | None =>
        let levelCount = count trie + CopyOnWriteArray.count values;
        let levelDepth = levelDepth + 1;
        Level levelDepth (ref levelCount) owner [| trie, Leaf owner values |];
  }
};

let rec removeFirstLeafUsingMutator
    (updateLevel: int => int => (t 'a) => (t 'a) => (t 'a))
    (owner: option Transient.Owner.t)
    (trie: t 'a): (t 'a, t 'a) => switch trie {
  | Leaf _ _ => (trie, Empty);
  | Level levelDepth levelCount _ tries when levelDepth > 1 =>
      let triesWidth = CopyOnWriteArray.count tries;
      let firstChildIndex = 0;
      let firstChild = tries |> CopyOnWriteArray.first;
      let (firstLeaf, newFirstChild) = firstChild |> removeFirstLeafUsingMutator updateLevel owner;

      switch newFirstChild {
        | Empty when triesWidth > 2 =>
            let newLevelCount = !levelCount - (count firstLeaf);
            let newTrie = Level levelDepth (ref newLevelCount) owner (CopyOnWriteArray.removeFirst tries);
            (firstLeaf, newTrie)
        | Empty =>
            (firstLeaf, CopyOnWriteArray.last tries)
        | _ =>
            let newLevelCount = !levelCount - (count firstLeaf);
            (firstLeaf, updateLevel newLevelCount firstChildIndex newFirstChild trie)
      };
  | Level 1 levelCount _ tries =>
      let triesWidth = CopyOnWriteArray.count tries;
      let firstChild = tries |> CopyOnWriteArray.first;

      triesWidth > 2 ? {
        let newLevelCount = !levelCount - (count firstChild);
        (firstChild, Level 1 (ref newLevelCount) owner (CopyOnWriteArray.removeFirst tries))
      } : {
        (firstChild, CopyOnWriteArray.last tries)
      };
  | _ => failwith "invalid state"
};

let rec removeLastLeafUsingMutator
    (updateLevel: int => int => (t 'a) => (t 'a) => (t 'a))
    (owner: option Transient.Owner.t)
    (trie: t 'a): (t 'a, t 'a) => switch trie {
  | Leaf _ _ => (Empty, trie);
  | Level levelDepth levelCount _ tries when levelDepth > 1 =>
      let triesWidth = CopyOnWriteArray.count tries;
      let lastChildIndex = tries |> CopyOnWriteArray.lastIndex;
      let lastChild = tries |> CopyOnWriteArray.last;
      let (newLastChild, lastLeaf) = lastChild |> removeLastLeafUsingMutator updateLevel owner;

      switch newLastChild {
        | Empty when triesWidth > 2 =>
            let newLevelCount = !levelCount - (count lastLeaf);
            let newTrie = Level levelDepth (ref newLevelCount) owner (CopyOnWriteArray.removeLast tries);
            (newTrie, lastLeaf)
        | Empty =>
            (CopyOnWriteArray.first tries, lastLeaf)
        | _ =>
            let newLevelCount = !levelCount - (count lastLeaf);
            (updateLevel newLevelCount lastChildIndex newLastChild trie, lastLeaf)
      };
  | Level 1 levelCount _ tries =>
      let triesWidth = CopyOnWriteArray.count tries;
      let lastChild = tries |> CopyOnWriteArray.last;

      triesWidth > 2 ? {
        let newLevelCount = !levelCount - (count lastChild);
        (Level 1 (ref newLevelCount) owner (CopyOnWriteArray.removeLast tries), lastChild)
      } : {
        (CopyOnWriteArray.first tries, lastChild)
      };
  | _ => failwith "invalid state"
};

let computeIndexUsingRadixSearch (depth: int) (index: int): int => {
  let mask = width - 1;
  let level = depth * bits;
  (index lsr level) land mask;
};

let canRadixSearch (trie: t 'a): bool => switch trie {
  | Empty => failwith "invalid state"
  | Leaf _ _ => true
  | Level depth count _ tries =>
      let childCapacity = depthCapacity (depth - 1);
      let triesCount = CopyOnWriteArray.count tries;
      !count == (triesCount * childCapacity)
};

type leveIndexContinuation 'a 'b =
  (/* parent */ t 'a) => (/* effective index */ int => (/* childIndex */ int) => 'b);

let computeLevelIndexUsingRadixSearch
    (index: int)
    (f: leveIndexContinuation 'a 'b)
    (Level depth _ _ _ as trie: t 'a): 'b => {
  let childIndex = computeIndexUsingRadixSearch depth index;
  f trie index childIndex;
};

let computeLevelIndexUsingCountSearch
    (index: int)
    (f: leveIndexContinuation 'a 'b)
    (Level _ _ _ tries as trie: t 'a): 'b => {
  let rec loop index childIndex => {
    let childNode = tries.(childIndex);
    let childCount = count childNode;

    index < childCount
      ? f trie index childIndex
      : loop (index - childCount) (childIndex + 1);
  };

  loop index 0;
};

let getImplLevelContinuation
    (get: int => (t 'a) => 'a)
    (Level _ _ _ tries: t 'a)
    (effectiveIndex: int)
    (childIndex: int): 'a => {
  let childNode = tries |> CopyOnWriteArray.get childIndex;
  childNode |> get effectiveIndex;
};

let getImpl
    (computeLevelIndex: int => (leveIndexContinuation 'a 'a) => (t 'a) => 'a)
    (get: int => (t 'a) => 'a)
    (index: int)
    (trie: t 'a): 'a => switch trie {
  | Leaf _ values =>
      values.(computeIndexUsingRadixSearch 0 index)
  | Level _ _ _ _ =>
      trie |> computeLevelIndex index (getImplLevelContinuation get);
  | Empty => failwith "invalid state"
};

let rec getUsingRadixSearch (index: int) (trie: t 'a): 'a => getImpl
  computeLevelIndexUsingRadixSearch
  getUsingRadixSearch
  index
  trie;

let rec get (index: int) (trie: t 'a): 'a => switch trie {
  | Level _ _ _ _ when not @@ canRadixSearch @@ trie => getImpl
      computeLevelIndexUsingCountSearch
      get
      index
      trie
  | _ => trie |> getUsingRadixSearch index
};

let skipImpl
    (computeLevelIndex: int => (leveIndexContinuation 'a (array 'a, t 'a)) => (t 'a) => (array 'a, t 'a))
    (skip:
      (option Transient.Owner.t) =>
      int =>
      (t 'a) =>
      (array 'a, t 'a)
    )
    (owner: option Transient.Owner.t)
    (skipCount: int)
    (trie: t 'a): (array 'a, t 'a) => switch trie {
  | Leaf _ nodes =>
    let skipCount = computeIndexUsingRadixSearch 0 (skipCount - 1) + 1;
    let result = CopyOnWriteArray.skip skipCount nodes;
    (result, empty)
  | Level _ _ _ _ =>
    trie |> computeLevelIndex (skipCount - 1) (fun (Level depth _ _ tries) _ childIndex => {
      let triesLastIndex = CopyOnWriteArray.lastIndex tries;
      let childNode = tries.(childIndex);
      let (tail, newChildNode) as childResult = childNode |> skip owner skipCount;

      switch newChildNode {
        | Empty when childIndex == triesLastIndex => childResult
        | Empty when childIndex == (triesLastIndex - 1) =>
          (tail, tries |> CopyOnWriteArray.last)
        | Empty =>
          let newTries = tries |> CopyOnWriteArray.skip (childIndex + 1);
          let levelCount = newTries |> CopyOnWriteArray.reduce (fun acc next => acc + (count next)) 0;
          (tail, Level depth (ref levelCount) owner newTries)
        | _ =>
          let newTries = tries |> CopyOnWriteArray.skip childIndex;
          newTries.(0) = newChildNode;
          let levelCount = newTries |> CopyOnWriteArray.reduce (fun acc next => acc + (count next)) 0;
          (tail, Level depth (ref levelCount) owner newTries)
      }
    });
  | Empty => failwith "invalid state"
};

let rec skipUsingRadixSearch
    (owner: option Transient.Owner.t)
    (skipCount: int)
    (trie: t 'a): (array 'a, t 'a) => skipImpl
  computeLevelIndexUsingRadixSearch
  skipUsingRadixSearch
  owner
  skipCount
  trie;

let rec skip
    (owner: option Transient.Owner.t)
    (count: int)
    (trie: t 'a): (array 'a, t 'a) => switch trie {
  | Level _ _ _ _ when not @@ canRadixSearch @@ trie => skipImpl
     computeLevelIndexUsingCountSearch
     skip
     owner
     count
     trie
  | _ => skipUsingRadixSearch
     owner
     count
     trie
};

let takeImpl
    (computeLevelIndex: int => (leveIndexContinuation 'a (t 'a, array 'a)) => (t 'a) => (t 'a, array 'a))
    (take:
      (option Transient.Owner.t) =>
      int =>
      (t 'a) =>
      (t 'a, array 'a)
    )
    (owner: option Transient.Owner.t)
    (takeCount: int)
    (trie: t 'a): (t 'a, array 'a) => switch trie {
  | Leaf _ nodes =>
    let takeCount = computeIndexUsingRadixSearch 0 (takeCount - 1) + 1;
    let result = CopyOnWriteArray.take takeCount nodes;
    (empty, result)
  | Level _ _ _ _ =>
    trie |> computeLevelIndex (takeCount - 1) (fun (Level depth _ _ tries) effectiveIndex childIndex => {
      let childNode = tries.(childIndex);
      let (newChildNode, tail) as childResult = childNode |> take owner (effectiveIndex + 1);

      switch newChildNode {
        | Empty when childIndex == 0 => childResult
        | Empty when childIndex == 1 => (tries.(0), tail)
        | Empty =>
          let newTries = tries |> CopyOnWriteArray.take childIndex;
          let levelCount = newTries |> CopyOnWriteArray.reduce (fun acc next => acc + (count next)) 0;
          (Level depth (ref levelCount) owner newTries, tail)
        | _ =>
          let newTries = tries |> CopyOnWriteArray.take (childIndex + 1);
          newTries.(childIndex) = newChildNode;
          let levelCount = newTries |> CopyOnWriteArray.reduce (fun acc next => acc + (count next)) 0;
          (Level depth (ref levelCount) owner newTries, tail)
      };
    });
  | Empty => failwith "invalid state"
};

let rec takeUsingRadixSearch
    (owner: option Transient.Owner.t)
    (takeCount: int)
    (trie: t 'a): (t 'a, array 'a) => takeImpl
  computeLevelIndexUsingRadixSearch
  takeUsingRadixSearch
  owner
  takeCount
  trie;

let rec take
    (owner: option Transient.Owner.t)
    (count: int)
    (trie: t 'a): (t 'a, array 'a) => switch trie {
 | Level _ _ _ _ when not @@ canRadixSearch @@ trie => takeImpl
     computeLevelIndexUsingCountSearch
     take
     owner
     count
     trie
  | _ => takeUsingRadixSearch
     owner
     count
     trie
};

let updateUsingMutatorImpl
    (computeLevelIndex: int => (leveIndexContinuation 'a (t 'a)) => (t 'a) => (t 'a))
    (updateUsingMutator:
      (int => int => (t 'a) => (t 'a) => (t 'a)) =>
      (int => 'a => (t 'a) => (t 'a)) =>
      int =>
      'a =>
      (t 'a) =>
      (t 'a)
    )
    (updateLevel: int => int => (t 'a) => (t 'a) => (t 'a))
    (updateLeaf: int => 'a => (t 'a) => (t 'a))
    (index: int)
    (value: 'a)
    (trie: t 'a): (t 'a) => switch trie {
  | Empty => failwith "invalid state"
  | Leaf _ _ => trie |> updateLeaf (computeIndexUsingRadixSearch 0 index) value;
  | Level _ _ _ _ =>
      trie |> computeLevelIndex index (fun (Level _ count _ tries) index childIndex => {
        let childNode = tries |> CopyOnWriteArray.get childIndex;
        let newChildNode = childNode |> updateUsingMutator updateLevel updateLeaf index value;
        childNode === newChildNode
          ? trie
          : trie |> updateLevel !count childIndex newChildNode;
      });
};

let rec updateUsingRadixSearchUsingMutator
    (updateLevel: int => int => (t 'a) => (t 'a) => (t 'a))
    (updateLeaf: int => 'a => (t 'a) => (t 'a))
    (index: int)
    (value: 'a)
    (trie: t 'a): (t 'a) => updateUsingMutatorImpl
  computeLevelIndexUsingRadixSearch
  updateUsingRadixSearchUsingMutator
  updateLevel
  updateLeaf
  index
  value
  trie;

let rec updateUsingMutator
    (updateLevel: int => int => (t 'a) => (t 'a) => (t 'a))
    (updateLeaf: int => 'a => (t 'a) => (t 'a))
    (index: int)
    (value: 'a)
    (trie: t 'a): (t 'a) => switch trie {
  | Level _ _ _ _ when not @@ canRadixSearch @@ trie => updateUsingMutatorImpl
     computeLevelIndexUsingCountSearch
     updateUsingMutator
     updateLevel
     updateLeaf
     index
     value
     trie;
  | _ => updateUsingRadixSearchUsingMutator
      updateLevel
      updateLeaf
      index
      value
      trie;
};

let rec updateAllUsingMutator
    (updateLevel: int => int => (t 'a) => (t 'a) => (t 'a))
    (updateLeaf: int => 'a => (t 'a) => (t 'a))
    (f: 'a => 'a)
    (trie: t 'a): (t 'a) => switch trie {
  | Empty => Empty
  | Leaf _ values =>
      let valuesCount = CopyOnWriteArray.count values;
      let rec loop index trie => index < valuesCount ? {
        let newValue = f values.(index);
        let newTrie = trie |> updateLeaf index newValue;
        loop (index + 1) newTrie;
      } : trie;

      loop 0 trie;
  | Level _ count _ tries =>
      let triesCount = CopyOnWriteArray.count tries;

      let rec loop index trie => index < triesCount ? {
        let childNode = tries.(index);
        let newChildNode = childNode |> updateAllUsingMutator updateLevel updateLeaf f;
        let newTrie = childNode === newChildNode
          ? trie
          : trie |> updateLevel !count index newChildNode;
        loop (index + 1) newTrie;
      }: trie;

      loop 0 trie;
};

let updateWithUsingMutatorImpl
    (computeLevelIndex: int => (leveIndexContinuation 'a (t 'a)) => (t 'a) => (t 'a))
    (updateUsingMutator:
      (int => int => (t 'a) => (t 'a) => (t 'a)) =>
      (int => 'a => (t 'a) => (t 'a)) =>
      int =>
      ('a => 'a) =>
      (t 'a) =>
      (t 'a)
    )
    (updateLevel: int => int => (t 'a) => (t 'a) => (t 'a))
    (updateLeaf: int => 'a => (t 'a) => (t 'a))
    (index: int)
    (f: 'a => 'a)
    (trie: t 'a): (t 'a) => switch trie {
  | Empty => failwith "invalid state"
  | Leaf _ values =>
      let arrIndex = (computeIndexUsingRadixSearch 0 index);
      let newValue = f values.(arrIndex);
      trie |> updateLeaf arrIndex newValue;
  | Level _ _ _ _ =>
      trie |> computeLevelIndex index (fun (Level _ count _ tries) index childIndex => {
        let childNode = tries |> CopyOnWriteArray.get childIndex;
        let newChildNode = childNode |> updateUsingMutator updateLevel updateLeaf index f;
        childNode === newChildNode
          ? trie
          : trie |> updateLevel !count childIndex newChildNode;
      });
};

let rec updateWithUsingRadixSearchUsingMutator
    (updateLevel: int => int => (t 'a) => (t 'a) => (t 'a))
    (updateLeaf: int => 'a => (t 'a) => (t 'a))
    (index: int)
    (f: 'a => 'a)
    (trie: t 'a): (t 'a) => updateWithUsingMutatorImpl
  computeLevelIndexUsingRadixSearch
  updateWithUsingRadixSearchUsingMutator
  updateLevel
  updateLeaf
  index
  f
  trie;

let rec updateWithUsingMutator
    (updateLevel: int => int => (t 'a) => (t 'a) => (t 'a))
    (updateLeaf: int => 'a => (t 'a) => (t 'a))
    (index: int)
    (f: 'a => 'a)
    (trie: t 'a): (t 'a) => switch trie {
  | Level _ _ _ _ when not @@ canRadixSearch @@ trie => updateWithUsingMutatorImpl
     computeLevelIndexUsingCountSearch
     updateWithUsingMutator
     updateLevel
     updateLeaf
     index
     f
     trie;
  | _ => updateWithUsingRadixSearchUsingMutator
      updateLevel
      updateLeaf
      index
      f
      trie;
};

let updateLevelPersistent
    (count: int)
    (index: int)
    (child: t 'a)
    (Level depth _ _ tries: t 'a): (t 'a) =>
  Level depth (ref count) None (CopyOnWriteArray.update index child tries);

let updateLeafPersistent
    (index: int)
    (value: 'a)
    (Leaf _ values: t 'a): (t 'a) =>
  Leaf None (values |> CopyOnWriteArray.update index value);

let updateLevelTransient
    (owner: Transient.Owner.t)
    (count: int)
    (index: int)
    (child: t 'a)
    (trie: t 'a): (t 'a) => switch trie {
  | Level _ trieCount (Some trieOwner) tries when trieOwner === owner =>
      tries.(index) = child;
      trieCount := count;
      trie
  | Level depth _ _ tries =>
      Level depth (ref count) (Some owner) (CopyOnWriteArray.update index child tries)
  | _ => failwith "Invalid state"
};

let updateLeafTransient
    (owner: Transient.Owner.t)
    (index: int)
    (value: 'a)
    (trie: t 'a): (t 'a) => switch trie {
  | Leaf (Some trieOwner) values when trieOwner === owner =>
      values.(index) = value;
      trie
  | Leaf _ values =>
      Leaf (Some owner) (values |> CopyOnWriteArray.update index value)
  | _ => failwith "Invalid state"
};

let rec validate (trie: t 'a) => switch trie {
  | Empty => ()
  | Leaf _ v => if ((CopyOnWriteArray.count v) != 32) (failwith "arr too small")
  | Level levelDepth levelCount _ tries =>
      print_string "depth: "; print_int levelDepth; print_newline ();
      print_string "count: "; print_int !levelCount; print_newline ();
      print_string "triesWidth: "; print_int (CopyOnWriteArray.count tries); print_newline ();
      print_newline ();
      let firstTrie = tries |> CopyOnWriteArray.first;
      if (levelDepth != ((depth firstTrie) + 1)) (failwith "parent level higher than child");
      if ((count firstTrie) != (capacity firstTrie)) {
        print_string "firstTrie count: "; print_int (count firstTrie); print_newline ();
        print_string "firstTrie capacity: "; print_int (capacity firstTrie); print_newline ();
        failwith " first Trie isn't full";
      };
      tries |> CopyOnWriteArray.forEach validate;
};
