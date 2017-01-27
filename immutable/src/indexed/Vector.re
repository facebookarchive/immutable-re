open Functions;
open Indexed;
open Option;
open Option.Operators;
open Preconditions;
open Seq;
open Transient;

type node 'a =
  | LeafNode (option owner) (array 'a)
  | LevelNode int (ref int) (option owner) (array (node 'a));

let module Node = {
  let rec reduce (f: 'acc => 'a => 'acc) (acc: 'acc) (node: node 'a): 'acc => switch node {
    | LeafNode _ values => values |> CopyOnWriteArray.reduce f acc
    | LevelNode _ _ _ nodes =>
        let reducer acc node => node |> reduce f acc;
        nodes |> CopyOnWriteArray.reduce reducer acc
  };

  let rec reduceRight (f: 'acc => 'a => 'acc) (acc: 'acc) (node: node 'a): 'acc => switch node {
    | LeafNode _ values => values |> CopyOnWriteArray.reduceRight f acc
    | LevelNode _ _ _ nodes =>
        let reducer acc node => node |> reduceRight f acc;
        nodes |> CopyOnWriteArray.reduceRight reducer acc
  };

  let rec toSeq (node: node 'a): (seq 'a) => switch node {
    | LeafNode _ values => values |> CopyOnWriteArray.toSeq
    | LevelNode _ _ _ nodes => nodes |> CopyOnWriteArray.toSeq |> Seq.flatMap toSeq
  };

  let rec toSeqReversed (node: node 'a): (seq 'a) => switch node {
    | LeafNode _ values => values |> CopyOnWriteArray.toSeqReversed
    | LevelNode _ _ _ nodes => nodes |> CopyOnWriteArray.toSeqReversed |> Seq.flatMap toSeqReversed
  };

  let bits = 5;
  let width = 1 lsl bits;

  let depth (node: node 'a): int => switch node {
    | LeafNode _ values => 0
    | LevelNode depth _ _ _ => depth
  };

  let depthCapacity (depth: int): int =>
    width lsl (depth * bits);

  let capacity (node: node 'a): int =>
    depthCapacity (depth node);

  let childCapacity (node: node 'a): int =>
    depthCapacity (depth node - 1);

  let empty = LeafNode None [||];

  let count (node: node 'a): int => switch node {
    | LeafNode _ values => CopyOnWriteArray.count values;
    | LevelNode _ count _ _ => !count;
  };

  let isEmpty (node: node 'a) =>
    (count node) == 0;

  let isNotEmpty (node: node 'a) =>
    (count node) != 0;

  let isFull (node: node 'a): bool => {
    let capacity = capacity node;
    let nodeCount = count node;
    capacity == nodeCount;
  };

  let rec addFirstWithMutator
      (updateLevelNode: int => int => (node 'a) => (node 'a) => (node 'a))
      (owner: option owner)
      (value: 'a)
      (node: node 'a): (node 'a) => switch node {
    | LeafNode _ values => isFull node
        ? LevelNode 1 (ref (count node + 1)) owner [| LeafNode owner [| value |], node |]
        : LeafNode owner (CopyOnWriteArray.addFirst value values)

    | LevelNode depth levelCount _ children =>
        let firstChild = children |> CopyOnWriteArray.first;

        isFull node ? {
          let nodes = [| LeafNode owner [| value |], node |];
          let levelCount = ref (!levelCount + 1);
          LevelNode (depth + 1) levelCount owner nodes;
        } :

        (count firstChild) < (depthCapacity (depth - 1)) ? {
          let newFirstChild = firstChild |> addFirstWithMutator updateLevelNode owner value;
          let firstIndex = 0;
          let levelCount = !levelCount + 1;
          node |> updateLevelNode levelCount firstIndex newFirstChild;
        } :

        {
          let newChildren = children |> CopyOnWriteArray.addFirst (LeafNode owner [| value |]);
          let levelCount = ref (!levelCount + 1);
          LevelNode depth levelCount owner newChildren;
        };
  };

  let rec addLastWithMutator
      (updateLevelNode: int => int => (node 'a) => (node 'a) => (node 'a))
      (owner: option owner)
      (value: 'a)
      (node: node 'a): (node 'a) => switch node {
    | LeafNode _ values => isFull node
        ? LevelNode 1 (ref (count node + 1)) owner [| node, LeafNode owner [| value |] |]
        : LeafNode owner (CopyOnWriteArray.addLast value values)

    | LevelNode depth levelCount _ children =>
        let lastChild = children |> CopyOnWriteArray.last;

        isFull node ? {
          let nodes = [| node, LeafNode owner [| value |] |];
          let levelCount = ref (!levelCount + 1);
          LevelNode (depth + 1) levelCount owner nodes;
        } :

        (count lastChild) < (depthCapacity (depth - 1)) ? {
          let newLastChild = lastChild |> addLastWithMutator updateLevelNode owner value;
          let lastIndex = (CopyOnWriteArray.count children) - 1;
          let levelCount = !levelCount + 1;
          node |> updateLevelNode levelCount lastIndex newLastChild;
        } :

        {
          let newChildren = children |> CopyOnWriteArray.addLast (LeafNode owner [| value |]);
          let levelCount = ref (!levelCount + 1);
          LevelNode depth levelCount owner newChildren;
        };
  };

  let rec removeFirstWithMutator
      (updateLevelNode: int => int => (node 'a) => (node 'a) => (node 'a))
      (owner: option owner)
      (node: node 'a): (node 'a) => switch node {
    | LeafNode _ values => LeafNode owner (CopyOnWriteArray.removeFirst values)
    | LevelNode depth levelCount _ nodes =>
        let nodesCount = (CopyOnWriteArray.count nodes);
        let childNodeIndex = 0;
        let childNode = nodes.(childNodeIndex);
        let newChildNode = removeFirstWithMutator updateLevelNode owner childNode;
        let levelCount = !levelCount - 1;

        (not @@ isEmpty @@ newChildNode) ?
          updateLevelNode levelCount childNodeIndex newChildNode node :

        (isEmpty newChildNode) && nodesCount > 2 ?
          LevelNode depth (ref levelCount) owner (CopyOnWriteArray.removeFirst nodes) :

        nodes |> CopyOnWriteArray.last;
  };

  let rec removeLastWithMutator
      (updateLevelNode: int => int => (node 'a) => (node 'a) => (node 'a))
      (owner: option owner)
      (node: node 'a): (node 'a) => switch node {
    | LeafNode _ values => LeafNode owner (CopyOnWriteArray.removeLast values)
    | LevelNode depth levelCount _ nodes =>
        let nodesCount = (CopyOnWriteArray.count nodes);
        let childNodeIndex = nodesCount - 1;
        let childNode = nodes.(childNodeIndex);
        let newChildNode = removeLastWithMutator updateLevelNode owner childNode;
        let levelCount = !levelCount - 1;

        (not @@ isEmpty @@ newChildNode) ?
          updateLevelNode levelCount childNodeIndex newChildNode node :

        (isEmpty newChildNode) && nodesCount > 2 ?
          LevelNode depth (ref levelCount) owner (CopyOnWriteArray.removeLast nodes) :

        nodes |> CopyOnWriteArray.first;
  };

  let computeIndexRadixSearch (depth: int) (index: int): int => {
    let mask = width - 1;
    let level = depth * bits;
    (index lsr level) land mask;
  };

  let canRadixSearch (node: node 'a): bool => switch node {
    | LeafNode _ _ => true
    | LevelNode depth count _ nodes =>
        let childCapacity = depthCapacity (depth - 1);
        let nodesCount = CopyOnWriteArray.count nodes;
        !count == (nodesCount * childCapacity)
  };

  let rec getRadixSearch (index: int) (node: node 'a): 'a => switch node {
    | LeafNode _ values =>
        values.(computeIndexRadixSearch 0 index)
    | LevelNode depth _ _ nodes =>
        nodes.(computeIndexRadixSearch depth index) |> getRadixSearch index
  };

  let rec get (index: int) (node: node 'a): 'a => switch node {
    | LevelNode _ _ _ nodes when not @@ canRadixSearch @@ node =>
        let rec loop index childIndex => {
          let childNode = nodes.(childIndex);
          let childCount = count childNode;

          index < childCount
            ? get index childNode
            : loop (index - childCount) (childIndex + 1);
        };

        loop index 0;
    | _ => node |> getRadixSearch index
  };

  let rec updateRadixSearchWithMutator
      (updateLevelNode: int => int => (node 'a) => (node 'a) => (node 'a))
      (updateLeafNode: int => 'a => (node 'a) => (node 'a))
      (index: int)
      (value: 'a)
      (node: node 'a): (node 'a) => switch node {
    | LeafNode _ _ => node |> updateLeafNode (computeIndexRadixSearch 0 index) value;
    | LevelNode depth count _ nodes =>
        let childIndex = computeIndexRadixSearch depth index;
        let childNode = nodes.(childIndex);
        let newChildNode = updateRadixSearchWithMutator updateLevelNode updateLeafNode index value childNode;

        childNode === newChildNode
          ? node
          : node |> updateLevelNode !count childIndex newChildNode;
  };

  let rec updateWithMutator
      (updateLevelNode: int => int => (node 'a) => (node 'a) => (node 'a))
      (updateLeafNode: int => 'a => (node 'a) => (node 'a))
      (index: int)
      (value: 'a)
      (node: node 'a): (node 'a) => switch node {
    | LevelNode _ levelCount _ nodes when not @@ canRadixSearch @@ node =>
        let rec loop index childIndex => {
          let childNode = nodes.(childIndex);
          let childCount = count childNode;

          index < childCount ? {
            let newChildNode = updateWithMutator updateLevelNode updateLeafNode index value childNode;
            childNode === newChildNode
              ? node
              : node |> updateLevelNode !levelCount childIndex newChildNode;
          } : loop (index - childCount) (childIndex + 1);
        };

        loop index 0;
    | _ => node |> updateRadixSearchWithMutator updateLevelNode updateLeafNode index value
  };

  let rec alterRadixSearchWithMutator
      (updateLevelNode: int => int => (node 'a) => (node 'a) => (node 'a))
      (updateLeafNode: int => 'a => (node 'a) => (node 'a))
      (index: int)
      (f: 'a => 'a)
      (node: node 'a): (node 'a) => switch node {
    | LeafNode _ values =>
        let index = computeIndexRadixSearch 0 index;
        let value = values.(index);
        let newValue = f value;
        node |> updateLeafNode index newValue;
    | LevelNode depth count _ nodes  =>
        let childIndex = computeIndexRadixSearch depth index;
        let childNode = nodes.(childIndex);
        let newChildNode = alterRadixSearchWithMutator updateLevelNode updateLeafNode index f childNode;

        childNode === newChildNode
          ? node
          : node |> updateLevelNode !count childIndex newChildNode;
  };

  let rec alterWithMutator
      (updateLevelNode: int => int => (node 'a) => (node 'a) => (node 'a))
      (updateLeafNode: int => 'a => (node 'a) => (node 'a))
      (index: int)
      (f: 'a => 'a)
      (node: node 'a): (node 'a) => switch node {
    | LevelNode depth levelCount _ nodes when not @@ canRadixSearch @@ node =>
        let rec loop index childIndex => {
          let childNode = nodes.(childIndex);
          let childCount = count childNode;

          index < childCount ? {
            let newChildNode = alterWithMutator updateLevelNode updateLeafNode index f childNode;
            childNode === newChildNode
              ? node
              : node |> updateLevelNode !levelCount childIndex newChildNode;
          } : loop (index - childCount) (childIndex + 1);
        };

        loop index 0;
    | _ => node |> alterRadixSearchWithMutator updateLevelNode updateLeafNode index f
  };
};

/* Eventually vector's type definition may expand to include focus nodes */
type vector 'a = node 'a;

type transientVector 'a = transient (vector 'a);

let reduce
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (vector: vector 'a): 'acc => vector |> Node.reduce f acc;

let reduceRight
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (vector: vector 'a): 'acc => vector |> Node.reduceRight f acc;

let count (vector: vector 'a): int => Node.count vector;

let isEmpty (vector: vector 'a): bool =>
  (count vector) == 0;

let empty: (vector 'a) = Node.empty;

let removeAll (trie: vector 'a): (vector 'a) => empty;

let addFirstWithMutator
    (updateLevelNode: int => int => (node 'a) => (node 'a) => (node 'a))
    (owner: option owner)
    (value: 'a)
    (vector: vector 'a): (vector 'a) =>
  vector |> Node.addFirstWithMutator updateLevelNode owner value;

let addLastWithMutator
    (updateLevelNode: int => int => (node 'a) => (node 'a) => (node 'a))
    (owner: option owner)
    (value: 'a)
    (vector: vector 'a): (vector 'a) =>
  vector |> Node.addLastWithMutator updateLevelNode owner value;

let removeFirstWithMutator
    (updateLevelNode: int => int => (node 'a) => (node 'a) => (node 'a))
    (owner: option owner)
    (vector: vector 'a): (vector 'a) =>
  (count vector) == 0 ? failwith "empty" :
  (count vector) == 1 ? empty :
  vector |> Node.removeFirstWithMutator updateLevelNode owner;

let removeLastWithMutator
    (updateLevelNode: int => int => (node 'a) => (node 'a) => (node 'a))
    (owner: option owner)
    (vector: vector 'a): (vector 'a) =>
  (count vector) == 0 ? failwith "empty" :
  (count vector) == 1 ? empty :
  vector |> Node.removeLastWithMutator updateLevelNode owner;

let updateWithMutator
    (updateLevelNode: int => int => (node 'a) => (node 'a) => (node 'a))
    (updateLeafNode: int => 'a => (node 'a) => (node 'a))
    (owner: option owner)
    (index: int)
    (value: 'a)
    (vector: vector 'a): (vector 'a) => {
  let trieCount = count vector;

  Preconditions.failIfOutOfRange trieCount index;
  vector |> Node.updateWithMutator updateLevelNode updateLeafNode index value;
};

let alterWithMutator
    (updateLevelNode: int => int => (node 'a) => (node 'a) => (node 'a))
    (updateLeafNode: int => 'a => (node 'a) => (node 'a))
    (owner: option owner)
    (index: int)
    (f: 'a => 'a)
    (vector: vector 'a): (vector 'a) => {
  let trieCount = count vector;

  Preconditions.failIfOutOfRange trieCount index;
  vector |> Node.alterWithMutator updateLevelNode updateLeafNode index f;
};

let getUnsafe (index: int) (vector: vector 'a): 'a =>
  vector |> Node.get index;

let get (index: int) (vector: vector 'a): 'a => {
  Preconditions.failIfOutOfRange (count vector) index;
  getUnsafe index vector;
};

let first (vector: vector 'a): 'a => get 0 vector;

let last (vector: vector 'a): 'a => get ((count vector) - 1) vector;

let tryGet (index: int) (vector: vector 'a): (option 'a) => {
  let trieCount = count vector;
  Preconditions.noneIfIndexOutOfRange trieCount index (flip getUnsafe vector);
};

let tryFirst (vector: vector 'a): (option 'a) => tryGet 0 vector;

let tryLast (vector: vector 'a): (option 'a) => tryGet ((count vector) - 1) vector;

let updateLevelNodePersistent
    (count: int)
    (index: int)
    (child: node 'a)
    (node: node 'a): (node 'a) => switch node {
  | LevelNode depth _ _ nodes =>
      LevelNode depth (ref count) None (CopyOnWriteArray.update index child nodes)
  | LeafNode _ _ => failwith "Invalid state"
};

let updateLeafNodePersistent
    (index: int)
    (value: 'a)
    (node: node 'a): (node 'a) => switch node {
  | LevelNode depth _ _ nodes => failwith "Invalid state"
  | LeafNode _ values =>
      LeafNode None (values |> CopyOnWriteArray.update index value)
};

let mutate (vector: vector 'a): (transientVector 'a) =>
  Transient.create vector;

let addFirst (value: 'a) (vector: vector 'a): (vector 'a) =>
  addFirstWithMutator updateLevelNodePersistent None value vector;

let addLast (value: 'a) (vector: vector 'a): (vector 'a) =>
  addLastWithMutator updateLevelNodePersistent None value vector;

let add = addLast;

let removeFirst (vector: vector 'a): (vector 'a) =>
  removeFirstWithMutator updateLevelNodePersistent None vector;

let removeLast (vector: vector 'a): (vector 'a) =>
  removeLastWithMutator updateLevelNodePersistent None vector;

let toSeq (vector: vector 'a): seq 'a => Node.toSeq vector;

let toSeqReversed (vector: vector 'a): seq 'a => Node.toSeqReversed vector;

let update (index: int) (value: 'a) (vector: vector 'a): (vector 'a) =>
  vector |> updateWithMutator updateLevelNodePersistent updateLeafNodePersistent None index value;

let alter (index: int) (f: 'a => 'a) (vector: vector 'a): (vector 'a) =>
  vector |> alterWithMutator updateLevelNodePersistent updateLeafNodePersistent None index f;

let toIndexed (vector: vector 'a): (indexed 'a) =>  Indexed.create
  count::(vector |> count)
  rseq::(vector |> toSeqReversed)
  seq::(vector |> toSeq)
  tryGet::(fun i => vector |> tryGet i);

let module TransientVector = {
  let updateLevelNodeTransient
      (owner: owner)
      (count: int)
      (index: int)
      (child: node 'a)
      (node: node 'a): (node 'a) => switch node {
    | LevelNode depth nodeCount (Some nodeOwner) nodes when nodeOwner === owner =>
        nodes.(index) = child;
        nodeCount := count;
        node
    | LevelNode depth _ _ nodes =>
        LevelNode depth (ref count) (Some owner) (CopyOnWriteArray.update index child nodes)
    | LeafNode _ _ => failwith "Invalid state"
  };

  let updateLeafNodeTransient
      (owner: owner)
      (index: int)
      (value: 'a)
      (node: node 'a): (node 'a) => switch node {
    | LeafNode (Some nodeOwner) values when nodeOwner === owner =>
        values.(index) = value;
        node
    | LeafNode _ values =>
        LeafNode (Some owner) (values |> CopyOnWriteArray.update index value)
    | _ => failwith "Invalid state"
  };

  let addFirst (value: 'a) (transient: transientVector 'a): (transientVector 'a) =>
    transient |> Transient.update (fun owner => addFirstWithMutator (updateLevelNodeTransient owner) (Some owner) value);

  let addLast (value: 'a) (transient: transientVector 'a): (transientVector 'a) =>
    transient |> Transient.update (fun owner => addLastWithMutator (updateLevelNodeTransient owner) (Some owner) value);

  let add = addLast;

  let addAll
      (seq: seq 'a)
      (transient: transientVector 'a): (transientVector 'a) => seq
    |> Seq.reduce (fun acc next => acc |> addLast next) transient;

  let count (transient: transientVector 'a): int => transient |> Transient.get |> count;

  let isEmpty (transient: transientVector 'a): bool =>
    (count transient) == 0;

  let persist (transient: transientVector 'a): (vector 'a) =>
    transient |> Transient.persist;

  let removeAll (transient: transientVector 'a): (transientVector 'a) =>
      transient |> Transient.update (fun owner _ => empty);

  let removeFirst (transient: transientVector 'a): (transientVector 'a) =>
    transient |> Transient.update (fun owner =>
      removeFirstWithMutator (updateLevelNodeTransient owner) (Some owner)
    );

  let removeLast (transient: transientVector 'a): (transientVector 'a) =>
    transient |> Transient.update (fun owner =>
      removeLastWithMutator (updateLevelNodeTransient owner) (Some owner)
    );

  let get (index: int) (transient: transientVector 'a): 'a =>
    transient |> Transient.get |> get index;

  let first (transient: transientVector 'a): 'a => get 0 transient;

  let last (transient: transientVector 'a): 'a => get ((count transient) - 1) transient;

  let tryGet (index: int) (transient: transientVector 'a): (option 'a) =>
    transient |> Transient.get |> tryGet index;

  let tryFirst (transient: transientVector 'a): (option 'a) => tryGet 0 transient;

  let tryLast (transient: transientVector 'a): (option 'a) => tryGet ((count transient) - 1) transient;

  let update (index: int) (value: 'a) (transient: transientVector 'a): (transientVector 'a) =>
    transient |> Transient.update(fun owner =>
      updateWithMutator (updateLevelNodeTransient owner) (updateLeafNodeTransient owner) (Some owner) index value
    );

  let alter (index: int) (f: 'a => 'a) (transient: transientVector 'a): (transientVector 'a) =>
    transient |> Transient.update(fun owner =>
      alterWithMutator (updateLevelNodeTransient owner) (updateLeafNodeTransient owner) (Some owner) index f
    );
};

let addAll (seq: seq 'a) (trie: vector 'a): (vector 'a) => trie
  |> mutate
  |> TransientVector.addAll seq
  |> TransientVector.persist;

let fromSeq (seq: seq 'a): (vector 'a) =>
  empty |> addAll seq;
