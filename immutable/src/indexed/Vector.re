open Option.Operators;

let module VectorImpl = {
  module type VectorBase = {
    type t 'a;

    let addFirst: Transient.Owner.t => 'a => (t 'a) => (t 'a);
    let addLast: Transient.Owner.t => 'a => (t 'a) => (t 'a);
    let count: (t 'a) => int;
    let empty: unit => (t 'a);
    let getUnsafe: int => (t 'a) => 'a;
    let removeFirst: Transient.Owner.t => (t 'a) => (t 'a);
    let removeLast: Transient.Owner.t => (t 'a) => (t 'a);
    let updateUnsafe: Transient.Owner.t => int => 'a => (t 'a) => (t 'a);
    let updateWithUnsafe: Transient.Owner.t => int => ('a => 'a) => (t 'a) => (t 'a);
  };

  module type S = {
    type t 'a;

    let addFirst: Transient.Owner.t => 'a => (t 'a) => (t 'a);
    let addFirstAll: Transient.Owner.t => (Seq.t 'a) => (t 'a) => (t 'a);
    let addLast: Transient.Owner.t => 'a => (t 'a) => (t 'a);
    let addLastAll: Transient.Owner.t => (Seq.t 'a) => (t 'a) => (t 'a);
    let count: (t 'a) => int;
    let empty: unit => (t 'a);
    let first: (t 'a) => 'a;
    let get: int => (t 'a) => 'a;
    let isEmpty: (t 'a) => bool;
    let isNotEmpty: (t 'a) => bool;
    let last: (t 'a) => 'a;
    let removeAll: (t 'a) => (t 'a);
    let removeFirst: Transient.Owner.t => (t 'a) => (t 'a);
    let removeLast: Transient.Owner.t => (t 'a) => (t 'a);
    let tryFirst: (t 'a) => (option 'a);
    let tryGet: int => (t 'a) => (option 'a);
    let tryLast: (t 'a) => (option 'a);
    let update: Transient.Owner.t => int => 'a => (t 'a) => (t 'a);
    let updateWith: Transient.Owner.t => int => ('a => 'a) => (t 'a) => (t 'a);
  };

  let module Make = fun (X: VectorBase) => {
    type t 'a = X.t 'a;

    let addFirstAll (owner: Transient.Owner.t) (iter: Iterable.t 'a) (vector: t 'a): (t 'a) => iter
      |> Iterable.reduce
        (fun acc next => acc |> X.addFirst owner next)
        vector;

    let addLastAll (owner: Transient.Owner.t) (iter: Iterable.t 'a) (vector: t 'a): (t 'a) => iter
      |> Iterable.reduce
        (fun acc next => acc |> X.addLast owner next)
        vector;

    let addFirst = X.addFirst;
    let addLast = X.addLast;
    let count = X.count;

    let get (index: int) (vector: t 'a): 'a => {
      Preconditions.failIfOutOfRange (X.count vector) index;
      X.getUnsafe index vector;
    };

    let empty (): (t 'a) => X.empty ();

    let first (vector: t 'a): 'a => get 0 vector;

    let isEmpty (vector: t 'a): bool =>
      (X.count vector) == 0;

    let isNotEmpty (vector: t 'a): bool =>
      (X.count vector) != 0;

    let last (vector: t 'a): 'a => get ((X.count vector) - 1) vector;

    let removeAll (_: t 'a): (t 'a) => X.empty ();

    let removeFirst = X.removeFirst;

    let removeLast = X.removeLast;

    let tryGet (index: int) (vector: t 'a): (option 'a) => {
      let trieCount = count vector;
      Preconditions.noneIfIndexOutOfRange trieCount index (Functions.flip X.getUnsafe vector);
    };

    let tryFirst (vector: t 'a): (option 'a) => tryGet 0 vector;

    let tryLast (vector: t 'a): (option 'a) => tryGet ((X.count vector) - 1) vector;

    let update (owner: Transient.Owner.t) (index: int) (value: 'a) (vector: t 'a): (t 'a) => {
      Preconditions.failIfOutOfRange (X.count vector) index;
      X.updateUnsafe owner index value vector;
    };

    let updateWith (owner: Transient.Owner.t) (index: int) (f: 'a => 'a) (vector: t 'a): (t 'a) => {
      Preconditions.failIfOutOfRange (X.count vector) index;
      X.updateWithUnsafe owner index f vector;
    };
  };
};

type t 'a = {
  left: array 'a,
  middle: IndexedTrie.t 'a,
  right: array 'a,
};

let empty = {
  left: [||],
  middle: IndexedTrie.empty,
  right: [||],
};

let module PersistentVector = VectorImpl.Make {
  type nonrec t 'a = t 'a;

  let tailIsFull (arr: array 'a): bool => (CopyOnWriteArray.count arr) == IndexedTrie.width;
  let tailIsNotFull (arr: array 'a): bool => (CopyOnWriteArray.count arr) != IndexedTrie.width;

  let count ({ left, middle, right }: t 'a): int => {
    let leftCount = CopyOnWriteArray.count left;
    let middleCount = IndexedTrie.count middle;
    let rightCount = CopyOnWriteArray.count right;

    leftCount + middleCount + rightCount;
  };

  let empty () => empty;

  let addFirst (_: Transient.Owner.t) (value: 'a) ({ left, middle, right }: t 'a): (t 'a) =>
    if ((tailIsFull left) && (CopyOnWriteArray.isNotEmpty right)) {
      left: [| value |],
      middle: IndexedTrie.addFirstLeafUsingMutator IndexedTrie.updateLevelPersistent Transient.Owner.none left middle,
      right,
    }
    else if ((tailIsFull left) && (CopyOnWriteArray.isEmpty right)) {
      left: [| value |],
      middle,
      right: left,
    }
    else {
      left: left |> CopyOnWriteArray.addFirst value,
      middle,
      right,
    };

  let addLast (_: Transient.Owner.t) (value: 'a) ({ left, middle, right }: t 'a): (t 'a) =>
    /* If right is empty, then middle is also empty */
    if ((tailIsNotFull left) && (CopyOnWriteArray.isEmpty right)) {
      left: left |> CopyOnWriteArray.addLast value,
      middle,
      right,
    }
    else if (tailIsNotFull right) {
      left,
      middle,
      right: right |> CopyOnWriteArray.addLast value,
    }
    else {
      left,
      middle: IndexedTrie.addLastLeafUsingMutator IndexedTrie.updateLevelPersistent Transient.Owner.none right middle,
      right: [| value |],
    };

  let removeFirst (_: Transient.Owner.t) ({ left, middle, right }: t 'a): (t 'a) => {
    let leftCount = CopyOnWriteArray.count left;
    let middleCount = IndexedTrie.count middle;
    let rightCount = CopyOnWriteArray.count right;

    if (leftCount > 1) {
      left: CopyOnWriteArray.removeFirst left,
      middle,
      right,
    }
    else if (middleCount > 0) {
      let (IndexedTrie.Leaf _ left, middle) =
        IndexedTrie.removeFirstLeafUsingMutator IndexedTrie.updateLevelPersistent Transient.Owner.none middle;
      { left, middle, right };
    }
    else if (rightCount > 0) {
      left: right,
      middle,
      right: [||],
    }
    else if (leftCount == 1) (empty ())
    else failwith "vector is empty";
  };

  let removeLast (_: Transient.Owner.t) ({ left, middle, right }: t 'a): (t 'a) => {
    let leftCount = CopyOnWriteArray.count left;
    let middleCount = IndexedTrie.count middle;
    let rightCount = CopyOnWriteArray.count right;

    if (rightCount > 1) {
      left,
      middle,
      right: CopyOnWriteArray.removeLast right,
    }
    else if (middleCount > 0) {
      let (middle, IndexedTrie.Leaf _ right) =
        IndexedTrie.removeLastLeafUsingMutator IndexedTrie.updateLevelPersistent Transient.Owner.none middle;
      { left, middle, right };
    }
    else if (rightCount == 1) {
      left,
      middle,
      right: [||],
    }
    else if (leftCount > 0) {
      left: CopyOnWriteArray.removeLast left,
      middle,
      right,
    }
    else failwith "vector is empty";
  };

  let getUnsafe (index: int) ({ left, middle, right }: t 'a): 'a => {
    let leftCount = CopyOnWriteArray.count left;
    let middleCount = IndexedTrie.count middle;

    let rightIndex = index - middleCount - leftCount;

    if (index < leftCount) left.(index)
    else if (rightIndex >= 0) right.(rightIndex)
    else {
      let index = index - leftCount;
      middle |> IndexedTrie.get index;
    }
  };

  let updateUnsafe
      (_: Transient.Owner.t)
      (index: int)
      (value: 'a)
      ({ left, middle, right }: t 'a): (t 'a) => {
    let leftCount = CopyOnWriteArray.count left;
    let middleCount = IndexedTrie.count middle;

    let rightIndex = index - middleCount - leftCount;

    if (index < leftCount) {
      left: left |>  CopyOnWriteArray.update index value,
      middle,
      right,
    }
    else if (rightIndex >= 0) {
      left,
      middle,
      right: right |> CopyOnWriteArray.update rightIndex value,
    }
    else {
      let index = (index - leftCount);
      let middle = middle |> IndexedTrie.updateUsingMutator
        IndexedTrie.updateLevelPersistent
        IndexedTrie.updateLeafPersistent
        Transient.Owner.none
        index
        value;

      { left, middle, right }
    };
  };

  let updateWithUnsafe
      (_: Transient.Owner.t)
      (index: int)
      (f: 'a => 'a)
      ({ left, middle, right }: t 'a): (t 'a) => {
    let leftCount = CopyOnWriteArray.count left;
    let middleCount = IndexedTrie.count middle;

    let rightIndex = index - middleCount - leftCount;

    if (index < leftCount) {
      left: left |>  CopyOnWriteArray.updateWith index f,
      middle,
      right,
    }
    else if (rightIndex >= 0) {
      left,
      middle,
      right: right |> CopyOnWriteArray.updateWith rightIndex f,
    }
    else {
      let index = (index - leftCount);
      let middle = middle |> IndexedTrie.updateWithUsingMutator
        IndexedTrie.updateLevelPersistent
        IndexedTrie.updateLeafPersistent
        Transient.Owner.none
        index
        f;

      { left, middle, right }
    };
  };
};

type transientVectorImpl 'a = {
  mutable left: array 'a,
  mutable leftCount: int,
  mutable middle: IndexedTrie.t 'a,
  mutable right: array 'a,
  mutable rightCount: int,
};

let tailCopyAndExpand (arr: array 'a): (array 'a) => {
  let arrCount = CopyOnWriteArray.count arr;
  let retval = Array.make IndexedTrie.width arr.(0);
  Array.blit arr 0 retval 0 (min arrCount IndexedTrie.width);
  retval;
};

let module TransientVectorImpl = VectorImpl.Make {
  type t 'a = transientVectorImpl 'a;

  let tailIsEmpty (count: int): bool => count == 0;
  let tailIsFull (count: int): bool => count == IndexedTrie.width;
  let tailIsNotEmpty (count: int): bool => count != 0;
  let tailIsNotFull (count: int): bool => count != IndexedTrie.width;

  let tailAddFirst (value: 'a) (arr: array 'a): (array 'a) => {
    let arr =
      if (CopyOnWriteArray.isEmpty arr) (Array.make IndexedTrie.width value)
      else arr;

    let rec loop index =>
      if (index > 0) {
        arr.(index) = arr.(index - 1);
        loop (index - 1);
      }
      else ();

    loop (CopyOnWriteArray.lastIndex arr);
    arr.(0) = value;
    arr;
  };

  let tailRemoveFirst (arr: array 'a): (array 'a) => {
    let countArr = CopyOnWriteArray.count arr;
    let rec loop index =>
      if (index < countArr) {
        arr.(index - 1) = arr.(index);
        loop (index + 1);
      }
      else arr;

    loop 1;
  };

  let tailUpdate (index: int) (value: 'a) (arr: array 'a): (array 'a) => {
    let arr =
      if (CopyOnWriteArray.isEmpty arr) (Array.make IndexedTrie.width value)
      else arr;

    arr.(index) = value;
    arr;
  };

  let count ({ leftCount, middle, rightCount }: t 'a): int => {
    let middleCount = IndexedTrie.count middle;
    leftCount + middleCount + rightCount;
  };

  let empty () => {
    left: [||],
    leftCount: 0,
    middle: IndexedTrie.empty,
    right: [||],
    rightCount: 0,
  };

  let addFirst
      (owner: Transient.Owner.t)
      (value: 'a)
      ({
        left,
        leftCount,
        middle,
        rightCount,
      } as transientVec: t 'a): (t 'a) => {
    if ((tailIsFull leftCount) && (tailIsNotEmpty rightCount)) {
      transientVec.left = Array.make IndexedTrie.width value;
      transientVec.leftCount = 1;
      transientVec.middle = IndexedTrie.addFirstLeafUsingMutator
        IndexedTrie.updateLevelTransient
        owner
        left
        middle;
    }
    else if ((tailIsFull leftCount) && (tailIsEmpty rightCount)) {
      transientVec.left = Array.make IndexedTrie.width value;
      transientVec.leftCount = 1;
      transientVec.right = left;
      transientVec.rightCount = leftCount;
    }
    else {
      transientVec.left = left |> tailAddFirst value;
      transientVec.leftCount = leftCount + 1;
    };

    transientVec
  };

  let addLast
      (owner: Transient.Owner.t)
      (value: 'a)
      ({
        left,
        leftCount,
        middle,
        right,
        rightCount,
      } as transientVec: t 'a): (t 'a) => {
    /* If right is empty, then middle is also empty */
    if ((tailIsNotFull leftCount) && (tailIsEmpty rightCount)) {
      transientVec.left = left |> tailUpdate leftCount value;
      transientVec.leftCount = leftCount + 1;
    }
    else if (tailIsNotFull rightCount) {
      transientVec.right = right |> tailUpdate rightCount value;
      transientVec.rightCount = rightCount + 1;
    }
    else {
      transientVec.middle = IndexedTrie.addLastLeafUsingMutator
        IndexedTrie.updateLevelTransient
        owner
        right
        middle;
      transientVec.right = Array.make IndexedTrie.width value;
      transientVec.rightCount = 1;
    };

    transientVec
  };

  let removeFirst
      (owner: Transient.Owner.t)
      ({
        left,
        leftCount,
        middle,
        right,
        rightCount,
      } as transientVec: t 'a): (t 'a) => {
    if (leftCount > 1) {
      transientVec.left = tailRemoveFirst left;
      transientVec.leftCount = leftCount - 1;
    }
    else if ((IndexedTrie.count middle) > 0) {
      let (IndexedTrie.Leaf leftOwner left, middle) = middle
        |> IndexedTrie.removeFirstLeafUsingMutator IndexedTrie.updateLevelTransient owner;
      let leftCount = CopyOnWriteArray.count left;

      let left =
        if (leftOwner === owner && leftCount == IndexedTrie.width) left
        else tailCopyAndExpand left;

      transientVec.left = left;
      transientVec.leftCount = leftCount;
      transientVec.middle = middle;
    }
    else if (rightCount > 0) {
      transientVec.left = right;
      transientVec.leftCount = rightCount;
      transientVec.right = Array.make IndexedTrie.width right.(0);
      transientVec.rightCount = 0;
    }
    else if (leftCount == 1) {
      transientVec.leftCount = 0;
    }
    else failwith "vector is empty";

    transientVec
  };

  let removeLast
      (owner: Transient.Owner.t)
      ({
        left,
        leftCount,
        middle,
        right,
        rightCount,
      } as transientVec: t 'a): (t 'a) => {
    if (rightCount > 1) {
      transientVec.rightCount = rightCount - 1;
    }
    else if ((IndexedTrie.count middle) > 0) {
      let (middle, IndexedTrie.Leaf rightOwner right) = middle
        |> IndexedTrie.removeLastLeafUsingMutator IndexedTrie.updateLevelTransient owner;
      let rightCount = CopyOnWriteArray.count right;

      let right =
        if (rightOwner === owner && rightCount == IndexedTrie.width) right
        else tailCopyAndExpand right;

      transientVec.middle = middle;
      transientVec.right = right;
      transientVec.rightCount = rightCount;
    }
    else if (rightCount == 1) {
      transientVec.rightCount = 0;
    }
    else if (leftCount > 0) {
      transientVec.leftCount = leftCount - 1;
    }
    else failwith "vector is empty";

    transientVec
  };

  let getUnsafe
      (index: int)
      ({
        left,
        leftCount,
        middle,
        right,
        _,
      }: t 'a): 'a => {
    let middleCount = IndexedTrie.count middle;
    let rightIndex = index - middleCount - leftCount;

    if (index < leftCount) left.(index)
    else if (rightIndex >= 0) right.(rightIndex)
    else {
      let index = index - leftCount;
      middle |> IndexedTrie.get index;
    }
  };

  let updateUnsafe
      (owner: Transient.Owner.t)
      (index: int)
      (value: 'a)
      ({
        left,
        leftCount,
        middle,
        right,
        rightCount,
      } as transientVec: t 'a): (t 'a) => {
    let middleCount = IndexedTrie.count middle;
    let rightIndex = index - middleCount - leftCount;

    if (index < leftCount) {
      transientVec.left = left |> tailUpdate index value;
    }
    else if (rightIndex >= 0) {
      transientVec.right = right |> tailUpdate rightIndex value;
    }
    else {
      let index = (index - leftCount);
      let middle = middle |> IndexedTrie.updateUsingMutator
        IndexedTrie.updateLevelTransient
        IndexedTrie.updateLeafTransient
        owner
        index
        value;

      transientVec.middle = middle;
    };

    transientVec
  };

  let updateWithUnsafe
      (owner: Transient.Owner.t)
      (index: int)
      (f: 'a => 'a)
      ({
        left,
        leftCount,
        middle,
        right,
        rightCount,
      } as transientVec: t 'a): (t 'a) => {
    let middleCount = IndexedTrie.count middle;
    let rightIndex = index - middleCount - leftCount;

    if (index < leftCount) {
      transientVec.left = left |> tailUpdate index (f left.(index));
    }
    else if (rightIndex >= 0) {
      transientVec.right = right |> tailUpdate rightIndex (f right.(rightIndex));
    }
    else {
      let index = (index - leftCount);
      let middle = middle |> IndexedTrie.updateWithUsingMutator
        IndexedTrie.updateLevelTransient
        IndexedTrie.updateLeafTransient
        owner
        index
        f;

      transientVec.middle = middle;
    };

    transientVec
  };
};

let addFirst value => PersistentVector.addFirst Transient.Owner.none value;
let addLast value => PersistentVector.addLast Transient.Owner.none value;
let count = PersistentVector.count;
let first = PersistentVector.first;
let get = PersistentVector.get;
let isEmpty = PersistentVector.isEmpty;
let isNotEmpty = PersistentVector.isNotEmpty;
let last = PersistentVector.last;
let removeAll = PersistentVector.removeAll;
let removeFirst vector => PersistentVector.removeFirst Transient.Owner.none vector;
let removeLast vector => PersistentVector.removeLast Transient.Owner.none vector;
let tryFirst = PersistentVector.tryFirst;
let tryGet = PersistentVector.tryGet;
let tryLast = PersistentVector.tryLast;
let update index => PersistentVector.update Transient.Owner.none index;
let updateWith index => PersistentVector.updateWith Transient.Owner.none index;

module TransientVector = {
  type vector 'a = t 'a;
  type t 'a = Transient.t (TransientVectorImpl.t 'a);

  let mutate ({ left, middle, right }: vector 'a): (t 'a) => Transient.create {
    left: if (CopyOnWriteArray.count left > 0) (tailCopyAndExpand left) else [||],
    leftCount: CopyOnWriteArray.count left,
    middle,
    right: if (CopyOnWriteArray.count right > 0) (tailCopyAndExpand right) else [||],
    rightCount: CopyOnWriteArray.count right,
  };

  let addFirst (value: 'a) (transient: t 'a): (t 'a) =>
    transient |> Transient.update1 TransientVectorImpl.addFirst value;

  let addFirstAll (iter: Iterable.t 'a) (transient: t 'a): (t 'a) =>
    transient |> Transient.update1 TransientVectorImpl.addFirstAll iter;

  let addLast (value: 'a) (transient: t 'a): (t 'a) =>
    transient |> Transient.update1 TransientVectorImpl.addLast value;

  let addLastAll (iter: Iterable.t 'a) (transient: t 'a): (t 'a) =>
    transient |> Transient.update1 TransientVectorImpl.addLastAll iter;

  let count (transient: t 'a): int =>
    transient |> Transient.get |> TransientVectorImpl.count;

  let empty () => empty |> mutate;

  let isEmpty (transient: t 'a): bool =>
    transient |> Transient.get |> TransientVectorImpl.isEmpty;

  let isNotEmpty (transient: t 'a): bool =>
    transient |> Transient.get |> TransientVectorImpl.isNotEmpty;

  let tailCompress (count: int) (arr: array 'a): (array 'a) => {
    let arrCount = CopyOnWriteArray.count arr;

    if (arrCount == count) arr
    else if (arrCount > 0) {
      let retval = Array.make count arr.(0);
      Array.blit arr 0 retval 0 count;
      retval;
    }
    else [||];
  };

  let persist (transient: t 'a): (vector 'a) => {
    let {
      left,
      leftCount,
      middle,
      right,
      rightCount,
    } = transient |> Transient.persist;

    {
      left: left |> tailCompress leftCount,
      middle,
      right: right |> tailCompress rightCount,
    }
  };

  let removeImpl
      (_: Transient.Owner.t)
      (vec: transientVectorImpl 'a) =>
    TransientVectorImpl.removeAll vec;

  let removeAll (transient: t 'a): (t 'a) =>
      transient |> Transient.update removeImpl;

  let removeFirst (transient: t 'a): (t 'a) =>
    transient |> Transient.update TransientVectorImpl.removeFirst;

  let removeLast (transient: t 'a): (t 'a) =>
    transient |> Transient.update TransientVectorImpl.removeLast;

  let get (index: int) (transient: t 'a): 'a =>
    transient |> Transient.get |> TransientVectorImpl.get index;

  let first (transient: t 'a): 'a =>
    transient |> Transient.get |> TransientVectorImpl.first;

  let last (transient: t 'a): 'a =>
    transient |> Transient.get |> TransientVectorImpl.last;

  let reverseImpl
      (owner: Transient.Owner.t)
      (vector: transientVectorImpl 'a): (transientVectorImpl 'a) => {
    let count = TransientVectorImpl.count vector;
    let lastIndex = count - 1;

    let rec loop indexFirst indexLast =>
      if (indexFirst < indexLast) {
        let first = vector |> TransientVectorImpl.get indexFirst;
        let last = vector |> TransientVectorImpl.get indexLast;

        vector
          |> TransientVectorImpl.update owner indexFirst first
          |> TransientVectorImpl.update owner indexLast last
          |> ignore;

        loop (indexFirst + 1) (indexLast - 1)
      }
      else vector;

    loop 0 lastIndex;
  };

  let reverse (transient: t 'a): (t 'a) =>
    transient |> Transient.update reverseImpl;

  let tryGet (index: int) (transient: t 'a): (option 'a) =>
    transient |> Transient.get |> TransientVectorImpl.tryGet index;

  let tryFirst (transient: t 'a): (option 'a) =>
    transient |> Transient.get |> TransientVectorImpl.tryFirst;

  let tryLast (transient: t 'a): (option 'a) =>
    transient |> Transient.get |> TransientVectorImpl.tryLast;

  let update (index: int) (value: 'a) (transient: t 'a): (t 'a) =>
    transient |> Transient.update2 TransientVectorImpl.update index value;

  let updateAllImpl
      (owner: Transient.Owner.t)
      (f: int => 'a => 'a)
      ({
        left,
        leftCount,
        middle,
        right,
        rightCount
      } as transientVec: transientVectorImpl 'a): (transientVectorImpl 'a) => {
    let index = ref 0;
    let updater value => {
      let result = f !index value;
      index := !index + 1;
      result;
    };

    for i in 0 to (leftCount - 1) { left.(i) = updater left.(i) };

    let middle = middle |> IndexedTrie.updateAllUsingMutator
      IndexedTrie.updateLevelTransient
      IndexedTrie.updateLeafTransient
      owner
      updater;

    for i in 0 to (rightCount - 1) { right.(i) = updater right.(i) };

    transientVec.middle = middle;
    transientVec
  };

  let updateAll (f: int => 'a => 'a) (transient: t 'a): (t 'a) =>
    transient |> Transient.update1 updateAllImpl f;

  let updateWith (index: int) (f: 'a => 'a) (transient: t 'a): (t 'a) =>
    transient |> Transient.update2 TransientVectorImpl.updateWith index f;

  /* Unimplemented functions */
  let insertAt (index: int) (value: 'a) (transient: t 'a): (t 'a) =>
    failwith "Not Implemented";

  let removeAt (index: int) (transient: t 'a): (t 'a) =>
    failwith "Not Implemented";
};

let mutate = TransientVector.mutate;

let addFirstAll (iter: Iterable.t 'a) (vec: t 'a): (t 'a) => vec
  |> mutate
  |> TransientVector.addFirstAll iter
  |> TransientVector.persist;

let addLastAll (iter: Iterable.t 'a) (vec: t 'a): (t 'a) => vec
  |> mutate
  |> TransientVector.addLastAll iter
  |> TransientVector.persist;

let every (f: 'a => bool) ({ left, middle, right }: t 'a): bool =>
  (CopyOnWriteArray.every f left) && (IndexedTrie.every f middle) && (CopyOnWriteArray.every f right);

let everyWithIndex (f: int => 'a => bool) (vec: t 'a): bool => {
  /* kind of a hack, but a lot less code to write */
  let index = ref 0;
  let f next => {
    let result = f !index next;
    index := !index + 1;
    result
  };

  every f vec;
};

let equalsWith
    (valueEquals: Equality.t 'a)
    ({ left: thisLeft, middle: thisMiddle, right: thisRight } as this: t 'a)
    ({ left: thatLeft, middle: thatMiddle, right: thatRight } as that: t 'a): bool =>
  if (this === that) true
  else if ((count this) != (count that)) false
  else (
    CopyOnWriteArray.equalsWith valueEquals thisLeft thatLeft &&
    /* Perhaps could make this more efficient by avoiding use of Seq */
    Seq.equalsWith valueEquals (IndexedTrie.toSeq thisMiddle) (IndexedTrie.toSeq thatMiddle) &&
    CopyOnWriteArray.equalsWith valueEquals thisRight thatRight
  );

let equals (this: t 'a) (that: t 'a): bool =>
  equalsWith Equality.structural this that;

let find (f: 'a => bool) ({ left, middle, right }: t 'a): 'a =>
  /* FIXME: Add an operator to Option for this use case */
  switch (left |> CopyOnWriteArray.tryFind f) {
    | Some v => v
    | _ => switch (middle |> IndexedTrie.tryFind f) {
      | Some v => v
      | _ => right |> CopyOnWriteArray.find f
    }
  };

let findWithIndex (f: int => 'a => bool) (vec: t 'a): 'a => {
  /* kind of a hack, but a lot less code to write */
  let index = ref 0;
  let f next => {
    let result = f !index next;
    index := !index + 1;
    result
  };

  find f vec;
};

let from (iter: Iterable.t 'a): (t 'a) =>
  empty |> addLastAll iter;

let fromReversed (iter: Iterable.t 'a): (t 'a) =>
  empty|> addFirstAll iter;

let indexOf (f: 'a => bool) (vec: t 'a): int => {
  /* kind of a hack, but a lot less code to write */
  let index = ref (-1);
  findWithIndex (fun i v => {
    let result = f v;
    if result { index := i };
    result;
  }) vec;

  if (!index >= 0) !index else failwith "not found";
};

let indexOfWithIndex (f: int => 'a => bool) (vec: t 'a): int => {
  /* kind of a hack, but a lot less code to write */
  let index = ref (-1);
  findWithIndex (fun i v => {
    let result = f i v;
    if result { index := i };
    result;
  }) vec;

  if (!index >= 0) !index else failwith "not found";
};

let init (count: int) (f: int => 'a): (t 'a) => IntRange.create 0 count
  |> IntRange.reduce (fun acc next => acc |> TransientVector.addLast (f next)) (mutate empty)
  |> TransientVector.persist;

let none (f: 'a => bool) ({ left, middle, right }: t 'a): bool =>
  (CopyOnWriteArray.none f left) && (IndexedTrie.none f middle) && (CopyOnWriteArray.none f right);

let noneWithIndex (f: int => 'a => bool) (vec: t 'a): bool => {
  /* kind of a hack, but a lot less code to write */
  let index = ref 0;
  let f next => {
    let result = f !index next;
    index := !index + 1;
    result
  };

  none f vec;
};

let some (f: 'a => bool) ({ left, middle, right }: t 'a): bool =>
  (CopyOnWriteArray.some f left) || (IndexedTrie.some f middle) || (CopyOnWriteArray.some f right);

let containsWith (valueEquals: Equality.t 'a) (value: 'a) (vec: t 'a): bool =>
  some (valueEquals value) vec;

let contains (value: 'a) (vec: t 'a): bool =>
  containsWith Equality.structural value vec;

let someWithIndex (f: int => 'a => bool) (vec: t 'a): bool => {
  /* kind of a hack, but a lot less code to write */
  let index = ref 0;
  let f next => {
    let result = f !index next;
    index := !index + 1;
    result
  };

  some f vec;
};

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) ({ left, middle, right }: t 'a): 'acc => {
  let acc = left |> CopyOnWriteArray.reduce f acc;
  let acc = middle |> IndexedTrie.reduce f acc;
  let acc = right |> CopyOnWriteArray.reduce f acc;
  acc;
};

let forEach (f: 'a => unit) (vec: t 'a): unit =>
  vec |> reduce (fun _ next => f next) ();

let reduceWithIndex (f: 'acc => int => 'a => 'acc) (acc: 'acc) (vec: t 'a): 'acc => {
  /* kind of a hack, but a lot less code to write */
  let index = ref 0;
  let reducer acc next => {
    let acc = f acc !index next;
    index := !index + 1;
    acc
  };

  reduce reducer acc vec;
};

let forEachWithIndex (f: int => 'a => unit) (vec: t 'a): unit =>
  vec |> reduceWithIndex (fun _ index next => f index next) ();

let reduceRight (f: 'acc => 'a => 'acc) (acc: 'acc) ({ left, middle, right }: t 'a): 'acc => {
  let acc = right |> CopyOnWriteArray.reduceRight f acc;
  let acc = middle |> IndexedTrie.reduceRight f acc;
  let acc = left |> CopyOnWriteArray.reduceRight f acc;
  acc;
};

let forEachReverse (f: 'a => unit) (vec: t 'a): unit =>
  vec |> reduceRight (fun _ next => f next) ();

let reduceRightWithIndex (f: 'acc => int => 'a => 'acc) (acc: 'acc) (vec: t 'a): 'acc => {
  /* kind of a hack, but a lot less code to write */
  let index = ref (count vec - 1);
  let reducer acc next => {
    let acc = f acc !index next;
    index := !index - 1;
    acc
  };

  reduceRight reducer acc vec;
};

let forEachReverseWithIndex (f: int => 'a => unit) (vec: t 'a): unit =>
  vec |> reduceRightWithIndex (fun _ index next => f index next) ();

let hashWith (hash: Hash.t 'a) (vec: t 'a): int =>
  vec |> reduce (Hash.reducer hash) Hash.initialValue;

let hash (vec: t 'a): int =>
  hashWith Hash.structural vec;

let map (f: 'a => 'b) (vector: t 'a): (t 'b) => vector
  |> reduce
    (fun acc next => acc |> TransientVector.addLast @@ f @@ next)
    (mutate empty)
  |> TransientVector.persist;

let mapWithIndex (f: int => 'a => 'b) (vector: t 'a): (t 'b) => vector
  |> reduceWithIndex
    (fun acc index next => acc |> TransientVector.addLast @@ f index @@ next)
    (mutate empty)
  |> TransientVector.persist;

let mapReverse (f: 'a => 'b) (vector: t 'a): (t 'b) => vector
  |> reduceRight
    (fun acc next => acc |> TransientVector.addLast @@ f @@ next)
    (mutate empty)
  |> TransientVector.persist;

let mapReverseWithIndex (f: int => 'a => 'b) (vector: t 'a): (t 'b) => vector
  |> reduceWithIndex
    (fun acc index next => acc |> TransientVector.addFirst @@ f index @@ next)
    (mutate empty)
  |> TransientVector.persist;

let return (value: 'a): (t 'a) =>
  empty |> addLast value;

let reverse (vector: t 'a): (t 'a) => vector
  |> reduceRight
    (fun acc next => acc |> TransientVector.addLast next)
    (mutate empty)
  |> TransientVector.persist;

let skip (skipCount: int) ({ left, middle, right } as vec: t 'a): (t 'a) => {
  let vectorCount = count vec;
  let leftCount = CopyOnWriteArray.count left;
  let middleCount = IndexedTrie.count middle;

  if (skipCount >= vectorCount) empty
  else if (skipCount < leftCount) {
    left: left |> CopyOnWriteArray.skip skipCount,
    middle,
    right,
  }
  else if (skipCount == leftCount) {
    let (IndexedTrie.Leaf _ left, middle) = IndexedTrie.removeFirstLeafUsingMutator
      IndexedTrie.updateLevelPersistent
      Transient.Owner.none
      middle;

    { left, middle, right }
  }
  else if (skipCount - leftCount < middleCount) {
    let skipCount = skipCount - leftCount;
    let (left, middle) = IndexedTrie.skip Transient.Owner.none skipCount middle;
    { left, middle, right }
  }
  else {
    let skipCount = skipCount - leftCount - middleCount;
    {
      left:  right |> CopyOnWriteArray.skip skipCount,
      middle: IndexedTrie.empty,
      right: [||],
    }
  }
};

let take (takeCount: int) ({ left, middle, right } as vec: t 'a): (t 'a) => {
  let vectorCount = count vec;
  let leftCount = CopyOnWriteArray.count left;
  let middleCount = IndexedTrie.count middle;

  if (takeCount >= vectorCount) vec
  else if (takeCount <= leftCount) {
    left: left |> CopyOnWriteArray.take takeCount,
    middle: IndexedTrie.empty,
    right: [||],
  }
  else if (takeCount - leftCount < middleCount) {
    let takeCount = takeCount - leftCount;
    let (middle, right) = IndexedTrie.take Transient.Owner.none takeCount middle;
    { left, middle, right }
  }
  else if (takeCount - leftCount == middleCount) {
    let (middle, IndexedTrie.Leaf _ right) = IndexedTrie.removeLastLeafUsingMutator
      IndexedTrie.updateLevelPersistent
      Transient.Owner.none
      middle;

    { left, middle, right }
  }
  else {
    let takeCount = takeCount - leftCount - middleCount;
    { left, middle, right: right |> CopyOnWriteArray.take takeCount }
  }
};

/* FIXME: Likely could be made more efficient with a custom implementation */
let range (startIndex: int) (takeCount: option int) (vec: t 'a): (t 'a) =>
   vec |> skip startIndex |> take (takeCount |? (count vec));

let toIterable (set: t 'a): (Iterable.t 'a) =>
  if (isEmpty set) Iterable.empty
  else { reduce: fun f acc => reduce f acc set };

let toIterableReversed (set: t 'a): (Iterable.t 'a) =>
  if (isEmpty set) Iterable.empty
  else { reduce: fun f acc => reduceRight f acc set };

let toKeyedIterable (arr: t 'a): (KeyedIterable.t int 'a) =>
  if (isEmpty arr) KeyedIterable.empty
  else { reduce: fun f acc => reduceWithIndex f acc arr };

let toKeyedIterableReversed (arr: t 'a): (KeyedIterable.t int 'a) =>
  if (isEmpty arr) KeyedIterable.empty
  else { reduce: fun f acc => reduceRightWithIndex f acc arr };

let toSeq ({ left, middle, right }: t 'a): (Seq.t 'a) => Seq.concat [
  CopyOnWriteArray.toSeq left,
  IndexedTrie.toSeq middle,
  CopyOnWriteArray.toSeq right,
];

let compareWith
    (compareValue: Comparator.t 'a)
    (this: t 'a)
    (that: t 'a): Ordering.t =>
  if (this === that) Ordering.equal
  else Seq.compareWith compareValue (toSeq this) (toSeq that);

let compare (this: t 'a) (that: t 'a): Ordering.t =>
  compareWith Comparator.structural this that;

let toSeqReversed ({ left, middle, right }: t 'a): (Seq.t 'a) => Seq.concat [
  CopyOnWriteArray.toSeqReversed right,
  IndexedTrie.toSeqReversed middle,
  CopyOnWriteArray.toSeqReversed left,
];

let tryFind (f: 'a => bool) ({ left, middle, right }: t 'a): (option 'a) =>
  /* FIXME: Add an operator to Option for this use case */
  switch (left |> CopyOnWriteArray.tryFind f) {
    | Some _ as v => v
    | _ => switch (middle |> IndexedTrie.tryFind f) {
      | Some _ as v => v
      | _ => right |> CopyOnWriteArray.tryFind f
    }
  };

let tryFindWithIndex (f: int => 'a => bool) (vec: t 'a): (option 'a) => {
  /* kind of a hack, but a lot less code to write */
  let index = ref 0;
  let f next => {
    let result = f !index next;
    index := !index + 1;
    result
  };

  tryFind f vec;
};

let tryIndexOf (f: 'a => bool) (vec: t 'a): (option int) => {
  /* kind of a hack, but a lot less code to write */
  let index = ref (-1);
  tryFindWithIndex (fun i v => {
    let result = f v;
    if result { index := i };
    result;
  }) vec |> ignore;

  if (!index >= 0) (Some !index) else None;
};

let tryIndexOfWithIndex (f: int => 'a => bool) (vec: t 'a): (option int) => {
  /* kind of a hack, but a lot less code to write */
  let index = ref (-1);
  tryFindWithIndex (fun i v => {
    let result = f i v;
    if result { index := i };
    result;
  }) vec |> ignore;

  if (!index >= 0) (Some !index) else None;
};

let toMap (vec: t 'a): (ImmMap.t int 'a) => {
  containsWith: fun equals index value =>
    if (index >= 0 && index < count vec) (equals (get index vec) value)
    else false,
  containsKey: fun index => index >= 0 && index < count vec,
  count: count vec,
  every: fun f => everyWithIndex f vec,
  find: fun f => {
    let index = indexOfWithIndex f vec;
    (index, vec |> get index)
  },
  forEach: fun f => forEachWithIndex f vec,
  get: fun index => get index vec,
  none: fun f => noneWithIndex f vec,
  reduce: fun f acc => reduceWithIndex f acc vec,
  some: fun f => someWithIndex f vec,
  toSeq: Seq.zip2
    (IntRange.create 0 (count vec) |> IntRange.toSeq)
    (toSeq vec),
  tryFind: fun f => tryIndexOfWithIndex f vec >>| fun index => (index, vec |> get index),
  tryGet: fun i => tryGet i vec,
  values: toSeq vec,
};

let updateAll (f: int => 'a => 'a) (vec: t 'a): (t 'a) => vec
  |> mutate
  |> TransientVector.updateAll f
  |> TransientVector.persist;

/* Unimplemented functions */
let concat (vectors: list (t 'a)): (t 'a) =>
  failwith "Not Implemented";

let insertAt (index: int) (value: 'a) (vec: t 'a): (t 'a) =>
  failwith "Not Implemented";

let removeAt (index: int) (vec: t 'a): (t 'a) =>
  failwith "Not Implemented";
