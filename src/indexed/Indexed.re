/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type s 'indexed 'a = {
  count: 'indexed => int,
  first: 'indexed => (option 'a),
  firstOrRaise: 'indexed => 'a,
  get: int => 'indexed => (option 'a),
  getOrRaise: int => 'indexed => 'a,
  reduce: 'acc . while_::('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => 'indexed => 'acc,
  toCollection: 'indexed => Collection.t 'a,
  toIterable: 'indexed => Iterable.t 'a,
  toKeyedCollection: 'indexed => (KeyedCollection.t int 'a),
  toKeyedIterable: 'indexed => (KeyedIterable.t int 'a),
  toMap: 'indexed => (ImmMap.t int 'a),
  toNavigableCollection: 'indexed => NavigableCollection.t 'a,
  toNavigableKeyedCollection: 'indexed => (NavigableKeyedCollection.t int 'a),
  toNavigableMap: 'indexed => (NavigableMap.t int 'a),
  toSequence: 'indexed => Sequence.t 'a,
  toSequentialCollection: 'indexed => SequentialCollection.t 'a,
};

type t 'a =
  | Empty
  | Instance 'indexed (s 'indexed 'a) (s 'indexed 'a): t 'a;

let count (indexed: t 'a): int => switch indexed {
  | Empty => 0
  | Instance indexed { count } _ => count indexed
};

let empty (): (t 'a) => Empty;

let first (indexed: t 'a): (option 'a) => switch indexed {
  | Empty => None
  | Instance indexed { first } _ => first indexed
};

let firstOrRaise (indexed: t 'a): 'a => switch indexed {
  | Empty => failwith "empty"
  | Instance indexed { firstOrRaise } _ => firstOrRaise indexed
};

let get (index: int) (indexed: t 'a): (option 'a) => switch indexed {
  | Empty => None
  | Instance indexed { get } _ => get index indexed
};

let getOrRaise (index: int) (indexed: t 'a): 'a => switch indexed {
  | Empty => failwith "empty"
  | Instance indexed { getOrRaise } _ => getOrRaise index indexed
};

let isEmpty (indexed: t 'a): bool =>
  (count indexed) === 0;

let isNotEmpty (indexed: t 'a): bool =>
  (count indexed) !== 0;

let last (indexed: t 'a): (option 'a) => switch indexed {
  | Empty => None
  | Instance indexed _ { first } => first indexed
};

let lastOrRaise (indexed: t 'a): 'a => switch indexed {
  | Empty => failwith "empty"
  | Instance indexed _ { firstOrRaise } => firstOrRaise indexed
};

let reduce
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (collection: t 'a): 'acc => switch collection {
  | Empty => acc
  | Instance indexed { reduce } _ =>
      indexed |> reduce while_::predicate f acc;
};

let reduceReversed
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (collection: t 'a): 'acc => switch collection {
  | Empty => acc
  | Instance indexed _ { reduce } =>
      indexed |> reduce while_::predicate f acc;
};

let toCollection (indexed: t 'a): (Collection.t 'a) => switch indexed {
  | Empty => Collection.empty ()
  | Instance indexed { toCollection } _ => toCollection indexed
};

let toCollectionReversed (indexed: t 'a): (Collection.t 'a) => switch indexed {
  | Empty => Collection.empty ()
  | Instance indexed _ { toCollection } => toCollection indexed
};

let toIndexed (indexed: t 'a): (t 'a) => indexed;

let toIndexedReversed (indexed: t 'a): (t 'a) => switch indexed {
  | Empty => Empty
  | Instance indexed impl implReverse => Instance indexed implReverse impl
};

let toIterable (indexed: t 'a): (Iterable.t 'a) => switch indexed {
  | Empty => Iterable.empty ()
  | Instance indexed { toIterable } _ => toIterable indexed
};

let toIterableReversed (indexed: t 'a): (Iterable.t 'a) => switch indexed {
  | Empty => Iterable.empty ()
  | Instance indexed _ { toIterable } => toIterable indexed
};

let toKeyedCollection (indexed: t 'a): (KeyedCollection.t int 'a) => switch indexed {
  | Empty => KeyedCollection.empty ()
  | Instance indexed { toKeyedCollection } _ => toKeyedCollection indexed
};

let toKeyedCollectionReversed (indexed: t 'a): (KeyedCollection.t int 'a) => switch indexed {
  | Empty => KeyedCollection.empty ()
  | Instance indexed _ { toKeyedCollection } => toKeyedCollection indexed
};

let toKeyedIterable (indexed: t 'a): (KeyedIterable.t int 'a) => switch indexed {
  | Empty => KeyedIterable.empty ()
  | Instance indexed { toKeyedIterable } _ => toKeyedIterable indexed
};

let toKeyedIterableReversed (indexed: t 'a): (KeyedIterable.t int 'a) => switch indexed {
  | Empty => KeyedIterable.empty ()
  | Instance indexed _ { toKeyedIterable } => toKeyedIterable indexed
};

let toMap (indexed: t 'a): (ImmMap.t int 'a) => switch indexed {
  | Empty => ImmMap.empty ()
  | Instance indexed { toMap } _ => toMap indexed
};

let toMapReversed (indexed: t 'a): (ImmMap.t int 'a) => switch indexed {
  | Empty => ImmMap.empty ()
  | Instance indexed _ { toMap } => toMap indexed
};

let toNavigableCollection (indexed: t 'a): (NavigableCollection.t 'a) => switch indexed {
  | Empty => NavigableCollection.empty ()
  | Instance indexed { toNavigableCollection } _ => toNavigableCollection indexed
};

let toNavigableCollectionReversed (indexed: t 'a): (NavigableCollection.t 'a) => switch indexed {
  | Empty => NavigableCollection.empty ()
  | Instance indexed _ { toNavigableCollection } => toNavigableCollection indexed
};

let toNavigableKeyedCollection (indexed: t 'a): (NavigableKeyedCollection.t int 'a) => switch indexed {
  | Empty => NavigableKeyedCollection.empty ()
  | Instance indexed { toNavigableKeyedCollection } _ => toNavigableKeyedCollection indexed
};

let toNavigableKeyedCollectionReversed (indexed: t 'a): (NavigableKeyedCollection.t int 'a) => switch indexed {
  | Empty => NavigableKeyedCollection.empty ()
  | Instance indexed _ { toNavigableKeyedCollection } => toNavigableKeyedCollection indexed
};

let toNavigableMap (indexed: t 'a): (NavigableMap.t int 'a) => switch indexed {
  | Empty => NavigableMap.empty ()
  | Instance indexed { toNavigableMap } _ => toNavigableMap indexed
};

let toNavigableMapReversed (indexed: t 'a): (NavigableMap.t int 'a) => switch indexed {
  | Empty => NavigableMap.empty ()
  | Instance indexed _ { toNavigableMap } => toNavigableMap indexed
};

let toSequence (indexed: t 'a): (Sequence.t 'a) => switch indexed {
  | Empty => Sequence.empty ()
  | Instance indexed { toSequence } _ => toSequence indexed
};

let toSequenceReversed (indexed: t 'a): (Sequence.t 'a) => switch indexed {
  | Empty => Sequence.empty ()
  | Instance indexed _ { toSequence } => toSequence indexed
};

let toSequentialCollection (indexed: t 'a): (SequentialCollection.t 'a) => switch indexed {
  | Empty => SequentialCollection.empty ()
  | Instance indexed { toSequentialCollection } _ => toSequentialCollection indexed
};

let toSequentialCollectionReversed (indexed: t 'a): (SequentialCollection.t 'a) => switch indexed {
  | Empty => SequentialCollection.empty ()
  | Instance indexed _ { toSequentialCollection } => toSequentialCollection indexed
};

type indexed 'a = t 'a;
module type S1 = {
  type t 'a;

  include NavigableCollection.S1 with type t 'a := t 'a;

  let get: int => (t 'a) => (option 'a);
  let getOrRaise: int => (t 'a) => 'a;
  let toIndexed: (t 'a) => (indexed 'a);
  let toIndexedReversed: (t 'a) => (indexed 'a);
  let toKeyedCollection: (t 'a) => (KeyedCollection.t int 'a);
  let toKeyedCollectionReversed: (t 'a) => (KeyedCollection.t int 'a);
  let toKeyedIterable: (t 'a) => (KeyedIterable.t int 'a);
  let toKeyedIterableReversed: (t 'a) => (KeyedIterable.t int 'a);
  let toMap: (t 'a) => (ImmMap.t int 'a);
  let toMapReversed: (t 'a) => (ImmMap.t int 'a);
  let toNavigableKeyedCollection: (t 'a) => (NavigableKeyedCollection.t int 'a);
  let toNavigableKeyedCollectionReversed: (t 'a) => (NavigableKeyedCollection.t int 'a);
  let toNavigableMap: (t 'a) => (NavigableMap.t int 'a);
  let toNavigableMapReversed: (t 'a) => (NavigableMap.t int 'a);
};

let module Make1 = fun (Base: {
  type t 'a;

  let count: t 'a => int;
  let get: int => (t 'a) => (option 'a);
  let getOrRaise: int => (t 'a) => 'a;
  let reduce: while_::('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => t 'a => 'acc;
  let reduceReversed: while_::('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => t 'a => 'acc;
  let toSequence: t 'a => Sequence.t 'a;
  let toSequenceReversed: t 'a => Sequence.t 'a;
}) => ({
  include Base;

  include (NavigableCollection.Make1 {
    include Base;

    let first (indexed: t 'a): (option 'a) => get 0 indexed;

    let firstOrRaise (indexed: t 'a): 'a => getOrRaise 0 indexed;

    let last (indexed: t 'a): (option 'a) => {
      let lastIndex = (count indexed) - 1;
      get lastIndex indexed;
    };

    let lastOrRaise (indexed: t 'a): 'a => {
      let lastIndex = (count indexed) - 1;
      getOrRaise lastIndex indexed;
    };

  }: NavigableCollection.S1 with type t 'a := Base.t 'a);

  let module NavigableMap = NavigableMap.Make1 {
    type nonrec k = int;
    type nonrec t 'v = t 'v;

    let containsKey (index: int) (indexed: t 'v): bool =>
      index >= 0 && index < (count indexed);

    let count = count;

    let firstValue = first;

    let firstValueOrRaise = firstOrRaise;

    let first (indexed: t 'v): (option (int, 'v)) =>
      if (count indexed > 0) (Some (0, indexed |> getOrRaise 0))
      else None;

    let firstOrRaise (indexed: t 'v): (int, 'v) =>
      if (count indexed > 0) (0, indexed |> getOrRaise 0)
      else failwith "empty";

    let firstKey (indexed: t 'v): (option int) =>
      if (count indexed > 0) (Some 0)
      else None;

    let firstKeyOrRaise (indexed: t 'v): int =>
      if (count indexed > 0) 0
      else failwith "empty";

    let get = get;

    let getOrRaise = getOrRaise;

    let keysSequence (indexed: t 'v): (Sequence.t int) =>
      IntRange.create start::0 count::(count indexed) |> IntRange.toSequence;

    let keysSequenceReversed (indexed: t 'v): (Sequence.t int) =>
      IntRange.create start::0 count::(count indexed) |> IntRange.toSequenceReversed;

    let lastValue = last;

    let lastValueOrRaise = lastOrRaise;

    let last (indexed: t 'v): (option (int, 'v)) => {
      let lastIndex = count indexed - 1;
      if (lastIndex >= 0) (Some (lastIndex, indexed |> getOrRaise lastIndex))
      else None;
    };

    let lastOrRaise (indexed: t 'v): (int, 'v) => {
      let lastIndex = count indexed - 1;
      if (lastIndex >= 0) (lastIndex, indexed |> getOrRaise lastIndex)
      else failwith "empty";
    };

    let lastKey (indexed: t 'v): (option int) => {
      let lastIndex = count indexed - 1;
      if (lastIndex >= 0) (Some lastIndex)
      else None;
    };

    let lastKeyOrRaise (indexed: t 'v): int => {
      let lastIndex = count indexed - 1;
      if (lastIndex >= 0) lastIndex
      else failwith "empty";
    };

    let reduceValues = Base.reduce;

    let reduceValuesReversed = Base.reduceReversed;

    let reduce
        while_::(predicate: 'acc => int => 'v => bool)
        (f: 'acc => int => 'v => 'acc)
        (acc: 'acc)
        (indexed: t 'v): 'acc => {
      let index = ref 0;

      let predicate acc next =>
        predicate acc !index next;

      let reducer acc next => {
        let acc = f acc !index next;
        index := !index + 1;
        acc
      };

      Base.reduce while_::predicate reducer acc indexed;
    };

    let reduceReversed
        while_::(predicate: 'acc => int => 'v => bool)
        (f: 'acc => int => 'v => 'acc)
        (acc: 'acc)
        (indexed: t 'v): 'acc => {
      let index = ref 0;

      let predicate acc next =>
        predicate acc !index next;

      let reducer acc next => {
        let acc = f acc !index next;
        index := !index + 1;
        acc
      };

      Base.reduceReversed while_::predicate reducer acc indexed;
    };

    let reduceKeys
        while_::(predicate: 'acc => int => bool)
        (f: 'acc => int => 'acc)
        (acc: 'acc)
        (indexed: t 'v): 'acc =>
      IntRange.create start::0 count::(count indexed)
        |> IntRange.reduce while_::predicate f acc;

    let reduceKeysReversed
        while_::(predicate: 'acc => int => bool)
        (f: 'acc => int => 'acc)
        (acc: 'acc)
        (indexed: t 'v): 'acc =>
      IntRange.create start::0 count::(count indexed)
        |> IntRange.reduceReversed while_::predicate f acc;

    let toSequence (indexed: t 'v): (Sequence.t (int, 'v)) =>
      Sequence.zip2With
        (fun a b => (a, b))
        (IntRange.create start::0 count::(count indexed) |> IntRange.toSequence)
        (Base.toSequence indexed);

    let toSequenceReversed (indexed: t 'v): (Sequence.t (int, 'v)) =>
      Sequence.zip2With
        (fun a b => (a, b))
        (IntRange.create start::0 count::(count indexed) |> IntRange.toSequenceReversed)
        (Base.toSequenceReversed indexed);

    let valuesSequence (indexed: t 'v): (Sequence.t 'v) =>
      Base.toSequence indexed;

    let valuesSequenceReversed (indexed: t 'v): (Sequence.t 'v) =>
      Base.toSequenceReversed indexed;
  };

  let toKeyedCollection = NavigableMap.toKeyedCollection;
  let toKeyedCollectionReversed = NavigableMap.toKeyedCollectionReversed;
  let toKeyedIterable = NavigableMap.toKeyedIterable;
  let toKeyedIterableReversed = NavigableMap.toKeyedIterableReversed;
  let toMap = NavigableMap.toMap;
  let toMapReversed = NavigableMap.toMapReversed;
  let toNavigableKeyedCollection = NavigableMap.toNavigableKeyedCollection;
  let toNavigableKeyedCollectionReversed = NavigableMap.toNavigableKeyedCollectionReversed;
  let toNavigableMap = NavigableMap.toNavigableMap;
  let toNavigableMapReversed = NavigableMap.toNavigableMapReversed;

  let indexedBase = {
    count,
    first,
    firstOrRaise,
    get,
    getOrRaise,
    reduce: Base.reduce,
    toCollection,
    toIterable,
    toKeyedCollection,
    toKeyedIterable,
    toMap,
    toNavigableCollection,
    toNavigableKeyedCollection,
    toNavigableMap,
    toSequence,
    toSequentialCollection,
  };

  let indexedReversedBase = {
    count,
    first: last,
    firstOrRaise: lastOrRaise,
    get: fun i indexed =>
      get (count indexed - i - 1) indexed,
    getOrRaise: fun i indexed =>
      getOrRaise (count indexed - i - 1) indexed,
    reduce: Base.reduceReversed,
    toCollection: toCollectionReversed,
    toIterable: toIterableReversed,
    toKeyedCollection: toKeyedCollectionReversed,
    toKeyedIterable: toKeyedIterableReversed,
    toMap: toMapReversed,
    toNavigableCollection: toNavigableCollectionReversed,
    toNavigableKeyedCollection: toNavigableKeyedCollectionReversed,
    toNavigableMap: toNavigableMapReversed,
    toSequence: toSequenceReversed,
    toSequentialCollection: toSequentialCollectionReversed,
  };

  let toIndexed (indexed: t 'a): (indexed 'a) =>
    if (isEmpty indexed) (empty ())
    else Instance indexed indexedBase indexedReversedBase;

  let toIndexedReversed (indexed: t 'a): (indexed 'a) =>
    if (isEmpty indexed) (empty ())
    else Instance indexed indexedReversedBase indexedBase;
}: S1 with type t 'a := Base.t 'a);
