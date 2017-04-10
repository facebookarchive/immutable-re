/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Functions.Operators;

let module Ops = {
  type t 'a 'indexed = {
    count: 'indexed => int,
    first: 'indexed => (option 'a),
    firstOrRaise: 'indexed => 'a,
    get: int => 'indexed => (option 'a),
    getOrRaise: int => 'indexed => 'a,
    last: 'indexed => (option 'a),
    lastOrRaise: 'indexed => 'a,
    toCollection: 'indexed => Collection.t 'a,
    toSequentialCollection: 'indexed => SequentialCollection.t 'a,
    toIterable: 'indexed => Iterable.t 'a,
    toIterableReversed: 'indexed => Iterable.t 'a,
    toKeyedCollection: 'indexed => (KeyedCollection.t int 'a),
    toKeyedIterable: 'indexed => (KeyedIterable.t int 'a),
    toKeyedIterableReversed: 'indexed => (KeyedIterable.t int 'a),
    toMap: 'indexed => (ImmMap.t int 'a),
    toNavigableCollection: 'indexed => (NavigableCollection.t 'a),
    toNavigableKeyedCollection: 'indexed => (NavigableKeyedCollection.t int 'a),
    toNavigableMap: 'indexed => (NavigableMap.t int 'a),
    toSequence: 'indexed => Sequence.t 'a,
    toSequenceReversed: 'indexed => Sequence.t 'a,
  };
};

type t 'a =
  | Empty
  | Indexed 'indexed (Ops.t 'a 'indexed): t 'a;

let count (indexed: t 'a): int => switch indexed {
  | Empty => 0
  | Indexed indexed { count } => count indexed
};

let empty (): (t 'a) => Empty;

let first (indexed: t 'a): (option 'a) => switch indexed {
  | Empty => None
  | Indexed indexed { first } => first indexed
};

let firstOrRaise (indexed: t 'a): 'a => switch indexed {
  | Empty => failwith "empty"
  | Indexed indexed { firstOrRaise } => firstOrRaise indexed
};

let get (index: int) (indexed: t 'a): (option 'a) => switch indexed {
  | Empty => None
  | Indexed indexed { get } => get index indexed
};

let getOrRaise (index: int) (indexed: t 'a): 'a => switch indexed {
  | Empty => failwith "empty"
  | Indexed indexed { getOrRaise } => getOrRaise index indexed
};

let isEmpty (indexed: t 'a): bool =>
  (count indexed) === 0;

let isNotEmpty (indexed: t 'a): bool =>
  (count indexed) !== 0;

let last (indexed: t 'a): (option 'a) => switch indexed {
  | Empty => None
  | Indexed indexed { last } => last indexed
};

let lastOrRaise (indexed: t 'a): 'a => switch indexed {
  | Empty => failwith "empty"
  | Indexed indexed { lastOrRaise } => lastOrRaise indexed
};

let toCollection (indexed: t 'a): (Collection.t 'a) => switch indexed {
  | Empty => Collection.empty ()
  | Indexed indexed { toCollection } => toCollection indexed
};

let toIndexed (indexed: t 'a): (t 'a) => indexed;

let toIterable (indexed: t 'a): (Iterable.t 'a) => switch indexed {
  | Empty => Iterable.empty ()
  | Indexed indexed { toIterable } => toIterable indexed
};

let toIterableReversed (indexed: t 'a): (Iterable.t 'a) => switch indexed {
  | Empty => Iterable.empty ()
  | Indexed indexed { toIterableReversed } => toIterableReversed indexed
};

let toKeyedCollection (indexed: t 'a): (KeyedCollection.t int 'a) => switch indexed {
  | Empty => KeyedCollection.empty ()
  | Indexed indexed { toKeyedCollection } => toKeyedCollection indexed
};

let toKeyedIterable (indexed: t 'a): (KeyedIterable.t int 'a) => switch indexed {
  | Empty => KeyedIterable.empty ()
  | Indexed indexed { toKeyedIterable } => toKeyedIterable indexed
};

let toKeyedIterableReversed (indexed: t 'a): (KeyedIterable.t int 'a) => switch indexed {
  | Empty => KeyedIterable.empty ()
  | Indexed indexed { toKeyedIterableReversed } => toKeyedIterableReversed indexed
};

let toIndexed (indexed: t 'a): (t 'a) => indexed;

let toMap (indexed: t 'a): (ImmMap.t int 'a) => switch indexed {
  | Empty => ImmMap.empty ()
  | Indexed indexed { toMap } => toMap indexed
};

let toNavigableCollection (indexed: t 'a): (NavigableCollection.t 'a) => switch indexed {
  | Empty => NavigableCollection.empty ()
  | Indexed indexed { toNavigableCollection } => toNavigableCollection indexed
};

let toNavigableKeyedCollection (indexed: t 'a): (NavigableKeyedCollection.t int 'a) => switch indexed {
  | Empty => NavigableKeyedCollection.empty ()
  | Indexed indexed { toNavigableKeyedCollection } => toNavigableKeyedCollection indexed
};

let toNavigableMap (indexed: t 'a): (NavigableMap.t int 'a) => switch indexed {
  | Empty => NavigableMap.empty ()
  | Indexed indexed { toNavigableMap } => toNavigableMap indexed
};

let toSequence (indexed: t 'a): (Sequence.t 'a) => switch indexed {
  | Empty => Sequence.empty ()
  | Indexed indexed { toSequence } => toSequence indexed
};

let toSequenceReversed (indexed: t 'a): (Sequence.t 'a) => switch indexed {
  | Empty => Sequence.empty ()
  | Indexed indexed { toSequenceReversed } => toSequenceReversed indexed
};

let toSequentialCollection (indexed: t 'a): (SequentialCollection.t 'a) => switch indexed {
  | Empty => SequentialCollection.empty ()
  | Indexed indexed { toSequentialCollection } => toSequentialCollection indexed
};

let reduce
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (indexed: t 'a): 'acc =>
  indexed |> toIterable |> Iterable.reduce while_::predicate f acc;

let reduceReversed
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (indexed: t 'a): 'acc =>
  indexed |> toIterableReversed |> Iterable.reduce while_::predicate f acc;
