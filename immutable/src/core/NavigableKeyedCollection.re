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
  type t 'k 'v 'keyed = {
    containsKey: 'k => 'keyed => bool,
    count: 'keyed => int,
    first: 'keyed => (option ('k, 'v)),
    firstOrRaise: 'keyed => ('k, 'v),
    firstKey: 'keyed => (option 'k),
    firstKeyOrRaise: 'keyed => 'k,
    firstValue: 'keyed => (option 'v),
    firstValueOrRaise: 'keyed => 'v,
    keys: 'keyed => Iterable.t 'k,
    keysReversed: 'keyed => Iterable.t 'k,
    last: 'keyed => (option ('k, 'v)),
    lastOrRaise: 'keyed => ('k, 'v),
    lastKey: 'keyed => (option 'k),
    lastKeyOrRaise: 'keyed => 'k,
    lastValue: 'keyed => (option 'v),
    lastValueOrRaise: 'keyed => 'v,
    toIterable: 'keyed => Iterable.t ('k, 'v),
    toIterableReversed: 'keyed => Iterable.t ('k, 'v),
    toKeyedCollection: 'keyed => KeyedCollection.t 'k 'v,
    toKeyedIterable: 'keyed => KeyedIterable.t 'k 'v,
    toKeyedIterableReversed: 'keyed => KeyedIterable.t 'k 'v,
    toSequence: 'keyed => Sequence.t ('k, 'v),
    toSequenceReversed: 'keyed => (Sequence.t ('k, 'v)),
    values: 'keyed => Iterable.t 'v,
    valuesReversed: 'keyed => Iterable.t 'v,
  };
};

type t 'k 'v =
  | Empty
  | NavigableKeyedCollection 'keyed (Ops.t 'k 'v 'keyed): t 'k 'v;

let containsKey (key: 'k) (keyed: t 'k 'v): bool => switch keyed {
  | Empty => false
  | NavigableKeyedCollection keyed { containsKey } => containsKey key keyed
};

let count (keyed: t 'k 'v): int => switch keyed {
  | Empty => 0
  | NavigableKeyedCollection keyed { count } => count keyed
};

let empty (): (t 'k 'v) => Empty;

let first (keyed: t 'k 'v): option ('k, 'v) => switch keyed {
  | Empty => None
  | NavigableKeyedCollection keyed { first } => first keyed
};

let firstOrRaise (keyed: t 'k 'v): ('k, 'v) => switch keyed {
  | Empty => failwith "empty"
  | NavigableKeyedCollection keyed { firstOrRaise } => firstOrRaise keyed
};

let firstKey (keyed: t 'k 'v): option 'k => switch keyed {
  | Empty => None
  | NavigableKeyedCollection keyed { firstKey } => firstKey keyed
};

let firstKeyOrRaise (keyed: t 'k 'v): 'k => switch keyed {
  | Empty => failwith "empty"
  | NavigableKeyedCollection keyed { firstKeyOrRaise } => firstKeyOrRaise keyed
};

let firstValue (keyed: t 'k 'v): option 'v => switch keyed {
  | Empty => None
  | NavigableKeyedCollection keyed { firstValue } => firstValue keyed
};

let firstValueOrRaise (keyed: t 'k 'v): 'v => switch keyed {
  | Empty => failwith "empty"
  | NavigableKeyedCollection keyed { firstValueOrRaise } => firstValueOrRaise keyed
};

let isEmpty (keyed: t 'k 'v): bool =>
  (count keyed) === 0;

let isNotEmpty (keyed: t 'k 'v): bool =>
  (count keyed) !== 0;

let keys (keyed: t 'k 'v): Iterable.t 'k => switch keyed {
  | Empty => Iterable.empty ()
  | NavigableKeyedCollection keyed { keys } => keys keyed
};

let keysReversed (keyed: t 'k 'v): Iterable.t 'k => switch keyed {
  | Empty => Iterable.empty ()
  | NavigableKeyedCollection keyed { keysReversed } => keysReversed keyed
};

let last (keyed: t 'k 'v): option ('k, 'v) => switch keyed {
  | Empty => None
  | NavigableKeyedCollection keyed { last } => last keyed
};

let lastOrRaise (keyed: t 'k 'v): ('k, 'v) => switch keyed {
  | Empty => failwith "empty"
  | NavigableKeyedCollection keyed { lastOrRaise } => lastOrRaise keyed
};

let lastKey (keyed: t 'k 'v): option 'k => switch keyed {
  | Empty => None
  | NavigableKeyedCollection keyed { lastKey } => lastKey keyed
};

let lastKeyOrRaise (keyed: t 'k 'v): 'k => switch keyed {
  | Empty => failwith "empty"
  | NavigableKeyedCollection keyed { lastKeyOrRaise } => lastKeyOrRaise keyed
};

let lastValue (keyed: t 'k 'v): option 'v => switch keyed {
  | Empty => None
  | NavigableKeyedCollection keyed { lastValue } => lastValue keyed
};

let lastValueOrRaise (keyed: t 'k 'v): 'v => switch keyed {
  | Empty => failwith "empty"
  | NavigableKeyedCollection keyed { lastValueOrRaise } => lastValueOrRaise keyed
};

let toIterable (keyed: t 'k 'v): Iterable.t ('k, 'v) => switch keyed {
  | Empty => Iterable.empty ()
  | NavigableKeyedCollection keyed { toIterable } => toIterable keyed
};

let toIterableReversed (keyed: t 'k 'v): Iterable.t ('k, 'v) => switch keyed {
  | Empty => Iterable.empty ()
  | NavigableKeyedCollection keyed { toIterableReversed } => toIterableReversed keyed
};

let toKeyedCollection (keyed: t 'k 'v): (KeyedCollection.t 'k 'v) => switch keyed {
  | Empty => KeyedCollection.empty ()
  | NavigableKeyedCollection keyed { toKeyedCollection } => toKeyedCollection keyed
};

let toKeyedIterable (keyed: t 'k 'v): KeyedIterable.t 'k 'v => switch keyed {
  | Empty => KeyedIterable.empty ()
  | NavigableKeyedCollection keyed { toKeyedIterable } => toKeyedIterable keyed
};

let toKeyedIterableReversed (keyed: t 'k 'v): KeyedIterable.t 'k 'v => switch keyed {
  | Empty => KeyedIterable.empty ()
  | NavigableKeyedCollection keyed { toKeyedIterableReversed } => toKeyedIterableReversed keyed
};

let toNavigableKeyedCollection (keyed: t 'k 'v): t 'k 'v => keyed;

let toSequence (keyed: t 'k 'v): Sequence.t ('k, 'v) => switch keyed {
  | Empty => Sequence.empty ()
  | NavigableKeyedCollection keyed { toSequence } => toSequence keyed
};

let toSequenceReversed (keyed: t 'k 'v): Sequence.t ('k, 'v) => switch keyed {
  | Empty => Sequence.empty ()
  | NavigableKeyedCollection keyed { toSequenceReversed } => toSequenceReversed keyed
};

let values (keyed: t 'k 'v): Iterable.t 'v => switch keyed {
  | Empty => Iterable.empty ()
  | NavigableKeyedCollection keyed { values } => values keyed
};

let valuesReversed (keyed: t 'k 'v): Iterable.t 'v => switch keyed {
  | Empty => Iterable.empty ()
  | NavigableKeyedCollection keyed { valuesReversed } => valuesReversed keyed
};

let reduce
    while_::(predicate: 'acc => 'k => 'v => bool)=Functions.alwaysTrue3
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    (keyed: t 'k 'v): 'acc =>
  keyed |> toKeyedIterable |> KeyedIterable.reduce while_::predicate f acc;

let reduceReversed
    while_::(predicate: 'acc => 'k => 'v => bool)=Functions.alwaysTrue3
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    (keyed: t 'k 'v): 'acc =>
  keyed |> toKeyedIterableReversed |> KeyedIterable.reduce while_::predicate f acc;
