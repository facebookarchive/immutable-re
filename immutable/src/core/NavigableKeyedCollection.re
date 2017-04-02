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
    keys: 'keyed => ImmSet.t 'k,
    last: 'keyed => (option ('k, 'v)),
    lastOrRaise: 'keyed => ('k, 'v),
    toIterable: 'keyed => Iterable.t ('k, 'v),
    toIterableRight: 'keyed => Iterable.t ('k, 'v),
    toKeyedCollection: 'keyed => KeyedCollection.t 'k 'v,
    toKeyedIterable: 'keyed => KeyedIterable.t 'k 'v,
    toKeyedIterableRight: 'keyed => KeyedIterable.t 'k 'v,
    toSequence: 'keyed => Sequence.t ('k, 'v),
    toSequenceRight: 'keyed => (Sequence.t ('k, 'v)),
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

let isEmpty (keyed: t 'k 'v): bool =>
  (count keyed) === 0;

let isNotEmpty (keyed: t 'k 'v): bool =>
  (count keyed) !== 0;

let keys (keyed: t 'k 'v): ImmSet.t 'k => switch keyed {
  | Empty => ImmSet.empty ()
  | NavigableKeyedCollection keyed { keys } => keys keyed
};

let last (keyed: t 'k 'v): option ('k, 'v) => switch keyed {
  | Empty => None
  | NavigableKeyedCollection keyed { last } => last keyed
};

let lastOrRaise (keyed: t 'k 'v): ('k, 'v) => switch keyed {
  | Empty => failwith "empty"
  | NavigableKeyedCollection keyed { lastOrRaise } => lastOrRaise keyed
};

let toIterable (keyed: t 'k 'v): Iterable.t ('k, 'v) => switch keyed {
  | Empty => Iterable.empty ()
  | NavigableKeyedCollection keyed { toIterable } => toIterable keyed
};

let toIterableRight (keyed: t 'k 'v): Iterable.t ('k, 'v) => switch keyed {
  | Empty => Iterable.empty ()
  | NavigableKeyedCollection keyed { toIterableRight } => toIterableRight keyed
};

let toKeyedCollection (keyed: t 'k 'v): (KeyedCollection.t 'k 'v) => switch keyed {
  | Empty => KeyedCollection.empty ()
  | NavigableKeyedCollection keyed { toKeyedCollection } => toKeyedCollection keyed
};

let toKeyedIterable (keyed: t 'k 'v): KeyedIterable.t 'k 'v => switch keyed {
  | Empty => KeyedIterable.empty ()
  | NavigableKeyedCollection keyed { toKeyedIterable } => toKeyedIterable keyed
};

let toKeyedIterableRight (keyed: t 'k 'v): KeyedIterable.t 'k 'v => switch keyed {
  | Empty => KeyedIterable.empty ()
  | NavigableKeyedCollection keyed { toKeyedIterableRight } => toKeyedIterableRight keyed
};

let toNavigableKeyedCollection (keyed: t 'k 'v): t 'k 'v => keyed;

let toSequence (keyed: t 'k 'v): Sequence.t ('k, 'v) => switch keyed {
  | Empty => Sequence.empty ()
  | NavigableKeyedCollection keyed { toSequence } => toSequence keyed
};

let toSequenceRight (keyed: t 'k 'v): Sequence.t ('k, 'v) => switch keyed {
  | Empty => Sequence.empty ()
  | NavigableKeyedCollection keyed { toSequenceRight } => toSequenceRight keyed
};
/*
let map (f: 'k => 'a => 'b) (keyed: t 'k 'a): (t 'k 'b) => switch keyed {
  | Empty => Empty
  | NavigableKeyedCollection keyed ops => NavigableKeyedCollection keyed {
      containsKey: ops.containsKey,
      count: ops.count,
      first: ops.first >> Option.map (fun (k, v) => (k, f k v)),
      firstOrRaise: fun map => {
        let (k,v) = ops.firstOrRaise map;
        (k, (f k v))
      },
      keys: ops.keys,
      last: ops.last >> Option.map (fun (k, v) => (k, f k v)),
      lastOrRaise: fun map => {
        let (k,v) = ops.lastOrRaise map;
        (k, (f k v))
      },
      toIterable: ops.toKeyedIterable >> KeyedIterable.mapValues f >> KeyedIterable.toIterable,
      toIterableRight: ops.toKeyedIterableRight >> KeyedIterable.mapValues f >> KeyedIterable.toIterable,
      toKeyedCollection: ops.toKeyedCollection >> KeyedCollection.map f,
      toKeyedIterable: ops.toKeyedIterable >> KeyedIterable.mapValues f,
      toKeyedIterableRight: ops.toKeyedIterableRight >> KeyedIterable.mapValues f,
      toSequence: ops.toSequence >> Sequence.map (fun (k, v) => (k, f k v)),
      toSequenceRight: ops.toSequenceRight >> Sequence.map (fun (k, v) => (k, f k v)),
    }
};*/

let reduce
    while_::(predicate: 'acc => 'k => 'v => bool)=Functions.alwaysTrue3
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    (keyed: t 'k 'v): 'acc =>
  keyed |> toKeyedIterable |> KeyedIterable.reduce while_::predicate f acc;

let reduceRight
    while_::(predicate: 'acc => 'k => 'v => bool)=Functions.alwaysTrue3
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    (keyed: t 'k 'v): 'acc =>
  keyed |> toKeyedIterableRight |> KeyedIterable.reduce while_::predicate f acc;


let module KeyedReducer = KeyedIterable.KeyedReducer.Make2 {
  type nonrec t 'k 'v = t 'k 'v;

  let reduce = reduce;
  let toIterable = toIterable;
  let toKeyedIterable = toKeyedIterable;
};
