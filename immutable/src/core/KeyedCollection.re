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
     keys: 'keyed => Iterable.t 'k,
     toIterable: 'keyed => Iterable.t ('k, 'v),
     toKeyedIterable: 'keyed => KeyedIterable.t 'k 'v,
     toSequence: 'keyed => Sequence.t ('k, 'v),
     values: 'keyed => Iterable.t 'v,
   };
 };

type t 'k 'v =
  | Empty
  | KeyedCollection 'keyed (Ops.t 'k 'v 'keyed): t 'k 'v;

let containsKey (key: 'k) (keyed: t 'k 'v): bool => switch keyed {
  | Empty => false
  | KeyedCollection keyed { containsKey } => containsKey key keyed
};

let count (keyed: t 'k 'v): int => switch keyed {
  | Empty => 0
  | KeyedCollection keyed { count } => count keyed
};

let empty (): (t 'k 'v) => Empty;

let isEmpty (keyed: t 'k 'v): bool =>
  (count keyed) === 0;

let isNotEmpty (keyed: t 'k 'v): bool =>
  (count keyed) !== 0;

let keys (keyed: t 'k 'v): Iterable.t 'k => switch keyed {
  | Empty => Iterable.empty ()
  | KeyedCollection keyed { keys } => keys keyed
};

let toIterable (keyed: t 'k 'v): Iterable.t ('k, 'v) => switch keyed {
  | Empty => Iterable.empty ()
  | KeyedCollection keyed { toIterable } => toIterable keyed
};

let toKeyedCollection (keyed: t 'k 'v): (t 'k 'v) => keyed;

let toKeyedIterable (keyed: t 'k 'v): KeyedIterable.t 'k 'v => switch keyed {
  | Empty => KeyedIterable.empty ()
  | KeyedCollection keyed { toKeyedIterable } => toKeyedIterable keyed
};

let toSequence (keyed: t 'k 'v): Sequence.t ('k, 'v) => switch keyed {
  | Empty => Sequence.empty ()
  | KeyedCollection keyed { toSequence } => toSequence keyed
};

let values (keyed: t 'k 'v): Iterable.t 'v => switch keyed {
  | Empty => Iterable.empty ()
  | KeyedCollection keyed { values } => values keyed
};

let reduce
    while_::(predicate: 'acc => 'k => 'v => bool)=Functions.alwaysTrue3
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    (keyed: t 'k 'v): 'acc =>
  keyed |> toKeyedIterable |> KeyedIterable.reduce while_::predicate f acc;
