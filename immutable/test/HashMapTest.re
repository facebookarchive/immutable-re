/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Immutable;
open ReUnit.Expect;
open ReUnit.Test;

let module HashIntMap: PersistentMap.S1 with type k = int = {
  type k = int;
  type t 'v = HashMap.t k 'v;

  let alter = HashMap.alter;
  let containsKey = HashMap.containsKey;
  let count = HashMap.count;
  let empty () => HashMap.emptyWith
    hash::(fun i => i)
    comparator::Comparator.int;

  let from iter => HashMap.fromWith
    hash::(fun i => i)
    comparator::Comparator.int
    iter;
  let isEmpty = HashMap.isEmpty;
  let isNotEmpty = HashMap.isNotEmpty;
  let get = HashMap.get;
  let getOrRaise = HashMap.getOrRaise;
  let keys = HashMap.keys;
  let map = HashMap.map;
  let merge = HashMap.merge;
  let put = HashMap.put;
  let putAll = HashMap.putAll;
  let reduce = HashMap.reduce;
  let remove = HashMap.remove;
  let removeAll = HashMap.removeAll;
  let toIterator = HashMap.toIterator;
  let toKeyedIterator = HashMap.toKeyedIterator;
  let toMap = HashMap.toMap;
  let toSequence = HashMap.toSequence;
  let values = HashMap.values;
};

let test = describe "HashMap" [
  PersistentMapTester.test (module HashIntMap: PersistentMap.S1 with type k = HashIntMap.k) 100,
  PersistentMapTester.test (module HashIntMap: PersistentMap.S1 with type k = HashIntMap.k) 10000,
];
