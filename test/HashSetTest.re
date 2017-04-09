/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Immutable;
open ReUnit.Test;

let module HashIntSet: Set.Persistent.S with type a = int = {
  type a = int;
  type t = HashSet.t a;

  let add = HashSet.add;
  let addAll = HashSet.addAll;
  let contains = HashSet.contains;
  let count = HashSet.count;
  let empty = HashSet.emptyWith
    hash::(fun i => i)
    comparator::Comparator.int;
  let equals = HashSet.equals;
  let from = HashSet.fromWith
    hash::(fun i => i)
    comparator::Comparator.int;
  let isEmpty = HashSet.isEmpty;
  let isNotEmpty = HashSet.isNotEmpty;
  let intersect = HashSet.intersect;
  let reduce = HashSet.reduce;
  let remove = HashSet.remove;
  let removeAll = HashSet.removeAll;
  let subtract = HashSet.subtract;
  let toCollection = HashSet.toCollection;
  let toIterable = HashSet.toIterable;
  let toSequence = HashSet.toSequence;
  let toSet = HashSet.toSet;
  let union = HashSet.union;
};

let badHashFunction i => i mod 100;

let module BadHashIntSet: Set.Persistent.S with type a = int = {
  type a = int;
  type t = HashSet.t a;

  let add = HashSet.add;
  let addAll = HashSet.addAll;
  let contains = HashSet.contains;
  let count = HashSet.count;
  let empty = HashSet.emptyWith
    hash::badHashFunction
    comparator::Comparator.int;
  let equals = HashSet.equals;
  let from = HashSet.fromWith
    hash::badHashFunction
    comparator::Comparator.int;
  let isEmpty = HashSet.isEmpty;
  let isNotEmpty = HashSet.isNotEmpty;
  let intersect = HashSet.intersect;
  let reduce = HashSet.reduce;
  let remove = HashSet.remove;
  let removeAll = HashSet.removeAll;
  let subtract = HashSet.subtract;
  let toCollection = HashSet.toCollection;
  let toIterable = HashSet.toIterable;
  let toSequence = HashSet.toSequence;
  let toSet = HashSet.toSet;
  let union = HashSet.union;
};

let test = describe "HashSet" [
  PersistentSetTester.test (module HashIntSet) 100,
  PersistentSetTester.test (module HashIntSet) 10000,
  describe "with bad hash function" [
    PersistentSetTester.test (module BadHashIntSet) 100,
    PersistentSetTester.test (module BadHashIntSet) 10000,
  ],
];
