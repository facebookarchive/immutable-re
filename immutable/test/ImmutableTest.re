/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open ReUnit.Test;

open Immutable;

ReUnit.run (describe "Immutable.re" [
  CopyOnWriteArrayTest.test,
  DequeTest.test,
  HashMapTest.test,
  HashSetTest.test,
  IntMapTest.test,
  IntRangeTest.test,
  IntSetTest.test,
  IteratorTest.test,
  KeyedIteratorTest.test,
  KeyedReducerTest.test,
  ListTest.test,
  MapTest.test,
  OptionTest.test,
  ReducerTest.test,
  SequenceTest.test,
  SetTest.test,
  SortedMapTest.test,
  SortedSetTest.test,
  StackTest.test,
  VectorTest.test,
]);
