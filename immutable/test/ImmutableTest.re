/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open ReUnit.Test;

ReUnit.run (describe "Immutable.re" [
  DequeTest.test,
  HashMapTest.test,
  HashSetTest.test,
  IntMapTest.test,
  IntRangeTest.test,
  IntSetTest.test,
  IterableTest.test,
  KeyedIterableTest.test,
  ListTest.test,
  MapTest.test,
  ReadOnlyArrayTest.test,
  SequenceTest.test,
  SetTest.test,
  SortedMapTest.test,
  SortedSetTest.test,
  StackTest.test,
  VectorTest.test,
]);
