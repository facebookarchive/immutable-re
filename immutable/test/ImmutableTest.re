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

/*
let count = 50000;
let vector = IntRange.create start::0 count::count
  |> IntRange.toIterator
  |> Vector.from;

let countDiv4 = count / 4;

vector
  |> Vector.slice start::countDiv4
  |> Vector.toSequence
  |> Sequence.zipLongest2With
      (fun a b => a == b)
      (IntRange.create start::countDiv4 count::(count - countDiv4) |> IntRange.toSequence)
  |> Sequence.Reducer.every (fun i => i)
  |> ignore;

vector
  |> Vector.slice start::(-countDiv4)
  |> Vector.toSequence
  |> Sequence.zipLongest2With
      (fun a b => a == b)
      (IntRange.create start::(count - countDiv4) count::countDiv4 |> IntRange.toSequence)
  |> Sequence.Reducer.every (fun i => i)
  |> ignore;
*/

ReUnit.run (describe "Immutable.re" [
  CopyOnWriteArrayTest.test,
  DequeTest.test,
  IntRangeTest.test,
  IteratorTest.test,
  KeyedIteratorTest.test,
  KeyedReducerTest.test,
  ListTest.test,
  MapTest.test,
  OptionTest.test,
  ReducerTest.test,
  SequenceTest.test,
  SetTest.test,
  StackTest.test,
  VectorTest.test,
]);
