/***
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open Printf;

open Immutable;

open ReUnit;

open ReUnit.Test;

module type S1 = {
  type t('a);
  include StackTester.S1 with type t('a) := t('a);
  include NavigableCollection.Persistent.S1 with type t('a) := t('a);
  let from: Iterable.t('a) => t('a);
};

module Make = (Deque: S1, Config: TesterConfig.S) => {
  module StackTester = StackTester.Make((Deque: StackTester.S1), Config);
  let tests = [
    describe(
      sprintf("count: %i", Config.count),
      [
        it(
          "addLast",
          () => {
            let testData = IntRange.create(~start=0, ~count=Config.count);
            testData
            |> IntRange.reduce(
                 (acc, i) => {
                   let acc = Deque.addLast(i, acc);
                   Deque.last(acc) |> Expect.toBeEqualToSomeOfInt(i);
                   Deque.lastOrRaise(acc) |> Expect.toBeEqualToInt(i);
                   Deque.count(acc) |> Expect.toBeEqualToInt(i + 1);
                   Deque.isEmpty(acc) |> Expect.toBeEqualToFalse;
                   Deque.isNotEmpty(acc) |> Expect.toBeEqualToTrue;
                   acc
                 },
                 Deque.empty()
               )
            |> Deque.toIterable
            |> List.fromReverse
            |> Expect.toBeEqualToListOfInt(List.fromReverse(IntRange.toIterable(testData)))
          }
        ),
        it(
          "addLastAll",
          () => {
            let testData = IntRange.create(~start=0, ~count=Config.count);
            Deque.empty()
            |> Deque.addLastAll(IntRange.toIterable(testData))
            |> Deque.toIterable
            |> List.fromReverse
            |> Expect.toBeEqualToListOfInt(List.fromReverse(IntRange.toIterable(testData)))
          }
        ),
        it(
          "from",
          () => {
            let testData = IntRange.create(~start=0, ~count=Config.count);
            IntRange.toIterable(testData)
            |> Deque.from
            |> Deque.toIterable
            |> List.fromReverse
            |> Expect.toBeEqualToListOfInt(List.fromReverse(IntRange.toIterable(testData)))
          }
        ),
        it("last", () => Deque.empty() |> Deque.last |> Expect.toBeEqualToNoneOfInt),
        it("lastOrRaise", () => (() => Deque.empty() |> Deque.lastOrRaise) |> Expect.shouldRaise),
        it(
          "reduceReversed",
          () =>
            IntRange.create(~start=0, ~count=Config.count)
            |> IntRange.toIterable
            |> Deque.from
            |> Deque.reduceReversed(~while_=(_, i) => i >= Config.count / 2, (_, i) => i, 0)
            |> Expect.toBeEqualToInt(Config.count / 2)
        ),
        it(
          "removeLastOrRaise",
          () => {
            let range = IntRange.create(~start=0, ~count=Config.count);
            let fullDeque = range |> IntRange.toIterable |> Deque.from;
            let emptyDeque =
              range
              |> IntRange.reduceReversed(
                   (acc, i) => {
                     Deque.last(acc) |> Expect.toBeEqualToSomeOfInt(i);
                     Deque.lastOrRaise(acc) |> Expect.toBeEqualToInt(i);
                     Deque.count(acc) |> Expect.toBeEqualToInt(i + 1);
                     Deque.isEmpty(acc) |> Expect.toBeEqualToFalse;
                     Deque.isNotEmpty(acc) |> Expect.toBeEqualToTrue;
                     Deque.removeLastOrRaise(acc)
                   },
                   fullDeque
                 );
            Deque.isEmpty(emptyDeque) |> Expect.toBeEqualToTrue
          }
        ),
        it(
          "toIterableReversed",
          () =>
            IntRange.create(~start=0, ~count=Config.count)
            |> IntRange.toIterable
            |> Deque.from
            |> Deque.toIterableReversed
            |> Iterable.reduce(
                 ~while_=(_, i) => i >= Config.count / 2,
                 (acc, _) => acc - 1,
                 Config.count
               )
            |> Expect.toBeEqualToInt(Config.count / 2)
        ),
        it(
          "toSequenceReversed",
          () =>
            IntRange.create(~start=0, ~count=Config.count)
            |> IntRange.toIterable
            |> Deque.from
            |> Deque.toSequenceReversed
            |> Sequence.reduce(
                 ~while_=(_, i) => i >= Config.count / 2,
                 (acc, _) => acc - 1,
                 Config.count
               )
            |> Expect.toBeEqualToInt(Config.count / 2)
        ),
        it(
          "addLast, removeFirstOrRaise",
          () => {
            let testData = IntRange.create(~start=0, ~count=Config.count);
            let deque = Deque.empty() |> Deque.addLastAll(IntRange.toIterable(testData));
            testData
            |> IntRange.reduce(
                 (acc, i) => {
                   Deque.first(acc) |> Expect.toBeEqualToSomeOfInt(i);
                   Deque.firstOrRaise(acc) |> Expect.toBeEqualToInt(i);
                   Deque.count(acc) |> Expect.toBeEqualToInt(Config.count - i);
                   Deque.isEmpty(acc) |> Expect.toBeEqualToFalse;
                   Deque.isNotEmpty(acc) |> Expect.toBeEqualToTrue;
                   Deque.removeFirstOrRaise(acc)
                 },
                 deque
               )
            |> ignore
          }
        ),
        it(
          "addFirst, removeLastOrRaise",
          () => {
            let testData = IntRange.create(~start=0, ~count=Config.count);
            let deque = Deque.empty() |> Deque.addFirstAll(IntRange.toIterableReversed(testData));
            testData
            |> IntRange.reduceReversed(
                 (acc, i) => {
                   Deque.last(acc) |> Expect.toBeEqualToSomeOfInt(i);
                   Deque.lastOrRaise(acc) |> Expect.toBeEqualToInt(i);
                   Deque.count(acc) |> Expect.toBeEqualToInt(i + 1);
                   Deque.isEmpty(acc) |> Expect.toBeEqualToFalse;
                   Deque.isNotEmpty(acc) |> Expect.toBeEqualToTrue;
                   Deque.removeLastOrRaise(acc)
                 },
                 deque
               )
            |> ignore
          }
        )
      ]
    ),
    ...StackTester.tests
  ];
};
