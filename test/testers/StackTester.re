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
  include SequentialCollection.Persistent.S1;
  let empty: unit => t('a);
  let fromReverse: Iterable.t('a) => t('a);
  let return: 'a => t('a);
};

module Make = (Stack: S1, Config: TesterConfig.S) => {
  let tests =
    describe(
      sprintf("count: %i", Config.count),
      [
        it(
          "addFirst",
          () => {
            let testData = IntRange.create(~start=0, ~count=Config.count);
            testData
            |> IntRange.reduce(
                 (acc, i) => {
                   let acc = Stack.addFirst(i, acc);
                   Stack.first(acc) |> Expect.toBeEqualToSomeOfInt(i);
                   Stack.firstOrRaise(acc) |> Expect.toBeEqualToInt(i);
                   Stack.count(acc) |> Expect.toBeEqualToInt(i + 1);
                   Stack.isEmpty(acc) |> Expect.toBeEqualToFalse;
                   Stack.isNotEmpty(acc) |> Expect.toBeEqualToTrue;
                   acc
                 },
                 Stack.empty()
               )
            |> Stack.toIterable
            |> List.fromReverse
            |> Expect.toBeEqualToListOfInt(List.fromReverse(IntRange.toIterableReversed(testData)))
          }
        ),
        it(
          "addFirstAll",
          () => {
            let testData = IntRange.create(~start=0, ~count=Config.count);
            Stack.empty()
            |> Stack.addFirstAll(IntRange.toIterable(testData))
            |> Stack.toIterable
            |> List.fromReverse
            |> Expect.toBeEqualToListOfInt(List.fromReverse(IntRange.toIterableReversed(testData)))
          }
        ),
        it("count", () => Stack.empty() |> Stack.count |> Expect.toBeEqualToInt(0)),
        it("first", () => Stack.empty() |> Stack.first |> Expect.toBeEqualToNoneOfInt),
        it(
          "firstOrRaise",
          () => (() => Stack.empty() |> Stack.firstOrRaise) |> Expect.shouldRaise
        ),
        it(
          "fromReverse",
          () => {
            let testData = IntRange.create(~start=0, ~count=Config.count);
            IntRange.toIterable(testData)
            |> Stack.fromReverse
            |> Stack.toIterable
            |> List.fromReverse
            |> Expect.toBeEqualToListOfInt(List.fromReverse(IntRange.toIterableReversed(testData)))
          }
        ),
        it("isEmpty", () => Stack.empty() |> Stack.isEmpty |> Expect.toBeEqualToTrue),
        it("isNotEmpty", () => Stack.empty() |> Stack.isNotEmpty |> Expect.toBeEqualToFalse),
        it(
          "reduce",
          () =>
            IntRange.create(~start=0, ~count=Config.count)
            |> IntRange.toIterableReversed
            |> Stack.fromReverse
            |> Stack.reduce(~while_=(_, i) => i < Config.count / 2, (acc, _) => acc + 1, 0)
            |> Expect.toBeEqualToInt(Config.count / 2)
        ),
        it(
          "removeAll",
          () =>
            IntRange.create(~start=0, ~count=Config.count)
            |> IntRange.toIterableReversed
            |> Stack.fromReverse
            |> Stack.removeAll
            |> Stack.count
            |> Expect.toBeEqualToInt(0)
        ),
        it(
          "removeFirstOrRaise",
          () => {
            let range = IntRange.create(~start=0, ~count=Config.count);
            let fullStack = range |> IntRange.toIterable |> Stack.fromReverse;
            let emptyStack =
              range
              |> IntRange.reduceReversed(
                   (acc, i) => {
                     Stack.first(acc) |> Expect.toBeEqualToSomeOfInt(i);
                     Stack.firstOrRaise(acc) |> Expect.toBeEqualToInt(i);
                     Stack.count(acc) |> Expect.toBeEqualToInt(i + 1);
                     Stack.isEmpty(acc) |> Expect.toBeEqualToFalse;
                     Stack.isNotEmpty(acc) |> Expect.toBeEqualToTrue;
                     Stack.removeFirstOrRaise(acc)
                   },
                   fullStack
                 );
            Stack.isEmpty(emptyStack) |> Expect.toBeEqualToTrue
          }
        ),
        it(
          "return",
          () => {
            let stack = Stack.return(1);
            stack |> Stack.count |> Expect.toBeEqualToInt(1);
            stack |> Stack.firstOrRaise |> Expect.toBeEqualToInt(1)
          }
        ),
        it(
          "toIterable",
          () =>
            IntRange.create(~start=0, ~count=Config.count)
            |> IntRange.toIterableReversed
            |> Stack.fromReverse
            |> Stack.toIterable
            |> Iterable.reduce(~while_=(_, i) => i < Config.count / 2, (acc, _) => acc + 1, 0)
            |> Expect.toBeEqualToInt(Config.count / 2)
        ),
        it(
          "toSequence",
          () =>
            IntRange.create(~start=0, ~count=Config.count)
            |> IntRange.toIterableReversed
            |> Stack.fromReverse
            |> Stack.toSequence
            |> Sequence.reduce(~while_=(_, i) => i < Config.count / 2, (acc, _) => acc + 1, 0)
            |> Expect.toBeEqualToInt(Config.count / 2)
        )
      ]
    )
    |> List.return;
};
