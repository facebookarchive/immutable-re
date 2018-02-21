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

module SortedIntSet =
  SortedSet.Make(
    {
      type t = int;
      let compare = Comparator.int;
      let equals = Equality.int;
    }
  );

let navigationTests = (count: int) => {
  let countDiv2 = count / 2;
  let countDiv4 = count / 4;
  describe(
    sprintf("count: %i", count),
    [
      it(
        "first",
        () => {
          SortedIntSet.empty() |> SortedIntSet.first |> Expect.toBeEqualToNoneOfInt;
          IntRange.create(~start=- countDiv2, ~count)
          |> IntRange.toIterable
          |> SortedIntSet.from
          |> SortedIntSet.first
          |> Expect.toBeEqualToSomeOfInt(- countDiv2)
        }
      ),
      it(
        "firstOrRaise",
        () => {
          (() => SortedIntSet.empty() |> SortedIntSet.firstOrRaise) |> Expect.shouldRaise;
          IntRange.create(~start=- countDiv2, ~count)
          |> IntRange.toIterable
          |> SortedIntSet.from
          |> SortedIntSet.firstOrRaise
          |> Expect.toBeEqualToInt(- countDiv2)
        }
      ),
      it(
        "last",
        () => {
          SortedIntSet.empty() |> SortedIntSet.last |> Expect.toBeEqualToNoneOfInt;
          IntRange.create(~start=- countDiv2, ~count)
          |> IntRange.toIterable
          |> SortedIntSet.from
          |> SortedIntSet.last
          |> Expect.toBeEqualToSomeOfInt(countDiv2 - 1)
        }
      ),
      it(
        "lastOrRaise",
        () => {
          (() => SortedIntSet.empty() |> SortedIntSet.lastOrRaise) |> Expect.shouldRaise;
          IntRange.create(~start=- countDiv2, ~count)
          |> IntRange.toIterable
          |> SortedIntSet.from
          |> SortedIntSet.lastOrRaise
          |> Expect.toBeEqualToInt(countDiv2 - 1)
        }
      ),
      it(
        "reduceReversed",
        () =>
          IntRange.create(~start=0, ~count)
          |> IntRange.toIterable
          |> SortedIntSet.from
          |> SortedIntSet.reduceReversed(
               ~while_=(acc, _) => acc < countDiv2,
               (acc, _) => 1 + acc,
               0
             )
          |> Expect.toBeEqualToInt(countDiv2)
      ),
      it(
        "removeFirstOrRaise",
        () => {
          let set = IntRange.create(~start=0, ~count) |> IntRange.toIterable |> SortedIntSet.from;
          IntRange.create(~start=0, ~count)
          |> IntRange.reduce(
               (acc, i) => {
                 acc |> SortedIntSet.firstOrRaise |> Expect.toBeEqualToInt(i);
                 acc |> SortedIntSet.removeFirstOrRaise
               },
               set
             )
          |> ignore;
          (() => SortedIntSet.empty() |> SortedIntSet.removeFirstOrRaise) |> Expect.shouldRaise
        }
      ),
      it(
        "removeLastOrRaise",
        () => {
          let set = IntRange.create(~start=0, ~count) |> IntRange.toIterable |> SortedIntSet.from;
          IntRange.create(~start=0, ~count)
          |> IntRange.reduceReversed(
               (acc, i) => {
                 acc |> SortedIntSet.lastOrRaise |> Expect.toBeEqualToInt(i);
                 acc |> SortedIntSet.removeLastOrRaise
               },
               set
             )
          |> ignore;
          (() => SortedIntSet.empty() |> SortedIntSet.removeLastOrRaise) |> Expect.shouldRaise
        }
      ),
      it(
        "toIterableReversed",
        () =>
          IntRange.create(~start=0, ~count)
          |> IntRange.toIterable
          |> SortedIntSet.from
          |> SortedIntSet.toIterableReversed
          |> Iterable.reduce(~while_=(acc, _) => acc < countDiv4, (acc, _) => 1 + acc, 0)
          |> Expect.toBeEqualToInt(countDiv4)
      ),
      it(
        "toSequenceReversed",
        () =>
          IntRange.create(~start=0, ~count)
          |> IntRange.toIterable
          |> SortedIntSet.from
          |> SortedIntSet.toSequenceReversed
          |> Sequence.reduce(~while_=(acc, _) => acc < countDiv4, (acc, _) => 1 + acc, 0)
          |> Expect.toBeEqualToInt(countDiv4)
      )
    ]
  )
};

let test =
  describe(
    "SortedSet",
    [
      PersistentSetTester.test((module SortedIntSet), 100),
      PersistentSetTester.test((module SortedIntSet), 10000),
      navigationTests(10000)
    ]
  );
