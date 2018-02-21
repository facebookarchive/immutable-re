/***
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open Immutable;

open Printf;

open ReUnit;

open ReUnit.Test;

module SortedIntMap =
  SortedMap.Make1(
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
          SortedIntMap.empty() |> SortedIntMap.first((k, _) => k) |> Expect.toBeEqualToNoneOfInt;
          IntRange.create(~start=- countDiv2, ~count)
          |> IntRange.toIterable
          |> Iterable.map((i) => (i, i))
          |> SortedIntMap.fromEntries
          |> SortedIntMap.first((k, _) => k)
          |> Expect.toBeEqualToSomeOfInt(- countDiv2)
        }
      ),
      it(
        "firstOrRaise",
        () => {
          (() => SortedIntMap.empty() |> SortedIntMap.firstOrRaise((k, _) => k))
          |> Expect.shouldRaise;
          IntRange.create(~start=- countDiv2, ~count)
          |> IntRange.toIterable
          |> Iterable.map((i) => (i, i))
          |> SortedIntMap.fromEntries
          |> SortedIntMap.firstOrRaise((k, _) => k)
          |> Expect.toBeEqualToInt(- countDiv2)
        }
      ),
      it(
        "last",
        () => {
          SortedIntMap.empty() |> SortedIntMap.last((k, _) => k) |> Expect.toBeEqualToNoneOfInt;
          IntRange.create(~start=- countDiv2, ~count)
          |> IntRange.toIterable
          |> Iterable.map((i) => (i, i))
          |> SortedIntMap.fromEntries
          |> SortedIntMap.last((k, _) => k)
          |> Expect.toBeEqualToSomeOfInt(countDiv2 - 1)
        }
      ),
      it(
        "lastOrRaise",
        () => {
          (() => SortedIntMap.empty() |> SortedIntMap.lastOrRaise((k, _) => k))
          |> Expect.shouldRaise;
          IntRange.create(~start=- countDiv2, ~count)
          |> IntRange.toIterable
          |> Iterable.map((i) => (i, i))
          |> SortedIntMap.fromEntries
          |> SortedIntMap.lastOrRaise((k, _) => k)
          |> Expect.toBeEqualToInt(countDiv2 - 1)
        }
      ), /*
    it "reduceReversed" (fun () => {
      IntRange.create start::0 count::count
        |> IntRange.toIterable
        |> SortedIntMap.from
        |> SortedIntMap.reduceReversed while_::(fun acc i => acc < countDiv2) (fun acc i => 1 + acc) 0
        |> Expect.toBeEqualToInt countDiv2;
    }),*/
      it(
        "removeFirstOrRaise",
        () => {
          let set =
            IntRange.create(~start=0, ~count)
            |> IntRange.toIterable
            |> Iterable.map((i) => (i, i))
            |> SortedIntMap.fromEntries;
          IntRange.create(~start=0, ~count)
          |> IntRange.reduce(
               (acc, i) => {
                 acc |> SortedIntMap.firstOrRaise((k, _) => k) |> Expect.toBeEqualToInt(i);
                 acc |> SortedIntMap.removeFirstOrRaise
               },
               set
             )
          |> ignore;
          (() => SortedIntMap.empty() |> SortedIntMap.removeFirstOrRaise) |> Expect.shouldRaise
        }
      ),
      it(
        "removeLastOrRaise",
        () => {
          let set =
            IntRange.create(~start=0, ~count)
            |> IntRange.toIterable
            |> Iterable.map((i) => (i, i))
            |> SortedIntMap.fromEntries;
          IntRange.create(~start=0, ~count)
          |> IntRange.reduceReversed(
               (acc, i) => {
                 acc |> SortedIntMap.lastOrRaise((k, _) => k) |> Expect.toBeEqualToInt(i);
                 acc |> SortedIntMap.removeLastOrRaise
               },
               set
             )
          |> ignore;
          (() => SortedIntMap.empty() |> SortedIntMap.removeLastOrRaise) |> Expect.shouldRaise
        }
      ),
      it(
        "toIterableReversed",
        () =>
          IntRange.create(~start=0, ~count)
          |> IntRange.toIterable
          |> Iterable.map((i) => (i, i))
          |> SortedIntMap.fromEntries
          |> SortedIntMap.toIterableReversed((_, _) => ())
          |> Iterable.reduce(~while_=(acc, _) => acc < countDiv4, (acc, _) => 1 + acc, 0)
          |> Expect.toBeEqualToInt(countDiv4)
      ),
      it(
        "toSequenceReversed",
        () =>
          IntRange.create(~start=0, ~count)
          |> IntRange.toIterable
          |> Iterable.map((i) => (i, i))
          |> SortedIntMap.fromEntries
          |> SortedIntMap.toSequenceReversed((_, _) => ())
          |> Sequence.reduce(~while_=(acc, _) => acc < countDiv4, (acc, _) => 1 + acc, 0)
          |> Expect.toBeEqualToInt(countDiv4)
      )
    ]
  )
};

let test =
  describe(
    "SortedMap",
    [
      PersistentMapTester.test((module SortedIntMap), 100),
      PersistentMapTester.test((module SortedIntMap), 10000)
    ]
  );
