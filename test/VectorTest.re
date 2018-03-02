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

module Tester10 =
  DequeTester.Make(
    (Vector: DequeTester.S1),
    {
      let count = 10;
    }
  );

module Tester50 =
  DequeTester.Make(
    (Vector: DequeTester.S1),
    {
      let count = 50;
    }
  );

module Tester200 =
  DequeTester.Make(
    (Vector: DequeTester.S1),
    {
      let count = 200;
    }
  );

module Tester2000 =
  DequeTester.Make(
    (Vector: DequeTester.S1),
    {
      let count = 2000;
    }
  );

module Tester50000 =
  DequeTester.Make(
    (Vector: DequeTester.S1),
    {
      let count = 50000;
    }
  );

let testVector = (count) =>
  describe(
    sprintf("count: %i", count),
    [
      describe(
        "concat",
        [
          it(
            "balanced segments",
            () => {
              let segmentCount = count / 2;
              let firstRange = IntRange.create(~start=0, ~count=segmentCount);
              let secondRange = IntRange.create(~start=segmentCount, ~count=segmentCount);
              let firstVector = firstRange |> IntRange.toIterable |> Vector.from;
              let secondVector = secondRange |> IntRange.toIterable |> Vector.from;
              Vector.concat([firstVector, secondVector])
              |> Vector.toIterable
              |> List.fromReverse
              |> Expect.toBeEqualToListOfInt(
                   Iterable.concat([
                     firstRange |> IntRange.toIterable,
                     secondRange |> IntRange.toIterable
                   ])
                   |> List.fromReverse
                 )
            }
          ),
          it(
            "left larger than right",
            () => {
              let firstSegmentCount = count / 4;
              let secondSegmentCount = count - firstSegmentCount;
              let firstRange = IntRange.create(~start=0, ~count=firstSegmentCount);
              let secondRange =
                IntRange.create(~start=firstSegmentCount, ~count=secondSegmentCount);
              let firstVector = firstRange |> IntRange.toIterable |> Vector.from;
              let secondVector = secondRange |> IntRange.toIterable |> Vector.from;
              Vector.concat([firstVector, secondVector])
              |> Vector.toIterable
              |> List.fromReverse
              |> Expect.toBeEqualToListOfInt(
                   Iterable.concat([
                     firstRange |> IntRange.toIterable,
                     secondRange |> IntRange.toIterable
                   ])
                   |> List.fromReverse
                 )
            }
          ),
          it(
            "right larger than left",
            () => {
              let secondSegmentCount = count / 4;
              let firstSegmentCount = count - secondSegmentCount;
              let firstRange = IntRange.create(~start=0, ~count=firstSegmentCount);
              let secondRange =
                IntRange.create(~start=firstSegmentCount, ~count=secondSegmentCount);
              let firstVector = firstRange |> IntRange.toIterable |> Vector.from;
              let secondVector = secondRange |> IntRange.toIterable |> Vector.from;
              Vector.concat([firstVector, secondVector])
              |> Vector.toIterable
              |> List.fromReverse
              |> Expect.toBeEqualToListOfInt(
                   Iterable.concat([
                     firstRange |> IntRange.toIterable,
                     secondRange |> IntRange.toIterable
                   ])
                   |> List.fromReverse
                 )
            }
          )
        ]
      ),
      it(
        "get",
        () => {
          let vector = IntRange.create(~start=0, ~count) |> IntRange.toIterable |> Vector.from;
          vector |> Vector.get(-1) |> Expect.toBeEqualToNoneOfInt;
          vector |> Vector.get(count) |> Expect.toBeEqualToNoneOfInt;
          vector
          |> Vector.toIterable
          |> Iterable.forEach((i) => vector |> Vector.get(i) |> Expect.toBeEqualToSomeOfInt(i))
        }
      ),
      it(
        "getOrRaise",
        () => {
          let vector = IntRange.create(~start=0, ~count) |> IntRange.toIterable |> Vector.from;
          (() => vector |> Vector.getOrRaise(-1)) |> Expect.shouldRaise;
          (() => vector |> Vector.getOrRaise(count)) |> Expect.shouldRaise;
          vector
          |> Vector.toIterable
          |> Iterable.forEach((i) => vector |> Vector.getOrRaise(i) |> Expect.toBeEqualToInt(i))
        }
      ),
      it(
        "init",
        () => {
          let vector = Vector.init(count, (i) => i + 1);
          vector
          |> Vector.toIterable
          |> Iterable.forEach(
               (i) => vector |> Vector.getOrRaise(i - 1) |> Expect.toBeEqualToInt(i)
             )
        }
      ),
      it(
        "insertAt",
        () => {
          let zeroedVectorOfSizeCount =
            Iterable.generate((i) => i, 0) |> Iterable.take(count) |> Vector.from;
          let alternatingOneZeroVectorOfSizeTwoTimesCount =
            IntRange.create(~start=0, ~count)
            |> IntRange.toIterable
            |> Iterable.map((i) => i * 2)
            |> Iterable.reduce((acc, i) => acc |> Vector.insertAt(i, 1), zeroedVectorOfSizeCount);
          alternatingOneZeroVectorOfSizeTwoTimesCount
          |> Vector.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt(
               Iterable.generate((i) => i, [1, 0])
               |> Iterable.take(count)
               |> Iterable.flatMap(List.toIterable)
               |> List.fromReverse
             )
        }
      ),
      it(
        "removeAt",
        () => {
          let vector =
            Iterable.generate((i) => i, [0, 1])
            |> Iterable.take(count / 2)
            |> Iterable.flatMap(List.toIterable)
            |> Vector.from;
          IntRange.create(~start=0, ~count=count / 2)
          |> IntRange.reduce((acc, i) => acc |> Vector.removeAt(i), vector)
          |> Vector.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt(
               Iterable.generate((i) => i, 1) |> Iterable.take(count / 2) |> List.fromReverse
             )
        }
      ),
      it(
        "slice",
        () => {
          let vector = IntRange.create(~start=0, ~count) |> IntRange.toIterable |> Vector.from;
          vector
          |> Vector.slice(~start=0, ~end_=count)
          |> Pervasives.(===)(vector)
          |> Expect.toBeEqualToTrue;
          vector |> Vector.slice |> Pervasives.(===)(vector) |> Expect.toBeEqualToTrue;
          let countDiv4 = count / 4;
          vector
          |> Vector.slice(~start=countDiv4)
          |> Vector.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt(
               IntRange.create(~start=countDiv4, ~count=count - countDiv4)
               |> IntRange.toIterable
               |> List.fromReverse
             );
          vector
          |> Vector.slice(~start=- countDiv4)
          |> Vector.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt(
               IntRange.create(~start=count - countDiv4, ~count=countDiv4)
               |> IntRange.toIterable
               |> List.fromReverse
             );
          vector
          |> Vector.slice(~end_=countDiv4)
          |> Vector.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt(
               IntRange.create(~start=0, ~count=countDiv4)
               |> IntRange.toIterable
               |> List.fromReverse
             );
          vector
          |> Vector.slice(~end_=- countDiv4)
          |> Vector.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt(
               IntRange.create(~start=0, ~count=count - countDiv4)
               |> IntRange.toIterable
               |> List.fromReverse
             );
          vector
          |> Vector.slice(~start=countDiv4, ~end_=- countDiv4)
          |> Vector.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt(
               IntRange.create(~start=countDiv4, ~count=count - countDiv4 - countDiv4)
               |> IntRange.toIterable
               |> List.fromReverse
             );
          vector
          |> Vector.slice(~start=- count, ~end_=count)
          |> Vector.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt(
               IntRange.create(~start=0, ~count) |> IntRange.toIterable |> List.fromReverse
             );
          vector
          |> Vector.slice(~start=- count, ~end_=count - countDiv4)
          |> Vector.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt(
               IntRange.create(~start=0, ~count=count - countDiv4)
               |> IntRange.toIterable
               |> List.fromReverse
             );
          vector
          |> Vector.slice(~start=- count + countDiv4, ~end_=count - countDiv4)
          |> Vector.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt(
               IntRange.create(~start=countDiv4, ~count=count - countDiv4 - countDiv4)
               |> IntRange.toIterable
               |> List.fromReverse
             )
        }
      ),
      it(
        "skip",
        () => {
          let countDiv4 = count / 4;
          let countDiv2 = count / 2;
          let vector = IntRange.create(~start=0, ~count) |> IntRange.toIterable |> Vector.from;
          vector |> Vector.skip(0) |> Pervasives.(===)(vector) |> Expect.toBeEqualToTrue;
          vector |> Vector.skip(count) |> Vector.isEmpty |> Expect.toBeEqualToTrue;
          vector
          |> Vector.skip(countDiv4)
          |> Vector.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt(
               IntRange.create(~start=countDiv4, ~count=count - countDiv4)
               |> IntRange.toIterable
               |> List.fromReverse
             );
          vector
          |> Vector.skip(countDiv2)
          |> Vector.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt(
               IntRange.create(~start=countDiv2, ~count=count - countDiv2)
               |> IntRange.toIterable
               |> List.fromReverse
             );
          vector
          |> Vector.skip(count - countDiv4)
          |> Vector.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt(
               IntRange.create(~start=count - countDiv4, ~count=countDiv4)
               |> IntRange.toIterable
               |> List.fromReverse
             )
        }
      ),
      it(
        "take",
        () => {
          let countDiv4 = count / 4;
          let countDiv2 = count / 2;
          let vector = IntRange.create(~start=0, ~count) |> IntRange.toIterable |> Vector.from;
          vector |> Vector.take(count) |> Pervasives.(===)(vector) |> Expect.toBeEqualToTrue;
          vector |> Vector.take(0) |> Vector.isEmpty |> Expect.toBeEqualToTrue;
          vector
          |> Vector.take(countDiv4)
          |> Vector.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt(
               IntRange.create(~start=0, ~count=countDiv4)
               |> IntRange.toIterable
               |> List.fromReverse
             );
          vector
          |> Vector.take(countDiv2)
          |> Vector.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt(
               IntRange.create(~start=0, ~count=count - countDiv2)
               |> IntRange.toIterable
               |> List.fromReverse
             );
          vector
          |> Vector.take(count - countDiv4)
          |> Vector.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt(
               IntRange.create(~start=0, ~count=count - countDiv4)
               |> IntRange.toIterable
               |> List.fromReverse
             )
        }
      ),
      it(
        "toKeyedIterable",
        () =>
          IntRange.create(~start=0, ~count)
          |> IntRange.toIterableReversed
          |> Vector.from
          |> Vector.toKeyedIterable
          |> KeyedIterable.forEach((i, v) => count - v - 1 |> Expect.toBeEqualToInt(i))
      ),
      it(
        "toKeyedIterableReversed",
        () =>
          IntRange.create(~start=0, ~count)
          |> IntRange.toIterableReversed
          |> Vector.from
          |> Vector.toKeyedIterableReversed
          |> KeyedIterable.forEach((i, v) => count - v - 1 |> Expect.toBeEqualToInt(i))
      ),
      it(
        "toMap",
        () => {
          let map =
            IntRange.create(~start=0, ~count) |> IntRange.toIterable |> Vector.from |> Vector.toMap;
          map |> Map.count |> Expect.toBeEqualToInt(count);
          let countDiv4 = count / 4;
          let countDiv2 = count / 2;
          map |> Map.containsKey(0) |> Expect.toBeEqualToTrue;
          map |> Map.containsKey(countDiv4) |> Expect.toBeEqualToTrue;
          map |> Map.containsKey(countDiv2) |> Expect.toBeEqualToTrue;
          map |> Map.containsKey(count - countDiv4) |> Expect.toBeEqualToTrue;
          map |> Map.containsKey(count - 1) |> Expect.toBeEqualToTrue;
          map |> Map.containsKey(-1) |> Expect.toBeEqualToFalse;
          map |> Map.containsKey(count) |> Expect.toBeEqualToFalse;
          map |> Map.get(0) |> Expect.toBeEqualToSomeOfInt(0);
          map |> Map.get(countDiv4) |> Expect.toBeEqualToSomeOfInt(countDiv4);
          map |> Map.get(countDiv2) |> Expect.toBeEqualToSomeOfInt(countDiv2);
          map |> Map.get(count - countDiv4) |> Expect.toBeEqualToSomeOfInt(count - countDiv4);
          map |> Map.get(count - 1) |> Expect.toBeEqualToSomeOfInt(count - 1);
          map |> Map.get(-1) |> Expect.toBeEqualToNoneOfInt;
          map |> Map.get(count) |> Expect.toBeEqualToNoneOfInt;
          map |> Map.getOrRaise(0) |> Expect.toBeEqualToInt(0);
          map |> Map.getOrRaise(countDiv4) |> Expect.toBeEqualToInt(countDiv4);
          map |> Map.getOrRaise(countDiv2) |> Expect.toBeEqualToInt(countDiv2);
          map |> Map.getOrRaise(count - countDiv4) |> Expect.toBeEqualToInt(count - countDiv4);
          map |> Map.getOrRaise(count - 1) |> Expect.toBeEqualToInt(count - 1);
          (() => map |> Map.getOrRaise(-1)) |> Expect.shouldRaise;
          (() => map |> Map.getOrRaise(count)) |> Expect.shouldRaise;
          map
          |> Map.reduce(~while_=(_, k, _) => k < countDiv4, (acc, _, _) => 1 + acc, 0)
          |> Expect.toBeEqualToInt(countDiv4)
        }
      ),
      it(
        "update",
        () => {
          let vector = Iterable.generate((i) => i, 0) |> Iterable.take(count) |> Vector.from;
          IntRange.create(~start=0, ~count)
          |> IntRange.reduce(
               (vec, i) =>
                 vec
                 |> Vector.update(
                      i,
                      if (i mod 2 === 0) {
                        0
                      } else {
                        1
                      }
                    ),
               vector
             )
          |> Vector.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt(
               Iterable.generate((i) => i, [0, 1])
               |> Iterable.take(count / 2)
               |> Iterable.flatMap(List.toIterable)
               |> List.fromReverse
             )
        }
      ),
      it(
        "updateAll",
        () => {
          let vector = Iterable.generate((i) => i, 0) |> Iterable.take(count) |> Vector.from;
          vector
          |> Vector.updateAll(
               (i, _) =>
                 if (i mod 2 === 0) {
                   0
                 } else {
                   1
                 }
             )
          |> Vector.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt(
               Iterable.generate((i) => i, [0, 1])
               |> Iterable.take(count / 2)
               |> Iterable.flatMap(List.toIterable)
               |> List.fromReverse
             )
        }
      ),
      it(
        "updateWith",
        () => {
          let vector =
            Iterable.generate((i) => i, [0, 1])
            |> Iterable.take(count / 2)
            |> Iterable.flatMap(List.toIterable)
            |> Vector.from;
          IntRange.create(~start=0, ~count)
          |> IntRange.reduce(
               (acc, i) =>
                 acc
                 |> Vector.updateWith(
                      i,
                      (v) =>
                        if (v === 0) {
                          1
                        } else {
                          0
                        }
                    ),
               vector
             )
          |> Vector.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt(
               Iterable.generate((i) => i, [1, 0])
               |> Iterable.take(count / 2)
               |> Iterable.flatMap(List.toIterable)
               |> List.fromReverse
             )
        }
      )
    ]
  );

let test =
  [
    Tester10.tests,
    [testVector(10)],
    Tester50.tests,
    [testVector(50)],
    Tester200.tests,
    [testVector(200)],
    Tester2000.tests,
    [testVector(2000)],
    Tester50000.tests
    /*[testVector 50000],*/
  ]
  |> List.toIterable
  |> Iterable.flatMap(List.toIterable)
  |> List.fromReverse
  |> describe("Vector");
