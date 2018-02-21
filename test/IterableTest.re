/***
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open Immutable;

open ReUnit;

open ReUnit.Test;

let test =
  describe(
    "Iterable",
    [
      it(
        "concat",
        () =>
          Iterable.concat([
            IntRange.create(~start=0, ~count=2) |> IntRange.toIterable,
            IntRange.create(~start=2, ~count=2) |> IntRange.toIterable,
            IntRange.create(~start=4, ~count=2) |> IntRange.toIterable
          ])
          |> Iterable.take(5)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([4, 3, 2, 1, 0])
      ),
      it("defer", () => ()),
      it(
        "distinctUntilChangedWith",
        () =>
          [1, 1, 1, 2, 2, 2, 3, 3, 4, 4]
          |> List.toIterable
          |> Iterable.distinctUntilChangedWith(Equality.int)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([4, 3, 2, 1])
      ),
      it(
        "doOnNext",
        () => {
          let last = ref(0);
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toIterable
          |> Iterable.doOnNext((i) => last := i)
          |> Iterable.forEach(ignore);
          last^ |> Expect.toBeEqualToInt(4);
          let empty = Iterable.empty();
          Pervasives.(===)(Iterable.doOnNext((_) => (), empty), empty) |> Expect.toBeEqualToTrue
        }
      ),
      it(
        "filter",
        () => {
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toIterable
          |> Iterable.filter((i) => i mod 2 === 0)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([4, 2, 0]);
          let empty = Iterable.empty();
          Pervasives.(===)(Iterable.filter((_) => true, empty), empty) |> Expect.toBeEqualToTrue
        }
      ),
      it(
        "flatMap",
        () =>
          IntRange.create(~start=0, ~count=3)
          |> IntRange.toIterable
          |> Iterable.flatMap((_) => List.toIterable([1, 1, 1]))
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([1, 1, 1, 1, 1, 1, 1, 1, 1])
      ),
      it(
        "flatten",
        () =>
          [
            IntRange.create(~start=0, ~count=2) |> IntRange.toIterable,
            IntRange.create(~start=2, ~count=2) |> IntRange.toIterable,
            IntRange.create(~start=4, ~count=2) |> IntRange.toIterable
          ]
          |> List.toIterable
          |> Iterable.flatten
          |> Iterable.take(5)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([4, 3, 2, 1, 0])
      ),
      it(
        "generate",
        () =>
          Iterable.generate((i) => i + 1, 0)
          |> Iterable.skip(3)
          |> Iterable.take(2)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([4, 3])
      ),
      it(
        "map",
        () =>
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toIterable
          |> Iterable.map((i) => i * 3)
          |> Iterable.take(3)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([6, 3, 0])
      ),
      it(
        "repeat",
        () =>
          Iterable.generate(Functions.identity, 5)
          |> Iterable.take(10)
          |> Iterable.reduce((acc, _) => acc + 5, 0)
          |> Expect.toBeEqualToInt(50)
      ),
      it(
        "return",
        () => {
          Iterable.return(1) |> Iterable.reduce((acc, i) => acc + i, 0) |> Expect.toBeEqualToInt(1);
          Iterable.return(1)
          |> Iterable.reduce(~while_=(_, i) => i < 0, (acc, i) => acc + i, 0)
          |> Expect.toBeEqualToInt(0)
        }
      ),
      it(
        "scan",
        () => {
          []
          |> List.toIterable
          |> Iterable.scan((_, i) => i, 0)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([0]);
          [1]
          |> List.toIterable
          |> Iterable.scan((_, i) => i, 0)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([1, 0]);
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toIterable
          |> Iterable.scan((acc, i) => acc + i, 0)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([10, 6, 3, 1, 0, 0])
        }
      ),
      it(
        "skip",
        () => {
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toIterable
          |> Iterable.skip(3)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([4, 3]);
          let empty = Iterable.empty();
          Pervasives.(===)(Iterable.skip(5, empty), empty) |> Expect.toBeEqualToTrue;
          let x = Iterable.return(8);
          Pervasives.(===)(Iterable.skip(0, x), x) |> Expect.toBeEqualToTrue
        }
      ),
      it(
        "skipWhile",
        () => {
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toIterable
          |> Iterable.skipWhile((i) => i < 3)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([4, 3]);
          let empty = Iterable.empty();
          Pervasives.(===)(Iterable.skipWhile((_) => true, empty), empty) |> Expect.toBeEqualToTrue
        }
      ),
      it(
        "startWith",
        () =>
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toIterable
          |> Iterable.startWith(-1)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([4, 3, 2, 1, 0, (-1)])
      ),
      it(
        "take",
        () => {
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toIterable
          |> Iterable.take(3)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([2, 1, 0]);
          let empty = Iterable.empty();
          Pervasives.(===)(Iterable.take(5, empty), empty) |> Expect.toBeEqualToTrue
        }
      ),
      it(
        "takeWhile",
        () => {
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toIterable
          |> Iterable.takeWhile((i) => i < 3)
          |> Iterable.take(2)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([1, 0]);
          let empty = Iterable.empty();
          Pervasives.(===)(Iterable.takeWhile((_) => true, empty), empty) |> Expect.toBeEqualToTrue
        }
      ),
      it(
        "every",
        () => {
          Iterable.empty() |> Iterable.every((_) => false) |> Expect.toBeEqualToTrue;
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toIterable
          |> Iterable.every((i) => i >= 0)
          |> Expect.toBeEqualToTrue;
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toIterable
          |> Iterable.every((i) => i < 3)
          |> Expect.toBeEqualToFalse
        }
      ),
      it(
        "find",
        () => {
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toIterable
          |> Iterable.find((i) => i === 2)
          |> Expect.toBeEqualToSomeOfInt(2);
          IntRange.empty()
          |> IntRange.toIterable
          |> Iterable.find((i) => i === 2)
          |> Expect.toBeEqualToNoneOfInt;
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toIterable
          |> Iterable.find((i) => i === 5)
          |> Expect.toBeEqualToNoneOfInt
        }
      ),
      it(
        "findOrRaise",
        () => {
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toIterable
          |> Iterable.findOrRaise((i) => i === 2)
          |> Expect.toBeEqualToInt(2);
          (() => Iterable.empty() |> Iterable.findOrRaise((i) => i === 2)) |> Expect.shouldRaise
        }
      ),
      it(
        "forEach",
        () => {
          let last = ref(0);
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toIterable
          |> Iterable.forEach(~while_=(i) => i < 3, (i) => last := i);
          last^ |> Expect.toBeEqualToInt(2)
        }
      ),
      it(
        "none",
        () => {
          Iterable.empty() |> Iterable.none((_) => false) |> Expect.toBeEqualToTrue;
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toIterable
          |> Iterable.none((i) => i >= 2)
          |> Expect.toBeEqualToFalse;
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toIterable
          |> Iterable.none((i) => i < 0)
          |> Expect.toBeEqualToTrue
        }
      ),
      it(
        "some",
        () => {
          Iterable.empty() |> Iterable.some((_) => false) |> Expect.toBeEqualToFalse;
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toIterable
          |> Iterable.some((i) => i >= 2)
          |> Expect.toBeEqualToTrue;
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toIterable
          |> Iterable.some((i) => i < 0)
          |> Expect.toBeEqualToFalse
        }
      )
    ]
  );
