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
    "Sequence",
    [
      it(
        "concat",
        () =>
          Sequence.concat([
            IntRange.create(~start=0, ~count=2) |> IntRange.toSequence,
            IntRange.create(~start=2, ~count=2) |> IntRange.toSequence,
            IntRange.create(~start=4, ~count=2) |> IntRange.toSequence
          ])
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([5, 4, 3, 2, 1, 0])
      ),
      it("defer", () => ()),
      it(
        "distinctUntilChangedWith",
        () =>
          [1, 1, 1, 2, 2, 2, 3, 3, 4, 4]
          |> List.toSequence
          |> Sequence.distinctUntilChangedWith(Equality.int)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([4, 3, 2, 1])
      ),
      it(
        "doOnNext",
        () => {
          let last = ref(0);
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toSequence
          |> Sequence.doOnNext((i) => last := i)
          |> Sequence.toIterable
          |> Iterable.forEach(ignore);
          last^ |> Expect.toBeEqualToInt(4)
        }
      ),
      it(
        "filter",
        () =>
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toSequence
          |> Sequence.filter((i) => i mod 2 === 0)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([4, 2, 0])
      ),
      it(
        "first",
        () => {
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toSequence
          |> Sequence.first
          |> Expect.toBeEqualToSomeOfInt(0);
          IntRange.empty() |> IntRange.toSequence |> Sequence.first |> Expect.toBeEqualToNoneOfInt
        }
      ),
      it(
        "firstOrRaise",
        () => {
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toSequence
          |> Sequence.firstOrRaise
          |> Expect.toBeEqualToInt(0);
          (() => Sequence.empty() |> Sequence.firstOrRaise) |> Expect.shouldRaise
        }
      ),
      it(
        "flatMap",
        () =>
          IntRange.create(~start=0, ~count=3)
          |> IntRange.toSequence
          |> Sequence.flatMap((_) => List.toSequence([1, 1, 1]))
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([1, 1, 1, 1, 1, 1, 1, 1, 1])
      ),
      it(
        "flatten",
        () =>
          [
            IntRange.create(~start=0, ~count=2) |> IntRange.toSequence,
            IntRange.create(~start=2, ~count=2) |> IntRange.toSequence,
            IntRange.create(~start=4, ~count=2) |> IntRange.toSequence
          ]
          |> List.toSequence
          |> Sequence.flatten
          |> Sequence.take(5)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([4, 3, 2, 1, 0])
      ),
      it(
        "generate",
        () =>
          Sequence.generate((i) => i + 1, 0)
          |> Sequence.skip(3)
          |> Sequence.take(2)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([4, 3])
      ),
      it(
        "map",
        () =>
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toSequence
          |> Sequence.map((i) => i * 3)
          |> Sequence.take(3)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([6, 3, 0])
      ),
      it(
        "return",
        () => {
          Sequence.return(1)
          |> Sequence.toIterable
          |> Iterable.reduce((acc, i) => acc + i, 0)
          |> Expect.toBeEqualToInt(1);
          Sequence.return(1)
          |> Sequence.toIterable
          |> Iterable.reduce(~while_=(_, i) => i < 0, (acc, i) => acc + i, 0)
          |> Expect.toBeEqualToInt(0)
        }
      ),
      it(
        "scan",
        () => {
          []
          |> List.toSequence
          |> Sequence.scan((_, i) => i, 0)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([0]);
          [1]
          |> List.toSequence
          |> Sequence.scan((_, i) => i, 0)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([1, 0]);
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toSequence
          |> Sequence.scan((acc, i) => acc + i, 0)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([10, 6, 3, 1, 0, 0])
        }
      ),
      it(
        "seek",
        () => {
          (() => Sequence.empty() |> Sequence.seek(-5)) |> Expect.shouldRaise;
          let seeked =
            IntRange.create(~start=0, ~count=5) |> IntRange.toSequence |> Sequence.seek(2);
          seeked |> Sequence.firstOrRaise |> Expect.toBeEqualToInt(2)
        }
      ),
      it(
        "seekWhile",
        () => {
          let seeked =
            IntRange.create(~start=0, ~count=5)
            |> IntRange.toSequence
            |> Sequence.seekWhile((i) => i < 2);
          seeked |> Sequence.firstOrRaise |> Expect.toBeEqualToInt(2)
        }
      ),
      it(
        "skip",
        () => {
          (() => Sequence.empty() |> Sequence.skip(-5)) |> Expect.shouldRaise;
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toSequence
          |> Sequence.skip(3)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([4, 3])
        }
      ),
      it(
        "skipWhile",
        () =>
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toSequence
          |> Sequence.skipWhile((i) => i < 3)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([4, 3])
      ),
      it(
        "startWith",
        () =>
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toSequence
          |> Sequence.startWith(-1)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([4, 3, 2, 1, 0, (-1)])
      ),
      it(
        "take",
        () => {
          (() => Sequence.empty() |> Sequence.take(-5)) |> Expect.shouldRaise;
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toSequence
          |> Sequence.take(3)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([2, 1, 0])
        }
      ),
      it(
        "takeWhile",
        () =>
          IntRange.create(~start=0, ~count=5)
          |> IntRange.toSequence
          |> Sequence.takeWhile((i) => i < 3)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([2, 1, 0])
      ),
      it(
        "zip",
        () => {
          Sequence.zip([
            List.toSequence([1, 2]),
            List.toSequence([4, 5, 6]),
            List.toSequence([7, 8, 9])
          ])
          |> Sequence.flatMap(List.toSequence)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([8, 5, 2, 7, 4, 1]);
          Sequence.zip([
            List.toSequence([1, 2, 3]),
            List.toSequence([4, 5, 6]),
            List.toSequence([7, 8])
          ])
          |> Sequence.flatMap(List.toSequence)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([8, 5, 2, 7, 4, 1]);
          Sequence.zip([
            List.toSequence([1, 2, 3]),
            List.toSequence([4, 5, 6]),
            List.toSequence([7, 8, 9])
          ])
          |> Sequence.flatMap(List.toSequence)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([9, 6, 3, 8, 5, 2, 7, 4, 1]);
          Sequence.zip([
            List.toSequence([1, 2, 3]),
            List.toSequence([4, 5]),
            List.toSequence([7, 8, 9])
          ])
          |> Sequence.flatMap(List.toSequence)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([8, 5, 2, 7, 4, 1])
        }
      ),
      it(
        "zip2With",
        () => {
          Sequence.zip2With(
            ~zipper=(a, b) => [a, b],
            List.toSequence([1, 2]),
            List.toSequence([4, 5, 6])
          )
          |> Sequence.flatMap(List.toSequence)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([5, 2, 4, 1]);
          Sequence.zip2With(
            ~zipper=(a, b) => [a, b],
            List.toSequence([1, 2, 3]),
            List.toSequence([4, 5])
          )
          |> Sequence.flatMap(List.toSequence)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([5, 2, 4, 1]);
          Sequence.zip2With(
            ~zipper=(a, b) => [a, b],
            List.toSequence([1, 2, 3]),
            List.toSequence([4, 5, 6])
          )
          |> Sequence.flatMap(List.toSequence)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([6, 3, 5, 2, 4, 1])
        }
      ),
      it(
        "zip3With",
        () => {
          Sequence.zip3With(
            ~zipper=(a, b, c) => [a, b, c],
            List.toSequence([1, 2]),
            List.toSequence([4, 5, 6]),
            List.toSequence([7, 8, 9])
          )
          |> Sequence.flatMap(List.toSequence)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([8, 5, 2, 7, 4, 1]);
          Sequence.zip3With(
            ~zipper=(a, b, c) => [a, b, c],
            List.toSequence([1, 2, 3]),
            List.toSequence([4, 5, 6]),
            List.toSequence([7, 8])
          )
          |> Sequence.flatMap(List.toSequence)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([8, 5, 2, 7, 4, 1]);
          Sequence.zip3With(
            ~zipper=(a, b, c) => [a, b, c],
            List.toSequence([1, 2, 3]),
            List.toSequence([4, 5, 6]),
            List.toSequence([7, 8, 9])
          )
          |> Sequence.flatMap(List.toSequence)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([9, 6, 3, 8, 5, 2, 7, 4, 1]);
          Sequence.zip3With(
            ~zipper=(a, b, c) => [a, b, c],
            List.toSequence([1, 2, 3]),
            List.toSequence([4, 5]),
            List.toSequence([7, 8, 9])
          )
          |> Sequence.flatMap(List.toSequence)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([8, 5, 2, 7, 4, 1])
        }
      ),
      it(
        "zipLongest",
        () => {
          Sequence.zipLongest([
            List.toSequence([1, 2, 3]),
            List.toSequence([4, 5, 6]),
            List.toSequence([7, 8, 9])
          ])
          |> Sequence.flatMap(List.toSequence)
          |> Sequence.toIterable
          |> Iterable.filter(Option.isNotEmpty)
          |> Iterable.map(Option.firstOrRaise)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([9, 6, 3, 8, 5, 2, 7, 4, 1]);
          Sequence.zipLongest([
            List.toSequence([1, 2]),
            List.toSequence([4, 5, 6]),
            List.toSequence([7, 8, 9])
          ])
          |> Sequence.flatMap(List.toSequence)
          |> Sequence.toIterable
          |> Iterable.filter(Option.isNotEmpty)
          |> Iterable.map(Option.firstOrRaise)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([9, 6, 8, 5, 2, 7, 4, 1]);
          Sequence.zipLongest([
            List.toSequence([1, 2, 3]),
            List.toSequence([4, 5, 6]),
            List.toSequence([7, 8])
          ])
          |> Sequence.flatMap(List.toSequence)
          |> Sequence.toIterable
          |> Iterable.filter(Option.isNotEmpty)
          |> Iterable.map(Option.firstOrRaise)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([6, 3, 8, 5, 2, 7, 4, 1]);
          Sequence.zipLongest([
            List.toSequence([1, 2, 3]),
            List.toSequence([4, 5]),
            List.toSequence([7, 8, 9])
          ])
          |> Sequence.flatMap(List.toSequence)
          |> Sequence.toIterable
          |> Iterable.filter(Option.isNotEmpty)
          |> Iterable.map(Option.firstOrRaise)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([9, 3, 8, 5, 2, 7, 4, 1]);
          Sequence.zipLongest([
            List.toSequence([1, 2]),
            List.toSequence([4, 5]),
            List.toSequence([7, 8, 9])
          ])
          |> Sequence.flatMap(List.toSequence)
          |> Sequence.toIterable
          |> Iterable.filter(Option.isNotEmpty)
          |> Iterable.map(Option.firstOrRaise)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([9, 8, 5, 2, 7, 4, 1]);
          Sequence.zipLongest([
            List.toSequence([1, 2]),
            List.toSequence([4, 5, 6]),
            List.toSequence([7, 8])
          ])
          |> Sequence.flatMap(List.toSequence)
          |> Sequence.toIterable
          |> Iterable.filter(Option.isNotEmpty)
          |> Iterable.map(Option.firstOrRaise)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([6, 8, 5, 2, 7, 4, 1]);
          Sequence.zipLongest([
            List.toSequence([1, 2, 3]),
            List.toSequence([4, 5]),
            List.toSequence([7, 8])
          ])
          |> Sequence.flatMap(List.toSequence)
          |> Sequence.toIterable
          |> Iterable.filter(Option.isNotEmpty)
          |> Iterable.map(Option.firstOrRaise)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([3, 8, 5, 2, 7, 4, 1])
        }
      ),
      it(
        "zipLongest2With",
        () => {
          Sequence.zipLongest2With(
            ~zipper=(a, b) => [a, b],
            List.toSequence([1, 2, 3]),
            List.toSequence([4, 5, 6])
          )
          |> Sequence.flatMap(List.toSequence)
          |> Sequence.toIterable
          |> Iterable.filter(Option.isNotEmpty)
          |> Iterable.map(Option.firstOrRaise)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([6, 3, 5, 2, 4, 1]);
          Sequence.zipLongest2With(
            ~zipper=(a, b) => [a, b],
            List.toSequence([1, 2]),
            List.toSequence([4, 5, 6])
          )
          |> Sequence.flatMap(List.toSequence)
          |> Sequence.toIterable
          |> Iterable.filter(Option.isNotEmpty)
          |> Iterable.map(Option.firstOrRaise)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([6, 5, 2, 4, 1]);
          Sequence.zipLongest2With(
            ~zipper=(a, b) => [a, b],
            List.toSequence([1, 2, 3]),
            List.toSequence([4, 5])
          )
          |> Sequence.flatMap(List.toSequence)
          |> Sequence.toIterable
          |> Iterable.filter(Option.isNotEmpty)
          |> Iterable.map(Option.firstOrRaise)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([3, 5, 2, 4, 1])
        }
      ),
      it(
        "zipLongest3With",
        () => {
          Sequence.zipLongest3With(
            ~zipper=(a, b, c) => [a, b, c],
            List.toSequence([1, 2, 3]),
            List.toSequence([4, 5, 6]),
            List.toSequence([7, 8, 9])
          )
          |> Sequence.flatMap(List.toSequence)
          |> Sequence.toIterable
          |> Iterable.filter(Option.isNotEmpty)
          |> Iterable.map(Option.firstOrRaise)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([9, 6, 3, 8, 5, 2, 7, 4, 1]);
          Sequence.zipLongest3With(
            ~zipper=(a, b, c) => [a, b, c],
            List.toSequence([1, 2]),
            List.toSequence([4, 5, 6]),
            List.toSequence([7, 8, 9])
          )
          |> Sequence.flatMap(List.toSequence)
          |> Sequence.filter(Option.isNotEmpty)
          |> Sequence.map(Option.firstOrRaise)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([9, 6, 8, 5, 2, 7, 4, 1]);
          Sequence.zipLongest3With(
            ~zipper=(a, b, c) => [a, b, c],
            List.toSequence([1, 2, 3]),
            List.toSequence([4, 5, 6]),
            List.toSequence([7, 8])
          )
          |> Sequence.flatMap(List.toSequence)
          |> Sequence.filter(Option.isNotEmpty)
          |> Sequence.map(Option.firstOrRaise)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([6, 3, 8, 5, 2, 7, 4, 1]);
          Sequence.zipLongest3With(
            ~zipper=(a, b, c) => [a, b, c],
            List.toSequence([1, 2, 3]),
            List.toSequence([4, 5]),
            List.toSequence([7, 8, 9])
          )
          |> Sequence.flatMap(List.toSequence)
          |> Sequence.filter(Option.isNotEmpty)
          |> Sequence.map(Option.firstOrRaise)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([9, 3, 8, 5, 2, 7, 4, 1]);
          Sequence.zipLongest3With(
            ~zipper=(a, b, c) => [a, b, c],
            List.toSequence([1, 2]),
            List.toSequence([4, 5]),
            List.toSequence([7, 8, 9])
          )
          |> Sequence.flatMap(List.toSequence)
          |> Sequence.filter(Option.isNotEmpty)
          |> Sequence.map(Option.firstOrRaise)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([9, 8, 5, 2, 7, 4, 1]);
          Sequence.zipLongest3With(
            ~zipper=(a, b, c) => [a, b, c],
            List.toSequence([1, 2]),
            List.toSequence([4, 5, 6]),
            List.toSequence([7, 8])
          )
          |> Sequence.flatMap(List.toSequence)
          |> Sequence.filter(Option.isNotEmpty)
          |> Sequence.map(Option.firstOrRaise)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([6, 8, 5, 2, 7, 4, 1]);
          Sequence.zipLongest3With(
            ~zipper=(a, b, c) => [a, b, c],
            List.toSequence([1, 2, 3]),
            List.toSequence([4, 5]),
            List.toSequence([7, 8])
          )
          |> Sequence.flatMap(List.toSequence)
          |> Sequence.filter(Option.isNotEmpty)
          |> Sequence.map(Option.firstOrRaise)
          |> Sequence.toIterable
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([3, 8, 5, 2, 7, 4, 1])
        }
      )
    ]
  );
