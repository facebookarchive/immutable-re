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

let generateTests =
    (
      getTestData: unit => 'vector,
      empty: unit => 'vector,
      add: (int, 'vector) => 'vector,
      update: (int, int, 'vector) => 'vector,
      removeLast: 'vector => 'vector,
      get: (int, 'vector) => option(int),
      n: int
    )
    : list(Test.t) => [
  it(
    sprintf("add %i elements", n),
    () => {
      let src = IntRange.create(~start=0, ~count=n);
      src |> IntRange.reduce((acc, i) => acc |> add(i), empty()) |> ignore
    }
  ),
  it(
    sprintf("vector with %i elements, removeLast %i elements", n, n / 2),
    () =>
      IntRange.create(~start=0, ~count=n / 2)
      |> IntRange.reduce((acc, _) => acc |> removeLast, getTestData())
      |> ignore
  ),
  it(
    sprintf("vector with %i elements, update %i elements alternating", n, n / 2),
    () =>
      Sequence.generate((i) => i + 2, 0)
      |> Sequence.take(n / 2)
      |> Sequence.reduce((acc, i) => acc |> update(i, n - i), getTestData())
      |> ignore
  ),
  it(
    sprintf("get %i values", n),
    () => {
      let vec = getTestData();
      IntRange.create(~start=0, ~count=n)
      |> IntRange.toIterable
      |> Iterable.forEach((i) => vec |> get(i) |> ignore)
    }
  )
];

let test = (n: int, count: int) : Test.t => {
  let indexes = IntRange.create(~start=0, ~count);
  let mutableArray = Array.init(count, (i) => i);
  let list = indexes |> IntRange.toIterable |> List.fromReverse;
  let stack = indexes |> IntRange.toIterable |> Stack.fromReverse;
  let vector =
    indexes
    |> IntRange.reduce((acc, i) => acc |> Vector.Transient.addLast(i), Vector.Transient.empty())
    |> Vector.Transient.persist;
  let mutableArray = Array.init(count, (i) => i);
  let list = indexes |> IntRange.toIterable |> List.fromReverse;
  let stack = indexes |> IntRange.toIterable |> Stack.fromReverse;
  let testGroup = [
    describe(
      "CamlMutableArray",
      generateTests(
        () => mutableArray,
        () => mutableArray,
        (a, v) => {
          v[0] = a;
          v
        },
        (i, a, v) => {
          v[i] = a;
          v
        },
        (v) => {
          v[0] |> ignore;
          v
        },
        (i, v) => Some(v[i]),
        count
      )
    ),
    describe(
      "List",
      generateTests(
        () => list,
        () => [],
        List.addFirst,
        (_, _, v) => v,
        List.removeFirstOrRaise,
        (_, _) => None,
        count
      )
    ),
    describe(
      "Stack",
      generateTests(
        () => stack,
        Stack.empty,
        Stack.addFirst,
        (_, _, v) => v,
        Stack.removeFirstOrRaise,
        (_, _) => None,
        count
      )
    ),
    describe(
      "Vector",
      generateTests(
        () => vector,
        Vector.empty,
        Vector.addLast,
        Vector.update,
        Vector.removeLastOrRaise,
        Vector.get,
        count
      )
    ),
    describe(
      "Vector.Transient",
      generateTests(
        () => vector |> Vector.mutate,
        () => Vector.empty() |> Vector.mutate,
        Vector.Transient.addLast,
        Vector.Transient.update,
        Vector.Transient.removeLastOrRaise,
        Vector.Transient.get,
        count
      )
    )
  ];
  let tests =
    Sequence.generate((i) => i, testGroup)
    |> Sequence.take(n)
    |> Sequence.flatMap(List.toSequence)
    |> Sequence.toIterable
    |> List.fromReverse;
  describe("VectorPerf", tests)
};
