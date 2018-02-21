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
    "ReadOnlyArray",
    [
      it(
        "count",
        () => {
          ReadOnlyArray.empty() |> ReadOnlyArray.count |> Expect.toBeEqualToInt(0);
          ReadOnlyArray.ofUnsafe([|1, 2, 3|]) |> ReadOnlyArray.count |> Expect.toBeEqualToInt(3)
        }
      ),
      it(
        "first",
        () => {
          ReadOnlyArray.ofUnsafe([|1, 2, 3|])
          |> ReadOnlyArray.first
          |> Expect.toBeEqualToSomeOfInt(1);
          ReadOnlyArray.empty() |> ReadOnlyArray.first |> Expect.toBeEqualToNoneOfInt
        }
      ),
      it(
        "firstOrRaise",
        () => {
          ReadOnlyArray.ofUnsafe([|1, 2, 3|])
          |> ReadOnlyArray.firstOrRaise
          |> Expect.toBeEqualToInt(1);
          (() => ReadOnlyArray.empty() |> ReadOnlyArray.firstOrRaise) |> Expect.shouldRaise
        }
      ),
      it(
        "get",
        () => {
          let arr = ReadOnlyArray.ofUnsafe([|1, 2, 3|]);
          arr |> ReadOnlyArray.get(0) |> Expect.toBeEqualToSomeOfInt(1);
          arr |> ReadOnlyArray.get(-1) |> Expect.toBeEqualToNoneOfInt;
          arr |> ReadOnlyArray.get(3) |> Expect.toBeEqualToNoneOfInt
        }
      ),
      it(
        "getOrRaise",
        () => {
          let arr = ReadOnlyArray.ofUnsafe([|1, 2, 3|]);
          arr |> ReadOnlyArray.getOrRaise(0) |> Expect.toBeEqualToInt(1);
          (() => arr |> ReadOnlyArray.getOrRaise(-1)) |> Expect.shouldRaise;
          (() => arr |> ReadOnlyArray.getOrRaise(3)) |> Expect.shouldRaise
        }
      ),
      it(
        "init",
        () => {
          let arr = ReadOnlyArray.init(20, (i) => i + 1);
          arr
          |> ReadOnlyArray.toIterable
          |> Iterable.forEach(
               (i) => arr |> ReadOnlyArray.getOrRaise(i - 1) |> Expect.toBeEqualToInt(i)
             )
        }
      ),
      it(
        "isEmpty",
        () => {
          ReadOnlyArray.empty() |> ReadOnlyArray.isEmpty |> Expect.toBeEqualToTrue;
          ReadOnlyArray.ofUnsafe([|1, 2, 3|]) |> ReadOnlyArray.isEmpty |> Expect.toBeEqualToFalse
        }
      ),
      it(
        "isNotEmpty",
        () => {
          ReadOnlyArray.empty() |> ReadOnlyArray.isNotEmpty |> Expect.toBeEqualToFalse;
          ReadOnlyArray.ofUnsafe([|1, 2, 3|]) |> ReadOnlyArray.isNotEmpty |> Expect.toBeEqualToTrue
        }
      ),
      it(
        "last",
        () => {
          ReadOnlyArray.ofUnsafe([|1, 2, 3|])
          |> ReadOnlyArray.last
          |> Expect.toBeEqualToSomeOfInt(3);
          ReadOnlyArray.empty() |> ReadOnlyArray.last |> Expect.toBeEqualToNoneOfInt
        }
      ),
      it(
        "lastOrRaise",
        () => {
          ReadOnlyArray.ofUnsafe([|1, 2, 3|])
          |> ReadOnlyArray.lastOrRaise
          |> Expect.toBeEqualToInt(3);
          (() => ReadOnlyArray.empty() |> ReadOnlyArray.lastOrRaise) |> Expect.shouldRaise
        }
      ),
      it("ofUnsafe", () => ()),
      it("reduce", () => ()),
      it("reduceReversed", () => ()),
      it("toIterable", () => ()),
      it("toIterableReversed", () => ()),
      it("toKeyedIterator", () => ()),
      it("toKeyedIteratorReversed", () => ()),
      it(
        "toMap",
        () => {
          let count = 12;
          let countDiv4 = count / 4;
          let map = ReadOnlyArray.init(count, (i) => i) |> ReadOnlyArray.toMap;
          map |> Map.count |> Expect.toBeEqualToInt(count);
          map |> Map.containsKey(0) |> Expect.toBeEqualToTrue;
          map |> Map.containsKey(-1) |> Expect.toBeEqualToFalse;
          map |> Map.containsKey(count) |> Expect.toBeEqualToFalse;
          map |> Map.get(0) |> Expect.toBeEqualToSomeOfInt(0);
          map |> Map.get(-1) |> Expect.toBeEqualToNoneOfInt;
          map |> Map.get(count) |> Expect.toBeEqualToNoneOfInt;
          map |> Map.getOrRaise(0) |> Expect.toBeEqualToInt(0);
          map |> Map.getOrRaise(count - 1) |> Expect.toBeEqualToInt(count - 1);
          (() => map |> Map.getOrRaise(-1)) |> Expect.shouldRaise;
          (() => map |> Map.getOrRaise(count)) |> Expect.shouldRaise;
          map
          |> Map.reduce(~while_=(_, k, _) => k < countDiv4, (acc, _, _) => 1 + acc, 0)
          |> Expect.toBeEqualToInt(countDiv4)
        }
      ),
      it("toSequence", () => ()),
      it("toSequenceReversed", () => ())
    ]
  );
