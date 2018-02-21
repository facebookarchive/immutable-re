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

let (>>) = (f1: 'a => 'b, f2: 'b => 'c, a: 'a) => f2(f1(a));

module SortedIntMap =
  SortedMap.Make1(
    {
      type t = int;
      let compare = Comparator.int;
      let equals = Equality.int;
    }
  );

let emptyHashIntMap = () => HashMap.emptyWith(~hash=(i) => i, ~comparator=Comparator.int);

let expectToBeEqualToIntPair =
  Expect.toBeEqualToWith(
    ~equals=((a, b), (c, d)) => a === c && b === d,
    ~toString=((a, b)) => "(" ++ string_of_int(a) ++ ", " ++ string_of_int(b) ++ ")"
  );

let expectToBeEqualToSomeOfIntPair =
  Expect.toBeEqualToSomeWith(
    ~equals=((a, b), (c, d)) => a === c && b === d,
    ~toString=((a, b)) => "(" ++ string_of_int(a) ++ ", " ++ string_of_int(b) ++ ")"
  );

let expectToBeEqualToNoneOfIntPair =
  Expect.toBeEqualToNoneWith(
    ~toString=((a, b)) => "(" ++ string_of_int(a) ++ ", " ++ string_of_int(b) ++ ")"
  );

let test =
  describe(
    "KeyedIterable",
    [
      it(
        "concat",
        () =>
          KeyedIterable.concat([
            IntRange.create(~start=0, ~count=2)
            |> IntRange.toIterable
            |> Iterable.map((i) => ((i, i): (int, int)))
            |> KeyedIterable.fromEntries,
            IntRange.create(~start=2, ~count=2)
            |> IntRange.toIterable
            |> Iterable.map((i) => ((i, i): (int, int)))
            |> KeyedIterable.fromEntries,
            IntRange.create(~start=4, ~count=2)
            |> IntRange.toIterable
            |> Iterable.map((i) => ((i, i): (int, int)))
            |> KeyedIterable.fromEntries
          ])
          |> KeyedIterable.take(5)
          |> KeyedIterable.keys
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([4, 3, 2, 1, 0])
      ),
      it("defer", () => ()),
      it(
        "distinctUntilChangedWith",
        () =>
          KeyedIterable.concat([
            KeyedIterable.return(1, 1),
            KeyedIterable.return(1, 1),
            KeyedIterable.return(1, 1),
            KeyedIterable.return(2, 2),
            KeyedIterable.return(2, 2),
            KeyedIterable.return(2, 2),
            KeyedIterable.return(3, 3),
            KeyedIterable.return(3, 3),
            KeyedIterable.return(3, 3),
            KeyedIterable.return(4, 4),
            KeyedIterable.return(4, 4),
            KeyedIterable.return(4, 4)
          ])
          |> KeyedIterable.distinctUntilChangedWith(
               ~keyEquals=Equality.int,
               ~valueEquals=Equality.int
             )
          |> KeyedIterable.values
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([4, 3, 2, 1])
      ),
      it(
        "doOnNext",
        () => {
          let last = ref(0);
          KeyedIterable.concat([
            KeyedIterable.return(0, 0),
            KeyedIterable.return(1, 1),
            KeyedIterable.return(2, 2),
            KeyedIterable.return(3, 3),
            KeyedIterable.return(4, 4)
          ])
          |> KeyedIterable.doOnNext((k, _) => last := k)
          |> KeyedIterable.forEach((_, _) => ());
          last^ |> Expect.toBeEqualToInt(4);
          let empty = KeyedIterable.empty();
          Pervasives.(===)(KeyedIterable.doOnNext((_, _) => (), empty), empty)
          |> Expect.toBeEqualToTrue
        }
      ),
      it(
        "filter",
        () => {
          KeyedIterable.concat([
            KeyedIterable.return(0, 0),
            KeyedIterable.return(1, 1),
            KeyedIterable.return(2, 2),
            KeyedIterable.return(3, 3),
            KeyedIterable.return(4, 4)
          ])
          |> KeyedIterable.filter((k, _) => k mod 2 === 0)
          |> KeyedIterable.keys
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([4, 2, 0]);
          let empty = KeyedIterable.empty();
          Pervasives.(===)(KeyedIterable.filter((_, _) => true, empty), empty)
          |> Expect.toBeEqualToTrue
        }
      ),
      it(
        "flatMap",
        () => {
          let flatMapped =
            KeyedIterable.concat([KeyedIterable.return(0, 0), KeyedIterable.return(1, 1)])
            |> KeyedIterable.flatMap(
                 (k, _) =>
                   KeyedIterable.concat([
                     KeyedIterable.return(k, 0),
                     KeyedIterable.return(k, 1),
                     KeyedIterable.return(k, 2)
                   ])
               );
          flatMapped
          |> KeyedIterable.keys
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([1, 1, 1, 0, 0, 0]);
          flatMapped
          |> KeyedIterable.values
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([2, 1, 0, 2, 1, 0])
        }
      ),
      it("generate", () => ()),
      it("keys", () => ()),
      it(
        "map",
        () => {
          let mapped =
            KeyedIterable.generate(~genKey=(k, _) => k, ~genValue=(_, v) => v, 2, 3)
            |> KeyedIterable.take(5)
            |> KeyedIterable.map(~keyMapper=(_, v) => v, ~valueMapper=(k, _) => k);
          KeyedIterable.keys(mapped)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([3, 3, 3, 3, 3]);
          KeyedIterable.values(mapped)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([2, 2, 2, 2, 2])
        }
      ),
      it(
        "mapKeys",
        () => {
          KeyedIterable.generate(~genKey=(k, _) => k, ~genValue=(_, v) => v, 2, 3)
          |> KeyedIterable.take(5)
          |> KeyedIterable.mapKeys((_, _) => 4)
          |> KeyedIterable.keys
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([4, 4, 4, 4, 4]);
          KeyedIterable.generate(~genKey=(k, _) => k, ~genValue=(_, v) => v, 2, 3)
          |> KeyedIterable.take(5)
          |> KeyedIterable.mapKeys((_, _) => 4)
          |> KeyedIterable.values
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([3, 3, 3, 3, 3])
        }
      ),
      it(
        "mapValues",
        () => {
          KeyedIterable.generate(~genKey=(k, _) => k, ~genValue=(_, v) => v, 2, 3)
          |> KeyedIterable.take(5)
          |> KeyedIterable.mapValues((_, _) => 4)
          |> KeyedIterable.values
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([4, 4, 4, 4, 4]);
          KeyedIterable.generate(~genKey=(k, _) => k, ~genValue=(_, v) => v, 2, 3)
          |> KeyedIterable.take(5)
          |> KeyedIterable.mapKeys((_, _) => 4)
          |> KeyedIterable.values
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([3, 3, 3, 3, 3])
        }
      ),
      it(
        "repeat",
        () =>
          KeyedIterable.generate(~genKey=(k, _) => k, ~genValue=(_, v) => v, 2, 3)
          |> KeyedIterable.take(10)
          |> KeyedIterable.doOnNext(
               (k, v) => {
                 k |> Expect.toBeEqualToInt(2);
                 v |> Expect.toBeEqualToInt(3)
               }
             )
          |> KeyedIterable.reduce((acc, _, _) => acc + 5, 0)
          |> Expect.toBeEqualToInt(50)
      ),
      it(
        "return",
        () => {
          KeyedIterable.return(1, 2)
          |> KeyedIterable.reduce((acc, k, v) => acc + k + v, 0)
          |> Expect.toBeEqualToInt(3);
          KeyedIterable.return(1, 2)
          |> KeyedIterable.reduce(~while_=(_, k, _) => k < 0, (acc, k, v) => acc + k + v, 0)
          |> Expect.toBeEqualToInt(0)
        }
      ),
      it(
        "scan",
        () => {
          KeyedIterable.empty()
          |> KeyedIterable.scan((_, k, v) => k + v, 1)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([1]);
          KeyedIterable.generate(~genKey=(k, _) => k, ~genValue=(_, v) => v, 2, 3)
          |> KeyedIterable.take(1)
          |> KeyedIterable.scan((_, k, v) => k + v, 1)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([5, 1]);
          KeyedIterable.generate(~genKey=(k, _) => k, ~genValue=(_, v) => v, 2, 3)
          |> KeyedIterable.scan((_, k, v) => k + v, 0)
          |> Iterable.take(5)
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([5, 5, 5, 5, 0])
        }
      ),
      it(
        "skip",
        () => {
          KeyedIterable.concat([
            KeyedIterable.generate(~genKey=(k, _) => k, ~genValue=(_, v) => v, 2, 2)
            |> KeyedIterable.take(2),
            KeyedIterable.generate(~genKey=(k, _) => k, ~genValue=(_, v) => v, 3, 3)
            |> KeyedIterable.take(5)
          ])
          |> KeyedIterable.skip(2)
          |> KeyedIterable.keys
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([3, 3, 3, 3, 3]);
          let empty = KeyedIterable.empty();
          Pervasives.(===)(KeyedIterable.skip(5, empty), empty) |> Expect.toBeEqualToTrue
        }
      ),
      it(
        "skipWhile",
        () => {
          KeyedIterable.concat([
            KeyedIterable.generate(~genKey=(k, _) => k, ~genValue=(_, v) => v, 2, 2)
            |> KeyedIterable.take(2),
            KeyedIterable.generate(~genKey=(k, _) => k, ~genValue=(_, v) => v, 3, 3)
            |> KeyedIterable.take(5)
          ])
          |> KeyedIterable.skipWhile((k, _) => k < 3)
          |> KeyedIterable.keys
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([3, 3, 3, 3, 3]);
          let empty = KeyedIterable.empty();
          Pervasives.(===)(KeyedIterable.skipWhile((_, _) => true, empty), empty)
          |> Expect.toBeEqualToTrue
        }
      ),
      it(
        "startWith",
        () =>
          KeyedIterable.generate(~genKey=(k, _) => k, ~genValue=(_, v) => v, 3, 3)
          |> KeyedIterable.startWith(1, 1)
          |> KeyedIterable.take(3)
          |> KeyedIterable.keys
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([3, 3, 1])
      ),
      it(
        "take",
        () => {
          KeyedIterable.generate(~genKey=(k, _) => k, ~genValue=(_, v) => v, 3, 3)
          |> KeyedIterable.take(3)
          |> KeyedIterable.values
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([3, 3, 3]);
          let empty = KeyedIterable.empty();
          Pervasives.(===)(KeyedIterable.take(5, empty), empty) |> Expect.toBeEqualToTrue
        }
      ),
      it(
        "takeWhile",
        () => {
          KeyedIterable.concat([
            KeyedIterable.generate(~genKey=(k, _) => k, ~genValue=(_, v) => v, 2, 2)
            |> KeyedIterable.take(2),
            KeyedIterable.generate(~genKey=(k, _) => k, ~genValue=(_, v) => v, 3, 3)
            |> KeyedIterable.take(2)
          ])
          |> KeyedIterable.takeWhile((k, _) => k < 3)
          |> KeyedIterable.keys
          |> List.fromReverse
          |> Expect.toBeEqualToListOfInt([2, 2]);
          let empty = KeyedIterable.empty();
          Pervasives.(===)(KeyedIterable.takeWhile((_, _) => true, empty), empty)
          |> Expect.toBeEqualToTrue
        }
      ),
      it("values", () => ()),
      it(
        "every",
        () => {
          KeyedIterable.empty() |> KeyedIterable.every((_, _) => false) |> Expect.toBeEqualToTrue;
          IntRange.create(~start=0, ~count=5)
          |> IntRange.reduce((acc, i) => acc |> HashMap.put(i, i), emptyHashIntMap())
          |> HashMap.toKeyedIterable
          |> KeyedIterable.every((k, v) => k >= 0 && v >= 0)
          |> Expect.toBeEqualToTrue;
          IntRange.create(~start=0, ~count=5)
          |> IntRange.reduce((acc, i) => acc |> HashMap.put(i, i), emptyHashIntMap())
          |> HashMap.toKeyedIterable
          |> KeyedIterable.every((k, v) => k < 3 && v < 3)
          |> Expect.toBeEqualToFalse
        }
      ),
      it(
        "find",
        () => {
          let findPair = KeyedIterable.find(~selector=(k, v) => (k, v));
          IntRange.create(~start=0, ~count=5)
          |> IntRange.reduce((acc, i) => acc |> HashMap.put(i, i), emptyHashIntMap())
          |> HashMap.toKeyedIterable
          |> findPair((k, v) => k === 2 && v === 2)
          |> expectToBeEqualToSomeOfIntPair((2, 2));
          KeyedIterable.empty()
          |> KeyedIterable.find(~selector=(k, v) => (k, v), (k, v) => k === 2 && v === 2)
          |> expectToBeEqualToNoneOfIntPair;
          IntRange.create(~start=0, ~count=5)
          |> IntRange.reduce((acc, i) => acc |> HashMap.put(i, i), emptyHashIntMap())
          |> HashMap.toKeyedIterable
          |> findPair((k, v) => k === 5 && v === 5)
          |> expectToBeEqualToNoneOfIntPair
        }
      ),
      it(
        "findOrRaise",
        () => {
          let findPairOrRaise = KeyedIterable.findOrRaise(~selector=(k, v) => (k, v));
          IntRange.create(~start=0, ~count=5)
          |> IntRange.reduce((acc, i) => acc |> HashMap.put(i, i), emptyHashIntMap())
          |> HashMap.toKeyedIterable
          |> findPairOrRaise((k, v) => k === 2 && v === 2)
          |> expectToBeEqualToIntPair((2, 2));
          (() => KeyedIterable.empty() |> findPairOrRaise((k, v) => k === 2 && v === 2))
          |> Expect.shouldRaise;
          (
            () =>
              IntRange.create(~start=0, ~count=5)
              |> IntRange.reduce((acc, i) => acc |> HashMap.put(i, i), emptyHashIntMap())
              |> HashMap.toKeyedIterable
              |> findPairOrRaise((k, v) => k === 5 && v === 5)
          )
          |> Expect.shouldRaise
        }
      ),
      it(
        "forEach",
        () => {
          let last = ref(0);
          IntRange.create(~start=0, ~count=5)
          |> IntRange.reduce((acc, i) => acc |> HashMap.put(i, i), emptyHashIntMap())
          |> HashMap.toKeyedIterable
          |> KeyedIterable.forEach(~while_=(k, _) => k < 3, (k, _) => last := k);
          last^ |> Expect.toBeEqualToInt(2)
        }
      ),
      it(
        "none",
        () => {
          KeyedIterable.empty() |> KeyedIterable.none((_, _) => false) |> Expect.toBeEqualToTrue;
          IntRange.create(~start=0, ~count=5)
          |> IntRange.reduce((acc, i) => acc |> HashMap.put(i, i), emptyHashIntMap())
          |> HashMap.toKeyedIterable
          |> KeyedIterable.none((k, v) => k >= 5 && v >= 5)
          |> Expect.toBeEqualToTrue;
          IntRange.create(~start=0, ~count=5)
          |> IntRange.reduce((acc, i) => acc |> HashMap.put(i, i), emptyHashIntMap())
          |> HashMap.toKeyedIterable
          |> KeyedIterable.none((k, v) => k < 3 && v < 3)
          |> Expect.toBeEqualToFalse
        }
      ),
      it(
        "some",
        () => {
          KeyedIterable.empty() |> KeyedIterable.some((_, _) => false) |> Expect.toBeEqualToFalse;
          IntRange.create(~start=0, ~count=5)
          |> IntRange.reduce((acc, i) => acc |> SortedIntMap.put(i, i), SortedIntMap.empty())
          |> SortedIntMap.toKeyedIterable
          |> KeyedIterable.some((k, v) => k >= 5 && v >= 5)
          |> Expect.toBeEqualToFalse;
          IntRange.create(~start=0, ~count=5)
          |> IntRange.reduce((acc, i) => acc |> SortedIntMap.put(i, i), SortedIntMap.empty())
          |> SortedIntMap.toKeyedIterable
          |> KeyedIterable.some((k, v) => k < 3 && k > 1 && v < 3 && v > 1)
          |> Expect.toBeEqualToTrue
        }
      )
    ]
  );
