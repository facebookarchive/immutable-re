/***
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
module CamlMap = Map;

open Immutable;

open Printf;

open ReUnit;

open ReUnit.Test;

let hash = Hashtbl.hash;

let generateTests =
    (
      getTestData: unit => 'map,
      keys: unit => Iterable.t(int),
      empty: unit => 'map,
      put: (int, int, 'map) => 'map,
      remove: (int, 'map) => 'map,
      get: (int, 'map) => int,
      n: int
    )
    : list(Test.t) => [
  it(
    sprintf("put %i elements", n),
    () =>
      IntRange.create(~start=0, ~count=n)
      |> IntRange.reduce((acc, i) => acc |> put(hash(i), i), empty())
      |> ignore
  ),
  it(
    sprintf("map with %i elements, remove %i elements", n, n),
    () => {
      let map = getTestData();
      let keysToRemove = keys();
      keysToRemove |> Iterable.reduce((acc, i) => acc |> remove(i), map) |> ignore
    }
  ),
  it(
    sprintf("map with %i elements, update %i elements", n, n),
    () => {
      let map = getTestData();
      let keysToUpdate = keys();
      /* Multiply the updated value to avoid optimizations */
      keysToUpdate |> Iterable.reduce((acc, i) => acc |> put(i, i + 1), map) |> ignore
    }
  ),
  it(
    sprintf("get %i values", n),
    () => {
      let map = getTestData();
      keys() |> Iterable.forEach((i) => map |> get(i) |> ignore)
    }
  )
];

module CamlIntMap =
  CamlMap.Make(
    {
      type t = int;
      let compare = (this: int, that: int) : int =>
        if (this < that) {
          (-1)
        } else if (this > that) {
          1
        } else {
          0
        };
    }
  );

module SortedIntMap =
  SortedMap.Make1(
    {
      type t = int;
      let compare = Comparator.int;
      let equals = Equality.int;
    }
  );

let test = (n: int, count: int) : Test.t => {
  let keys = IntRange.create(~start=0, ~count) |> IntRange.toIterable |> Iterable.map(hash);
  let camlIntMap =
    keys |> Iterable.reduce((acc, i) => acc |> CamlIntMap.add(i, i), CamlIntMap.empty);
  let hashMapEmpty = HashMap.emptyWith(~hash=(i) => i, ~comparator=Comparator.int);
  let (<|) = (f: 'a => 'b, a: 'a) : 'b => f(a);
  let hashMap = keys |> Iterable.map((i) => (i, i)) |> HashMap.putAllEntries <| hashMapEmpty;
  let intMap = keys |> Iterable.map((i) => (i, i)) |> IntMap.fromEntries;
  let sortedMap = keys |> Iterable.map((i) => (i, i)) |> SortedIntMap.fromEntries;
  let immJSMapEmpty = ImmJSRe.Map.fromJS([||]);
  let immJSMap = keys |> Iterable.reduce((acc, i) => acc |> ImmJSRe.Map.set(i, i), immJSMapEmpty);
  let testGroup = [
    describe(
      "CamlIntMap",
      generateTests(
        () => camlIntMap,
        () => keys,
        () => CamlIntMap.empty,
        CamlIntMap.add,
        CamlIntMap.remove,
        (k, map) => CamlIntMap.find(k, map),
        count
      )
    ),
    describe(
      "SortedIntMap",
      generateTests(
        () => sortedMap,
        () => keys,
        () => SortedIntMap.empty(),
        SortedIntMap.put,
        SortedIntMap.remove,
        SortedIntMap.getOrRaise,
        count
      )
    ),
    describe(
      "HashMap",
      generateTests(
        () => hashMap,
        () => keys,
        () => hashMapEmpty,
        HashMap.put,
        HashMap.remove,
        HashMap.getOrRaise,
        count
      )
    ),
    describe(
      "HashMap.Transient",
      generateTests(
        () => hashMap |> HashMap.mutate,
        () => keys,
        () => hashMapEmpty |> HashMap.mutate,
        HashMap.Transient.put,
        HashMap.Transient.remove,
        HashMap.Transient.getOrRaise,
        count
      )
    ),
    describe(
      "IntMap",
      generateTests(
        () => intMap,
        () => keys,
        () => IntMap.empty(),
        IntMap.put,
        IntMap.remove,
        IntMap.getOrRaise,
        count
      )
    ),
    describe(
      "IntMap.Transient",
      generateTests(
        () => intMap |> IntMap.mutate,
        () => keys,
        IntMap.Transient.empty,
        IntMap.Transient.put,
        IntMap.Transient.remove,
        IntMap.Transient.getOrRaise,
        count
      )
    ),
    describe(
      "ImmJSMap",
      generateTests(
        () => immJSMap,
        () => keys,
        () => immJSMapEmpty,
        ImmJSRe.Map.set,
        ImmJSRe.Map.delete,
        ImmJSRe.Map.get,
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
  describe(sprintf("MapPerf"), tests)
};
