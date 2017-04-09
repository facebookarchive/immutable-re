/**
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

let test (module PersistentMap: Map.Persistent.S1 with type k = int) (count: int) => {
  let countDiv2 = count / 2;
  let countDiv4 = count / 4;

  let hash = Hashtbl.hash;

  let keyValuePairs = IntRange.create start::0 count::count
    |> IntRange.toIterable
    |> Iterable.map (fun i => (i, i));
  let map = PersistentMap.fromEntries keyValuePairs;

  let keyValuePairsHashed = keyValuePairs |> Iterable.map (fun (k, v) => (hash k, v));
  let mapHashed = PersistentMap.fromEntries keyValuePairsHashed;

  describe (sprintf "count: %i" count) [
    it "alter" (fun () => {
      mapHashed
        |> PersistentMap.reduce (fun acc k _ => {
            if (k mod 2 === 0) (acc |> PersistentMap.alter k (fun _ => Some k))
            else (acc |> PersistentMap.alter k (fun _ => None))
          }) mapHashed
        |> PersistentMap.toKeyedIterable
        |> KeyedIterable.forEach (fun k v => {
            k === v |> Expect.toBeEqualToTrue;
            k mod 2 === 0 |> Expect.toBeEqualToTrue;
          });
    }),
    it "containsKey" (fun () => {
      keyValuePairsHashed |> Iterable.forEach (fun (k, _) => {
        mapHashed |> PersistentMap.containsKey k |> Expect.toBeEqualToTrue
      });

      map |> PersistentMap.containsKey (-1) |> Expect.toBeEqualToFalse;
      map |> PersistentMap.containsKey count |> Expect.toBeEqualToFalse;
    }),
    it "count" (fun () => {
      PersistentMap.empty ()
        |> PersistentMap.count
        |> Expect.toBeEqualToInt 0;

      mapHashed
        |> PersistentMap.count
        |> Expect.toBeEqualToInt count;
    }),
    it "from" (fun () => ()),
    it "get" (fun () => {
      keyValuePairsHashed |> Iterable.forEach (fun (k, v) => {
        mapHashed |> PersistentMap.get k |> Expect.toBeEqualToSomeOfInt v;
      });

      map |> PersistentMap.get (-1) |> Expect.toBeEqualToNoneOfInt;
      map |> PersistentMap.get count |> Expect.toBeEqualToNoneOfInt;
    }),
    it "getOrRaise" (fun () => {
      keyValuePairsHashed |> Iterable.forEach (fun (k, v) => {
        mapHashed |> PersistentMap.getOrRaise k |> Expect.toBeEqualToInt v;
      });

      (fun () => map |> PersistentMap.getOrRaise (-1)) |> Expect.shouldRaise;
      (fun () => map |> PersistentMap.getOrRaise count) |> Expect.shouldRaise;
    }),
    it "isEmpty" (fun () => {
      PersistentMap.empty () |> PersistentMap.isEmpty |> Expect.toBeEqualToTrue;
      mapHashed |> PersistentMap.isEmpty |> Expect.toBeEqualToFalse;
    }),
    it "isNotEmpty" (fun () => {
      PersistentMap.empty () |> PersistentMap.isNotEmpty |> Expect.toBeEqualToFalse;
      mapHashed |> PersistentMap.isNotEmpty |> Expect.toBeEqualToTrue;
    }),
    it "keySet" (fun () => {
      let keySet = mapHashed |> PersistentMap.keySet;

      keyValuePairsHashed |> Iterable.forEach (fun (i, _) => {
        keySet |> Set.contains i |> Expect.toBeEqualToTrue;
      });

      map |> PersistentMap.keySet |> Set.contains (-1) |> Expect.toBeEqualToFalse;
      map |> PersistentMap.keySet |> Set.contains count |> Expect.toBeEqualToFalse;
    }),
    it "merge" (fun () => {
      let acc = IntRange.create start::0 count::countDiv2
        |> IntRange.toIterable
        |> Iterable.map (fun i => (i, i))
        |> PersistentMap.fromEntries;

      let next = IntRange.create start::countDiv2 count::countDiv2
        |> IntRange.toIterable
        |> Iterable.map (fun i => (i, i))
        |> PersistentMap.fromEntries;

      let merged = PersistentMap.merge
        (fun _ vAcc vNext => switch (vAcc, vNext) {
          | (Some _, None) => None
          | (None, Some _) => vNext
          | _ => None
        })
        acc
        next;

      IntRange.create start::0 count::countDiv2
        |> IntRange.toIterable
        |> Iterable.forEach (
        fun i => merged |> PersistentMap.get i |> Expect.toBeEqualToNoneOfInt
      );

      IntRange.create start::countDiv2 count::countDiv2
        |> IntRange.toIterable
        |> Iterable.forEach (
        fun i => merged |> PersistentMap.get i |> Expect.toBeEqualToSomeOfInt i
      );
    }),
    it "put" (fun () => {
      let map = keyValuePairsHashed
        |> Iterable.reduce (fun acc (k, v) => {
            acc |> PersistentMap.containsKey k |> Expect.toBeEqualToFalse;
            let acc = acc |> PersistentMap.put k v;
            acc |> PersistentMap.getOrRaise k |> Expect.toBeEqualToInt v;
            acc
          })
          (PersistentMap.empty ());

      map |> PersistentMap.keys |> Iterable.forEach (fun k => {
        map |> PersistentMap.containsKey k |> Expect.toBeEqualToTrue;
      });
    }),
    it "putAll" (fun () => {
      let map = PersistentMap.empty ();
      keyValuePairsHashed |> Iterable.forEach (fun (k, _) => {
        map |> PersistentMap.containsKey k |> Expect.toBeEqualToFalse;
      });

      let map = PersistentMap.empty () |> PersistentMap.putAllEntries keyValuePairsHashed;
      map |> PersistentMap.keys |> Iterable.forEach (fun k => {
        map |> PersistentMap.containsKey k |> Expect.toBeEqualToTrue;
      });
    }),
    it "reduce" (fun () => {
      ()
    }),
    it "remove" (fun () => {
      let mapWithOddKeys = IntRange.create start::0 count::countDiv2
        |> IntRange.toIterable
        |> Iterable.map (fun i => i * 2)
        |> Iterable.map hash
        |> Iterable.reduce (fun acc k => acc |> PersistentMap.remove k) mapHashed;

      IntRange.create start::0 count::countDiv2
        |> IntRange.toIterable
        |> Iterable.map (fun i => i * 2)
        |> Iterable.map hash
        |> Iterable.forEach (fun i => {
              mapWithOddKeys |> PersistentMap.containsKey i |> Expect.toBeEqualToFalse;
          });

      IntRange.create start::0 count::countDiv2
        |> IntRange.toIterable
        |> Iterable.map (fun i => i * 2 + 1)
        |> Iterable.map hash
        |> Iterable.forEach (fun i => {
              mapWithOddKeys |> PersistentMap.containsKey i |> Expect.toBeEqualToTrue;
          });
    }),
    it "removeAll" (fun () => {
      mapHashed
        |> PersistentMap.removeAll
        |> PersistentMap.isEmpty
        |> Expect.toBeEqualToTrue;
    }),
    it "toIterable" (fun () => {
      PersistentMap.toIterable map
        |> Iterable.reduce (fun acc (k, v) => {
            k === v |> Expect.toBeEqualToTrue;
            acc + 1;
          }) 0
        |> Expect.toBeEqualToInt (PersistentMap.count mapHashed);
    }),
    it "toKeyedIterable" (fun () => {
      PersistentMap.toKeyedIterable map
        |> KeyedIterable.reduce (fun acc k v => {
            k === v |> Expect.toBeEqualToTrue;
            acc + 1;
          }) 0
        |> Expect.toBeEqualToInt (PersistentMap.count mapHashed);
    }),
    it "toMap" (fun () => {
      let asMap = PersistentMap.toMap map;

      asMap |> Map.count |> Expect.toBeEqualToInt count;

      asMap |> Map.containsKey 0 |> Expect.toBeEqualToTrue;
      asMap |> Map.containsKey countDiv4 |> Expect.toBeEqualToTrue;
      asMap |> Map.containsKey countDiv2 |> Expect.toBeEqualToTrue;
      asMap |> Map.containsKey (count - countDiv4) |> Expect.toBeEqualToTrue;
      asMap |> Map.containsKey (count - 1) |> Expect.toBeEqualToTrue;
      asMap |> Map.containsKey (-1) |> Expect.toBeEqualToFalse;
      asMap |> Map.containsKey count |> Expect.toBeEqualToFalse;

      asMap |> Map.get 0 |> Expect.toBeEqualToSomeOfInt 0;
      asMap |> Map.get countDiv4 |> Expect.toBeEqualToSomeOfInt countDiv4;
      asMap |> Map.get countDiv2 |> Expect.toBeEqualToSomeOfInt countDiv2;
      asMap |> Map.get (count - countDiv4) |> Expect.toBeEqualToSomeOfInt (count - countDiv4);
      asMap |> Map.get (count - 1) |> Expect.toBeEqualToSomeOfInt (count - 1);
      asMap |> Map.get (-1) |> Expect.toBeEqualToNoneOfInt;
      asMap |> Map.get count |> Expect.toBeEqualToNoneOfInt;

      asMap |> Map.getOrRaise 0 |> Expect.toBeEqualToInt 0;
      asMap |> Map.getOrRaise countDiv4 |> Expect.toBeEqualToInt countDiv4;
      asMap |> Map.getOrRaise countDiv2 |> Expect.toBeEqualToInt countDiv2;
      asMap |> Map.getOrRaise (count - countDiv4) |> Expect.toBeEqualToInt (count - countDiv4);
      asMap |> Map.getOrRaise (count - 1) |> Expect.toBeEqualToInt (count - 1);
      (fun () => asMap |> Map.getOrRaise (-1)) |> Expect.shouldRaise;
      (fun () => asMap |> Map.getOrRaise count) |> Expect.shouldRaise;

      asMap
        |> Map.reduce while_::(fun acc _ _ => acc < countDiv4 ) (fun acc _ _ => 1 + acc) 0
        |> Expect.toBeEqualToInt countDiv4;
    }),
    it "toSequence" (fun () => {
      PersistentMap.toSequence map
        |> Sequence.reduce (fun acc (k, v) => {
            map |> PersistentMap.getOrRaise k |> Expect.toBeEqualToInt v;
            acc + 1;
          }) 0
        |> Expect.toBeEqualToInt (PersistentMap.count mapHashed);
    }),
  ];
};
