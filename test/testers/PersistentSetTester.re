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

let test (module PersistentSet: Set.Persistent.S with type a = int) (count: int) => {
  let countDiv2 = count / 2;
  let countDiv4 = count / 4;

  describe (sprintf "count: %i" count) [
    it "add" (fun () => {
      let values = IntRange.create start::(-countDiv2) count::count
        |> IntRange.toIterable
        |> Iterable.map Hashtbl.hash;

      let set = values
        |> Iterable.reduce (fun acc i => acc |> PersistentSet.add i) PersistentSet.empty;
      set |> PersistentSet.count |> Expect.toBeEqualToInt count;

      values |> Iterable.forEach (fun i => {
        set |> PersistentSet.contains i |> Expect.toBeEqualToTrue;
      });
    }),
    it "addAll" (fun () => {
      let values = IntRange.create start::(-countDiv2) count::count
        |> IntRange.toIterable
        |> Iterable.map Hashtbl.hash;

      let set = PersistentSet.empty |> PersistentSet.addAll values;
      set |> PersistentSet.count |> Expect.toBeEqualToInt count;

      values |> Iterable.forEach (fun i => {
        set |> PersistentSet.contains i |> Expect.toBeEqualToTrue;
      });
    }),
    it "contains" (fun () => {
      let range = IntRange.create start::0 count::count
        |> IntRange.toIterable
        |> PersistentSet.from;

      range |> PersistentSet.contains (-1) |> Expect.toBeEqualToFalse;
      range |> PersistentSet.contains countDiv4 |> Expect.toBeEqualToTrue;
      range |> PersistentSet.contains count |> Expect.toBeEqualToFalse;
    }),
    it "count" (fun () => {
      PersistentSet.empty |> PersistentSet.count |> Expect.toBeEqualToInt 0;
    }),
    it "equals" (fun () => {
      let set1 = IntRange.create start::0 count::count
        |> IntRange.toIterable
        |> PersistentSet.from;

      PersistentSet.equals set1 set1
        |> Expect.toBeEqualToTrue;

      let setEqualToPersistentSet1 = IntRange.create start::0 count::count
        |> IntRange.toIterable
        |> PersistentSet.from;

      PersistentSet.equals set1 setEqualToPersistentSet1
        |> Expect.toBeEqualToTrue;

      let setSameCountDifferentStartThanPersistentSet1 = IntRange.create start::(-countDiv4) count::count
        |> IntRange.toIterable
        |> PersistentSet.from;

      PersistentSet.equals set1 setSameCountDifferentStartThanPersistentSet1
        |> Expect.toBeEqualToFalse;

      let setDifferentCountSameStartAsPersistentSet1 = IntRange.create start::0 count::(count - 1)
        |> IntRange.toIterable
        |> PersistentSet.from;

      PersistentSet.equals set1 setDifferentCountSameStartAsPersistentSet1
        |> Expect.toBeEqualToFalse;
    }),
    it "from" (fun () => ()),
    it "intersect" (fun () => {
      let set1 = IntRange.create start::0 count::count
        |> IntRange.toIterable
        |> Iterable.map Hashtbl.hash
        |> PersistentSet.from;

      let set2 = IntRange.create start::countDiv4 count::count
        |> IntRange.toIterable
        |> Iterable.map Hashtbl.hash
        |> PersistentSet.from;

      PersistentSet.intersect set1 set2
        |> PersistentSet.equals (
            IntRange.create start::countDiv4 count::(count - countDiv4)
              |> IntRange.toIterable
              |> Iterable.map Hashtbl.hash
              |> PersistentSet.from
          )
        |> Expect.toBeEqualToTrue;
    }),
    it "isEmpty" (fun () => {
      PersistentSet.empty |> PersistentSet.isEmpty |> Expect.toBeEqualToTrue;

      IntRange.create start::(-countDiv2) count::count
       |> IntRange.toIterable
       |> Iterable.map Hashtbl.hash
       |> PersistentSet.from
       |> PersistentSet.isEmpty
       |> Expect.toBeEqualToFalse;
    }),
    it "isNotEmpty" (fun () => {
      PersistentSet.empty |> PersistentSet.isNotEmpty |> Expect.toBeEqualToFalse;

      IntRange.create start::(-countDiv2) count::count
       |> IntRange.toIterable
       |> Iterable.map Hashtbl.hash
       |> PersistentSet.from
       |> PersistentSet.isNotEmpty
       |> Expect.toBeEqualToTrue;
    }),
    it "reduce" (fun () => {
      IntRange.create start::0 count::count
        |> IntRange.toIterable
        |> PersistentSet.from
        |> PersistentSet.reduce while_::(fun acc _ => acc < countDiv2) (fun acc _ => 1 + acc) 0
        |> Expect.toBeEqualToInt countDiv2;
    }),
    it "remove" (fun () => {
      let set = IntRange.create start::0 count::count
        |> IntRange.toIterable
        |> PersistentSet.from;

      IntRange.create start::0 count::count
        |> IntRange.toIterable
        |> Iterable.reduce (fun acc i =>
            if (i mod 2 ===0) (acc |> PersistentSet.remove i)
            else acc
          ) set
        |> PersistentSet.toIterable
        |> Iterable.every (fun i => i mod 2 != 0)
        |> Expect.toBeEqualToTrue;
    }),
    it "removeAll" (fun () => {
      IntRange.create start::0 count::count
        |> IntRange.toIterable
        |> PersistentSet.from
        |> PersistentSet.removeAll
        |> PersistentSet.isEmpty
        |> Expect.toBeEqualToTrue;
    }),
    it "subtract" (fun () => {
      let set1 = IntRange.create start::0 count::count
        |> IntRange.toIterable
        |> Iterable.map Hashtbl.hash
        |> PersistentSet.from;
      let set2 = IntRange.create start::countDiv4 count::count
        |> IntRange.toIterable
        |> Iterable.map Hashtbl.hash
        |> PersistentSet.from;

      PersistentSet.subtract set1 set2
        |> PersistentSet.equals (
            IntRange.create start::0 count::countDiv4
              |> IntRange.toIterable
              |> Iterable.map Hashtbl.hash
              |> PersistentSet.from
          )
        |> Expect.toBeEqualToTrue;
    }),
    it "toIterable" (fun () => {
      IntRange.create start::0 count::count
        |> IntRange.toIterable
        |> PersistentSet.from
        |> PersistentSet.toIterable
        |> Iterable.reduce while_::(fun acc _ => acc < countDiv4) (fun acc _ => 1 + acc) 0
        |> Expect.toBeEqualToInt countDiv4;
    }),
    it "toMap" (fun () => ()),
    it "toSequence" (fun () => {
      IntRange.create start::0 count::count
        |> IntRange.toIterable
        |> PersistentSet.from
        |> PersistentSet.toSequence
        |> Sequence.reduce while_::(fun acc _ => acc < countDiv4) (fun acc _ => 1 + acc) 0
        |> Expect.toBeEqualToInt countDiv4;
    }),
    it "toSet" (fun () => {
      let set = IntRange.create start::0 count::count
        |> IntRange.toIterable
        |> PersistentSet.from
        |> PersistentSet.toSet;

      set |> Set.contains (-1) |> Expect.toBeEqualToFalse;
      set |> Set.contains countDiv4 |> Expect.toBeEqualToTrue;
      set |> Set.contains count |> Expect.toBeEqualToFalse;
      set |> Set.count |> Expect.toBeEqualToInt count;

      set
        |> Set.reduce while_::(fun acc _ => acc < countDiv4) (fun acc _ => 1 + acc) 0
        |> Expect.toBeEqualToInt countDiv4;

      set
        |> Set.toSequence
        |> Sequence.reduce while_::(fun acc _ => acc < countDiv4) (fun acc _ => 1 + acc) 0
        |> Expect.toBeEqualToInt countDiv4;
    }),
    it "union" (fun () => {
      let set1 = IntRange.create start::0 count::count
        |> IntRange.toIterable
        |> Iterable.map Hashtbl.hash
        |> PersistentSet.from;

      let set2 = IntRange.create start::countDiv4 count::count
        |> IntRange.toIterable
        |> Iterable.map Hashtbl.hash
        |> PersistentSet.from;

      PersistentSet.union set1 set2
        |> PersistentSet.equals (
            IntRange.create start::0 count::(count + countDiv4)
              |> IntRange.toIterable
              |> Iterable.map Hashtbl.hash
              |> PersistentSet.from
          )
        |> Expect.toBeEqualToTrue;
    }),
  ];
};
