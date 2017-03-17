/**
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
open ReUnit.Expect;
open ReUnit.Test;

module type Deque = {
  type t 'a;

  let addFirst: 'a => (t 'a) => (t 'a);
  let addFirstAll: (Iterator.t 'a) => (t 'a) => (t 'a);
  let addLast: 'a => (t 'a) => (t 'a);
  let addLastAll: (Iterator.t 'a) => (t 'a) => (t 'a);
  let compare: (Comparator.t (t 'a));
  let compareWith: (Comparator.t 'a) => (Comparator.t (t 'a));
  let contains: 'a => (t 'a) => bool;
  let containsWith: (Equality.t 'a) => 'a => (t 'a) => bool;
  let count: (t 'a) => int;
  let empty: (t 'a);
  let equals: (Equality.t (t 'a));
  let equalsWith: (Equality.t 'a) => (Equality.t (t 'a));
  let every: ('a => bool) => (t 'a) => bool;
  let find: ('a => bool) => (t 'a) => 'a;
  let first: (t 'a) => 'a;
  let forEach: ('a => unit) => (t 'a) => unit;
  let forEachReverse: ('a => unit) => (t 'a) => unit;
  let from: (Iterator.t 'a) => (t 'a);
  let fromReversed: (Iterator.t 'a) => (t 'a);
  let hash: (Hash.t (t 'a));
  let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
  let isEmpty: t 'a => bool;
  let isNotEmpty: t 'a => bool;
  let last: (t 'a) => 'a;
  let map: ('a => 'b) => (t 'a) => (t 'b);
  let mapReverse: ('a => 'b) => (t 'a) => (t 'b);
  let none: ('a => bool) => (t 'a) => bool;
  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let reduceRight: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let removeAll: (t 'a) => (t 'a);
  let removeFirst: (t 'a) => (t 'a);
  let removeLast: (t 'a) => (t 'a);
  let return: 'a => (t 'a);
  let reverse: (t 'a) => (t 'a);
  let some: ('a => bool) => (t 'a) => bool;
  let toSequence: (t 'a) => (Sequence.t 'a);
  let toSequenceReversed: (t 'a) => (Sequence.t 'a);
  let tryFind: ('a => bool) => (t 'a) => (option 'a);
  let tryFirst: (t 'a) => (option 'a);
  let tryLast: (t 'a) => option 'a;
};

let test (count: int) (module Deque: Deque): (list Test.t) => {
  let stackTests = StackTester.test count (module {
    type t 'a = Deque.t 'a;

    let addFirst = Deque.addFirst;
    let addFirstAll = Deque.addFirstAll;
    let compare = Deque.compare;
    let compareWith = Deque.compareWith;
    let contains = Deque.contains;
    let containsWith = Deque.containsWith;
    let count = Deque.count;
    let empty = Deque.empty;
    let equals = Deque.equals;
    let equalsWith = Deque.equalsWith;
    let every = Deque.every;
    let find = Deque.find;
    let first = Deque.first;
    let forEach = Deque.forEach;
    let fromReversed = Deque.fromReversed;
    let hash = Deque.hash;
    let hashWith = Deque.hashWith;
    let isEmpty = Deque.isEmpty;
    let isNotEmpty = Deque.isNotEmpty;
    let mapReverse = Deque.mapReverse;
    let none = Deque.none;
    let reduce = Deque.reduce;
    let removeAll = Deque.removeAll;
    let removeFirst = Deque.removeFirst;
    let return = Deque.return;
    let reverse = Deque.reverse;
    let some = Deque.some;
    let toSequence = Deque.toSequence;
    let tryFind = Deque.tryFind;
    let tryFirst = Deque.tryFirst;
  });

  [
    it (sprintf "addLast and removeLast %i elements" count) (fun () => {
      let empty = Deque.empty;

      expect (Deque.isNotEmpty empty) |> toBeEqualToFalse;
      expect (Deque.isEmpty empty) |> toBeEqualToTrue;
      expect (Deque.count empty) |> toBeEqualToInt 0;
      defer (fun () => empty |> Deque.first) |> throws;
      defer (fun () => empty |> Deque.last) |> throws;
      expect (empty |> Deque.tryFirst) |> toBeEqualToNoneOfInt;
      expect (empty |> Deque.tryLast) |> toBeEqualToNoneOfInt;

      let deque = IntRange.create 0 count |> IntRange.reduce (fun acc i => {
        let acc = acc |> Deque.addLast i;

        expect (Deque.isNotEmpty acc) |> toBeEqualToTrue;
        expect (Deque.isEmpty acc) |> toBeEqualToFalse;
        expect (Deque.count acc) |> toBeEqualToInt (i + 1);
        expect (acc |> Deque.first) |> toBeEqualToInt 0;
        expect (acc |> Deque.last) |> toBeEqualToInt i;
        expect (acc |> Deque.tryFirst) |> toBeEqualToSomeOfInt 0;
        expect (acc |> Deque.tryLast) |> toBeEqualToSomeOfInt i;

        acc;
      }) empty;

      let shouldBeEmpty = IntRange.create 0 count |> IntRange.reduceRight (fun acc i => {
        expect (Deque.isNotEmpty acc) |> toBeEqualToTrue;
        expect (Deque.isEmpty acc) |> toBeEqualToFalse;
        expect (Deque.count acc) |> toBeEqualToInt (i + 1);
        expect (acc |> Deque.first) |> toBeEqualToInt 0;
        expect (acc |> Deque.last) |> toBeEqualToInt i;
        expect (acc |> Deque.tryFirst) |> toBeEqualToSomeOfInt 0;
        expect (acc |> Deque.tryLast) |> toBeEqualToSomeOfInt i;
        acc |> Deque.removeLast;
      }) deque;

      expect (Deque.isNotEmpty shouldBeEmpty) |> toBeEqualToFalse;
      expect (Deque.isEmpty shouldBeEmpty) |> toBeEqualToTrue;
      expect (Deque.count shouldBeEmpty) |> toBeEqualToInt 0;
      defer (fun () => shouldBeEmpty |> Deque.first) |> throws;
      defer (fun () => shouldBeEmpty |> Deque.last) |> throws;
      expect (shouldBeEmpty |> Deque.tryFirst) |>  toBeEqualToNoneOfInt;
      expect (shouldBeEmpty |> Deque.tryLast) |>  toBeEqualToNoneOfInt;

      expect @@ Deque.toSequence @@ deque |> toBeEqualToSequenceOfInt (
        IntRange.create 0 count |> IntRange.toSequence
      );
      expect @@ Deque.toSequenceReversed @@ deque |> toBeEqualToSequenceOfInt (
        IntRange.create 0 count |> IntRange.toSequenceReversed
      );
    }),

    it (sprintf "addLast and removeFirst %i elements" count) (fun () => {
      let empty = Deque.empty;

      expect (Deque.isNotEmpty empty) |> toBeEqualToFalse;
      expect (Deque.isEmpty empty) |> toBeEqualToTrue;
      expect (Deque.count empty) |> toBeEqualToInt 0;
      defer (fun () => empty |> Deque.first) |> throws;
      defer (fun () => empty |> Deque.last) |> throws;
      expect (empty |> Deque.tryFirst) |> toBeEqualToNoneOfInt;
      expect (empty |> Deque.tryLast) |> toBeEqualToNoneOfInt;

      let deque = IntRange.create 0 count |> IntRange.reduce (fun acc i => {
        let acc = acc |> Deque.addLast i;

        expect (Deque.isNotEmpty acc) |> toBeEqualToTrue;
        expect (Deque.isEmpty acc) |> toBeEqualToFalse;
        expect (Deque.count acc) |> toBeEqualToInt (i + 1);
        expect (acc |> Deque.first) |> toBeEqualToInt 0;
        expect (acc |> Deque.last) |> toBeEqualToInt i;
        expect (acc |> Deque.tryFirst) |> toBeEqualToSomeOfInt 0;
        expect (acc |> Deque.tryLast) |> toBeEqualToSomeOfInt i;

        acc;
      }) empty;

      let shouldBeEmpty = IntRange.create 0 count |> IntRange.reduce (fun acc i => {
        expect (Deque.isNotEmpty acc) |> toBeEqualToTrue;
        expect (Deque.isEmpty acc) |> toBeEqualToFalse;
        expect (Deque.count acc) |> toBeEqualToInt (count - i);
        expect (acc |> Deque.first) |> toBeEqualToInt i;
        expect (acc |> Deque.last) |> toBeEqualToInt (count - 1);
        expect (acc |> Deque.tryFirst) |> toBeEqualToSomeOfInt i;
        expect (acc |> Deque.tryLast) |> toBeEqualToSomeOfInt (count - 1);
        acc |> Deque.removeFirst;
      }) deque;

      expect (Deque.isNotEmpty shouldBeEmpty) |> toBeEqualToFalse;
      expect (Deque.isEmpty shouldBeEmpty) |> toBeEqualToTrue;
      expect (Deque.count shouldBeEmpty) |> toBeEqualToInt 0;
      defer (fun () => shouldBeEmpty |> Deque.first) |> throws;
      defer (fun () => shouldBeEmpty |> Deque.last) |> throws;
      expect (shouldBeEmpty |> Deque.tryFirst) |> toBeEqualToNoneOfInt;
      expect (shouldBeEmpty |> Deque.tryLast) |> toBeEqualToNoneOfInt;

      expect @@ Deque.toSequence @@ deque |> toBeEqualToSequenceOfInt (
        IntRange.create 0 count |> IntRange.toSequence
      );
      expect @@ Deque.toSequenceReversed @@ deque |> toBeEqualToSequenceOfInt (
        IntRange.create 0 count |> IntRange.toSequenceReversed
      );
    }),

    it (sprintf "addFirst and removeLast %i elements" count) (fun () => {
      let empty = Deque.empty;

      expect (Deque.isNotEmpty empty) |> toBeEqualToFalse;
      expect (Deque.isEmpty empty) |> toBeEqualToTrue;
      expect (Deque.count empty) |> toBeEqualToInt 0;
      defer (fun () => empty |> Deque.first) |> throws;
      defer (fun () => empty |> Deque.last) |> throws;
      expect (empty |> Deque.tryFirst) |> toBeEqualToNoneOfInt;
      expect (empty |> Deque.tryLast) |> toBeEqualToNoneOfInt;

      let deque = IntRange.create 0 count |> IntRange.reduce (fun acc i => {
        let acc = acc |> Deque.addFirst i;

        expect (Deque.isNotEmpty acc) |> toBeEqualToTrue;
        expect (Deque.isEmpty acc) |> toBeEqualToFalse;
        expect (Deque.count acc) |> toBeEqualToInt (i + 1);
        expect (acc |> Deque.first) |> toBeEqualToInt i;
        expect (acc |> Deque.last) |> toBeEqualToInt 0;
        expect (acc |> Deque.tryFirst) |> toBeEqualToSomeOfInt i;
        expect (acc |> Deque.tryLast) |> toBeEqualToSomeOfInt 0;

        acc;
      }) empty;

      let shouldBeEmpty = IntRange.create 0 count |> IntRange.reduce (fun acc i => {
        expect (Deque.isNotEmpty acc) |> toBeEqualToTrue;
        expect (Deque.isEmpty acc) |> toBeEqualToFalse;
        expect (Deque.count acc) |> toBeEqualToInt (count - i);
        expect (acc |> Deque.first) |> toBeEqualToInt (count - 1);
        expect (acc |> Deque.last) |> toBeEqualToInt i;
        expect (acc |> Deque.tryFirst) |> toBeEqualToSomeOfInt (count - 1);
        expect (acc |> Deque.tryLast) |> toBeEqualToSomeOfInt i;
        acc |> Deque.removeLast;
      }) deque;

      expect (Deque.isNotEmpty shouldBeEmpty) |> toBeEqualToFalse;
      expect (Deque.isEmpty shouldBeEmpty) |> toBeEqualToTrue;
      expect (Deque.count shouldBeEmpty) |> toBeEqualToInt 0;
      defer (fun () => shouldBeEmpty |> Deque.first) |> throws;
      defer (fun () => shouldBeEmpty |> Deque.last) |> throws;
      expect (shouldBeEmpty |> Deque.tryFirst) |>  toBeEqualToNoneOfInt;
      expect (shouldBeEmpty |> Deque.tryLast) |>  toBeEqualToNoneOfInt;

      expect @@ Deque.toSequence @@ deque |> toBeEqualToSequenceOfInt (
        IntRange.create 0 count |> IntRange.toSequenceReversed
      );
      expect @@ Deque.toSequenceReversed @@ deque |> toBeEqualToSequenceOfInt (
        IntRange.create 0 count |> IntRange.toSequence
      );
    }),

    it (sprintf "map %i elements" count) (fun () => {
      IntRange.create 0 count
        |> IntRange.reduce (fun acc i => acc |> Deque.addLast i) Deque.empty
        |> Deque.map (fun i => i + 1)
        |> Deque.toSequence
        |> expect
        |> toBeEqualToSequenceOfInt (
          IntRange.create 1 count |> IntRange.toSequence
        );
    }),

    it (sprintf "reduceRight %i elements" count) (fun () => {
      /* FIXME: This test could be better by not using a single repeated value. */
      Sequence.repeat 1
        |> Sequence.take count
        |> Sequence.reduce (fun acc i => acc |> Deque.addFirst i) Deque.empty
        |> Deque.reduceRight (fun acc i => acc + i) 0
        |> expect
        |> toBeEqualToInt count;
    }),

    it (sprintf "addLastAll with %i elements" count) (fun () => {
      let seq = IntRange.create 0 count |> IntRange.toSequence;
      let result = Deque.empty |> Deque.addLastAll (Sequence.toIterator seq);

      expect (Deque.toSequence result) |> toBeEqualToSequenceOfInt seq;
    }),

    it (sprintf "fromSequence with %i elements" count) (fun () => {
      let seq = IntRange.create 0 count |> IntRange.toSequence;
      let result = Deque.from (Sequence.toIterator seq);

      expect (Deque.toSequence result) |> toBeEqualToSequenceOfInt seq;
    }),

    it (sprintf "forEachReverse with %i elements" count) (fun () => {
      let counted = ref count;
      let result = IntRange.create 0 count
        |> IntRange.toIterator
        |> Deque.fromReversed;
      result |> Deque.forEachReverse (fun i => {
        expect (count - i) |> toBeEqualToInt !counted;
        counted := !counted - 1;
      });
    }),

    ...stackTests
  ];
};
