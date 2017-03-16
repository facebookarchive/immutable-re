open Immutable;
open Printf;
open ReUnit;
open ReUnit.Expect;
open ReUnit.Test;

module type TransientDeque = {
  type t 'a;

  let addFirst: 'a => (t 'a) => (t 'a);
  let addLast: 'a => (t 'a) => (t 'a);
  let count: (t 'a) => int;
  let empty: unit => (t 'a);
  let first: (t 'a) => 'a;
  let isEmpty: t 'a => bool;
  let isNotEmpty: t 'a => bool;
  let last: (t 'a) => 'a;
  let removeAll: (t 'a) => (t 'a);
  let removeFirst: (t 'a) => (t 'a);
  let removeLast: (t 'a) => (t 'a);
  let reverse: (t 'a) => (t 'a);
  let tryFirst: (t 'a) => (option 'a);
  let tryLast: (t 'a) => option 'a;
};

let test (count: int) (module TransientDeque: TransientDeque): (list Test.t) => {
  let stackTests = TransientStackTester.test count (module {
    type t 'a = TransientDeque.t 'a;

    let addFirst = TransientDeque.addFirst;
    let count = TransientDeque.count;
    let empty = TransientDeque.empty;
    let first = TransientDeque.first;
    let isEmpty = TransientDeque.isEmpty;
    let isNotEmpty = TransientDeque.isNotEmpty;
    let removeAll = TransientDeque.removeAll;
    let removeFirst = TransientDeque.removeFirst;
    let reverse = TransientDeque.reverse;
    let tryFirst = TransientDeque.tryFirst;
  });

  [
    it (sprintf "addLast and removeLast %i elements" count) (fun () => {
      let empty = TransientDeque.empty ();

      expect (TransientDeque.isNotEmpty empty) |> toBeEqualToFalse;
      expect (TransientDeque.isEmpty empty) |> toBeEqualToTrue;
      expect (TransientDeque.count empty) |> toBeEqualToInt 0;
      defer (fun () => empty |> TransientDeque.first) |> throws;
      defer (fun () => empty |> TransientDeque.last) |> throws;
      expect (empty |> TransientDeque.tryFirst) |> toBeEqualToNoneOfInt;
      expect (empty |> TransientDeque.tryLast) |> toBeEqualToNoneOfInt;

      let deque = IntRange.create 0 count |> IntRange.reduce (fun acc i => {
        let acc = acc |> TransientDeque.addLast i;

        expect (TransientDeque.isNotEmpty acc) |> toBeEqualToTrue;
        expect (TransientDeque.isEmpty acc) |> toBeEqualToFalse;
        expect (TransientDeque.count acc) |> toBeEqualToInt (i + 1);
        expect (acc |> TransientDeque.first) |> toBeEqualToInt 0;
        expect (acc |> TransientDeque.last) |> toBeEqualToInt i;
        expect (acc |> TransientDeque.tryFirst) |> toBeEqualToSomeOfInt 0;
        expect (acc |> TransientDeque.tryLast) |> toBeEqualToSomeOfInt i;

        acc;
      }) empty;

      let shouldBeEmpty = IntRange.create 0 count |> IntRange.reduceRight (fun acc i => {
        expect (TransientDeque.isNotEmpty acc) |> toBeEqualToTrue;
        expect (TransientDeque.isEmpty acc) |> toBeEqualToFalse;
        expect (TransientDeque.count acc) |> toBeEqualToInt (i + 1);
        expect (acc |> TransientDeque.first) |> toBeEqualToInt 0;
        expect (acc |> TransientDeque.last) |> toBeEqualToInt i;
        expect (acc |> TransientDeque.tryFirst) |> toBeEqualToSomeOfInt 0;
        expect (acc |> TransientDeque.tryLast) |> toBeEqualToSomeOfInt i;
        acc |> TransientDeque.removeLast;
      }) deque;

      expect (TransientDeque.isNotEmpty shouldBeEmpty) |> toBeEqualToFalse;
      expect (TransientDeque.isEmpty shouldBeEmpty) |> toBeEqualToTrue;
      expect (TransientDeque.count shouldBeEmpty) |> toBeEqualToInt 0;
      defer (fun () => shouldBeEmpty |> TransientDeque.first) |> throws;
      defer (fun () => shouldBeEmpty |> TransientDeque.last) |> throws;
      expect (shouldBeEmpty |> TransientDeque.tryFirst) |>  toBeEqualToNoneOfInt;
      expect (shouldBeEmpty |> TransientDeque.tryLast) |>  toBeEqualToNoneOfInt;
    }),
    it (sprintf "addLast and removeFirst %i elements" count) (fun () => {
      let empty = TransientDeque.empty ();

      expect (TransientDeque.isNotEmpty empty) |> toBeEqualToFalse;
      expect (TransientDeque.isEmpty empty) |> toBeEqualToTrue;
      expect (TransientDeque.count empty) |> toBeEqualToInt 0;
      defer (fun () => empty |> TransientDeque.first) |> throws;
      defer (fun () => empty |> TransientDeque.last) |> throws;
      expect (empty |> TransientDeque.tryFirst) |> toBeEqualToNoneOfInt;
      expect (empty |> TransientDeque.tryLast) |> toBeEqualToNoneOfInt;

      let deque = IntRange.create 0 count |> IntRange.reduce (fun acc i => {
        let acc = acc |> TransientDeque.addLast i;

        expect (TransientDeque.isNotEmpty acc) |> toBeEqualToTrue;
        expect (TransientDeque.isEmpty acc) |> toBeEqualToFalse;
        expect (TransientDeque.count acc) |> toBeEqualToInt (i + 1);
        expect (acc |> TransientDeque.first) |> toBeEqualToInt 0;
        expect (acc |> TransientDeque.last) |> toBeEqualToInt i;
        expect (acc |> TransientDeque.tryFirst) |> toBeEqualToSomeOfInt 0;
        expect (acc |> TransientDeque.tryLast) |> toBeEqualToSomeOfInt i;

        acc;
      }) empty;

      let shouldBeEmpty = IntRange.create 0 count |> IntRange.reduce (fun acc i => {
        expect (TransientDeque.isNotEmpty acc) |> toBeEqualToTrue;
        expect (TransientDeque.isEmpty acc) |> toBeEqualToFalse;
        expect (TransientDeque.count acc) |> toBeEqualToInt (count - i);
        expect (acc |> TransientDeque.first) |> toBeEqualToInt i;
        expect (acc |> TransientDeque.last) |> toBeEqualToInt (count - 1);
        expect (acc |> TransientDeque.tryFirst) |> toBeEqualToSomeOfInt i;
        expect (acc |> TransientDeque.tryLast) |> toBeEqualToSomeOfInt (count - 1);
        acc |> TransientDeque.removeFirst;
      }) deque;

      expect (TransientDeque.isNotEmpty shouldBeEmpty) |> toBeEqualToFalse;
      expect (TransientDeque.isEmpty shouldBeEmpty) |> toBeEqualToTrue;
      expect (TransientDeque.count shouldBeEmpty) |> toBeEqualToInt 0;
      defer (fun () => shouldBeEmpty |> TransientDeque.first) |> throws;
      defer (fun () => shouldBeEmpty |> TransientDeque.last) |> throws;
      expect (shouldBeEmpty |> TransientDeque.tryFirst) |>  toBeEqualToNoneOfInt;
      expect (shouldBeEmpty |> TransientDeque.tryLast) |>  toBeEqualToNoneOfInt;
    }),
    it (sprintf "addFirst and removeLast %i elements" count) (fun () => {
      let empty = TransientDeque.empty ();

      expect (TransientDeque.isNotEmpty empty) |> toBeEqualToFalse;
      expect (TransientDeque.isEmpty empty) |> toBeEqualToTrue;
      expect (TransientDeque.count empty) |> toBeEqualToInt 0;
      defer (fun () => empty |> TransientDeque.first) |> throws;
      defer (fun () => empty |> TransientDeque.last) |> throws;
      expect (empty |> TransientDeque.tryFirst) |> toBeEqualToNoneOfInt;
      expect (empty |> TransientDeque.tryLast) |> toBeEqualToNoneOfInt;

      let deque = IntRange.create 0 count |> IntRange.reduce (fun acc i => {
        let acc = acc |> TransientDeque.addFirst i;

        expect (TransientDeque.isNotEmpty acc) |> toBeEqualToTrue;
        expect (TransientDeque.isEmpty acc) |> toBeEqualToFalse;
        expect (TransientDeque.count acc) |> toBeEqualToInt (i + 1);
        expect (acc |> TransientDeque.first) |> toBeEqualToInt i;
        expect (acc |> TransientDeque.last) |> toBeEqualToInt 0;
        expect (acc |> TransientDeque.tryFirst) |> toBeEqualToSomeOfInt i;
        expect (acc |> TransientDeque.tryLast) |> toBeEqualToSomeOfInt 0;

        acc;
      }) empty;

      let shouldBeEmpty = IntRange.create 0 count |> IntRange.reduce (fun acc i => {
        expect (TransientDeque.isNotEmpty acc) |> toBeEqualToTrue;
        expect (TransientDeque.isEmpty acc) |> toBeEqualToFalse;
        expect (TransientDeque.count acc) |> toBeEqualToInt (count - i);
        expect (acc |> TransientDeque.first) |> toBeEqualToInt (count - 1);
        expect (acc |> TransientDeque.last) |> toBeEqualToInt i;
        expect (acc |> TransientDeque.tryFirst) |> toBeEqualToSomeOfInt (count - 1);
        expect (acc |> TransientDeque.tryLast) |> toBeEqualToSomeOfInt i;
        acc |> TransientDeque.removeLast;
      }) deque;

      expect (TransientDeque.isNotEmpty shouldBeEmpty) |> toBeEqualToFalse;
      expect (TransientDeque.isEmpty shouldBeEmpty) |> toBeEqualToTrue;
      expect (TransientDeque.count shouldBeEmpty) |> toBeEqualToInt 0;
      defer (fun () => shouldBeEmpty |> TransientDeque.first) |> throws;
      defer (fun () => shouldBeEmpty |> TransientDeque.last) |> throws;
      expect (shouldBeEmpty |> TransientDeque.tryFirst) |>  toBeEqualToNoneOfInt;
      expect (shouldBeEmpty |> TransientDeque.tryLast) |>  toBeEqualToNoneOfInt;
    }),
    ...stackTests
  ];
};
