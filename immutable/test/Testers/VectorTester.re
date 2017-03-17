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

module type Vector = {
  type t 'a;

  let addFirst: 'a => (t 'a) => (t 'a);
  let addFirstAll: (Iterator.t 'a) => (t 'a) => (t 'a);
  let addLast: 'a => (t 'a) => (t 'a);
  let addLastAll: (Iterator.t 'a) => (t 'a) => (t 'a);
  let compare: (Comparator.t (t 'a));
  let compareWith: (Comparator.t 'a) => (Comparator.t (t 'a));
  let concat: (list (t 'a)) => (t 'a);
  let contains: 'a => (t 'a) => bool;
  let containsWith: (Equality.t 'a) => 'a => (t 'a) => bool;
  let count: (t 'a) => int;
  let empty: (t 'a);
  let equals: (Equality.t (t 'a));
  let equalsWith: (Equality.t 'a) => (Equality.t (t 'a));
  let every: ('a => bool) => (t 'a) => bool;
  let everyWithIndex: (int => 'a => bool) => (t 'a) => bool;
  let find: ('a => bool) => (t 'a) => 'a;
  let findWithIndex: (int => 'a => bool) => (t 'a) => 'a;
  let first: (t 'a) => 'a;
  let forEach: ('a => unit) => (t 'a) => unit;
  let forEachWithIndex: (int => 'a => unit) => (t 'a) => unit;
  let forEachReverse: ('a => unit) => (t 'a) => unit;
  let forEachReverseWithIndex: (int => 'a => unit) => (t 'a) => unit;
  let from: (Iterator.t 'a) => (t 'a);
  let fromReversed: (Iterator.t 'a) => (t 'a);
  let get: int => (t 'a) => 'a;
  let hash: (Hash.t (t 'a));
  let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
  let indexOf: ('a => bool) => (t 'a) => int;
  let indexOfWithIndex: (int => 'a => bool) => (t 'a) => int;
  let init: int => (int => 'a) => (t 'a);
  let insertAt: int => 'a => (t 'a) => (t 'a);
  let isEmpty: t 'a => bool;
  let isNotEmpty: t 'a => bool;
  let last: (t 'a) => 'a;
  let map: ('a => 'b) => (t 'a) => (t 'b);
  let mapWithIndex: (int => 'a => 'b) => (t 'a) => (t 'b);
  let mapReverse: ('a => 'b) => (t 'a) => (t 'b);
  let mapReverseWithIndex: (int => 'a => 'b) => (t 'a) => (t 'b);
  let none: ('a => bool) => (t 'a) => bool;
  let noneWithIndex: (int => 'a => bool) => (t 'a) => bool;
  let range: int => (option int) => (t 'a) => (t 'a);
  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let reduceWithIndex: ('acc => int => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let reduceRight: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let reduceRightWithIndex: ('acc => int => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let removeAll: (t 'a) => (t 'a);
  let removeAt: int => (t 'a) => (t 'a);
  let removeFirst: (t 'a) => (t 'a);
  let removeLast: (t 'a) => (t 'a);
  let return: 'a => (t 'a);
  let reverse: (t 'a) => (t 'a);
  let skip: int => (t 'a) => (t 'a);
  let some: ('a => bool) => (t 'a) => bool;
  let someWithIndex: (int => 'a => bool) => (t 'a) => bool;
  let toMap: (t 'a) => (Map.t int 'a);
  let toSequence: (t 'a) => (Sequence.t 'a);
  let toSequenceReversed: (t 'a) => (Sequence.t 'a);
  let tryFind: ('a => bool) => (t 'a) => (option 'a);
  let tryFindWithIndex: (int => 'a => bool) => (t 'a) => (option 'a);
  let tryFirst: (t 'a) => (option 'a);
  let tryGet: int => (t 'a) => (option 'a);
  let tryIndexOf: ('a => bool) => (t 'a) => (option int);
  let tryIndexOfWithIndex: (int => 'a => bool) => (t 'a) => (option int);
  let tryLast: (t 'a) => option 'a;
  let take: int => (t 'a) => (t 'a);
  let update: int => 'a => (t 'a) => (t 'a);
  let updateAll: (int => 'a => 'a) => (t 'a) => (t 'a);
  let updateWith: int => ('a => 'a) => (t 'a) => (t 'a);
};

let test (count: int) (module Vector: Vector): (list Test.t) => {
  let dequeTests = DequeTester.test count (module {
    type t 'a = Vector.t 'a;

    let addFirst = Vector.addFirst;
    let addFirstAll = Vector.addFirstAll;
    let addLast = Vector.addLast;
    let addLastAll = Vector.addLastAll;
    let compare = Vector.compare;
    let compareWith = Vector.compareWith;
    let contains = Vector.contains;
    let containsWith = Vector.containsWith;
    let count = Vector.count;
    let equals = Vector.equals;
    let equalsWith = Vector.equalsWith;
    let empty = Vector.empty;
    let every = Vector.every;
    let find = Vector.find;
    let first = Vector.first;
    let forEach = Vector.forEach;
    let forEachReverse = Vector.forEachReverse;
    let from = Vector.from;
    let fromReversed = Vector.fromReversed;
    let hash = Vector.hash;
    let hashWith = Vector.hashWith;
    let isEmpty = Vector.isEmpty;
    let isNotEmpty = Vector.isNotEmpty;
    let last = Vector.last;
    let map = Vector.map;
    let mapReverse = Vector.mapReverse;
    let none = Vector.none;
    let reduce = Vector.reduce;
    let reduceRight = Vector.reduceRight;
    let removeAll = Vector.removeAll;
    let removeFirst = Vector.removeFirst;
    let removeLast = Vector.removeLast;
    let return = Vector.return;
    let reverse = Vector.reverse;
    let some = Vector.some;
    let toSequence = Vector.toSequence;
    let toSequenceReversed = Vector.toSequenceReversed;
    let tryFind = Vector.tryFind;
    let tryFirst = Vector.tryFirst;
    let tryLast = Vector.tryLast;
  });

  [
    it (sprintf "update with %i elements" count) (fun () => {
      let vector = IntRange.create 0 count
        |> IntRange.reduce
          (fun acc i => acc |> Vector.addLast i)
          Vector.empty;

      IntRange.create 0 count
        |> IntRange.forEach (fun i => {
          expect (vector |> Vector.get i) |> toBeEqualToInt i;
          expect (vector |> Vector.tryGet i) |> toBeEqualToSomeOfInt i;
        });

      let updated = IntRange.create 0 count
        |> IntRange.reduce
          (fun acc i => acc |> Vector.update i (i + 1))
          vector;

      IntRange.create 0 count
        |> IntRange.forEach (fun i => {
          expect (updated |> Vector.get i) |> toBeEqualToInt (i + 1);
          expect (updated |> Vector.tryGet i) |> toBeEqualToSomeOfInt (i + 1);
        });
    }),

    it (sprintf "updateAll with %i elements" count) (fun () => {
      let vector = IntRange.create 0 count
        |> IntRange.toIterator
        |> Vector.from;

      expect (
        vector |> Vector.updateAll (
          fun i v => {
            expect i |> toBeEqualToInt v;
            v + 1;
          }
        ) |> Vector.toSequence
      ) |> toBeEqualToSequenceOfInt
        (IntRange.create 1 count |> IntRange.toSequence);
    }),

    it (sprintf "updateWith with %i elements" count) (fun () => {
      let vector = IntRange.create 0 count |> IntRange.reduce (fun acc i =>
        acc |> Vector.addLast i
      ) Vector.empty;

      IntRange.create 0 count |> IntRange.forEach (fun i => {
        expect (vector |> Vector.get i) |> toBeEqualToInt i;
        expect (vector |> Vector.tryGet i) |> toBeEqualToSomeOfInt i;
      });

      let updated = IntRange.create 0 count |> IntRange.reduce (fun acc i =>
        acc |> Vector.updateWith i (fun v => v + 1)
      ) vector;

      IntRange.create 0 count |> IntRange.forEach (fun i => {
        expect (updated |> Vector.get i) |> toBeEqualToInt (i + 1);
        expect (updated |> Vector.tryGet i) |> toBeEqualToSomeOfInt (i + 1);
      });
    }),

    it (sprintf "mapWithIndex %i elements" count) (fun () => {
      IntRange.create 0 count
        |> IntRange.reduce (fun acc i => acc |> Vector.addLast i) Vector.empty
        |> Vector.mapWithIndex (fun i _ => i + 1)
        |> Vector.toSequence
        |> expect
        |> toBeEqualToSequenceOfInt (IntRange.create 1 count |> IntRange.toSequence);
    }),

    it (sprintf "mapReverseWithIndex %i elements" count) (fun () => {
      IntRange.create 0 count
        |> IntRange.reduce (fun acc i => acc |> Vector.addLast i) Vector.empty
        |> Vector.mapReverseWithIndex (fun i _ => i + 1)
        |> Vector.toSequence
        |> expect
        |> toBeEqualToSequenceOfInt (IntRange.create 1 count |> IntRange.toSequenceReversed);
    }),

    it (sprintf "reduceWithIndex with %i elements" count) (fun () => {
      let result = Sequence.repeat 1
        |> Sequence.take count
        |> Sequence.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.reduceWithIndex (fun acc i _ => acc + i) 0;

      let expected = IntRange.create 0 count
        |> IntRange.reduce (fun acc i => acc + i) 0;

      expect result |> toBeEqualToInt expected;
    }),

    it (sprintf "reduceRightWithIndex %i elements" count) (fun () => {
      let result = Sequence.repeat 1
        |> Sequence.take count
        |> Sequence.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.reduceRightWithIndex (fun acc i _ => acc + i) 0;

      let expected = IntRange.create 0 count
        |> IntRange.reduceRight (fun acc i => acc + i) 0;

      expect result |> toBeEqualToInt expected;
    }),

    it (sprintf "take with %i elements" count) (fun () => {
      let vector = IntRange.create 0 count
        |> IntRange.reduce
          (fun acc i => acc |> Vector.addLast i)
          Vector.empty;

      let taken = vector |> Vector.take 1;
      expect @@ Vector.toSequence @@ taken |> toBeEqualToSequenceOfInt (Sequence.return 0);

      let taken = vector |> Vector.take (count / 2);
      expect @@ Vector.toSequence @@ taken |> toBeEqualToSequenceOfInt (
        IntRange.create 0 (count / 2) |> IntRange.toSequence
      );

      let taken = vector |> Vector.take (count - 1);
      expect @@ Vector.toSequence @@ taken |> toBeEqualToSequenceOfInt (
        IntRange.create 0 (count - 1) |> IntRange.toSequence
      );
    }),
    it (sprintf "skip with %i elements" count) (fun () => {
      let vector = IntRange.create 0 count
        |> IntRange.reduce
          (fun acc i => acc |> Vector.addLast i)
          Vector.empty;

      let skipped = vector |> Vector.skip 1;
      expect @@ Vector.toSequence @@ skipped |> toBeEqualToSequenceOfInt (
        IntRange.create 1 (count - 1) |> IntRange.toSequence
      );

      let skipped = vector |> Vector.skip (count / 2);
      expect @@ Vector.toSequence @@ skipped |> toBeEqualToSequenceOfInt (
        IntRange.create (count / 2) (count / 2) |> IntRange.toSequence
      );

      let skipped = vector |> Vector.skip (count - 1);
      expect @@ Vector.toSequence @@ skipped |> toBeEqualToSequenceOfInt (Sequence.return (count - 1));
    }),
    it (sprintf "range with %i elements" count) (fun () => {
      let vector = IntRange.create 0 count |> IntRange.reduce (fun acc i =>
        acc |> Vector.addLast i
      ) Vector.empty;

      let rangeOneToCountMinusTwo = vector |> Vector.range 1 (count - 2 |> Option.return);
      expect @@ Vector.toSequence @@ rangeOneToCountMinusTwo |> toBeEqualToSequenceOfInt (
        IntRange.create 1 (count - 2) |> IntRange.toSequence
      );
    }),
    it (sprintf "init with %i elements" count) (fun () => {
      let vector = Vector.init count (fun i => i);
      vector |> Vector.forEachWithIndex (fun i v => {
        expect i |> toBeEqualToInt v
      });
    }),
    it (sprintf "forEachWithIndex with %i elements" count) (fun () => {
      let counted = ref 0;
      let result = IntRange.create 0 count
        |> IntRange.toIteratorReversed
        |> Vector.fromReversed;
      result |> Vector.forEachWithIndex (fun i v => {
        expect i |> toBeEqualToInt v;
        expect i |> toBeEqualToInt !counted;
        counted := !counted + 1;
      });
    }),
    it (sprintf "forEachReverseWithIndex with %i elements" count) (fun () => {
      let counted = ref (count - 1);

      let result = IntRange.create 0 count
        |> IntRange.toIterator
        |> Vector.from;

      result |> Vector.forEachReverseWithIndex (fun i v => {
        expect i |> toBeEqualToInt v;
        expect i |> toBeEqualToInt !counted;
        counted := !counted - 1;
      });
    }),
    it (sprintf "everyWithIndex with %i elements" count) (fun () => {
      let previousIndex = ref (-1);
      Sequence.concat [Sequence.return false, Sequence.repeat true |> Sequence.take (count - 1)]
        |> Sequence.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.everyWithIndex (fun i v => {
            expect i |> toBeEqualToInt (!previousIndex + 1);
            previousIndex := i;
            v
          })
        |> expect |> toBeEqualToFalse;

      let previousIndex = ref (-1);
      Sequence.concat [Sequence.repeat true |> Sequence.take (count - 1), Sequence.return false]
        |> Sequence.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.everyWithIndex (fun i v => {
            expect i |> toBeEqualToInt (!previousIndex + 1);
            previousIndex := i;
            v
          })
        |> expect |> toBeEqualToFalse;

      let previousIndex = ref (-1);
      Sequence.concat [
        Sequence.repeat true |> Sequence.take (count / 2 - 1),
        Sequence.return false,
        Sequence.repeat true |> Sequence.take (count / 2 - 1),
      ]
        |> Sequence.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.everyWithIndex (fun i v => {
            expect i |> toBeEqualToInt (!previousIndex + 1);
            previousIndex := i;
            v
          })
        |> expect |> toBeEqualToFalse;

      let previousIndex = ref (-1);
      Sequence.repeat true
        |> Sequence.take count
        |> Sequence.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.everyWithIndex (fun i v => {
            expect i |> toBeEqualToInt (!previousIndex + 1);
            previousIndex := i;
            v
          })
        |> expect |> toBeEqualToTrue;
    }),
    it (sprintf "findWithIndex and tryFindWithIndex in %i elements" count) (fun () => {
      let vector = IntRange.create 0 count |> IntRange.reduce
        (fun acc i => acc |> Vector.addFirst i)
        Vector.empty;

      let previousIndex = ref (-1);
      let find0 i v => {
        expect i |> toBeEqualToInt (!previousIndex + 1);
        previousIndex := i;
        v == 0
      };
      let findCountMinusOne i v => {
        expect i |> toBeEqualToInt (!previousIndex + 1);
        previousIndex := i;
        v == (count - 1);
      };
      let findCountDividedByTwo i v => {
        expect i |> toBeEqualToInt (!previousIndex + 1);
        previousIndex := i;
        v == (count / 2);
      };

      let alwaysFalse i _ => {
        expect i |> toBeEqualToInt (!previousIndex + 1);
        previousIndex := i;
        false;
      };

      previousIndex := (-1);
      expect (Vector.findWithIndex find0 vector) |> toBeEqualToInt 0;

      previousIndex := (-1);
      expect (Vector.findWithIndex findCountMinusOne vector) |> toBeEqualToInt (count - 1);

      previousIndex := (-1);
      expect (Vector.findWithIndex findCountDividedByTwo vector) |> toBeEqualToInt (count / 2);

      previousIndex := (-1);
      expect (Vector.tryFindWithIndex find0 vector) |> toBeEqualToSomeOfInt 0;

      previousIndex := (-1);
      expect (Vector.tryFindWithIndex findCountMinusOne vector) |> toBeEqualToSomeOfInt (count - 1);

      previousIndex := (-1);
      expect (Vector.tryFindWithIndex findCountDividedByTwo vector) |> toBeEqualToSomeOfInt (count / 2);

      previousIndex := (-1);
      defer (fun () => Vector.findWithIndex alwaysFalse vector) |> throws;

      previousIndex := (-1);
      expect (Vector.tryFindWithIndex alwaysFalse vector) |> toBeEqualToNoneOfInt;
    }),
    it (sprintf "noneWithIndex with %i elements" count) (fun () => {
      let previousIndex = ref (-1);
      Sequence.concat [Sequence.repeat false |> Sequence.take (count - 1), Sequence.return true]
        |> Sequence.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.noneWithIndex (fun i v => {
            expect i |> toBeEqualToInt (!previousIndex + 1);
            previousIndex := i;
            v;
          })
        |> expect |> toBeEqualToFalse;

      let previousIndex = ref (-1);
      Sequence.concat [Sequence.return true, Sequence.repeat false |> Sequence.take (count - 1)]
        |> Sequence.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.noneWithIndex (fun i v => {
            expect i |> toBeEqualToInt (!previousIndex + 1);
            previousIndex := i;
            v;
          })
        |> expect |> toBeEqualToFalse;

      let previousIndex = ref (-1);
      Sequence.concat [
        Sequence.repeat false |> Sequence.take (count / 2 - 1),
        Sequence.return true,
        Sequence.repeat false |> Sequence.take (count / 2 - 1),
      ]
        |> Sequence.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.noneWithIndex (fun i v => {
            expect i |> toBeEqualToInt (!previousIndex + 1);
            previousIndex := i;
            v;
          })
        |> expect |> toBeEqualToFalse;

      let previousIndex = ref (-1);
      Sequence.repeat false
        |> Sequence.take count
        |> Sequence.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.noneWithIndex (fun i v => {
            expect i |> toBeEqualToInt (!previousIndex + 1);
            previousIndex := i;
            v;
          })
        |> expect |> toBeEqualToTrue;
    }),
    it (sprintf "someWithIndex with %i elements" count) (fun () => {
      let previousIndex = ref (-1);
      Sequence.concat [Sequence.repeat false |> Sequence.take (count - 1), Sequence.return true]
        |> Sequence.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.someWithIndex (fun i v => {
            expect i |> toBeEqualToInt (!previousIndex + 1);
            previousIndex := i;
            v;
          })
        |> expect |> toBeEqualToTrue;

      let previousIndex = ref (-1);
      Sequence.concat [Sequence.return true, Sequence.repeat false |> Sequence.take (count - 1)]
        |> Sequence.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.someWithIndex (fun i v => {
            expect i |> toBeEqualToInt (!previousIndex + 1);
            previousIndex := i;
            v;
          })
        |> expect |> toBeEqualToTrue;

      let previousIndex = ref (-1);
      Sequence.concat [
        Sequence.repeat false |> Sequence.take (count / 2 - 1),
        Sequence.return true,
        Sequence.repeat false |> Sequence.take (count / 2 - 1),
      ]
        |> Sequence.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.someWithIndex (fun i v => {
            expect i |> toBeEqualToInt (!previousIndex + 1);
            previousIndex := i;
            v;
          })
        |> expect |> toBeEqualToTrue;

      let previousIndex = ref (-1);
      Sequence.repeat false
        |> Sequence.take count
        |> Sequence.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.someWithIndex (fun i v => {
            expect i |> toBeEqualToInt (!previousIndex + 1);
            previousIndex := i;
            v;
          })
        |> expect |> toBeEqualToFalse;
    }),
    it (sprintf "insertAt with %i elements" count) (fun () => {
      let result = Sequence.generate (fun i => i + 2) 1
        |> Sequence.take count
        |> Sequence.reduce
          (fun acc i => acc |> Vector.insertAt i 1)
          (Sequence.repeat 0 |> Sequence.take count |> Sequence.toIterator |> Vector.from);

      expect @@ Vector.toSequence @@ result
        |> toBeEqualToSequenceOfInt (Sequence.repeat [0, 1] |> Sequence.take count |> Sequence.flatMap List.toSequence);
    }),
    it (sprintf "removeAt with %i elements" count) (fun () => {
      let initialValue = Sequence.repeat [0, 1]
        |> Sequence.take count
        |> Sequence.flatMap List.toSequence
        |> Sequence.toIterator
        |> Vector.from;

      let result = IntRange.create 1 count |> IntRange.reduce
        (fun acc i => acc |> Vector.removeAt i)
        initialValue;

      expect @@ Vector.toSequence @@ result
        |> toBeEqualToSequenceOfInt (Sequence.repeat 0 |> Sequence.take count);
    }),
    describe (sprintf "concat with %i elements" count) [
      it "balanced segments" (fun () => {
        let segmentCount = count / 2;
        let vecFirst = Sequence.repeat 0
          |> Sequence.take segmentCount
          |> Sequence.toIterator
          |> Vector.from;

        let vecSecond = Sequence.repeat 1
          |> Sequence.take segmentCount
          |> Sequence.toIterator
          |> Vector.from;

        Vector.concat [vecFirst, vecSecond]
        |> Vector.toSequence
        |> expect
        |> toBeEqualToSequenceOfInt @@ Sequence.concat @@ [
            Sequence.repeat 0 |> Sequence.take segmentCount,
            Sequence.repeat 1 |> Sequence.take segmentCount,
          ];
      }),
      it "left larger than right" (fun () => {
        let segmentRightCount = count / 4;
        let segmentLeftCount = count - segmentRightCount;

        let vecLeft = Sequence.repeat 0
          |> Sequence.take segmentLeftCount
          |> Sequence.toIterator
          |> Vector.from;

        let vecRight = Sequence.repeat 1
          |> Sequence.take segmentRightCount
          |> Sequence.toIterator
          |> Vector.from;

        Vector.concat [vecLeft, vecRight]
        |> Vector.toSequence
        |> expect
        |> toBeEqualToSequenceOfInt @@ Sequence.concat @@ [
            Sequence.repeat 0 |> Sequence.take segmentLeftCount,
            Sequence.repeat 1 |> Sequence.take segmentRightCount,
          ];
      }),
      it "right larger than left" (fun () => {
        let segmentLeftCount = count / 4;
        let segmentRightCount = count - segmentLeftCount;

        let vecLeft = Sequence.repeat 0
          |> Sequence.take segmentLeftCount
          |> Sequence.toIterator
          |> Vector.from;

        let vecRight = Sequence.repeat 1
          |> Sequence.take segmentRightCount
          |> Sequence.toIterator
          |> Vector.from;

        Vector.concat [vecLeft, vecRight]
        |> Vector.toSequence
        |> expect
        |> toBeEqualToSequenceOfInt @@ Sequence.concat @@ [
            Sequence.repeat 0 |> Sequence.take segmentLeftCount,
            Sequence.repeat 1 |> Sequence.take segmentRightCount,
          ];
      }),
    ],
    it (sprintf "indexOf with %i elements" count) (fun () => {
      let vec = IntRange.create 0 count |> IntRange.toIterator|> Vector.from;
      let f pos v => v == pos;
      expect (Vector.indexOf (f (count / 2)) vec) |> toBeEqualToInt (count / 2);
      defer (fun () => Vector.indexOf (f (-1)) vec) |> throws;
    }),
    it (sprintf "indexOfWithIndex with %i elements" count) (fun () => {
      let vec = IntRange.create 0 count |> IntRange.toIterator |> Vector.from;
      let f pos i _ => i == pos;
      expect (Vector.indexOfWithIndex (f (count / 2)) vec) |> toBeEqualToInt (count / 2);
      defer (fun () => Vector.indexOfWithIndex (f (-1)) vec) |> throws;
    }),
    it (sprintf "tryIndexOf with %i elements" count) (fun () => {
      let vec = IntRange.create 0 count |> IntRange.toIterator |> Vector.from;
      let f pos v => v == pos;
      expect (Vector.tryIndexOf (f (count / 2)) vec) |> toBeEqualToSomeOfInt (count / 2);
      expect (Vector.tryIndexOf (f (-1)) vec) |> toBeEqualToNoneOfInt;
    }),
    it (sprintf "tryIndexOfWithIndex with %i elements" count) (fun () => {
      let vec = IntRange.create 0 count |> IntRange.toIterator |> Vector.from;
      let f pos i _ => i == pos;
      expect (Vector.tryIndexOfWithIndex (f (count / 2)) vec) |> toBeEqualToSomeOfInt (count / 2);
      expect (Vector.tryIndexOfWithIndex (f (-1)) vec) |> toBeEqualToNoneOfInt;
    }),
    describe (sprintf "toMap with %i elements" count) [
      it "containsWith" (fun () => {
        let map = IntRange.create 0 count |> IntRange.toIterator |> Vector.from |> Vector.toMap;
        IntRange.create 0 count |> IntRange.forEach (fun i => {
          expect (map |> Map.contains i i) |> toBeEqualToTrue;
        });

        expect (map |> Map.contains (-1) (-1)) |> toBeEqualToFalse;
        expect (map |> Map.contains count count) |> toBeEqualToFalse;
        expect (map |> Map.contains 0 1) |> toBeEqualToFalse;
      }),
      it "containsKey" (fun () => {
        let map = IntRange.create 0 count |> IntRange.toIterator |> Vector.from |> Vector.toMap;
        IntRange.create 0 10 |> IntRange.forEach (fun i => {
          expect (map |> Map.containsKey i) |> toBeEqualToTrue;
        });

        expect (map |> Map.containsKey (-1)) |> toBeEqualToFalse;
        expect (map |> Map.containsKey count) |> toBeEqualToFalse;
      }),
      it "count" (fun () => {
        let map = IntRange.create 0 count |> IntRange.toIterator |> Vector.from |> Vector.toMap;
        expect (map |> Map.count) |> toBeEqualToInt count;
      }),
      it "every" (fun () => {
        let map = IntRange.create 0 count |> IntRange.toIterator |> Vector.from |> Vector.toMap;
        expect (map |> Map.every (fun i v => i == v)) |> toBeEqualToTrue;
        expect (map |> Map.every (fun i v => i != v)) |> toBeEqualToFalse;
      }),
      it "find" (fun () => {
        let map = IntRange.create 0 count |> IntRange.toIterator |> Vector.from |> Vector.toMap;
        expect (map |> Map.find (fun i v => i == v)) |> toBeEqualTo (fun _ => "") (0, 0);
        defer (fun () => map |> Map.find (fun i v => i != v)) |> throws;
      }),
      it "forEach" (fun () => {
        let map = IntRange.create 0 count |> IntRange.toIterator |> Vector.from |> Vector.toMap;
        let loopCount = ref 0;
        map |> Map.forEach (fun i v => {
          expect i |> toBeEqualToInt !loopCount;
          expect i |> toBeEqualToInt v;
          loopCount := !loopCount + 1;
        });

        expect !loopCount |> toBeEqualToInt count;
      }),
      it "get" (fun () => {
        let map = IntRange.create 0 count |> IntRange.toIterator |> Vector.from |> Vector.toMap;
        IntRange.create 0 10 |> IntRange.forEach (fun i => {
          expect (map |> Map.get i) |> toBeEqualToInt i;
        });

        defer (fun () => map |> Map.get (-1)) |> throws;
        defer (fun () => map |> Map.get count) |> throws;
      }),
      it "keys" (fun () => {
        let map = IntRange.create 0 count |> IntRange.toIterator |> Vector.from |> Vector.toMap;
        expect (map |> Map.keys |> Set.toSequence) |> toBeEqualToSequenceOfInt (
          IntRange.create 0 count |> IntRange.toSequence
        );
      }),
      it "none" (fun () => {
        let map = IntRange.create 0 count |> IntRange.toIterator |> Vector.from |> Vector.toMap;
        expect (map |> Map.none (fun i v => i != v)) |> toBeEqualToTrue;
        expect (map |> Map.none (fun i v => i == v)) |> toBeEqualToFalse;
      }),
      it "reduce" (fun () => {
        let map = IntRange.create 0 count |> IntRange.toIterator |> Vector.from |> Vector.toMap;
        let reduced = map |> Map.reduce (fun acc _ _ => acc + 1) 0;
        expect reduced |> toBeEqualToInt count;
      }),
      it "some" (fun () => {
        let map = IntRange.create 0 count |> IntRange.toIterator |> Vector.from |> Vector.toMap;
        expect (map |> Map.some (fun i v => i == v)) |> toBeEqualToTrue;
        expect (map |> Map.some (fun i v => i != v)) |> toBeEqualToFalse;
      }),
      it "toSequence" (fun () => {
        let map = IntRange.create 0 count |> IntRange.toIterator |> Vector.from |> Vector.toMap;
        map |> Map.toSequence |> Sequence.forEach (fun (i, v) => {
          expect i |> toBeEqualToInt v;
        });
      }),
      it "tryFind" (fun () => {
        let map = IntRange.create 0 count |> IntRange.toIterator |> Vector.from |> Vector.toMap;
        expect (map |> Map.tryFind (fun i v => i == v)) |> toBeEqualToSome (fun _ => "") (0, 0);
        expect (map |> Map.tryFind (fun i v => i != v)) |> toBeEqualToNone (fun _ => "");
      }),
      it "tryGet" (fun () => {
        let map = IntRange.create 0 count |> IntRange.toIterator |> Vector.from |> Vector.toMap;
        IntRange.create 0 10 |> IntRange.forEach (fun i => {
          expect (map |> Map.tryGet i) |> toBeEqualToSomeOfInt i;
        });

        expect (map |> Map.tryGet (-1)) |> toBeEqualToNoneOfInt;
        expect (map |> Map.tryGet count) |> toBeEqualToNoneOfInt;
      }),
      it "values" (fun () => {
        let map = IntRange.create 0 count
          |> IntRange.toIterator
          |> Vector.from
          |> Vector.toMap;
        expect (map |> Map.values |> Vector.from |> Vector.toSequence) |> toBeEqualToSequenceOfInt (
          IntRange.create 0 count |> IntRange.toSequence
        );
      }),
    ],
    ...dequeTests
  ]
};
