open Immutable;
open Printf;
open ReUnit;
open ReUnit.Expect;
open ReUnit.Test;

module type Vector = {
  type t 'a;

  let addFirst: 'a => (t 'a) => (t 'a);
  let addFirstAll: (Seq.t 'a) => (t 'a) => (t 'a);
  let addLast: 'a => (t 'a) => (t 'a);
  let addLastAll: (Seq.t 'a) => (t 'a) => (t 'a);
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
  let fromSeq: (Seq.t 'a) => (t 'a);
  let fromSeqReversed: (Seq.t 'a) => (t 'a);
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
  let toSeq: (t 'a) => (Seq.t 'a);
  let toSeqReversed: (t 'a) => (Seq.t 'a);
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
    let fromSeq = Vector.fromSeq;
    let fromSeqReversed = Vector.fromSeqReversed;
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
    let toSeq = Vector.toSeq;
    let toSeqReversed = Vector.toSeqReversed;
    let tryFind = Vector.tryFind;
    let tryFirst = Vector.tryFirst;
    let tryLast = Vector.tryLast;
  });

  [
    it (sprintf "update with %i elements" count) (fun () => {
      let vector = Seq.inRange 0 (Some count) 1 |> Seq.reduce (fun acc i =>
        acc |> Vector.addLast i
      ) Vector.empty;

      Seq.inRange 0 (Some count) 1 |> Seq.forEach (fun i => {
        expect (vector |> Vector.get i) |> toBeEqualToInt i;
        expect (vector |> Vector.tryGet i) |> toBeEqualToSomeOfInt i;
      });

      let updated = Seq.inRange 0 (Some count) 1 |> Seq.reduce (fun acc i =>
        acc |> Vector.update i (i + 1)
      ) vector;

      Seq.inRange 0 (Some count) 1 |> Seq.forEach (fun i => {
        expect (updated |> Vector.get i) |> toBeEqualToInt (i + 1);
        expect (updated |> Vector.tryGet i) |> toBeEqualToSomeOfInt (i + 1);
      });
    }),

    it (sprintf "updateAll with %i elements" count) (fun () => {
      let vector = Seq.inRange 0 (Some count) 1 |> Vector.fromSeq;
      expect (vector |> Vector.updateAll (
        fun i v => {
          expect i |> toBeEqualToInt v;
          v + 1;
        }
      ) |> Vector.toSeq) |> toBeEqualToSeqOfInt (Seq.inRange 1 (Some count) 1);
    }),

    it (sprintf "updateWith with %i elements" count) (fun () => {
      let vector = Seq.inRange 0 (Some count) 1 |> Seq.reduce (fun acc i =>
        acc |> Vector.addLast i
      ) Vector.empty;

      Seq.inRange 0 (Some count) 1 |> Seq.forEach (fun i => {
        expect (vector |> Vector.get i) |> toBeEqualToInt i;
        expect (vector |> Vector.tryGet i) |> toBeEqualToSomeOfInt i;
      });

      let updated = Seq.inRange 0 (Some count) 1 |> Seq.reduce (fun acc i =>
        acc |> Vector.updateWith i (fun v => v + 1)
      ) vector;

      Seq.inRange 0 (Some count) 1 |> Seq.forEach (fun i => {
        expect (updated |> Vector.get i) |> toBeEqualToInt (i + 1);
        expect (updated |> Vector.tryGet i) |> toBeEqualToSomeOfInt (i + 1);
      });
    }),

    it (sprintf "mapWithIndex %i elements" count) (fun () => {
      Seq.inRange 0 (Some count) 1
        |> Seq.reduce (fun acc i => acc |> Vector.addLast i) Vector.empty
        |> Vector.mapWithIndex (fun i _ => i + 1)
        |> Vector.toSeq
        |> expect
        |> toBeEqualToSeqOfInt (Seq.inRange 1 (Some count) 1);
    }),

    it (sprintf "mapReverseWithIndex %i elements" count) (fun () => {
      Seq.inRange 0 (Some count) 1
        |> Seq.reduce (fun acc i => acc |> Vector.addLast i) Vector.empty
        |> Vector.mapReverseWithIndex (fun i _ => i + 1)
        |> Vector.toSeq
        |> expect
        |> toBeEqualToSeqOfInt (Seq.inRange count (Some count) (-1));
    }),

    it (sprintf "reduceWithIndex with %i elements" count) (fun () => {
      let result = Seq.repeat 1 (Some count)
        |> Seq.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.reduceWithIndex (fun acc i _ => acc + i) 0;

      let expected = Seq.inRange 0 (Some count) 1
        |> Seq.reduce (fun acc i => acc + i) 0;

      expect result |> toBeEqualToInt expected;
    }),

    it (sprintf "reduceRightWithIndex %i elements" count) (fun () => {
      let result = Seq.repeat 1 (Some count)
        |> Seq.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.reduceRightWithIndex (fun acc i _ => acc + i) 0;

      let expected = Seq.inRange (count - 1) (Some count) (-1)
        |> Seq.reduce (fun acc i => acc + i) 0;

      expect result |> toBeEqualToInt expected;
    }),

    it (sprintf "take with %i elements" count) (fun () => {
      let vector = Seq.inRange 0 (Some count) 1 |> Seq.reduce (fun acc i =>
        acc |> Vector.addLast i
      ) Vector.empty;

      let taken = vector |> Vector.take 1;
      expect @@ Vector.toSeq @@ taken |> toBeEqualToSeqOfInt (Seq.return 0);

      let taken = vector |> Vector.take (count / 2);
      expect @@ Vector.toSeq @@ taken |> toBeEqualToSeqOfInt (Seq.inRange 0 (Some (count / 2)) 1);

      let taken = vector |> Vector.take (count - 1);
      expect @@ Vector.toSeq @@ taken |> toBeEqualToSeqOfInt (Seq.inRange 0 (Some (count - 1)) 1);
    }),
    it (sprintf "skip with %i elements" count) (fun () => {
      let vector = Seq.inRange 0 (Some count) 1 |> Seq.reduce (fun acc i =>
        acc |> Vector.addLast i
      ) Vector.empty;

      let skipped = vector |> Vector.skip 1;
      expect @@ Vector.toSeq @@ skipped |> toBeEqualToSeqOfInt (Seq.inRange 1 (Some (count - 1)) 1);

      let skipped = vector |> Vector.skip (count / 2);
      expect @@ Vector.toSeq @@ skipped |> toBeEqualToSeqOfInt (Seq.inRange (count / 2) (Some (count / 2)) 1);

      let skipped = vector |> Vector.skip (count - 1);
      expect @@ Vector.toSeq @@ skipped |> toBeEqualToSeqOfInt (Seq.return (count - 1));
    }),
    it (sprintf "range with %i elements" count) (fun () => {
      let vector = Seq.inRange 0 (Some count) 1 |> Seq.reduce (fun acc i =>
        acc |> Vector.addLast i
      ) Vector.empty;

      let rangeOneToCountMinusTwo = vector |> Vector.range 1 (count - 2 |> Option.return);
      expect @@ Vector.toSeq @@ rangeOneToCountMinusTwo
        |> toBeEqualToSeqOfInt (Seq.inRange 1 (count - 2 |> Option.return) 1);
    }),
    it (sprintf "init with %i elements" count) (fun () => {
      let vector = Vector.init count (fun i => i);
      vector |> Vector.forEachWithIndex (fun i v => {
        expect i |> toBeEqualToInt v
      });
    }),
    it (sprintf "forEachWithIndex with %i elements" count) (fun () => {
      let counted = ref 0;
      let seq = Seq.inRange (count - 1) (Some count) (-1);
      let result = Vector.fromSeqReversed seq;
      result |> Vector.forEachWithIndex (fun i v => {
        expect i |> toBeEqualToInt v;
        expect i |> toBeEqualToInt !counted;
        counted := !counted + 1;
      });
    }),
    it (sprintf "forEachReverseWithIndex with %i elements" count) (fun () => {
      let counted = ref (count - 1);
      let seq = Seq.inRange 0 (Some count) 1;
      let result = Vector.fromSeq seq;
      result |> Vector.forEachReverseWithIndex (fun i v => {
        expect i |> toBeEqualToInt v;
        expect i |> toBeEqualToInt !counted;
        counted := !counted - 1;
      });
    }),
    it (sprintf "everyWithIndex with %i elements" count) (fun () => {
      let previousIndex = ref (-1);
      Seq.concat [Seq.return false, Seq.repeat true (Some (count - 1))]
        |> Seq.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.everyWithIndex (fun i v => {
            expect i |> toBeEqualToInt (!previousIndex + 1);
            previousIndex := i;
            v
          })
        |> expect |> toBeEqualToFalse;

      let previousIndex = ref (-1);
      Seq.concat [Seq.repeat true (Some (count - 1)), Seq.return false]
        |> Seq.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.everyWithIndex (fun i v => {
            expect i |> toBeEqualToInt (!previousIndex + 1);
            previousIndex := i;
            v
          })
        |> expect |> toBeEqualToFalse;

      let previousIndex = ref (-1);
      Seq.concat [
        Seq.repeat true (Some (count / 2 - 1)),
        Seq.return false,
        Seq.repeat true (Some (count / 2 - 1)),
      ]
        |> Seq.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.everyWithIndex (fun i v => {
            expect i |> toBeEqualToInt (!previousIndex + 1);
            previousIndex := i;
            v
          })
        |> expect |> toBeEqualToFalse;

      let previousIndex = ref (-1);
      Seq.repeat true (Some count)
        |> Seq.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.everyWithIndex (fun i v => {
            expect i |> toBeEqualToInt (!previousIndex + 1);
            previousIndex := i;
            v
          })
        |> expect |> toBeEqualToTrue;
    }),
    it (sprintf "findWithIndex and tryFindWithIndex in %i elements" count) (fun () => {
      let vector = Seq.inRange 0 (Some count) 1
        |> Seq.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty;

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
      Seq.concat [Seq.repeat false (Some (count - 1)), Seq.return true]
        |> Seq.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.noneWithIndex (fun i v => {
            expect i |> toBeEqualToInt (!previousIndex + 1);
            previousIndex := i;
            v;
          })
        |> expect |> toBeEqualToFalse;

      let previousIndex = ref (-1);
      Seq.concat [Seq.return true, Seq.repeat false (Some (count - 1))]
        |> Seq.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.noneWithIndex (fun i v => {
            expect i |> toBeEqualToInt (!previousIndex + 1);
            previousIndex := i;
            v;
          })
        |> expect |> toBeEqualToFalse;

      let previousIndex = ref (-1);
      Seq.concat [
        Seq.repeat false (Some (count / 2 - 1)),
        Seq.return true,
        Seq.repeat false (Some (count / 2 - 1)),
      ]
        |> Seq.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.noneWithIndex (fun i v => {
            expect i |> toBeEqualToInt (!previousIndex + 1);
            previousIndex := i;
            v;
          })
        |> expect |> toBeEqualToFalse;

      let previousIndex = ref (-1);
      Seq.repeat false (Some count)
        |> Seq.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.noneWithIndex (fun i v => {
            expect i |> toBeEqualToInt (!previousIndex + 1);
            previousIndex := i;
            v;
          })
        |> expect |> toBeEqualToTrue;
    }),
    it (sprintf "someWithIndex with %i elements" count) (fun () => {
      let previousIndex = ref (-1);
      Seq.concat [Seq.repeat false (Some (count - 1)), Seq.return true]
        |> Seq.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.someWithIndex (fun i v => {
            expect i |> toBeEqualToInt (!previousIndex + 1);
            previousIndex := i;
            v;
          })
        |> expect |> toBeEqualToTrue;

      let previousIndex = ref (-1);
      Seq.concat [Seq.return true, Seq.repeat false (Some (count - 1))]
        |> Seq.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.someWithIndex (fun i v => {
            expect i |> toBeEqualToInt (!previousIndex + 1);
            previousIndex := i;
            v;
          })
        |> expect |> toBeEqualToTrue;

      let previousIndex = ref (-1);
      Seq.concat [
        Seq.repeat false (Some (count / 2 - 1)),
        Seq.return true,
        Seq.repeat false (Some (count / 2 - 1)),
      ]
        |> Seq.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.someWithIndex (fun i v => {
            expect i |> toBeEqualToInt (!previousIndex + 1);
            previousIndex := i;
            v;
          })
        |> expect |> toBeEqualToTrue;

      let previousIndex = ref (-1);
      Seq.repeat false (Some count)
        |> Seq.reduce (fun acc i => acc |> Vector.addFirst i) Vector.empty
        |> Vector.someWithIndex (fun i v => {
            expect i |> toBeEqualToInt (!previousIndex + 1);
            previousIndex := i;
            v;
          })
        |> expect |> toBeEqualToFalse;
    }),
    it (sprintf "insertAt with %i elements" count) (fun () => {
      let result = Seq.inRange 1 (Some count) 2 |> Seq.reduce
        (fun acc i => acc |> Vector.insertAt i 1)
        (Vector.fromSeq @@ Seq.repeat 0 @@ (Some count));

      expect @@ Vector.toSeq @@ result
        |> toBeEqualToSeqOfInt (Seq.repeat [0, 1] (Some count) |> Seq.flatMap List.toSeq);
    }),
    it (sprintf "removeAt with %i elements" count) (fun () => {
      let initialValue = Seq.repeat [0, 1] (Some count)
        |> Seq.flatMap List.toSeq
        |> Vector.fromSeq;

      let result = Seq.inRange 1 (Some count) 1 |> Seq.reduce
        (fun acc i => acc |> Vector.removeAt i)
        initialValue;

      expect @@ Vector.toSeq @@ result
        |> toBeEqualToSeqOfInt (Seq.repeat 0 (Some count));
    }),
    describe (sprintf "concat with %i elements" count) [
      it "balanced segments" (fun () => {
        let segmentCount = count / 2;
        let vecFirst = Seq.repeat 0 (Some segmentCount) |> Vector.fromSeq;
        let vecSecond = Seq.repeat 1 (Some segmentCount) |> Vector.fromSeq;

        Vector.concat [vecFirst, vecSecond]
        |> Vector.toSeq
        |> expect
        |> toBeEqualToSeqOfInt @@ Seq.concat @@ [
            Seq.repeat 0 (Some segmentCount),
            Seq.repeat 1 (Some segmentCount),
          ];
      }),
      it "left larger than right" (fun () => {
        let segmentRightCount = count / 4;
        let segmentLeftCount = count - segmentRightCount;

        let vecLeft = Seq.repeat 0 (Some segmentLeftCount) |> Vector.fromSeq;
        let vecRight = Seq.repeat 1 (Some segmentRightCount) |> Vector.fromSeq;

        Vector.concat [vecLeft, vecRight]
        |> Vector.toSeq
        |> expect
        |> toBeEqualToSeqOfInt @@ Seq.concat @@ [
            Seq.repeat 0 (Some segmentLeftCount),
            Seq.repeat 1 (Some segmentRightCount),
          ];
      }),
      it "right larger than left" (fun () => {
        let segmentLeftCount = count / 4;
        let segmentRightCount = count - segmentLeftCount;

        let vecLeft = Seq.repeat 0 (Some segmentLeftCount) |> Vector.fromSeq;
        let vecRight = Seq.repeat 1 (Some segmentRightCount) |> Vector.fromSeq;

        Vector.concat [vecLeft, vecRight]
        |> Vector.toSeq
        |> expect
        |> toBeEqualToSeqOfInt @@ Seq.concat @@ [
            Seq.repeat 0 (Some segmentLeftCount),
            Seq.repeat 1 (Some segmentRightCount),
          ];
      }),
    ],
    it (sprintf "indexOf with %i elements" count) (fun () => {
      let vec = Seq.inRange 0 (Some count) 1 |> Vector.fromSeq;
      let f pos v => v == pos;
      expect (Vector.indexOf (f (count / 2)) vec) |> toBeEqualToInt (count / 2);
      defer (fun () => Vector.indexOf (f (-1)) vec) |> throws;
    }),
    it (sprintf "indexOfWithIndex with %i elements" count) (fun () => {
      let vec = Seq.inRange 0 (Some count) 1 |> Vector.fromSeq;
      let f pos i _ => i == pos;
      expect (Vector.indexOfWithIndex (f (count / 2)) vec) |> toBeEqualToInt (count / 2);
      defer (fun () => Vector.indexOfWithIndex (f (-1)) vec) |> throws;
    }),
    it (sprintf "tryIndexOf with %i elements" count) (fun () => {
      let vec = Seq.inRange 0 (Some count) 1 |> Vector.fromSeq;
      let f pos v => v == pos;
      expect (Vector.tryIndexOf (f (count / 2)) vec) |> toBeEqualToSomeOfInt (count / 2);
      expect (Vector.tryIndexOf (f (-1)) vec) |> toBeEqualToNoneOfInt;
    }),
    it (sprintf "tryIndexOfWithIndex with %i elements" count) (fun () => {
      let vec = Seq.inRange 0 (Some count) 1 |> Vector.fromSeq;
      let f pos i _ => i == pos;
      expect (Vector.tryIndexOfWithIndex (f (count / 2)) vec) |> toBeEqualToSomeOfInt (count / 2);
      expect (Vector.tryIndexOfWithIndex (f (-1)) vec) |> toBeEqualToNoneOfInt;
    }),
    describe (sprintf "toMap with %i elements" count) [
      it "containsWith" (fun () => {
        let map = Seq.inRange 0 (Some count) 1 |> Vector.fromSeq |> Vector.toMap;
        Seq.inRange 0 (Some count) 1 |> Seq.forEach (fun i => {
          expect (map |> Map.contains i i) |> toBeEqualToTrue;
        });

        expect (map |> Map.contains (-1) (-1)) |> toBeEqualToFalse;
        expect (map |> Map.contains count count) |> toBeEqualToFalse;
        expect (map |> Map.contains 0 1) |> toBeEqualToFalse;
      }),
      it "containsKey" (fun () => {
        let map = Seq.inRange 0 (Some count) 1 |> Vector.fromSeq |> Vector.toMap;
        Seq.inRange 0 (Some count) 1 |> Seq.forEach (fun i => {
          expect (map |> Map.containsKey i) |> toBeEqualToTrue;
        });

        expect (map |> Map.containsKey (-1)) |> toBeEqualToFalse;
        expect (map |> Map.containsKey count) |> toBeEqualToFalse;
      }),
      it "count" (fun () => {
        let map = Seq.inRange 0 (Some count) 1 |> Vector.fromSeq |> Vector.toMap;
        expect (map |> Map.count) |> toBeEqualToInt count;
      }),
      it "every" (fun () => {
        let map = Seq.inRange 0 (Some count) 1 |> Vector.fromSeq |> Vector.toMap;
        expect (map |> Map.every (fun i v => i == v)) |> toBeEqualToTrue;
        expect (map |> Map.every (fun i v => i != v)) |> toBeEqualToFalse;
      }),
      it "find" (fun () => {
        let map = Seq.inRange 0 (Some count) 1 |> Vector.fromSeq |> Vector.toMap;
        expect (map |> Map.find (fun i v => i == v)) |> toBeEqualTo (fun _ => "") (0, 0);
        defer (fun () => map |> Map.find (fun i v => i != v)) |> throws;
      }),
      it "forEach" (fun () => {
        let map = Seq.inRange 0 (Some count) 1 |> Vector.fromSeq |> Vector.toMap;
        let loopCount = ref 0;
        map |> Map.forEach (fun i v => {
          expect i |> toBeEqualToInt !loopCount;
          expect i |> toBeEqualToInt v;
          loopCount := !loopCount + 1;
        });

        expect !loopCount |> toBeEqualToInt count;
      }),
      it "get" (fun () => {
        let map = Seq.inRange 0 (Some count) 1 |> Vector.fromSeq |> Vector.toMap;
        Seq.inRange 0 (Some count) 1 |> Seq.forEach (fun i => {
          expect (map |> Map.get i) |> toBeEqualToInt i;
        });

        defer (fun () => map |> Map.get (-1)) |> throws;
        defer (fun () => map |> Map.get count) |> throws;
      }),
      it "keys" (fun () => {
        let map = Seq.inRange 0 (Some count) 1 |> Vector.fromSeq |> Vector.toMap;
        expect (map |> Map.keys |> Set.toSeq) |> toBeEqualToSeqOfInt (Seq.inRange 0 (Some count) 1);
      }),
      it "none" (fun () => {
        let map = Seq.inRange 0 (Some count) 1 |> Vector.fromSeq |> Vector.toMap;
        expect (map |> Map.none (fun i v => i != v)) |> toBeEqualToTrue;
        expect (map |> Map.none (fun i v => i == v)) |> toBeEqualToFalse;
      }),
      it "reduce" (fun () => {
        let map = Seq.inRange 0 (Some count) 1 |> Vector.fromSeq |> Vector.toMap;
        let reduced = map |> Map.reduce (fun acc _ _ => acc + 1) 0;
        expect reduced |> toBeEqualToInt count;
      }),
      it "some" (fun () => {
        let map = Seq.inRange 0 (Some count) 1 |> Vector.fromSeq |> Vector.toMap;
        expect (map |> Map.some (fun i v => i == v)) |> toBeEqualToTrue;
        expect (map |> Map.some (fun i v => i != v)) |> toBeEqualToFalse;
      }),
      it "toSeq" (fun () => {
        let map = Seq.inRange 0 (Some count) 1 |> Vector.fromSeq |> Vector.toMap;
        map |> Map.toSeq |> Seq.forEach (fun (i, v) => {
          expect i |> toBeEqualToInt v;
        });
      }),
      it "tryFind" (fun () => {
        let map = Seq.inRange 0 (Some count) 1 |> Vector.fromSeq |> Vector.toMap;
        expect (map |> Map.tryFind (fun i v => i == v)) |> toBeEqualToSome (fun _ => "") (0, 0);
        expect (map |> Map.tryFind (fun i v => i != v)) |> toBeEqualToNone (fun _ => "");
      }),
      it "tryGet" (fun () => {
        let map = Seq.inRange 0 (Some count) 1 |> Vector.fromSeq |> Vector.toMap;
        Seq.inRange 0 (Some count) 1 |> Seq.forEach (fun i => {
          expect (map |> Map.tryGet i) |> toBeEqualToSomeOfInt i;
        });

        expect (map |> Map.tryGet (-1)) |> toBeEqualToNoneOfInt;
        expect (map |> Map.tryGet count) |> toBeEqualToNoneOfInt;
      }),
      it "values" (fun () => {
        let map = Seq.inRange 0 (Some count) 1 |> Vector.fromSeq |> Vector.toMap;
        expect (map |> Map.values) |> toBeEqualToSeqOfInt (Seq.inRange 0 (Some count) 1);
      }),
    ],
    ...dequeTests
  ]
};
