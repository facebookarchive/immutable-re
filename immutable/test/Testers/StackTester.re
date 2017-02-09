open Immutable;
open Printf;
open ReUnit;
open ReUnit.Expect;
open ReUnit.Test;

module type Stack = {
  type t 'a;

  let addFirst: 'a => (t 'a) => (t 'a);
  let addFirstAll: (Seq.t 'a) => (t 'a) => (t 'a);
  let compare: (Comparator.t (t 'a));
  let compareWith: (Comparator.t 'a) => (Comparator.t (t 'a));
  let count: (t 'a) => int;
  let empty: (t 'a);
  let equals: (Equality.t (t 'a));
  let equalsWith: (Equality.t 'a) => (Equality.t (t 'a));
  let every: ('a => bool) => (t 'a) => bool;
  let find: ('a => bool) => (t 'a) => 'a;
  let first: (t 'a) => 'a;
  let fromSeqReversed: (Seq.t 'a) => (t 'a);
  let hash: (Hash.t (t 'a));
  let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
  let isEmpty: t 'a => bool;
  let isNotEmpty: t 'a => bool;
  let mapReverse: ('a => 'b) => (t 'a) => (t 'b);
  let none: ('a => bool) => (t 'a) => bool;
  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let removeAll: (t 'a) => (t 'a);
  let removeFirst: (t 'a) => (t 'a);
  let reverse: (t 'a) => (t 'a);
  let some: ('a => bool) => (t 'a) => bool;
  let toSeq: (t 'a) => (Seq.t 'a);
  let tryFind: ('a => bool) => (t 'a) => (option 'a);
  let tryFirst: (t 'a) => (option 'a);
};

let test (count: int) (module Stack: Stack): (list Test.t) => [
  it (sprintf "addFirst and removeFirst %i elements" count) (fun () => {
    let empty = Stack.empty;

    expect (Stack.isNotEmpty empty) |> toBeEqualToFalse;
    expect (Stack.isEmpty empty) |> toBeEqualToTrue;
    expect (Stack.count empty) |> toBeEqualToInt 0;
    defer (fun () => empty |> Stack.first) |> throws;
    expect (empty |> Stack.tryFirst) |> toBeEqualToNoneOfInt;

    let stack = Seq.inRange 0 (Some count) 1 |> Seq.reduce (fun acc i => {
      let acc = acc |> Stack.addFirst i;

      expect (Stack.isNotEmpty acc) |> toBeEqualToTrue;
      expect (Stack.isEmpty acc) |> toBeEqualToFalse;
      expect (Stack.count acc) |> toBeEqualToInt (i + 1);
      expect (acc |> Stack.first) |> toBeEqualToInt i;
      expect (acc |> Stack.tryFirst) |> toBeEqualToSomeOfInt i;

      acc;
    }) empty;

    let shouldBeEmpty = Seq.inRange (count - 1) (Some count) (-1) |> Seq.reduce (fun acc i => {
      expect (Stack.isNotEmpty acc) |> toBeEqualToTrue;
      expect (Stack.isEmpty acc) |> toBeEqualToFalse;
      expect (Stack.count acc) |> toBeEqualToInt (i + 1);
      expect (acc |> Stack.first) |> toBeEqualToInt i;
      expect (acc |> Stack.tryFirst) |> toBeEqualToSomeOfInt i;
      acc |> Stack.removeFirst;
    }) stack;

    expect (Stack.isNotEmpty shouldBeEmpty) |> toBeEqualToFalse;
    expect (Stack.isEmpty shouldBeEmpty) |> toBeEqualToTrue;
    expect (Stack.count shouldBeEmpty) |> toBeEqualToInt 0;
    defer (fun () => shouldBeEmpty |> Stack.first) |> throws;
    expect (shouldBeEmpty |> Stack.tryFirst) |>  toBeEqualToNoneOfInt;

    let seqsEquality = Seq.equals (Stack.toSeq stack) (Seq.inRange (count - 1) (Some count) (-1));
    expect seqsEquality |> toBeEqualToTrue;
  }),

  it (sprintf "every with %i elements" count) (fun () => {
    Seq.concat [Seq.return false, Seq.repeat true (Some (count - 1))]
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty
      |> Stack.every (fun v => v)
      |> expect |> toBeEqualToFalse;

    Seq.concat [Seq.repeat true (Some (count - 1)), Seq.return false]
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty
      |> Stack.every (fun v => v)
      |> expect |> toBeEqualToFalse;

    Seq.concat [
      Seq.repeat true (Some (count / 2 - 1)),
      Seq.return false,
      Seq.repeat true (Some (count / 2 - 1)),
    ]
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty
      |> Stack.every (fun v => v)
      |> expect |> toBeEqualToFalse;

    Seq.repeat true (Some count)
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty
      |> Stack.every (fun v => v)
      |> expect |> toBeEqualToTrue;
  }),

  it (sprintf "none with %i elements" count) (fun () => {
    Seq.concat [Seq.repeat false (Some (count - 1)), Seq.return true]
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty
      |> Stack.none (fun v => v)
      |> expect |> toBeEqualToFalse;

    Seq.concat [Seq.return true, Seq.repeat false (Some (count - 1))]
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty
      |> Stack.none (fun v => v)
      |> expect |> toBeEqualToFalse;

    Seq.concat [
      Seq.repeat false (Some (count / 2 - 1)),
      Seq.return true,
      Seq.repeat false (Some (count / 2 - 1)),
    ]
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty
      |> Stack.none (fun v => v)
      |> expect |> toBeEqualToFalse;

    Seq.repeat false (Some count)
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty
      |> Stack.none (fun v => v)
      |> expect |> toBeEqualToTrue;
  }),

  it (sprintf "some with %i elements" count) (fun () => {
    Seq.concat [Seq.repeat false (Some (count - 1)), Seq.return true]
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty
      |> Stack.some (fun v => v)
      |> expect |> toBeEqualToTrue;

    Seq.concat [Seq.return true, Seq.repeat false (Some (count - 1))]
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty
      |> Stack.some (fun v => v)
      |> expect |> toBeEqualToTrue;

    Seq.concat [
      Seq.repeat false (Some (count / 2 - 1)),
      Seq.return true,
      Seq.repeat false (Some (count / 2 - 1)),
    ]
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty
      |> Stack.some (fun v => v)
      |> expect |> toBeEqualToTrue;

    Seq.repeat false (Some count)
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty
      |> Stack.some (fun v => v)
      |> expect |> toBeEqualToFalse;
  }),

  it (sprintf "mapReverse %i elements" count) (fun () => {
    Seq.inRange 0 (Some count) 1
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty
      |> Stack.mapReverse (fun i => i + 1)
      |> Stack.toSeq
      |> expect
      |> toBeEqualToSeq string_of_int (Seq.inRange 1 (Some count) 1);
  }),

  it (sprintf "reverse %i elements" count) (fun () => {
    let stack = Seq.inRange 0 (Some count) 1
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty;

    let reversed = stack |> Stack.reverse;

    reversed
      |> Stack.toSeq
      |> expect
      |> toBeEqualToSeq string_of_int (Seq.inRange 0 (Some count) 1);

    reversed
      |> Stack.reverse
      |> Stack.toSeq
      |> expect
      |> toBeEqualToSeq string_of_int (stack |> Stack.toSeq);
  }),

  it (sprintf "addFirst and removeAll %i elements" count) (fun () => {
    let stack = Seq.inRange 0 (Some count) 1
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty;

    expect (Stack.isNotEmpty stack) |> toBeEqualToTrue;
    expect (Stack.isEmpty stack) |> toBeEqualToFalse;
    expect (Stack.count stack) |> toBeEqualToInt count;

    let shouldBeEmpty = Stack.removeAll stack;
    expect (Stack.isNotEmpty shouldBeEmpty) |> toBeEqualToFalse;
    expect (Stack.isEmpty shouldBeEmpty) |> toBeEqualToTrue;
    expect (Stack.count shouldBeEmpty) |> toBeEqualToInt 0;
  }),

  it (sprintf "reduce with %i elements" count) (fun () => {
    /* FIXME: This test could be better by not using a single repeated value. */
    Seq.repeat 1 (Some count)
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty
      |> Stack.reduce (fun acc i => acc + i) 0
      |> expect
      |> toBeEqualToInt count;
  }),

  it (sprintf "find and tryFind in %i elements" count) (fun () => {
    let stack = Seq.inRange 0 (Some count) 1
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty;

    let find0 i => i == 0;
    let findCountMinusOne i => i == (count - 1);
    let findCountDividedByTwo i => i == (count / 2);

    expect (Stack.find find0 stack) |> toBeEqualToInt 0;
    expect (Stack.find findCountMinusOne stack) |> toBeEqualToInt (count - 1);
    expect (Stack.find findCountDividedByTwo stack) |> toBeEqualToInt (count / 2);

    expect (Stack.tryFind find0 stack) |> toBeEqualToSomeOfInt 0;
    expect (Stack.tryFind findCountMinusOne stack) |> toBeEqualToSomeOfInt (count - 1);
    expect (Stack.tryFind findCountDividedByTwo stack) |> toBeEqualToSomeOfInt (count / 2);

    defer (fun () => Stack.find Functions.alwaysFalse stack) |> throws;
    expect (Stack.tryFind Functions.alwaysFalse stack) |> toBeEqualToNoneOfInt;
  }),

  it (sprintf "compare %i elements" count) (fun () => {
    let orderingToString (ord: Ordering.t): string =>
      ord === Ordering.equal ? "Equals" :
      ord === Ordering.greaterThan ? "GreaterThan" :
      "LesserThan";

    let stackCount = Seq.inRange 0 (Some count) 1
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty;

    let stackCountPlusOne = Seq.inRange 0 (Some (count + 1)) 1
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty;

    let stackCountDup = Seq.inRange 0 (Some count) 1
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty;

    let stackGreaterCount = Seq.inRange 1 (Some count) 1
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty;

    expect (Stack.compare stackCount stackCount) |> toBeEqualTo orderingToString Ordering.equal;
    expect (Stack.compare stackCount stackCountDup) |> toBeEqualTo orderingToString Ordering.equal;
    expect (Stack.compare stackGreaterCount stackCount) |> toBeEqualTo orderingToString Ordering.greaterThan;
    expect (Stack.compare stackCount stackGreaterCount) |> toBeEqualTo orderingToString Ordering.lessThan;
    expect (Stack.compare stackCountPlusOne stackCount) |> toBeEqualTo orderingToString Ordering.greaterThan;
    expect (Stack.compare stackCount stackCountPlusOne) |> toBeEqualTo orderingToString Ordering.lessThan;
  }),

  it (sprintf "equals %i elements" count) (fun () => {
    let stackCountMinusOne = Seq.inRange 0 (Some (count - 1)) 1
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty;

    let stackCount = Seq.inRange 0 (Some count) 1
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty;

    let stackCountDup = Seq.inRange 0 (Some count) 1
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty;

    let stackCountPlusOne = Seq.inRange 0 (Some (count + 1)) 1
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty;

    expect (Stack.equals stackCount stackCount) |> toBeEqualToTrue;
    expect (Stack.equals stackCount stackCountDup) |> toBeEqualToTrue;
    expect (Stack.equals stackCount stackCountMinusOne) |> toBeEqualToFalse;
    expect (Stack.equals stackCount stackCountPlusOne) |> toBeEqualToFalse;
  }),

  it (sprintf "hash %i elements" count) (fun () => {
    let stackCountMinusOne = Seq.inRange 0 (Some (count - 1)) 1
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty;

    let stackCount = Seq.inRange 0 (Some count) 1
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty;

    let stackCountDup = Seq.inRange 0 (Some count) 1
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty;

    let stackCountPlusOne = Seq.inRange 0 (Some (count + 1)) 1
      |> Seq.reduce (fun acc i => acc |> Stack.addFirst i) Stack.empty;

    expect (Stack.hash stackCount) |> toBeEqualToInt (Stack.hash stackCount);
    expect (Stack.hash stackCount) |> toBeEqualToInt (Stack.hash stackCountDup);
    expect ((Stack.hash stackCount) != (Stack.hash stackCountMinusOne)) |> toBeEqualToTrue;
    expect ((Stack.hash stackCount) != (Stack.hash stackCountPlusOne)) |> toBeEqualToTrue;
  }),

  it (sprintf "addFirstAll with %i elements" count) (fun () => {
    let seq = Seq.inRange 0 (Some count) 1;
    let result = Stack.empty |> Stack.addFirstAll seq;

    (Stack.toSeq result)
      |> Seq.equals (Seq.inRange (count - 1) (Some count) (-1))
      |> expect
      |> toBeEqualToTrue;
  }),

  it (sprintf "fromSeqReversed with %i elements" count) (fun () => {
    let seq = Seq.inRange 0 (Some count) 1;
    let result = Stack.fromSeqReversed seq;

    (Stack.toSeq result)
      |> Seq.equals (Seq.inRange (count - 1) (Some count) (-1))
      |> expect
      |> toBeEqualToTrue;
  }),
];
