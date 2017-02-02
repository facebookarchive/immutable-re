open Immutable;
open Printf;
open ReUnit;
open ReUnit.Expect;
open ReUnit.Test;

module type Stack = {
  type t 'a;

  let addFirst: 'a => (t 'a) => (t 'a);
  let count: (t 'a) => int;
  let empty: (t 'a);
  let every: ('a => bool) => (t 'a) => bool;
  let first: (t 'a) => 'a;
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

  describe "predicates" [
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
  ],

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
];
