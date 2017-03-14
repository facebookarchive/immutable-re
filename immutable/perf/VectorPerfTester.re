open Immutable;
open Printf;
open ReUnit;
open ReUnit.Test;

let generateTests
    (getTestData: unit => 'vector)
    (empty: unit => 'vector)
    (add: int  => 'vector => 'vector)
    (update: int => int => 'vector => 'vector)
    (removeLast: 'vector => 'vector)
    (tryGet: int => 'vector => option int)
    (n: int): list Test.t => [
  it (sprintf "add %i elements" n) (fun () => {
    let src = ContiguousIntSet.create 0 n;
    src |> ContiguousIntSet.reduce (fun acc i => acc |> add i) (empty ()) |> ignore;
  }),
  it (sprintf "vector with %i elements, removeLast %i elements" n (n / 2)) (fun () => {
    ContiguousIntSet.create 0 (n / 2)
      |> ContiguousIntSet.reduce (fun acc _ => acc |> removeLast) (getTestData ()) |> ignore;
  }),
  it (sprintf "vector with %i elements, update %i elements alternating" n (n / 2)) (fun () => {
    Seq.inRange 0 (Some (n / 2)) 2
      |> Seq.reduce (fun acc i => acc |> update i (n - i)) (getTestData ()) |> ignore;
  }),
  it (sprintf "tryGet %i values" n) (fun () => {
    let vec = getTestData ();
    ContiguousIntSet.create 0 n
      |> ContiguousIntSet.forEach
        (fun i => vec |> tryGet i |> ignore);
  }),
];

let test (n: int) (count: int): Test.t => {
  let indexes = ContiguousIntSet.create 0 count;

  let mutableArray = Array.init count (fun i => i);

  let list = indexes |> ContiguousIntSet.toSeq |> List.fromSeqReversed;
  let stack = indexes |> ContiguousIntSet.toSeq |> Stack.fromSeqReversed;
  let vector = indexes
    |> ContiguousIntSet.reduce (fun acc i => acc |> TransientVector.addLast i) (TransientVector.empty ())
    |> TransientVector.persist;

  let mutableArray = Array.init count (fun i => i);

  let list = indexes |> ContiguousIntSet.toSeq |> List.fromSeqReversed;
  let stack = indexes |> ContiguousIntSet.toSeq |> Stack.fromSeqReversed;

  let testGroup = [
    describe "CamlMutableArray" (
      generateTests
        (fun () => mutableArray)
        (fun () => mutableArray)
        (fun a v => { v.(0) = a; v })
        (fun i a v => { v.(i) = a; v })
        (fun v => { v.(0) |> ignore; v })
        (fun i v => { Some (v.(i)) })
        count
    ),
    describe "List" (
      generateTests
        (fun () => list)
        (fun () => [])
        List.addFirst
        (fun _ _ v => v)
        List.removeFirst
        (fun _ _ => None)
        count
    ),
    describe "Stack" (
      generateTests
        (fun () => stack)
        (fun () => Stack.empty)
        Stack.addFirst
        (fun _ _ v => v)
        Stack.removeFirst
        (fun _ _ => None)
        count
    ),
    describe "Vector" (
      generateTests
        (fun () => vector)
        (fun () => Vector.empty)
        Vector.addLast
        Vector.update
        Vector.removeLast
        Vector.tryGet
        count
    ),
    describe "TransientVector" (
      generateTests
        (fun () => vector |> Vector.mutate)
        (fun () => Vector.empty |> Vector.mutate)
        TransientVector.addLast
        TransientVector.update
        TransientVector.removeLast
        TransientVector.tryGet
        count
    ),
  ];

  let tests = Seq.repeat testGroup (Some n) |> Seq.flatMap List.toSeq |> List.fromSeqReversed;
  describe "VectorPerf" tests;
};
