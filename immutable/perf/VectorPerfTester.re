open Immutable;
open Printf;
open ReUnit;
open ReUnit.Expect;
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
    let src = Seq.inRange 0 (Some n) 1;
    src |> Seq.reduce (fun acc i => acc |> add i) (empty ()) |> ignore;
  }),
  it (sprintf "vector with %i elements, pop %i elements" n (n / 2)) (fun () => {
    Seq.inRange 0 (Some (n / 2)) 1
      |> Seq.reduce (fun acc _ => acc |> removeLast) (getTestData ()) |> ignore;
  }),
  it (sprintf "vector with %i elements, update %i elements alternating" n (n / 2)) (fun () => {
    Seq.inRange 0 (Some (n / 2)) 2
      |> Seq.reduce (fun acc i => acc |> update i (n - i)) (getTestData ()) |> ignore;
  }),
  it (sprintf "tryGet %i values" n) (fun () => {
    let vec = getTestData ();
    Seq.inRange 0 (Some n) 1 |> Seq.forEach (fun i => vec |> tryGet i |> ignore);
  }),
];

let test (n: int): Test.t => {
  let indexes = Seq.inRange 0 (Some n) 1;
  let vector = indexes |> Seq.reduce (fun acc i => acc |> Vector.addLast i) Vector.empty;

  describe "VectorPerf" [
    describe "Vector" (
      generateTests
        (fun () => vector)
        (fun () => Vector.empty)
        Vector.addLast
        Vector.update
        Vector.removeLast
        Vector.tryGet
        n
    ),
    describe "TransientVector" (
      generateTests
        (fun () => vector |> Vector.mutate)
        (fun () => Vector.empty |> Vector.mutate)
        TransientVector.addLast
        TransientVector.update
        TransientVector.removeLast
        TransientVector.tryGet
        n
    ),
  ];
};
