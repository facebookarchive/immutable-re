/*open Immutable;
open Printf;
open ReUnit;
open ReUnit.Expect;
open ReUnit.Test;

let test
    (empty: unit => 'map)
    (put: int => int => 'map => 'map)
    (remove: int => 'map => 'map)
    (tryGet: int => 'map => option int)
    (n: int): list Test.t => [
  it (sprintf "put %i elements" n) (fun () => {
    let src = IntRange.create 0 n |> IntRange.toSeq |> Seq.map (Hash.random ());

    let (_, mapOfSizeN) = src
      |> Seq.scan
        (fun (_, acc) i => (i, acc |> put i i))
        (0, empty ())
      |> Seq.doOnNext(fun (i, acc) =>
        expect (acc |> tryGet i) |> toBeEqualToSomeOfInt i
      )
      |> Seq.tryLast |> Option.get;

    src |> Seq.forEach (fun i =>
      expect (mapOfSizeN |> tryGet i) |> toBeEqualToSomeOfInt i
    )
  }),

  it (sprintf "remove %i elements" n) (fun () => {
    let src = IntRange.create 0 n |> IntRange.toSeq
    let mapOfSizeN = src |> Seq.reduce (fun acc i => acc |> put i i) (empty ());

    /* FIMXE: Maybe add Seq.sample, Seq.filteri */
    let removed = src
      |> Seq.map(fun i => (i, i))
      |> Seq.filter (fun (i, v) => i mod 3 == 0)
      |> Seq.map (fun (i, v) => v);

    let remaining = src
      |> Seq.map (fun i => (i, i))
      |> Seq.filter (fun (i, v) => i mod 3 != 0)
      |> Seq.map (fun (i, v) => v);

    let mapOfSizeNdiv3 = removed |> Seq.reduce (fun acc i => acc |> remove i) mapOfSizeN;

    removed |> Seq.forEach(fun i =>
      expect (mapOfSizeNdiv3 |> tryGet i) |> toBeEqualToNoneOfInt
    );

    remaining |> Seq.forEach(fun i =>
      expect (mapOfSizeNdiv3 |> tryGet i) |> toBeEqualToSomeOfInt i
    );
  }),
];*/
