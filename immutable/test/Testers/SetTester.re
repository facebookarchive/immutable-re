/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Immutable;
open Printf;
open ReUnit;
open ReUnit.Expect;
open ReUnit.Test;

let test
    (empty: unit => 'set)
    (put: int => 'set => 'set)
    (remove: int => 'set => 'set)
    (contains: int => 'set => bool)
    (n: int): list Test.t => [
  it (sprintf "put %i elements" n) (fun () => {
    let src = Seq.inRange 0 (Some n) 1 |> Seq.map (Hash.random ());

    let (_, mapOfSizeN) = src
      |> Seq.scan
        (fun (hash, acc) i => (i, acc |> put i))
        (0, empty ())
      |> Seq.doOnNext(fun (i, acc) =>
        expect (acc |> contains i) |> toBeEqualToTrue
      )
      |> Seq.tryLast |> Option.get;

    src |> Seq.forEach (fun i =>
      expect (mapOfSizeN |> contains i) |> toBeEqualToTrue
    );
  }),

  it (sprintf "remove %i elements" n) (fun () => {
    let src = Seq.inRange 0 (Some n) 1;
    let mapOfSizeN = src |> Seq.reduce (fun acc i => acc |> put i) (empty ());

    /* FIMXE: Maybe add Seq.sample, Seq.filteri */
    let removed = src
      |> Seq.map (fun i => (i, i))
      |> Seq.filter (fun (i, v) => i mod 3 == 0)
      |> Seq.map (fun (i, v) => v);

    let remaining = src
      |> Seq.map (fun i => (i, i))
      |> Seq.filter (fun (i, v) => i mod 3 != 0)
      |> Seq.map (fun (i, v) => v);

    let mapOfSizeNdiv3 = removed |> Seq.reduce (fun acc i => acc |> remove i) mapOfSizeN;

    removed |> Seq.forEach(fun i =>
      expect (mapOfSizeNdiv3 |> contains i) |> toBeEqualToFalse
    );

    remaining |> Seq.forEach(fun i =>
      expect (mapOfSizeNdiv3 |> contains i) |> toBeEqualToTrue
    );
  }),
];
