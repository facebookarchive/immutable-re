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
    (empty: unit => 'vector)
    (add: int  => 'vector => 'vector)
    (tryGet: int => 'vector => option int)
    (update: int => int => 'vector => 'vector)
    (removeLast: 'vector => 'vector)
    (n: int): (list Test.t) => [
  it (sprintf "add %i elements" n) (fun () => {
    let src = Seq.inRange 0 (Some n) 1;
    let vector = src |> Seq.reduce (fun acc i => acc |> add i) (empty ());
    src |> Seq.forEach (fun i =>
      expect (vector |> tryGet i) |> toBeEqualToSomeOfInt i
    );
  }),
  it (sprintf "add %i elements and pop %i elements" n (n / 2)) (fun () => {
    let src = Seq.inRange 0 (Some n) 1;
    let vector = src |> Seq.reduce (fun acc i => acc |> add i) (empty ());
    let poppedVector = Seq.repeat () (Some (n / 2))
      |> Seq.reduce (fun acc _ => acc |> removeLast) vector;
    Seq.inRange 0 (Some (n / 2 - 1)) 1 |> Seq.forEach (fun i =>
      expect (poppedVector |> tryGet i) |> toBeEqualToSomeOfInt i
    );
  }),
  it (sprintf "add %i elements and update %i elements alternating" n (n / 2)) (fun () => {
    let src = Seq.inRange 0 (Some n) 1;
    let vector = src |> Seq.reduce (fun acc i => acc |> add i) (empty ());
    let updatedVector = Seq.inRange 0 (Some (n / 2)) 2
      |> Seq.reduce (fun acc i => acc |> update i (n - i)) vector;
    src |> Seq.forEach (fun i => (i mod 2) == 0
      ? expect (updatedVector |> tryGet i) |> toBeEqualToSomeOfInt (n - i)
      : expect (updatedVector |> tryGet i) |> toBeEqualToSomeOfInt i
    );
  }),

];
