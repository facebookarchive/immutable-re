/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Immutable;
open ReUnit;
open ReUnit.Test;
open VectorTester;

let n = 10000;

let test = describe "Deque" [
  describe "TransientDeque" (
    VectorTester.test
      (fun () => Deque.empty |> Deque.mutate)
      TransientDeque.add
      TransientDeque.tryGet
      TransientDeque.update
      TransientDeque.removeLast
      n
  ),
  ...(
    VectorTester.test
      (fun () => Deque.empty)
      Deque.add
      Deque.tryGet
      Deque.update
      Deque.removeLast
      n
  )
];
