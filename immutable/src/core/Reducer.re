/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Functions.Operators;

module type S = {
  type a;
  type t;

  let count: t => int;
  let every: (a => bool) => t => bool;
  let find: (a => bool) => t => (option a);
  let findOrRaise: (a => bool) => t => a;
  let forEach: while_::(a => bool)? => (a => unit) => t => unit;
  let none: (a => bool) => t => bool;
  let some: (a => bool) => t => bool;
};

module type S1 = {
  type t 'a;

  let count: t 'a => int;
  let every: ('a => bool) => (t 'a) => bool;
  let find: ('a => bool) => (t 'a) => (option 'a);
  let findOrRaise: ('a => bool) => (t 'a) => 'a;
  let forEach: while_::('a => bool)? => ('a => unit) => (t 'a) => unit;
  let none: ('a => bool) => (t 'a) => bool;
  let some: ('a => bool) => (t 'a) => bool;
};

let module Make = fun (Reduceable: Reduceable.S) => {
  type a = Reduceable.a;
  type t = Reduceable.t;

  let increment acc _ => acc + 1;
  let count (reducer: t): int =>
    reducer |> Reduceable.reduce increment 0;

  let every (f: a => bool) (reducer: t): bool =>
    reducer |> Reduceable.reduce while_::(fun acc _ => acc) (fun _ => f) true;

  let find (f: a => bool) (reducer: t): (option a) =>
    reducer |> Reduceable.reduce while_::(fun acc _ => Option.isEmpty acc) (
      fun _ next => if (f next) (Some next) else None
    ) None;

  let findOrRaise (f: a => bool) (reducer: t): a =>
    find f reducer |> Option.firstOrRaise;

  let forEach while_::(predicate: a => bool)=Functions.alwaysTrue (f: a => unit) (reducer: t) =>
    if (predicate === Functions.alwaysTrue ) {
      reducer |> Reduceable.reduce (fun _ => f) ();
    }
    else reducer |> Reduceable.reduce while_::(fun _ => predicate) (fun _ => f) ();

  let none (f: a => bool) (reducer: t): bool =>
    reducer |> Reduceable.reduce while_::(fun acc _ => acc) (fun _ => f >> not) true;

  let some (f: a => bool) (reducer: t): bool =>
    reducer |> Reduceable.reduce while_::(fun acc _ => not acc) (fun _ => f) false;
};

let module Make1 = fun (Reduceable: Reduceable.S1) => {
  type t 'a = Reduceable.t 'a;

  let increment acc _ => acc + 1;
  let count (reducer: t 'a): int =>
    reducer |> Reduceable.reduce increment 0;

  let every (f: 'a => bool) (reducer: t 'a): bool =>
    reducer |> Reduceable.reduce while_::(fun acc _ => acc) (fun _ => f) true;

  let find (f: 'a => bool) (reducer: t 'a): (option 'a) =>
    reducer |> Reduceable.reduce while_::(fun acc _ => Option.isEmpty acc) (
      fun _ next => if (f next) (Some next) else None
    ) None;

  let findOrRaise (f: 'a => bool) (reducer: t 'a): 'a =>
    find f reducer |> Option.firstOrRaise;

  let forEach while_::(predicate: 'a => bool)=Functions.alwaysTrue (f: 'a => unit) (reducer: t 'a) =>
    if (predicate === Functions.alwaysTrue ) {
      reducer |> Reduceable.reduce (fun _ => f) ();
    }
    else reducer |> Reduceable.reduce while_::(fun _ => predicate) (fun _ => f) ();

  let none (f: 'a => bool) (reducer: t 'a): bool =>
    reducer |> Reduceable.reduce while_::(fun acc _ => acc) (fun _ => f >> not) true;

  let some (f: 'a => bool) (reducer: t 'a): bool =>
    reducer |> Reduceable.reduce while_::(fun acc _ => not acc) (fun _ => f) false;
};
