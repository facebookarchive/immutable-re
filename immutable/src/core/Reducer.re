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
  let first: t => (option a);
  let firstOrRaise: t => a;
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
  let first: t 'a => (option 'a);
  let firstOrRaise: t 'a => 'a;
  let forEach: while_::('a => bool)? => ('a => unit) => (t 'a) => unit;
  let none: ('a => bool) => (t 'a) => bool;
  let some: ('a => bool) => (t 'a) => bool;
};

let module Make = fun (Reduceable: Reduceable.S) => {
  type a = Reduceable.a;
  type t = Reduceable.t;

  let increment acc _ => acc + 1;
  let count (reduceable: t): int =>
    reduceable |> Reduceable.reduce increment 0;

  let every (f: a => bool) (reduceable: t): bool =>
    reduceable |> Reduceable.reduce while_::(fun acc _ => acc) (fun _ => f) true;

  let find (f: a => bool) (reduceable: t): (option a) =>
    reduceable |> Reduceable.reduce while_::(fun acc _ => Option.isEmpty acc) (
      fun _ next => if (f next) (Some next) else None
    ) None;

  let findOrRaise (f: a => bool) (reduceable: t): a =>
    find f reduceable |> Option.firstOrRaise;

  let first (reduceable: t): (option a) =>
    reduceable |> Reduceable.reduce
      while_::(fun acc _ => Option.isEmpty acc)
      (fun _ => Option.return)
      None;

  let firstOrRaise (reduceable: t): a =>
    reduceable |> first |> Option.firstOrRaise;

  let forEach while_::(predicate: a => bool)=Functions.alwaysTrue (f: a => unit) (reduceable: t) =>
    if (predicate === Functions.alwaysTrue ) {
      reduceable |> Reduceable.reduce (fun _ => f) ();
    }
    else reduceable |> Reduceable.reduce while_::(fun _ => predicate) (fun _ => f) ();

  let none (f: a => bool) (reduceable: t): bool =>
    reduceable |> Reduceable.reduce while_::(fun acc _ => acc) (fun _ => f >> not) true;

  let some (f: a => bool) (reduceable: t): bool =>
    reduceable |> Reduceable.reduce while_::(fun acc _ => not acc) (fun _ => f) false;
};

let module Make1 = fun (Reduceable: Reduceable.S1) => {
  type t 'a = Reduceable.t 'a;

  let increment acc _ => acc + 1;
  let count (reduceable: t 'a): int =>
    reduceable |> Reduceable.reduce increment 0;

  let every (f: 'a => bool) (reduceable: t 'a): bool =>
    reduceable |> Reduceable.reduce while_::(fun acc _ => acc) (fun _ => f) true;

  let find (f: 'a => bool) (reduceable: t 'a): (option 'a) =>
    reduceable |> Reduceable.reduce while_::(fun acc _ => Option.isEmpty acc) (
      fun _ next => if (f next) (Some next) else None
    ) None;

  let findOrRaise (f: 'a => bool) (reduceable: t 'a): 'a =>
    find f reduceable |> Option.firstOrRaise;

  let first (reduceable: t 'a): (option 'a) =>
    reduceable |> Reduceable.reduce
      while_::(fun acc _ => Option.isEmpty acc)
      (fun _ => Option.return)
      None;

  let firstOrRaise (reduceable: t 'a): 'a =>
    reduceable |> first |> Option.firstOrRaise;

  let forEach while_::(predicate: 'a => bool)=Functions.alwaysTrue (f: 'a => unit) (reduceable: t 'a) =>
    if (predicate === Functions.alwaysTrue ) {
      reduceable |> Reduceable.reduce (fun _ => f) ();
    }
    else reduceable |> Reduceable.reduce while_::(fun _ => predicate) (fun _ => f) ();

  let none (f: 'a => bool) (reduceable: t 'a): bool =>
    reduceable |> Reduceable.reduce while_::(fun acc _ => acc) (fun _ => f >> not) true;

  let some (f: 'a => bool) (reduceable: t 'a): bool =>
    reduceable |> Reduceable.reduce while_::(fun acc _ => not acc) (fun _ => f) false;
};
