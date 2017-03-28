/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

module type S1 = {
  type k;
  type t 'v;

  let count: (t 'v) => int;
  let every: (k => 'v => bool) => (t 'v) => bool;
  let find: (k => 'v => bool) => (t 'v) => (option (k, 'v));
  let findOrRaise: (k => 'v => bool) => (t 'v) => (k, 'v);
  let findKey: (k => 'v => bool) => (t 'v) => (option k);
  let findKeyOrRaise: (k => 'v => bool) => (t 'v) => k;
  let findValue: (k => 'v => bool) => (t 'v) => (option 'v);
  let findValueOrRaise: (k => 'v => bool) => (t 'v) => 'v;
  let first: (t 'v) => (option (k, 'v));
  let firstOrRaise: (t 'v) => (k, 'v);
  let forEach: while_::(k => 'v => bool)? => (k => 'v => unit) => (t 'v) => unit;
  let none: (k => 'v => bool) => (t 'v) => bool;
  let some: (k => 'v => bool) => (t 'v) => bool;
};

module type S2 = {
  type t 'k 'v;

  let count: (t 'k 'v) => int;
  let every: ('k => 'v => bool) => (t 'k 'v) => bool;
  let find: ('k => 'v => bool) => (t 'k 'v) => (option ('k, 'v));
  let findOrRaise: ('k => 'v => bool) => (t 'k 'v) => ('k, 'v);
  let findKey: ('k => 'v => bool) => (t 'k 'v) => (option 'k);
  let findKeyOrRaise: ('k => 'v => bool) => (t 'k 'v) => 'k;
  let findValue: ('k => 'v => bool) => (t 'k 'v) => (option 'v);
  let findValueOrRaise: ('k => 'v => bool) => (t 'k 'v) => 'v;
  let first: (t 'k 'v) => (option ('k, 'v));
  let firstOrRaise: (t 'k 'v) => ('k, 'v);
  let forEach: while_::('k => 'v => bool)? => ('k => 'v => unit) => (t 'k 'v) => unit;
  let none: ('k => 'v => bool) => (t 'k 'v) => bool;
  let some: ('k => 'v => bool) => (t 'k 'v) => bool;
};

let module Make1 = fun (KeyedReduceable: KeyedReduceable.S1) => {
  type k = KeyedReduceable.k;
  type t 'v = KeyedReduceable.t 'v;

  let increment acc _ _ => acc + 1;
  let count (reducer: t 'v): int =>
    reducer |> KeyedReduceable.reduce increment 0;

  let every (f: k => 'v => bool) (keyedReduceable: t 'v): bool =>
    keyedReduceable |> KeyedReduceable.reduce
      while_::(fun acc _ _ => acc)
      (fun _ => f)
      true;

  let find (f: k => 'v => bool) (keyedReduceable: t 'v): (option (k, 'v)) =>
    keyedReduceable |> KeyedReduceable.reduce
      while_::(fun acc _ _ => Option.isEmpty acc)
      (fun _ k v => if (f k v) (Some (k, v)) else None)
      None;

  let findOrRaise (f: k => 'v => bool) (keyedReduceable: t 'v): (k, 'v) =>
    find f keyedReduceable |> Option.firstOrRaise;

  let findKey (f: k => 'v => bool) (keyedReduceable: t 'v): (option k) =>
    keyedReduceable |> KeyedReduceable.reduce
      while_::(fun acc _ _ => Option.isEmpty acc)
      (fun _ k v => if (f k v) (Some k) else None)
      None;

  let findKeyOrRaise (f: k => 'v => bool) (keyedReduceable: t 'v): k =>
    findKey f keyedReduceable |> Option.firstOrRaise;

  let findValue (f: k => 'v => bool) (keyedReduceable: t 'v): (option 'v) =>
    keyedReduceable |> KeyedReduceable.reduce
      while_::(fun acc _ _ => Option.isEmpty acc)
      (fun _ k v => if (f k v) (Some v) else None)
      None;

  let findValueOrRaise (f: k => 'v => bool) (keyedReduceable: t 'v): 'v =>
    findValue f keyedReduceable |> Option.firstOrRaise;

  let first (keyedReduceable: t 'v): (option (k, 'v)) =>
    keyedReduceable |> KeyedReduceable.reduce
      while_::(fun acc _ _ => Option.isEmpty acc)
      (fun _ k v => Option.return (k, v))
      None;

  let firstOrRaise (keyedReduceable: t 'v): ('k, 'v) =>
    keyedReduceable |> first |> Option.firstOrRaise;

  let forEach
      while_::(predicate: k => 'v => bool)=Functions.alwaysTrue2
      (f: k => 'v => unit)
      (keyedReduceable: t 'v) =>
    keyedReduceable |> KeyedReduceable.reduce while_::(fun _ => predicate) (fun _ => f) ();

  let none (f: k => 'v => bool) (keyedReduceable: t 'v): bool =>
    keyedReduceable |> KeyedReduceable.reduce
      while_::(fun acc _ _ => acc)
      (fun _ k v => f k v |> not)
      true;

  let some (f: k => 'v => bool) (keyedReduceable: t 'v): bool =>
    keyedReduceable |> KeyedReduceable.reduce
      while_::(fun acc _ _ => not acc)
      (fun _ => f)
      false;
};

let module Make2 = fun (KeyedReduceable: KeyedReduceable.S2) => {
  type t 'k 'v = KeyedReduceable.t 'k 'v;

  let increment acc _ _ => acc + 1;
  let count (reducer: t 'k 'v): int =>
    reducer |> KeyedReduceable.reduce increment 0;

  let every (f: 'k => 'v => bool) (keyedReduceable: t 'k 'v): bool =>
    keyedReduceable |> KeyedReduceable.reduce
      while_::(fun acc _ _ => acc)
      (fun _ => f)
      true;

  let find (f: 'k => 'v => bool) (keyedReduceable: t 'k 'v): (option ('k, 'v)) =>
    keyedReduceable |> KeyedReduceable.reduce
      while_::(fun acc _ _ => Option.isEmpty acc)
      (fun _ k v => if (f k v) (Some (k, v)) else None)
      None;

  let findOrRaise (f: 'k => 'v => bool) (keyedReduceable: t 'k 'v): ('k, 'v) =>
    find f keyedReduceable |> Option.firstOrRaise;

  let findKey (f: 'k => 'v => bool) (keyedReduceable: t 'k 'v): (option 'k) =>
    keyedReduceable |> KeyedReduceable.reduce
      while_::(fun acc _ _ => Option.isEmpty acc)
      (fun _ k v => if (f k v) (Some k) else None)
      None;

  let findKeyOrRaise (f: 'k => 'v => bool) (keyedReduceable: t 'k 'v): 'k =>
    findKey f keyedReduceable |> Option.firstOrRaise;

  let findValue (f: 'k => 'v => bool) (keyedReduceable: t 'k 'v): (option 'v) =>
    keyedReduceable |> KeyedReduceable.reduce
      while_::(fun acc _ _ => Option.isEmpty acc)
      (fun _ k v => if (f k v) (Some v) else None)
      None;

  let findValueOrRaise (f: 'k => 'v => bool) (keyedReduceable: t 'k 'v): 'v =>
    findValue f keyedReduceable |> Option.firstOrRaise;

  let first (keyedReduceable: t 'k 'v): (option ('k, 'v)) =>
    keyedReduceable |> KeyedReduceable.reduce
      while_::(fun acc _ _ => Option.isEmpty acc)
      (fun _ k v => Option.return (k, v))
      None;

  let firstOrRaise (keyedReduceable: t 'k 'v): ('k, 'v) =>
    keyedReduceable |> first |> Option.firstOrRaise;

  let forEach
      while_::(predicate: 'k => 'v => bool)=Functions.alwaysTrue2
      (f: 'k => 'v => unit)
      (keyedReduceable: t 'k 'v) =>
    keyedReduceable |> KeyedReduceable.reduce while_::(fun _ => predicate) (fun _ => f) ();

  let none (f: 'k => 'v => bool) (keyedReduceable: t 'k 'v): bool =>
    keyedReduceable |> KeyedReduceable.reduce
      while_::(fun acc _ _ => acc)
      (fun _ k v => f k v |> not)
      true;

  let some (f: 'k => 'v => bool) (keyedReduceable: t 'k 'v): bool =>
    keyedReduceable |> KeyedReduceable.reduce
      while_::(fun acc _ _ => not acc)
      (fun _ => f)
      false;
};
