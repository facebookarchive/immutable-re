/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Option.Operators;

type t 'k 'v = CopyOnWriteArray.t ('k, 'v);

let entryFinder (equals: Equality.t 'k) (key: 'k) (entry: ('k, _)): bool => {
  let (entryKey, _) = entry;
  equals key entryKey;
};

let alter (equals: Equality.t 'k) (key: 'k) (f: option 'v => option 'v) (map: t 'k 'v): (t 'k 'v) =>
  map |> CopyOnWriteArray.indexOf (entryFinder equals key) >>| (fun index => {
    let (entryKey, entryValue) = map |> CopyOnWriteArray.getOrRaise index;
    switch (f (Some entryValue)) {
      | Some newValue => map |> CopyOnWriteArray.update index (entryKey, newValue)
      | None => map |> CopyOnWriteArray.removeAt index;
    }
  }) |? map;

let contains
    (keyEquals: Equality.t 'k)
    (valueEquals: Equality.t 'v)
    (key: 'k)
    (value: 'v)
    (map: t 'k 'v) =>
  map |> CopyOnWriteArray.find (entryFinder keyEquals key)
    >>| (fun (_, v) => valueEquals v value)
    |> Option.isNotEmpty;

let containsKey (equals: Equality.t 'k) (key: 'k) (map: t 'k 'v) =>
  map |> CopyOnWriteArray.find (entryFinder equals key) |> Option.isNotEmpty;

let count = CopyOnWriteArray.count;

let empty = [||];

let every (f: 'k => 'v => bool) (map: t 'k 'v): bool =>
  map |> CopyOnWriteArray.every (fun (k, v) => f k v);

let find (f: 'k => 'v => bool) (map: t 'k 'v): (option ('k, 'v)) =>
  map |> CopyOnWriteArray.find (fun (k, v) => f k v);

let findOrRaise (f: 'k => 'v => bool) (map: t 'k 'v): ('k, 'v) =>
  map |> CopyOnWriteArray.findOrRaise (fun (k, v) => f k v);

let firstOrRaise = CopyOnWriteArray.firstOrRaise;

let forEach (f: 'k => 'v => unit) (map: t 'k 'v): 'acc =>
  map |> CopyOnWriteArray.forEach (fun (k, v) => f k v);

let get (equals: Equality.t 'k) (key: 'k) (map: t 'k 'v): (option 'v) =>
  map |> CopyOnWriteArray.find (entryFinder equals key) >>= (fun (_, v) => Some v);

let getOrRaise (equals: Equality.t 'k) (key: 'k) (map: t 'k 'v): 'v => {
  let (_, v) = map |> CopyOnWriteArray.findOrRaise (entryFinder equals key);
  v
};

let none (f: 'k => 'v => bool) (map: t 'k 'v): bool =>
  map |> CopyOnWriteArray.none (fun (k, v) => f k v);

let put (equals: Equality.t 'k) (key: 'k) (value: 'v) (map: t 'k 'v): (t 'k 'v) =>
  alter equals key (Functions.return @@ Option.return @@ value) map;

let reduce (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) (map: t 'k 'v): 'acc =>
  map |> CopyOnWriteArray.reduce (fun acc (k, v) => f acc k v) acc;

let some (f: 'k => 'v => bool) (map: t 'k 'v): bool =>
  map |> CopyOnWriteArray.some (fun (k, v) => f k v);

let toSequence = CopyOnWriteArray.toSequence;

let values (map: t 'k 'v): (Iterator.t 'v) =>
  map |> CopyOnWriteArray.toIterator |> Iterator.map (fun (_, v) => v);
