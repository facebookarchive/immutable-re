/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Option.Operators;

/* FIXME: Ideally use vector, once it implements removeAt */
type t 'a = CopyOnWriteArray.t 'a;

let add (equality: Equality.t 'a) (value: 'a) (set: t 'a): (t 'a) =>
  if (CopyOnWriteArray.containsWith equality value set) set
  else (CopyOnWriteArray.addLast value set);

let contains = CopyOnWriteArray.containsWith;

let count = CopyOnWriteArray.count;

let empty = [||];

let remove (equality: Equality.t 'a) (value: 'a) (set: t 'a): (t 'a) =>
  set |> CopyOnWriteArray.tryIndexOf (equality value) >>| (fun index =>
    set |> CopyOnWriteArray.removeAt index
  ) |? set;

let toSequence = CopyOnWriteArray.toSequence;
