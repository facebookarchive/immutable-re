/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

module type SGeneric = {
  type t 'a;
  type elt 'a;

  include TransientCollection.SGeneric with type t 'a := t 'a and type elt 'a := elt 'a;

  let add: (elt 'a) => (t 'a) => (t 'a);
  let addAll: (Iterable.t (elt 'a)) => (t 'a) => (t 'a);
  let contains: elt 'a => (t 'a) => bool;
  let remove: elt 'a => (t 'a) => (t 'a);
};

module type S = {
  type a;
  type t;

  include TransientCollection.S with type a := a and type t := t;

  let add: a => t => t;
  let addAll: (Iterable.t a) => t => t;
  let contains: a => t => bool;
  let remove: a => t => t;
};

module type S1 = {
  type t 'a;

  include TransientCollection.S1 with type t 'a := t 'a;

  let add: 'a => (t 'a) => (t 'a);
  let addAll: (Iterable.t 'a) => (t 'a) => (t 'a);
  let contains: 'a => (t 'a) => bool;
  let remove: 'a => (t 'a) => (t 'a);
};

module type Base = {
  type elt 'a;
  type t 'a;

  include TransientCollection.Base with type t 'a := t 'a and type elt 'a := elt 'a;

  let add: elt 'a => (t 'a) => (t 'a);
  let contains: elt 'a => (t 'a) => bool;
  let remove: (elt 'a) => (t 'a) => (t 'a);
};

let module MakeGeneric = fun (Base: Base) => ({
  include Base;

  include (TransientCollection.MakeGeneric Base: TransientCollection.SGeneric with type t 'a := t 'a and type elt 'a := elt 'a);

  let addAll
      (iter: Iterable.t (elt 'a))
      (transient: t 'a): t 'a =>
    iter |> Iterable.reduce while_::Functions.alwaysTrue2 (fun acc next => acc |> add next) transient;
}: SGeneric with type t 'a := Base.t 'a and type elt 'a := Base.elt 'a);
