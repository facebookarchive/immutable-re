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

  let addFirst: elt 'a => (t 'a) => (t 'a);
  let addFirstAll: (Iterable.t (elt 'a)) => (t 'a) => (t 'a);
  let first: (t 'a) => option (elt 'a);
  let firstOrRaise: (t 'a) => (elt 'a);
  let removeFirstOrRaise: (t 'a) => (t 'a);
};

module type S1 = {
  type t 'a;

  include TransientCollection.S1 with type t 'a := t 'a;

  let addFirst: 'a => (t 'a) => (t 'a);
  let addFirstAll: (Iterable.t 'a) => (t 'a) => (t 'a);
  let first: (t 'a) => option 'a;
  let firstOrRaise: (t 'a) => 'a;
  let removeFirstOrRaise: (t 'a) => (t 'a);
};

module type Base = {
  type elt 'a;
  type t 'a;

  include TransientCollection.Base with type t 'a := t 'a and type elt 'a := elt 'a;

  let addFirst: (elt 'a) => (t 'a) => (t 'a);
  let firstOrRaise: (t 'a) => (elt 'a);
  let removeFirstOrRaise: (t 'a) => (t 'a);
};

let module MakeGeneric = fun (Base: Base) => ({
  include Base;

  include (TransientCollection.MakeGeneric Base: TransientCollection.SGeneric with type t 'a := t 'a and type elt 'a := elt 'a);

  let first (collection: t 'a): (option (elt 'a)) =>
    if (isEmpty collection) None
    else Some (firstOrRaise collection);

  let addFirstAll (iter: Iterable.t (elt 'a)) (transient: t 'a): (t 'a) =>
    iter |> Iterable.reduce while_::Functions.alwaysTrue2 (fun acc next => acc |> addFirst next) transient;
}: SGeneric with type t 'a := Base.t 'a and type elt 'a := Base.elt 'a);
