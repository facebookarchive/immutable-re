/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

module type SGeneric = {
  type elt 'a;
  type t 'a;

  let count: t 'a => int;
  let isEmpty: t 'a => bool;
  let isNotEmpty: t 'a => bool;
  let removeAll: t 'a => t 'a;
};

module type S = {
  type a;
  type t;

  let count: t => int;
  let isEmpty: t => bool;
  let isNotEmpty: t => bool;
  let removeAll: t => t;
};

module type S1 = {
  type t 'a;

  let count: (t 'a) => int;
  let isEmpty: (t 'a) => bool;
  let isNotEmpty: (t 'a) => bool;
  let removeAll: (t 'a) => (t 'a);
};

module type Base = {
  type elt 'a;
  type t 'a;

  let count: t 'a => int;
  let removeAll: (t 'a) => (t 'a);
};

let module MakeGeneric = fun (Base: Base) => ({
  include Base;

  let isEmpty (collection: t 'a): bool =>
    (count collection) === 0;

  let isNotEmpty (collection: t 'a): bool =>
    (count collection) !== 0;
}: SGeneric with type t 'a := Base.t 'a and type elt 'a := Base.elt 'a);
