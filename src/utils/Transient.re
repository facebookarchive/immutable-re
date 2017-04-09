/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

let module Owner = {
  type ownerT = | Owner;
  type t = ref ownerT;

  let create () => ref Owner;

  let none = ref Owner;
};

type t 'a = {
  owner: Owner.t,
  mutable editable: bool,
  mutable value: 'a,
};

let create (value: 'a): (t 'a) => ({
  owner: Owner.create (),
  editable: true,
  value: value,
});

let ensureEditable ({ editable } as transient: t 'a): (t 'a) =>
  if (not editable) { failwith "Transient has already been persisted" }
  else transient;

let get (transient: t 'a): 'a => {
  let { value } = ensureEditable transient;
  value
};

let persist (transient: t 'a): 'a =>  {
  let transient = ensureEditable transient;
  transient.editable = false;
  transient.value
};

let update (f: Owner.t => 'a => 'a) (transient: t 'a): (t 'a) => {
  let transient = ensureEditable transient;
  transient.value = f transient.owner transient.value;
  transient
};

let update1 (f: Owner.t => 'b => 'a => 'a) (arg: 'b) (transient: t 'a): (t 'a) => {
  let transient = ensureEditable transient;
  transient.value = f transient.owner arg transient.value ;
  transient
};

let update2 (f: Owner.t => 'b => 'c => 'a => 'a) (arg1: 'b) (arg2: 'c) (transient: t 'a): (t 'a) => {
  let transient = ensureEditable transient;
  transient.value = f transient.owner arg1 arg2 transient.value ;
  transient
};
