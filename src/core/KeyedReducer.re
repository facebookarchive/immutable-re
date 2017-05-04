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

  let reduce: while_::('acc => k => 'v => bool) => ('acc => k => 'v => 'acc) => 'acc => (t 'v) => 'acc;
  let reduceKeys: while_::('acc => k => bool) => ('acc => k => 'acc) => 'acc => (t 'v) => 'acc;
  let reduceValues: while_::('acc => 'v => bool) => ('acc => 'v => 'acc) => 'acc => (t 'v) => 'acc;
};

module type S2 = {
  type t 'k 'v;

  let reduce: while_::('acc => 'k => 'v => bool) => ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceKeys: while_::('acc => 'k => bool) => ('acc => 'k => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceValues: while_::('acc => 'v => bool) => ('acc => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
};


let module Make1 = fun (Base: {
  type k;
  type t 'v;

  let reduce: while_::('acc => k => 'v => bool) => ('acc => k => 'v => 'acc) => 'acc => (t 'v) => 'acc;
}) => ({
  include Base;

  let reduceKeys
      while_::(predicate: 'acc => k => bool)
      (reducer: 'acc => k => 'acc)
      (acc: 'acc)
      (iter: t 'v): 'acc => {
    let reducer acc k _ => reducer acc k;
    if (predicate === Functions.alwaysTrue2) (reduce while_::Functions.alwaysTrue3 reducer acc iter)
    else {
      let predicate acc k _ => predicate acc k;
      reduce while_::predicate reducer acc iter;
    };
  };

  let reduceValues
      while_::(predicate: 'acc => 'v => bool)
      (reducer: 'acc => 'v => 'acc)
      (acc: 'acc)
      (iter: t 'v): 'acc => {
    let reducer acc _ v => reducer acc v;
    if (predicate === Functions.alwaysTrue2) (reduce while_::Functions.alwaysTrue3 reducer acc iter)
    else {
      let predicate acc _ v => predicate acc v;
      reduce while_::predicate reducer acc iter;
    };
  };
}: S1 with type t 'v := Base.t 'v and type k := Base.k);


let module Make2 = fun (Base: {
  type t 'k 'v;

  let reduce: while_::('acc => 'k => 'v => bool) => ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
}) => ({
  include Base;

  let reduceKeys
      while_::(predicate: 'acc => 'k => bool)
      (reducer: 'acc => 'k => 'acc)
      (acc: 'acc)
      (iter: t 'k 'v): 'acc => {
    let reducer acc k _ => reducer acc k;
    if (predicate === Functions.alwaysTrue2) (reduce while_::Functions.alwaysTrue3 reducer acc iter)
    else {
      let predicate acc k _ => predicate acc k;
      reduce while_::predicate reducer acc iter;
    };
  };

  let reduceValues
      while_::(predicate: 'acc => 'v => bool)
      (reducer: 'acc => 'v => 'acc)
      (acc: 'acc)
      (iter: t 'k 'v): 'acc => {
    let reducer acc _ v => reducer acc v;
    if (predicate === Functions.alwaysTrue2) (reduce while_::Functions.alwaysTrue3 reducer acc iter)
    else {
      let predicate acc _ v => predicate acc v;
      reduce while_::predicate reducer acc iter;
    };
  };
}: S2 with type t 'k 'v := Base.t 'k 'v);
