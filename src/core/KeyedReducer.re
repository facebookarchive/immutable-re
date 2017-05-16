/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

module type SGeneric = {
  type t 'k 'v;
  type k 'k;
  type v 'v;

  let reduce: while_::('acc => k 'k => v 'v => bool) => ('acc => k 'k => v 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceKeys: while_::('acc => k 'k => bool) => ('acc => k 'k => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceValues: while_::('acc => v 'v => bool) => ('acc => v 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
};

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

let module MakeGeneric = fun (Base: {
  type t 'k 'v;
  type k 'k;
  type v 'v;

  let reduce: while_::('acc => k 'k => v 'v => bool) => ('acc => k 'k => v 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
}) => ({
  include Base;

  let reduceKeys
      while_::(predicate: 'acc => k 'k => bool)
      (reducer: 'acc => k 'k => 'acc)
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
      while_::(predicate: 'acc => v 'v => bool)
      (reducer: 'acc => v 'v => 'acc)
      (acc: 'acc)
      (iter: t 'k 'v): 'acc => {
    let reducer acc _ v => reducer acc v;
    if (predicate === Functions.alwaysTrue2) (reduce while_::Functions.alwaysTrue3 reducer acc iter)
    else {
      let predicate acc _ v => predicate acc v;
      reduce while_::predicate reducer acc iter;
    };
  };
}: SGeneric with type t 'k 'v := Base.t 'k 'v and type k 'k := Base.k 'k and type v 'v := Base.v 'v);

let module Make1 = fun (Base: {
  type k;
  type t 'v;

  let reduce: while_::('acc => k => 'v => bool) => ('acc => k => 'v => 'acc) => 'acc => (t 'v) => 'acc;
}) => ((MakeGeneric {
  type t 'k 'v  = Base.t 'v;
  type k 'k = Base.k;
  type v 'v = 'v;

  let reduce = Base.reduce;
}): S1 with type t 'v := Base.t 'v and type k := Base.k);

let module Make2 = fun (Base: {
  type t 'k 'v;

  let reduce: while_::('acc => 'k => 'v => bool) => ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
}) => ((MakeGeneric {
  type t 'k 'v  = Base.t 'k 'v;
  type k 'k = 'k;
  type v 'v = 'v;

  let reduce = Base.reduce;
}): S2 with type t 'k 'v := Base.t 'k 'v);
