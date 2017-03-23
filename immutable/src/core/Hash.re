/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'a = 'a => int;

let prng = lazy (Random.State.make_self_init ());

let initialValue = 17;

let reducer (hash: t 'a) (acc: int) (next: 'a): int =>
  (31 * acc) + (hash next);
