/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

let hash (keyHash: Hash.t 'k) (valueHash: Hash.t 'v) (key: 'k) (value: 'v): int =>
  (keyHash key) lxor (valueHash value);

let hashReducer
    (keyHash: Hash.t 'k)
    (valueHash: Hash.t 'v)
    (acc: int)
    (key: 'k)
    (value: 'v): int =>
  acc + (hash keyHash valueHash key value);
