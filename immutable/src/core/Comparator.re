/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'a = 'a => 'a => Ordering.t;

let make (compare: 'a => 'a => int) (that: 'a) (this: 'a): Ordering.t => {
  let cmp = compare that this;

  if (cmp > 0) Ordering.greaterThan
  else if (cmp < 0) Ordering.lessThan
  else Ordering.equal;
};

let bytes = make Bytes.compare;
let char = make Char.compare;
let int32 = make Int32.compare;
let int64 = make Int64.compare;
let nativeInt = make Nativeint.compare;
let string = make String.compare;
let structural (that: 'a) (this: 'a): Ordering.t => make compare that this;

let toEquality (comparator: t 'a): (Equality.t 'a) => fun x y =>
  if ((comparator x y) === Ordering.equal) true
  else false;
