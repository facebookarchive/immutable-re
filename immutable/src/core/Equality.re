/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'a = 'a => 'a => bool;


let make (compare: 'a => 'a => int) (this: 'a) (that: 'a): bool => {
  let cmp = compare this that;
  cmp === 0;
};

let bytes = make Bytes.compare;
let char = make Char.compare;
let int (this: int) (that: int) => this === that;
let int32 = make Int32.compare;
let int64 = make Int64.compare;
let nativeInt = make Nativeint.compare;
let reference (that: 'a) (this: 'a): bool => that === this;
let string = make String.compare;
