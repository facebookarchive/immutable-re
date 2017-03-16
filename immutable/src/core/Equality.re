/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'a = 'a => 'a => bool;

/* Will be available in Ocaml 4.03
let bytes = Bytes.equal;
let char = Char.equal;
let float = Float.equal;
let int32 = Int32.equal;
let int64 = Int64.equal;
let nativeInt = Nativeint.equal;
let string = String.equal
*/

let reference (that: 'a) (this: 'a): bool => that === this;
let structural (that: 'a) (this: 'a): bool => that == this;
