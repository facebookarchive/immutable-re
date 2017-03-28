/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

let failEmpty () =>
  failwith "empty";

let failIfOutOfRange (count: int) (index: int) =>
  if (index < 0) (failwith "Index must be greater than 0")
  /* FIXME: When reason support string interpolation include the count */
  else if (index >= count) (failwith "Index must be less than count")
  else ();

let noneIfIndexOutOfRange (count:int) (index: int) (f: int => 'a): (option 'a) =>
  if (index < 0) None
  else if (index >= count) None
  else f index |> Option.return;

let failIf (msg: string) (condition: bool): unit =>
  if condition (failwith msg)
  else ();
