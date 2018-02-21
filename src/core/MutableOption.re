/***
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
type t('a) = ref(array('a));

let create = (value: 'a) : t('a) => ref([|value|]);

let empty = () : t('a) => ref([||]);

let firstOrRaise = (opt: t('a)) : 'a => opt^[0];

let isEmpty = (opt: t('a)) : bool => Array.length(opt^) === 0;

let isNotEmpty = (opt: t('a)) : bool => Array.length(opt^) !== 0;

let set = (value: 'a, opt: t('a)) =>
  if (isEmpty(opt)) {
    opt := [|value|]
  } else {
    opt^[0] = value
  };

let unset = (opt: t('a)) => opt := [||];
