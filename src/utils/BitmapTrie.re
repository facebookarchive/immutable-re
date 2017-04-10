/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

/* FIXME: It's not clear which bit counting strategy to use. The bitcount table
 * is likely faster but uses more memory.
 */
let bitCountTable = {
  let table = Array.make 65536 0;
  let position1 = ref (-1);
  let position2 = ref (-1);
  for i in 1 to 65535 {
    if (!position1 === !position2) {
      position1 := 0;
      position2 := i;
    };

    table.(i) = table.(!position1) + 1;
    position1 := !position1 + 1;
  };

  table
};

let countBits (x: int32): int => {
  let intValue = Int32.to_int x;
  let intBits = bitCountTable.(intValue land 65535) + bitCountTable.((intValue asr 16) land 65535);
  intBits + (if (x < 0l) 1 else 0);
};
/*
let countBits (x: int32): int => {
  let intBits = {
    let x = Int32.to_int x;
    let x = x - ((x asr 1) land 0x55555555);
    let x = (x land 0x33333333) + ((x asr 2) land 0x33333333);
    let x = (x + (x asr 4)) land 0x0f0f0f0f;
    let x = x + (x asr 8);
    let x = x + (x asr 16);
    x land 0x7f;
  };
  intBits + (if (x < 0l) 1 else 0);
};*/

let shift = 5;
let width = 1 lsl shift;

let bitPos (key: int) (depth: int): int32 => {
  let mask = (key lsr (depth * shift)) land 0x1F;
  Int32.shift_left 1l mask;
};

let index (bitmap: int32) (bit: int32): int  =>
  Int32.logand bitmap (Int32.sub bit 1l) |> countBits;

let containsNode (bitmap: int32) (bit: int32): bool =>
  /* FIXME: Should avoid structural equality at all costs */
  (Int32.logand bitmap bit) != 0l;
