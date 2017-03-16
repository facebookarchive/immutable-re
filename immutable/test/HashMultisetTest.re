/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

/*open Immutable;
open ReUnit;
open ReUnit.Expect;
open ReUnit.Test;

let test = describe "HashMultiset" [
  it "add" (fun () => {
    let set = HashMultiset.empty ()
      |> HashMultiset.add "a"
      |> HashMultiset.add "b"
      |> HashMultiset.add "a"
      |> HashMultiset.add "a"
      |> HashMultiset.add "c";
    expect (set |> HashMultiset.count) |> toBeEqualToInt 5;
    expect (set |> HashMultiset.get "a") |> toBeEqualToInt 3;
    expect (set |> HashMultiset.get "b") |> toBeEqualToInt 1;
    expect (set |> HashMultiset.get "c") |> toBeEqualToInt 1;
    expect (set |> HashMultiset.get "d") |> toBeEqualToInt 0;
  }),

  it "contains" (fun () => {
    let set = HashMultiset.empty ()
      |> HashMultiset.add "a"
      |> HashMultiset.add "b"
      |> HashMultiset.add "a"
      |> HashMultiset.add "a"
      |> HashMultiset.add "c";

    expect (set |> HashMultiset.contains "a") |> toBeEqualToTrue;
    expect (set |> HashMultiset.contains "d") |> toBeEqualToFalse;
  }),

  it "fromSeq" (fun () => {
    let set = ["a", "b", "c", "d", "a", "a", "a", "d"]
      |> List.toSeq
      |> HashMultiset.fromSeq;

    expect (set |> HashMultiset.count) |> toBeEqualToInt 8;
    expect (set |> HashMultiset.get "a") |> toBeEqualToInt 4;
    expect (set |> HashMultiset.get "b") |> toBeEqualToInt 1;
    expect (set |> HashMultiset.get "c") |> toBeEqualToInt 1;
    expect (set |> HashMultiset.get "d") |> toBeEqualToInt 2;
  }),

  it "remove" (fun () => {
    let set = HashMultiset.empty ()
      |> HashMultiset.add "a"
      |> HashMultiset.add "b"
      |> HashMultiset.add "a"
      |> HashMultiset.add "a"
      |> HashMultiset.add "c"
      |> HashMultiset.remove "a";
    expect (set |> HashMultiset.count) |> toBeEqualToInt 2;
    expect (set |> HashMultiset.get "a") |> toBeEqualToInt 0;
    expect (set |> HashMultiset.get "b") |> toBeEqualToInt 1;
    expect (set |> HashMultiset.get "c") |> toBeEqualToInt 1;
    expect (set |> HashMultiset.get "d") |> toBeEqualToInt 0;
  }),

  it "set" (fun () => {
    let set = HashMultiset.empty ()
      |> HashMultiset.add "a"
      |> HashMultiset.add "b"
      |> HashMultiset.add "a"
      |> HashMultiset.add "a"
      |> HashMultiset.add "c"
      |> HashMultiset.set "a" 10;
    expect (set |> HashMultiset.count) |> toBeEqualToInt 12;
    expect (set |> HashMultiset.get "a") |> toBeEqualToInt 10;
    expect (set |> HashMultiset.get "b") |> toBeEqualToInt 1;
    expect (set |> HashMultiset.get "c") |> toBeEqualToInt 1;
    expect (set |> HashMultiset.get "d") |> toBeEqualToInt 0;
  }),

  describe "TransientHashMultiset" [
    it "add" (fun () => {
      let set = HashMultiset.empty ()
        |> HashMultiset.mutate
        |> TransientHashMultiset.add "a"
        |> TransientHashMultiset.add "b"
        |> TransientHashMultiset.add "a"
        |> TransientHashMultiset.add "a"
        |> TransientHashMultiset.add "c"
        |> TransientHashMultiset.persist;
      expect (set |> HashMultiset.count) |> toBeEqualToInt 5;
      expect (set |> HashMultiset.get "a") |> toBeEqualToInt 3;
      expect (set |> HashMultiset.get "b") |> toBeEqualToInt 1;
      expect (set |> HashMultiset.get "c") |> toBeEqualToInt 1;
      expect (set |> HashMultiset.get "d") |> toBeEqualToInt 0;
    }),

    it "contains" (fun () => {
      let set = HashMultiset.empty ()
        |> HashMultiset.mutate
        |> TransientHashMultiset.add "a"
        |> TransientHashMultiset.add "b"
        |> TransientHashMultiset.add "a"
        |> TransientHashMultiset.add "a"
        |> TransientHashMultiset.add "c"
        |> TransientHashMultiset.persist;

      expect (set |> HashMultiset.contains "a") |> toBeEqualToTrue;
      expect (set |> HashMultiset.contains "d") |> toBeEqualToFalse;
    }),

    it "remove" (fun () => {
      let set = HashMultiset.empty ()
        |> HashMultiset.mutate
        |> TransientHashMultiset.add "a"
        |> TransientHashMultiset.add "b"
        |> TransientHashMultiset.add "a"
        |> TransientHashMultiset.add "a"
        |> TransientHashMultiset.add "c"
        |> TransientHashMultiset.remove "a"
        |> TransientHashMultiset.persist;

      expect (set |> HashMultiset.count) |> toBeEqualToInt 2;
      expect (set |> HashMultiset.get "a") |> toBeEqualToInt 0;
      expect (set |> HashMultiset.get "b") |> toBeEqualToInt 1;
      expect (set |> HashMultiset.get "c") |> toBeEqualToInt 1;
      expect (set |> HashMultiset.get "d") |> toBeEqualToInt 0;
    }),

    it "set" (fun () => {
      let set = HashMultiset.empty ()
        |> HashMultiset.mutate
        |> TransientHashMultiset.add "a"
        |> TransientHashMultiset.add "b"
        |> TransientHashMultiset.add "a"
        |> TransientHashMultiset.add "a"
        |> TransientHashMultiset.add "c"
        |> TransientHashMultiset.set "a" 10
        |> TransientHashMultiset.persist;
      expect (set |> HashMultiset.count) |> toBeEqualToInt 12;
      expect (set |> HashMultiset.get "a") |> toBeEqualToInt 10;
      expect (set |> HashMultiset.get "b") |> toBeEqualToInt 1;
      expect (set |> HashMultiset.get "c") |> toBeEqualToInt 1;
      expect (set |> HashMultiset.get "d") |> toBeEqualToInt 0;
    }),
  ],
];*/
