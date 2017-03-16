/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open ReUnit.Expect;
open ReUnit.Test;

let test = describe "Transient" [
  describe "get" [
    it "throws if persisted" (fun () => {
      let transient = Transient.create "a";
      transient |> Transient.persist |> ignore;

      defer (fun () => transient |> Transient.get) |> throws;
    }),
    it "returns the value" (fun () => {
      let aString = "a";
      let transient = Transient.create aString;
      expect (transient |> Transient.get) |> toBeEqualToWith Equality.reference (fun s => s) aString;
    }),
  ],
  describe "update" [
    it "throws if persisted" (fun () => {
      let aString = "a";
      let transient = Transient.create aString;
      transient |> Transient.persist |> ignore;

      defer (fun () =>
        transient |> Transient.update (fun _ str => str)
      ) |> throws;
    }),
    it "with new value" (fun () => {
      let aString = "a";
      let bString = "b";

      let transient = Transient.create aString;
      transient |> Transient.update (fun _ _ => bString) |> ignore;
      expect (transient |> Transient.get)
        |> toBeEqualToWith Equality.reference (fun s => s) bString;
    })
  ],
];
