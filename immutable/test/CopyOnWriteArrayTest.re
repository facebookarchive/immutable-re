/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Immutable;
open ReUnit;
open ReUnit.Expect;
open ReUnit.Test;

let test = describe "CopyOnWriteArray" [
  describe "add" [
    it "a value into an array" (fun () => {
      let arr = [| "a", "b", "c" |] |> CopyOnWriteArray.ofUnsafe;
      let copy = arr |> CopyOnWriteArray.add "d";
      expect (Equality.reference arr copy) |> toBeEqualToFalse;
      expect (CopyOnWriteArray.toSeq copy) |> toBeEqualToSeqOfString (List.toSeq [ "a", "b", "c", "d" ]);
    }),
  ],
  describe "insertAt" [
    it "throws when index out of bounds" (fun () => {
      let arrayOfLength5 = [| "0", "1", "2", "3", "4" |] |> CopyOnWriteArray.ofUnsafe;
      defer (fun () => arrayOfLength5 |> CopyOnWriteArray.insertAt (-1) "") |> throws;
      defer (fun () => arrayOfLength5 |> CopyOnWriteArray.insertAt 6 "") |> throws;
    }),
    it "insert node at 0" (fun () => {
      let arr = [| "0", "1", "2", "3", "4" |] |> CopyOnWriteArray.ofUnsafe;
      let copy = arr |> CopyOnWriteArray.insertAt 0 "";
      expect (Equality.reference arr copy) |> toBeEqualToFalse;
      expect (CopyOnWriteArray.count copy) |> toBeEqualToInt 6;
      expect (copy |> CopyOnWriteArray.tryGet 0) |> toBeEqualToSomeOfString "";
      expect (copy |> CopyOnWriteArray.tryGet 1) |> toBeEqualToSomeOfString "0";
    }),
    it "inserts node at length" (fun () => {
      let arr = [| "0", "1", "2", "3", "4" |] |> CopyOnWriteArray.ofUnsafe;
      let copy = arr |> CopyOnWriteArray.insertAt (CopyOnWriteArray.count arr) "";
      expect (Equality.reference arr copy) |> toBeEqualToFalse;
      expect (CopyOnWriteArray.count copy) |> toBeEqualToInt 6;
      expect (copy |> CopyOnWriteArray.tryGet 4) |> toBeEqualToSomeOfString "4";
      expect (copy |> CopyOnWriteArray.tryGet 5) |> toBeEqualToSomeOfString "";
    }),
    it "inserts node in the middle" (fun () => {
      let arr = [| "0", "1", "2", "3", "4" |] |> CopyOnWriteArray.ofUnsafe;
      let copy = arr |> CopyOnWriteArray.insertAt 3 "";
      expect (Equality.reference arr copy) |> toBeEqualToFalse;
      expect (CopyOnWriteArray.count copy) |> toBeEqualToInt 6;
      expect (copy |> CopyOnWriteArray.tryGet 2) |> toBeEqualToSomeOfString "2";
      expect (copy |> CopyOnWriteArray.tryGet 3) |> toBeEqualToSomeOfString "";
      expect (copy |> CopyOnWriteArray.tryGet 4) |> toBeEqualToSomeOfString "3";
    }),
  ],
  describe "removeAt" [
    it "throws when index out of bounds" (fun () => {
      let arrayOfLength5 = [| "0", "1", "2", "3", "4" |] |> CopyOnWriteArray.ofUnsafe;
      defer (fun () => arrayOfLength5 |> CopyOnWriteArray.removeAt (-1)) |> throws;
      defer (fun () => arrayOfLength5 |> CopyOnWriteArray.removeAt 5) |> throws;
    }),
    it "node at index 0" (fun () => {
      let arr = [| "0", "1", "2", "3", "4" |] |> CopyOnWriteArray.ofUnsafe;
      let copy = arr |> CopyOnWriteArray.removeAt 0;
      expect (Equality.reference arr copy) |> toBeEqualToFalse;
      expect (CopyOnWriteArray.toSeq copy) |> toBeEqualToSeqOfString (List.toSeq [ "1", "2", "3", "4" ]);
    }),
    it "node at index (length - 1)" (fun () => {
      let arr = [| "0", "1", "2", "3", "4" |] |> CopyOnWriteArray.ofUnsafe;
      let copy = arr |> CopyOnWriteArray.removeAt 4;
      expect (Equality.reference arr copy) |> toBeEqualToFalse;
      expect (CopyOnWriteArray.toSeq copy) |> toBeEqualToSeqOfString (List.toSeq [ "0", "1", "2", "3" ]);
    }),
  ],
  describe "removeLast" [
    it "throws when empty" (fun () =>
      defer (fun () => CopyOnWriteArray.empty |> CopyOnWriteArray.removeLast) |> throws
    ),
    it "returns empty array when array has only one element" (fun () => {
      let arr = [| "a" |] |> CopyOnWriteArray.ofUnsafe;
      let copy = arr |> CopyOnWriteArray.removeLast;
      expect (Equality.reference arr copy) |> toBeEqualToFalse;
      expect (CopyOnWriteArray.toSeq copy) |> toBeEqualToEmptySeqOfString;
    }),
    it "an array of values" (fun () => {
      let arr = [| "a", "b", "c" |] |> CopyOnWriteArray.ofUnsafe;
      let copy = arr |> CopyOnWriteArray.removeLast;
      expect (Equality.reference arr copy) |> toBeEqualToFalse;
      expect (CopyOnWriteArray.toSeq copy) |> toBeEqualToSeqOfString (List.toSeq [ "a", "b" ]);
    })
  ],
  describe "tryGet" [
    it "returns None if index out of bounds" (fun () => {
      let arr = [| "0" |] |> CopyOnWriteArray.ofUnsafe;
      expect (arr |> CopyOnWriteArray.tryGet (-1)) |> toBeEqualToNoneOfString;
      expect (arr |> CopyOnWriteArray.tryGet (1)) |> toBeEqualToNoneOfString;
    }),
    it "returns (Some value) if index is in bounds" (fun () =>
      expect ([| "0" |] |> CopyOnWriteArray.ofUnsafe |> CopyOnWriteArray.tryGet (0)) |> toBeEqualToSomeOfString "0"
    ),
  ],
  describe "update" [
    it "throws when index out of bounds" (fun () => {
      let arrayOfLength5 = [| "0", "1", "2", "3", "4" |] |> CopyOnWriteArray.ofUnsafe;
      defer (fun () => arrayOfLength5 |> CopyOnWriteArray.update (-1) "") |> throws;
      defer (fun () => arrayOfLength5 |> CopyOnWriteArray.update 5 "") |> throws;
    }),
    it "a random node" (fun () => {
      let arr = [| "0", "1", "2", "3", "4" |] |> CopyOnWriteArray.ofUnsafe;
      let copy = arr |> CopyOnWriteArray.update 3 "";
      expect (Equality.reference arr copy) |> toBeEqualToFalse;
      expect (CopyOnWriteArray.count copy) |> toBeEqualToInt (CopyOnWriteArray.count arr);
      expect (copy |> CopyOnWriteArray.tryGet 3) |> toBeEqualToSomeOfString "";
    }),
  ],
];
