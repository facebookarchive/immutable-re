open Option.Operators;
open Immutable;
open ReUnit;
open ReUnit.Expect;
open ReUnit.Test;

let test = describe "Option" [
  it "compareWith" (fun () => {
    expect (Option.compare (Some 0) (Some 1)) |> toBeEqualTo (fun _ => "") Ordering.lessThan;
    expect (Option.compare (Some 1) (Some 0)) |> toBeEqualTo (fun _ => "") Ordering.greaterThan;
    expect (Option.compare (Some 0) (Some 0)) |> toBeEqualTo (fun _ => "") Ordering.equal;
    expect (Option.compare None None) |> toBeEqualTo (fun _ => "") Ordering.equal;
    expect (Option.compare None (Some 0)) |> toBeEqualTo (fun _ => "") Ordering.lessThan;
    expect (Option.compare (Some 1) None) |> toBeEqualTo (fun _ => "") Ordering.greaterThan;
   }),
  it "containsWith" (fun () => {
    expect ((Some 1) |> Option.contains 1) |> toBeEqualToTrue;
    expect ((Some 1) |> Option.contains 0) |> toBeEqualToFalse;
    expect (None |>  Option.contains 1) |> toBeEqualToFalse;
  }),
  it "count" (fun () => {
    expect (Option.count (Some 1)) |> toBeEqualToInt 1;
    expect (Option.count None) |> toBeEqualToInt 0;
  }),
  it "empty" (fun () => {
    expect Option.empty |> toBeEqualToNoneOfInt;
  }),
  it "equals" (fun () => {
    expect (Option.equals None None) |> toBeEqualToTrue;
    expect (Option.equals (Some 1) (Some 1)) |> toBeEqualToTrue;

    let reference = Some 1;
    expect (Option.equals reference reference) |> toBeEqualToTrue;
    expect (Option.equals (Some 1) (Some 2)) |> toBeEqualToFalse;
    expect (Option.equals None (Some 1)) |> toBeEqualToFalse;
  }),
  it "every" (fun () => {
    expect (Option.every Functions.alwaysTrue None) |> toBeEqualToFalse;
    expect (Option.every Functions.alwaysTrue (Some 1)) |> toBeEqualToTrue;
    expect (Option.every Functions.alwaysFalse (Some 1)) |> toBeEqualToFalse;
    ()
  }),
  it "filter" (fun () => {
    expect (Option.filter Functions.alwaysFalse (Some 1)) |> toBeEqualToNoneOfInt;
    expect (Option.filter Functions.alwaysTrue (Some 1)) |> toBeEqualToSomeOfInt 1;
  }),
  it "find" (fun () => {
    expect (Option.find Functions.alwaysTrue (Some 1)) |> toBeEqualToInt 1;
    defer (fun () => Option.find Functions.alwaysTrue None) |> throws;
    defer (fun () => Option.find Functions.alwaysFalse (Some 1)) |> throws;
  }),
  it "first" (fun () => {
    expect (Some "a" |> Option.first) |> toBeEqualToString "a";
    defer (fun () => None |> Option.first) |> throws;
  }),
  it "flatMap" (fun () => {
    let someA = Some "a";
    let result = someA |> Option.flatMap (fun _ => Some "b");
    expect result |> toBeEqualToSomeOfString "b";

    let result = someA |> Option.flatMap (fun _ => None);
    expect result |> toBeEqualToNoneOfString;

    let result = None |> Option.flatMap (fun _ => Some "a");
    expect result |> toBeEqualToNoneOfString;
  }),
  it "flatten" (fun () => {
    expect (Some (Some 1) |> Option.flatten) |> toBeEqualToSomeOfInt 1;
    expect (Some None |> Option.flatten) |> toBeEqualToNoneOfInt;
    expect (None |> Option.flatten) |> toBeEqualToNoneOfInt;
  }),
  it "forEach" (fun () => {
    None |> Option.forEach (Functions.alwaysFailWith "should not have been called");
    Some 1 |> Option.forEach (fun i => {
      expect i |> toBeEqualToInt 1;
    });
  }),
  it "hash" (fun () => {
    expect (Some 1 |> Option.hash) |> toBeEqualToInt (Hash.structural 1);
  }),
  it "isEmpty" (fun () => {
    expect (Some "a" |> Option.isEmpty) |> toBeEqualToFalse;
    expect (None |> Option.isEmpty) |> toBeEqualToTrue;
  }),
  it "isNotEmpty" (fun () => {
    expect (Some "a" |> Option.isNotEmpty) |> toBeEqualToTrue;
    expect (None |> Option.isNotEmpty) |> toBeEqualToFalse;
  }),
  it "map" (fun () => {
    expect (None |> Option.map (fun _ => "a")) |> toBeEqualToNoneOfString;
    expect (Some "a" |> Option.map (fun _ => "b")) |> toBeEqualToSomeOfString "b";
  }),
  it "none" (fun () => {
    expect (None |> Option.none Functions.alwaysTrue) |> toBeEqualToTrue;
    expect (Some 1 |> Option.none Functions.alwaysTrue) |> toBeEqualToFalse;
    expect (Some 1 |> Option.none Functions.alwaysFalse) |> toBeEqualToTrue;
  }),
  it "reduce" (fun () => {
    let reducer acc next => next;
    expect (None |> Option.reduce reducer 0) |> toBeEqualToInt 0;
    expect (Some 1 |> Option.reduce reducer 0) |> toBeEqualToInt 1;
  }),
  it "return" (fun () => {
    expect (Option.return "a") |> toBeEqualToSomeOfString "a";
  }),
  it "some" (fun () => {
    expect (None |> Option.some Functions.alwaysTrue) |> toBeEqualToFalse;
    expect (Some 1 |> Option.some Functions.alwaysTrue) |> toBeEqualToTrue;
    expect (Some 1 |> Option.some Functions.alwaysFalse) |> toBeEqualToFalse;
  }),
  describe "toCollection" [
    /* The collection functions are straight pass through */
  ],
  it "toSeq" (fun () => {
    let emptySeq = None |> Option.toSeq;
    expect emptySeq |> toBeEqualToEmptySeqOfString;

    let seqOfA = Some "a" |> Option.toSeq;
    let first = Seq.tryFirst seqOfA;
    expect first |> toBeEqualToSomeOfString "a";
  }),
  it "tryFind" (fun () => {
    expect (Option.tryFind Functions.alwaysTrue (Some 1)) |> toBeEqualToSomeOfInt 1;
    expect (Option.tryFind Functions.alwaysTrue None) |> toBeEqualToNoneOfInt;
    expect (Option.tryFind Functions.alwaysFalse (Some 1)) |> toBeEqualToNoneOfInt;
  }),
];
