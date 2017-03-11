open Functions;
open Functions.Operators;
open ReUnit.Expect;
open ReUnit.Test;

let test = describe "Functions" [
  it "alwaysFailWith" (fun () => {
    defer (alwaysFailWith "failstring") |> throws;
  }),
  it "alwaysFalse" (fun () => {
    expect (alwaysFalse ()) |> toBeEqualToFalse;
  }),
  it "alwaysNone" (fun () => {
    expect (alwaysNone ()) |> toBeEqualToNoneOfString;
  }),
  it "alwaysTrue" (fun () => {
    expect (alwaysTrue ()) |> toBeEqualToTrue;
  }),
  it "call" (fun () => {
    let text () => "";
    expect (call text) |> toBeEqualToString "";
  }),
  it "compose" (fun () => {
    let f1 _ => 2;
    let f2 _ => "c";
    let composed = compose f1 f2;
    expect (composed "hello") |> toBeEqualToString "c";
  }),
  it "identity" (fun () => {
    expect (identity 1) |> toBeEqualToInt 1
  }),
  it "isFalse" (fun () => {
    expect (isFalse false) |> toBeEqualToTrue;
    expect (isFalse true) |> toBeEqualToFalse;
  }),
  it "isTrue" (fun () => {
    expect (isTrue false) |> toBeEqualToFalse;
    expect (isTrue true) |> toBeEqualToTrue;
  }),
  it "return" (fun () => {
    expect (Functions.return "" ()) |> toBeEqualToString "";
  }),
  describe "Operators" [
    it "(>>)" (fun () => {
      let f1 _ => 2;
      let f2 _ => "c";
      let composed = f1 >> f2;
      expect (composed "hello") |> toBeEqualToString "c";
    }),
  ],
];
