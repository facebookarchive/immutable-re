open Option.Operators;
open Immutable;
open ReUnit;
open ReUnit.Expect;
open ReUnit.Test;

let test = describe "Option" [
  describe "flatMap" [
    it "Some a to Some b" (fun () => {
      let someA = Some "a";
      let result = someA |> Option.flatMap (fun _ => Some "b");
      expect result |> toBeEqualToSomeOfString "b";
    }),
    it "Some a to None" (fun () => {
      let someA = Some "a";
      let result = someA |> Option.flatMap (fun _ => None);
      expect result |> toBeEqualToNoneOfString;
    }),
    it "None" (fun () => {
      let none = None;
      let result = none |> Option.flatMap (fun _ => Some "a");
      expect result |> toBeEqualToNoneOfString;
    }),
  ],
  describe "get" [
    it "Some a" (fun () => {
      expect (Some "a" |> Option.get) |> toBeEqualToString "a";
    }),
    it "None" (fun () => {
      defer (fun () => None |> Option.get) |> throws;
    }),
  ],
  describe "isEmpty" [
    it "Some a" (fun () => {
      expect (Some "a" |> Option.isEmpty) |> toBeEqualToFalse;
    }),
    it "None" (fun () => {
      expect (None |> Option.isEmpty) |> toBeEqualToTrue;
    }),
  ],
  describe "isNotEmpty" [
    it "Some a" (fun () => {
      expect (Some "a" |> Option.isNotEmpty) |> toBeEqualToTrue;
    }),
    it "None" (fun () => {
      expect (None |> Option.isNotEmpty) |> toBeEqualToFalse;
    }),
  ],
  describe "map" [
    it "None" (fun () => {
      expect (None |> Option.map (fun _ => "a")) |> toBeEqualToNoneOfString;
    }),
    it "Some a to Some b" (fun () => {
      expect (Some "a" |> Option.map (fun _ => "b")) |> toBeEqualToSomeOfString "b";
    }),
  ],
  it "return" (fun () => {
    expect (Option.return "a") |> toBeEqualToSomeOfString "a";
  }),
  describe "toSeq" [
    it "of None" (fun () => {
      let emptySeq = None |> Option.toSeq;
      expect emptySeq |> toBeEqualToEmptySeqOfString;
    }),

    it "of Some a" (fun () => {
      let seqOfA = Some "a" |> Option.toSeq;
      let first = Seq.tryFirst seqOfA;
      expect first |> toBeEqualToSomeOfString "a";
    }),
  ],
  describe "Operators" [
    describe "(>>=)" [
      it "Some a to Some b" (fun () => {
        let someA = Some "a";
        let result = someA >>= (fun _ => Some "b");
        expect result |> toBeEqualToSomeOfString "b";
      }),
      it "Some a to None" (fun () => {
        let someA = Some "a";
        let result = someA >>= (fun _ => None);
        expect result |> toBeEqualToNoneOfString;
      }),
      it "None" (fun () => {
        let result = None >>= (fun _ => Some "a");
        expect result |> toBeEqualToNoneOfString;
      }),
    ],
    describe "(>>|)" [
      it "None" (fun () => {
        expect (None >>| (fun _ => "a")) |> toBeEqualToNoneOfString;
      }),
      it "Some a to Some b" (fun () => {
        expect (Some "a" >>| (fun _ => "b")) |> toBeEqualToSomeOfString "b";
      }),
    ],
    describe "(|?)" [
      it "from None" (fun () => {
        expect (None |? "a") |> toBeEqualToString "a";
      }),
      it "from Some" (fun () => {
        expect (Some "a" |? "b") |> toBeEqualToString "a";
      }),
    ],
  ],
];
