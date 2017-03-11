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
