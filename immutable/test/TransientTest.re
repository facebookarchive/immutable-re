open ReUnit;
open ReUnit.Expect;
open ReUnit.Test;

let test = describe "Transient" [
  describe "get" [
    it "throws if persisted" (fun () => {
      let transient = Transient.create "a";
      let persisted = transient |> Transient.persist;

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
      let persisted = transient |> Transient.persist;

      defer (fun () =>
        transient |> Transient.update (fun owner str => str)
      ) |> throws;
    }),
    it "with new value" (fun () => {
      let aString = "a";
      let bString = "b";

      let transient = Transient.create aString;
      transient |> Transient.update (fun owner str => bString) |> ignore;
      expect (transient |> Transient.get)
        |> toBeEqualToWith Equality.reference (fun s => s) bString;
    })
  ],
];
