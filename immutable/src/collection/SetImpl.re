open Collection;
open Equality;
open Functions;
open Keyed;
open Option.Operators;
open Pair;
open Seq;

module type SetBase = {
  type t 'a;

  let contains: 'a => (t 'a) => bool;
  let count: (t 'a) => int;
  let toSeq: (t 'a) => (seq 'a);
};

module type S = {
  type t 'a;

  let toCollection: (t 'a) => (collection 'a);
  let toKeyed: (t 'a) => (keyed 'a 'a);
};

let module Make: (
  X: SetBase
) => S with type t 'a = X.t 'a = fun (X: SetBase) => {
  type t 'a = X.t 'a;

  let toCollection (set: t 'a): (collection 'a) => {
    contains: fun a => set |> X.contains a,
    count: (set |> X.count),
    toSeq: (set |> X.toSeq),
  };

  let toKeyed (set: t 'a): (keyed 'a 'a) => Keyed.create
    count::(set |> X.count)
    seq::(set |> X.toSeq |> Seq.map Pair.pairify)
    tryGet::(fun v => set |> X.contains v ? Some v : None);
};
