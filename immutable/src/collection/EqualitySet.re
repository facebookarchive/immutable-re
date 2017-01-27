open Collection;
open Equality;
open Functions;
open Keyed;
open Option.Operators;
open Ordering;

/* FIXME: Ideally use vector, once its optimized more */
open TreeList;
open Seq;

type equalitySet 'a = {
  equality: equality 'a,
  treeList: treeList 'a,
};

let alter
    (predicate: 'a => ordering)
    (f: option 'a => option 'a)
    ({ equality, treeList } as set: equalitySet 'a): (equalitySet 'a) => treeList
  |> TreeList.toSeq
  |> Seq.tryFindIndex (fun a => (predicate a) === Equal)
  >>| (fun index => {
      let current = treeList |> TreeList.tryGet index;
      let next = f current;

      let current = current |> Option.get;
      switch next {
        | None => { equality, treeList: treeList |> TreeList.removeAt index }
        | (Some next) when current === next => set
        | (Some next) => { equality, treeList: treeList |> TreeList.update index next }
      }
    })
  |> Option.orCompute (fun () => switch (f None) {
      | Some v => { equality, treeList: treeList |> TreeList.add v }
      | _ => set
    }
  );

let contains (value: 'a) ({ equality, treeList }: equalitySet 'a): bool => treeList
  |> TreeList.toSeq
  |> Seq.tryFind (equality value)
  |> Option.isNotEmpty;

let count ({ treeList }: equalitySet 'a): int => treeList |> TreeList.count;

let empty: equalitySet 'a = { equality: Equality.structural, treeList: TreeList.empty };

let emptyWith (equality: equality 'a): (equalitySet 'a) => {
  equality,
  treeList: TreeList.empty,
};

let find (predicate: 'a => ordering) ({ treeList }: equalitySet 'a): (option 'a) => treeList
  |> TreeList.toSeq
  |> Seq.tryFind (fun a => (predicate a) === Equal);

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) ({ treeList }: equalitySet 'a): 'acc =>
  treeList |> TreeList.reduce f acc;

let reduceRight (f: 'acc => 'a => 'acc) (acc: 'acc) ({ treeList }: equalitySet 'a): 'acc =>
  treeList |> TreeList.reduceRight f acc;

let put (value: 'a) ({ equality, treeList }: equalitySet 'a): (equalitySet 'a) => treeList
  |> TreeList.toSeq
  |> Seq.tryFindIndex (equality value)
  >>| (fun index =>
      { equality, treeList: treeList |> TreeList.update index value }
    )
  |> Option.orCompute (fun () => { equality, treeList: treeList |> TreeList.add value });

let remove (value: 'a) ({ equality, treeList } as equalitySet: equalitySet 'a): (equalitySet 'a) => treeList
  |> TreeList.toSeq
  |> Seq.tryFindIndex (equality value)
  >>| (fun index => { equality, treeList: treeList |> TreeList.removeAt index })
  |? equalitySet;

let toSeq ({ treeList }: equalitySet 'a): (seq 'a) => treeList |> TreeList.toSeq;

let module EqualitySetSetImpl = SetImpl.Make {
  type t 'a = equalitySet 'a;

  let contains = contains;
  let count = count;
  let toSeq = toSeq;
};

let toCollection = EqualitySetSetImpl.toCollection;
let toKeyed = EqualitySetSetImpl.toKeyed;
