/* FIXME: Ideally use vector, once it implements removeAt */
open CopyOnWriteArray;
open Equality;
open Option.Operators;
open Seq;

type equalitySet 'a = copyOnWriteArray 'a;

let add (equality: equality 'a) (value: 'a) (set: equalitySet 'a): (equalitySet 'a) =>
  if (CopyOnWriteArray.containsWith equality value set) set
  else (CopyOnWriteArray.addLast value set);

let contains = CopyOnWriteArray.containsWith;

let count = CopyOnWriteArray.count;

let empty = [||];

let remove (equality: equality 'a) (value: 'a) (set: equalitySet 'a): (equalitySet 'a) =>
  set |> CopyOnWriteArray.tryIndexOf (equality value) >>| (fun index =>
    set |> CopyOnWriteArray.removeAt index
  ) |? set;

let toSeq = CopyOnWriteArray.toSeq;
