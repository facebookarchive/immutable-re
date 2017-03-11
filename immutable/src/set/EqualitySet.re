/* FIXME: Ideally use vector, once it implements removeAt */
open CopyOnWriteArray;
open Option.Operators;

type equalitySet 'a = copyOnWriteArray 'a;

let add (equality: Equality.t 'a) (value: 'a) (set: equalitySet 'a): (equalitySet 'a) =>
  if (CopyOnWriteArray.containsWith equality value set) set
  else (CopyOnWriteArray.addLast value set);

let contains = CopyOnWriteArray.containsWith;

let count = CopyOnWriteArray.count;

let empty = [||];

let remove (equality: Equality.t 'a) (value: 'a) (set: equalitySet 'a): (equalitySet 'a) =>
  set |> CopyOnWriteArray.tryIndexOf (equality value) >>| (fun index =>
    set |> CopyOnWriteArray.removeAt index
  ) |? set;

let toSeq = CopyOnWriteArray.toSeq;
