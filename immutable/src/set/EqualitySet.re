open Option.Operators;

/* FIXME: Ideally use vector, once it implements removeAt */
type t 'a = CopyOnWriteArray.t 'a;

let add (equality: Equality.t 'a) (value: 'a) (set: t 'a): (t 'a) =>
  if (CopyOnWriteArray.containsWith equality value set) set
  else (CopyOnWriteArray.addLast value set);

let contains = CopyOnWriteArray.containsWith;

let count = CopyOnWriteArray.count;

let empty = [||];

let remove (equality: Equality.t 'a) (value: 'a) (set: t 'a): (t 'a) =>
  set |> CopyOnWriteArray.tryIndexOf (equality value) >>| (fun index =>
    set |> CopyOnWriteArray.removeAt index
  ) |? set;

let toSeq = CopyOnWriteArray.toSeq;
