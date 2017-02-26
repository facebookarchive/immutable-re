/* FIXME: Ideally use vector, once it implements removeAt */
open CopyOnWriteArray;
open Equality;
open Option.Operators;
open Seq;

type equalitySet 'a = {
  array: copyOnWriteArray 'a,
  equality: equality 'a,
};

let add (value: 'a) ({ array, equality } as set: equalitySet 'a): (equalitySet 'a) =>
  if (CopyOnWriteArray.containsWith equality value array) set
  else {
    array: array |> CopyOnWriteArray.addLast value,
    equality,
  };

let contains (value: 'a) ({ array, equality }: equalitySet 'a): bool =>
  array |> CopyOnWriteArray.containsWith equality value;


let count ({ array, equality }: equalitySet 'a): int =>
  array |> CopyOnWriteArray.count;

let emptyWith (equality: equality 'a): (equalitySet 'a) => {
  array: [||],
  equality,
};

let remove (value: 'a) ({ array, equality } as equalitySet: equalitySet 'a): (equalitySet 'a) =>
  array |> CopyOnWriteArray.tryIndexOf (equality value)  >>| (fun index => {
    array: array |> CopyOnWriteArray.removeAt index,
    equality,
  }) |? equalitySet;

let toSeq ({ array }: equalitySet 'a): (seq 'a) => array |> CopyOnWriteArray.toSeq;
