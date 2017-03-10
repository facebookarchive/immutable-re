open CopyOnWriteArray;
open Equality;
open Option.Operators;
open Seq;

type equalityMap 'k 'v = copyOnWriteArray ('k, 'v);

let entryFinder (equals: equality 'k) (key: 'k) (entry: ('k, _)): bool => {
  let (entryKey, _) = entry;
  equals key entryKey;
};

let alter (equals: equality 'k) (key: 'k) (f: option 'v => option 'v) (map: equalityMap 'k 'v): (equalityMap 'k 'v) =>
  map |> CopyOnWriteArray.tryIndexOf (entryFinder equals key) >>| (fun index => {
    let (entryKey, entryValue) = map |> CopyOnWriteArray.get index;
    switch (f (Some entryValue)) {
      | Some newValue => map |> CopyOnWriteArray.update index (entryKey, newValue)
      | None => map |> CopyOnWriteArray.removeAt index;
    }
  }) |? map;

let contains
    (keyEquals: equality 'k)
    (valueEquals: equality 'v)
    (key: 'k)
    (value: 'v)
    (map: equalityMap 'k 'v) =>
  map |> CopyOnWriteArray.tryFind (entryFinder keyEquals key)
    >>| (fun (_, v) => valueEquals v value)
    |> Option.isNotEmpty;

let containsKey (equals: equality 'k) (key: 'k) (map: equalityMap 'k 'v) =>
  map |> CopyOnWriteArray.tryFind (entryFinder equals key) |> Option.isNotEmpty;

let count = CopyOnWriteArray.count;

let empty = [||];

let every (f: 'k => 'v => bool) (map: equalityMap 'k 'v): bool =>
  map |> CopyOnWriteArray.every (fun (k, v) => f k v);

let find (f: 'k => 'v => bool) (map: equalityMap 'k 'v): ('k, 'v) =>
  map |> CopyOnWriteArray.find (fun (k, v) => f k v);

let rec first = CopyOnWriteArray.first;

let forEach (f: 'k => 'v => unit) (map: equalityMap 'k 'v): 'acc =>
  map |> CopyOnWriteArray.forEach (fun (k, v) => f k v);

let get (equals: equality 'k) (key: 'k) (map: equalityMap 'k 'v): 'v => {
  let (_, v) = map |> CopyOnWriteArray.find (entryFinder equals key);
  v
};

let none (f: 'k => 'v => bool) (map: equalityMap 'k 'v): bool =>
  map |> CopyOnWriteArray.none (fun (k, v) => f k v);

let put (equals: equality 'k) (key: 'k) (value: 'v) (map: equalityMap 'k 'v): (equalityMap 'k 'v) =>
  alter equals key (Functions.return @@ Option.return @@ value) map;

let reduce (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) (map: equalityMap 'k 'v): 'acc =>
  map |> CopyOnWriteArray.reduce (fun acc (k, v) => f acc k v) acc;

let some (f: 'k => 'v => bool) (map: equalityMap 'k 'v): bool =>
  map |> CopyOnWriteArray.some (fun (k, v) => f k v);

let toSeq = CopyOnWriteArray.toSeq;

let tryFind (f: 'k => 'v => bool) (map: equalityMap 'k 'v): (option ('k, 'v)) =>
  map |> CopyOnWriteArray.tryFind (fun (k, v) => f k v);

let tryGet (equals: equality 'k) (key: 'k) (map: equalityMap 'k 'v): (option 'v) =>
  map |> CopyOnWriteArray.tryFind (entryFinder equals key) >>= (fun (k, v) => Some v);

let values (map: equalityMap 'k 'v): (seq 'v) =>
  map |> toSeq |> Seq.map (fun (_, v) => v);