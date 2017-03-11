open Set;
open Equality;
open Hash;
open HashMap;
open HashStrategy;
open Option.Operators;
open Pair;
open Seq;
open Stack;

type stackMultimap 'k 'v = {
  count: int,
  map: hashMap 'k (stack 'v),
};

let add (key: 'k) (value: 'v) ({ count, map }: stackMultimap 'k 'v): (stackMultimap 'k 'v) => {
  count: count + 1,
  map: map |> HashMap.alter key (fun stack => switch stack {
    | Some stack => stack
    | None => Stack.empty
  } |> Stack.addFirst value |> Option.return),
};

let addAllValues (key: 'k) (values: (seq 'v)) ({ count, map } as multimap: stackMultimap 'k 'v): (stackMultimap 'k 'v) => {
  let increment = ref 0;

  let newMap = map |> HashMap.alter key (fun stack => {
    switch stack {
    | Some stack =>
        let newStack = stack |> Stack.addFirstAll values;
        increment := (Stack.count newStack) - (Stack.count stack);
        Some newStack;
    | None =>
        let newStack = Stack.fromSeqReversed values;
        if (Stack.isEmpty newStack) None else {
          increment := (Stack.count newStack);
          Some newStack;
        }
    }
  });

  if (newMap === map) multimap else {
    count: count + !increment,
    map: newMap,
  };
};

let containsWith
    (equals: equality 'v)
    (key: 'k)
    (value: 'v)
    ({ map }: stackMultimap 'k 'v): bool =>
  map |> HashMap.tryGet key >>| Stack.containsWith equals value |? false;

let contains (key: 'k) (value: 'v) (multimap: stackMultimap 'k 'v): bool =>
  multimap |> containsWith Equality.structural key value;

let containsKey (key: 'k) ({ map }: stackMultimap 'k 'v): bool =>
  map |> HashMap.containsKey key;

let count ({ count }: stackMultimap 'k 'v) => count;

let empty: (stackMultimap 'k 'v) = {
  count: 0,
  map: HashMap.empty,
};

let emptyWith (hashStrategy: hashStrategy 'k): (stackMultimap 'k 'v) => {
  count: 0,
  map: HashMap.emptyWith hashStrategy,
};

let equalsWith
    (equals: equality 'v)
    (this: stackMultimap 'k 'v)
    (that: stackMultimap 'k 'v): bool =>
  HashMap.equalsWith (Stack.equalsWith equals) this.map that.map;

let equals (this: stackMultimap 'k 'v) (that: stackMultimap 'k 'v): bool =>
  HashMap.equalsWith Stack.equals this.map that.map;

let every (f: 'k => 'v => bool) ({ map }: stackMultimap 'k 'v): bool => {
  let f' k stack =>
    stack |> Stack.every (fun v => f k v);
  map |> HashMap.every f';
};

let forEach (f: 'k => 'v => unit) ({ map }: stackMultimap 'k 'v): unit => {
  let f' k stack =>
    stack |> Stack.forEach (fun v => f k v);
  map |> HashMap.forEach f';
};

let get (key: 'k) ({ map }: stackMultimap 'k 'v): (stack 'v) =>
  map |> HashMap.tryGet key |? Stack.empty;

let hash ({ map }: stackMultimap 'k 'v): int =>
  map |> HashMap.hashWith Stack.hash;

let hashWith (valueHash: hash 'v) ({ map }: stackMultimap 'k 'v): int =>
  map |> HashMap.hashWith (Stack.hashWith valueHash);

let isEmpty ({ map }: stackMultimap 'k 'v): bool =>
  map |> HashMap.isEmpty;

let isNotEmpty ({ map }: stackMultimap 'k 'v): bool =>
  map |> HashMap.isNotEmpty;

let keys ({ map }: stackMultimap 'k 'v): (set 'k) =>
  map |> HashMap.keys;

let none (f: 'k => 'v => bool) ({ map }: stackMultimap 'k 'v): bool => {
  let f' k stack =>
    stack |> Stack.none (fun v => f k v);
  map |> HashMap.every f';
};

let reduce (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) ({ map }: stackMultimap 'k 'v): 'acc => {
  let rec reducer acc key values =>
    values |> Stack.reduce (fun acc v => f acc key v) acc;

  map |> HashMap.reduce reducer acc;
};

let remove (key: 'k) ({ count, map } as stackMultimap: stackMultimap 'k 'v): (stackMultimap 'k 'v) =>
  map |> HashMap.tryGet key >>| (fun stack => ({
    count: count - (Stack.count stack),
    map: map |> HashMap.remove key,
  })) |? stackMultimap;

let removeAll ({ map }: stackMultimap 'k 'v): (stackMultimap 'k 'v) =>
  { count: 0, map: map |> HashMap.removeAll };

let some (f: 'k => 'v => bool) ({ map }: stackMultimap 'k 'v): bool => {
  let f' k stack =>
    stack |> Stack.some (fun v => f k v);
  map |> HashMap.some f';
};

let toSeq ({ map }: stackMultimap 'k 'v): (seq ('k, 'v)) => map
  |> HashMap.toSeq
  |> Seq.flatMap (
    fun (k, stack) =>
      stack |> Stack.toSeq |> Seq.map (Pair.create k)
  );

let tryFind (f: 'k => 'v => bool) ({ map }: stackMultimap 'k 'v): (option ('k, 'v)) => {
  let result = ref None;
  let f' k set => set |> Stack.tryFind (
    fun v => if (f k v) { result := Some (k, v); true } else false
  ) |> Option.isNotEmpty;
  map |> HashMap.tryFind f' |> ignore;
  !result
};

let find (f: 'k => 'v => bool) (multimap: stackMultimap 'k 'v): ('k, 'v) =>
  multimap |> tryFind f |> Option.first;

let values ({ map }: stackMultimap 'k 'v): (seq 'v) =>
  map |> HashMap.values |> Seq.flatMap Stack.toSeq;
