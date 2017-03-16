type t 'a = {
  count: int,
  list: list 'a,
};

let addFirst (value: 'a) ({ count, list }: t 'a): (t 'a) => ({
  count: count + 1,
  list: [value, ...list],
});

let addFirstAll (values: Iterable.t 'a) ({ count, list }: t 'a): (t 'a) => {
  let newCount = ref count;

  let newList = values |> Iterable.reduce
    (fun acc next => {
      newCount := !newCount + 1;
      [next, ...acc]
    })
    list;

  { count: !newCount, list: newList }
};

let compare
    ({ list: thisList } as this: t 'a)
    ({ list: thatList } as that: t 'a): Ordering.t =>
  if (this === that) Ordering.equal
  else ImmList.compare thisList thatList;

let compareWith
    (valueCompare: Comparator.t 'a)
    ({ list: thisList } as this: t 'a)
    ({ list: thatList } as that: t 'a): Ordering.t =>
  if (this === that) Ordering.equal
  else ImmList.compareWith valueCompare thisList thatList;

let contains (value: 'a) ({ list }: t 'a): bool =>
  list |> ImmList.contains value;

let containsWith (valueEquals: Equality.t 'a) (value: 'a) ({ list }: t 'a): bool =>
  list |> ImmList.containsWith valueEquals value;

let count ({ count }: t 'a): int => count;

let empty: t 'a = {
  count: 0,
  list: [],
};

let equals
    ({ count: thisCount, list: thisList } as this: t 'a)
    ({ count: thatCount, list: thatList } as that: t 'a): bool =>
  if (this === that) true
  else if (thisCount != thatCount) false
  else ImmList.equals thisList thatList;

let equalsWith
    (valueEquals: Equality.t 'a)
    ({ count: thisCount, list: thisList } as this: t 'a)
    ({ count: thatCount, list: thatList } as that: t 'a): bool =>
  if (this === that) true
  else if (thisCount != thatCount) false
  else ImmList.equalsWith valueEquals thisList thatList;

let every (f: 'a => bool) ({ list }: t 'a): bool =>
  list |> ImmList.every f;

let find (f: 'a => bool) ({ list }: t 'a): 'a =>
  list |> ImmList.find f;

let first ({ list }: t 'a): 'a => list |> ImmList.first;

let forEach (f: 'a => unit) ({ list }: t 'a): unit =>
  list |> ImmList.forEach f;

let fromList (list: list 'a): (t 'a) =>
  { count: list |> ImmList.count, list };

let fromReversed (iter: Iterable.t 'a): (t 'a) =>
  empty |> addFirstAll iter;

let hash ({ list }: t 'a): int =>
  ImmList.hash list;

let hashWith (valueHash: Hash.t 'a) ({ list }: t 'a): int =>
  ImmList.hashWith valueHash list;

let isEmpty ({ list }: t 'a): bool =>
  list |> ImmList.isEmpty;

let isNotEmpty ({ list }: t 'a): bool =>
  list |> ImmList.isNotEmpty;

let mapReverse (f: 'a => 'b) ({ count, list }: t 'a): (t 'b) => {
  count,
  list: list |> ImmList.mapReverse f,
};

let none (f: 'a => bool) ({ list }: t 'a): bool =>
  list |> ImmList.none f;

let reduce (f: 'acc => 'a => 'acc ) (acc: 'acc) ({ list }: t 'a): 'acc =>
  list |> ImmList.reduce f acc;

let removeAll (_: t 'a): (t 'a) => empty;

let removeFirst ({ count, list }: t 'a): (t 'a) => ({
  count: count - 1,
  list: switch list {
    | [_, ...tail] => tail
    | [] => failwith "stack is empty"
  },
});

let return (value: 'a): (t 'a) => {
  count: 1,
  list: [value],
};

let reverse ({ count, list }: t 'a): (t 'a) => {
  count,
  list: list |> ImmList.reverse,
};

let some (f: 'a => bool) ({ list }: t 'a): bool =>
  list |> ImmList.some f;

let toIterable ({ list }: t 'a): (Iterable.t 'a) =>
  Iterable.ofList list;

let toList ({ list }: t 'a): (list 'a) => list;

let toSeq ({ list }: t 'a): (Seq.t 'a) => Seq.ofList list;

let tryFind (f: 'a => bool) ({ list }: t 'a): (option 'a) =>
  list |> ImmList.tryFind f;

let tryFirst ({ list }: t 'a): (option 'a) => list |> ImmList.tryFirst;
