type stack 'a = {
  count: int,
  list: list 'a,
};

let addFirst (value: 'a) ({ count, list }: stack 'a): (stack 'a) => ({
  count: count + 1,
  list: [value, ...list],
});

let addFirstAll (values: Seq.t 'a) ({ count, list }: stack 'a): (stack 'a) => {
  let newCount = ref count;

  let newList = values |> Seq.reduce
    (fun acc next => {
      newCount := !newCount + 1;
      [next, ...acc]
    })
    list;

  { count: !newCount, list: newList }
};

let compare
    ({ list: thisList } as this: stack 'a)
    ({ list: thatList } as that: stack 'a): Ordering.t => this === that
  ? Ordering.equal
  : ImmList.compare thisList thatList;

let compareWith
    (valueCompare: Comparator.t 'a)
    ({ list: thisList } as this: stack 'a)
    ({ list: thatList } as that: stack 'a): Ordering.t => this === that
  ? Ordering.equal
  : ImmList.compareWith valueCompare thisList thatList;

let contains (value: 'a) ({ list }: stack 'a): bool =>
  list |> ImmList.contains value;

let containsWith (valueEquals: Equality.t 'a) (value: 'a) ({ list }: stack 'a): bool =>
  list |> ImmList.containsWith valueEquals value;

let count ({ count, list }: stack 'a): int => count;

let empty: stack 'a = {
  count: 0,
  list: [],
};

let equals
    ({ count: thisCount, list: thisList } as this: stack 'a)
    ({ count: thatCount, list: thatList } as that: stack 'a): bool =>
  this === that ? true :
  thisCount != thatCount ? false :
  ImmList.equals thisList thatList;

let equalsWith
    (valueEquals: Equality.t 'a)
    ({ count: thisCount, list: thisList } as this: stack 'a)
    ({ count: thatCount, list: thatList } as that: stack 'a): bool =>
  this === that ? true :
  thisCount != thatCount ? false :
  ImmList.equalsWith valueEquals thisList thatList;

let every (f: 'a => bool) ({ list }: stack 'a): bool =>
  list |> ImmList.every f;

let find (f: 'a => bool) ({ list }: stack 'a): 'a =>
  list |> ImmList.find f;

let first ({ list }: stack 'a): 'a => list |> ImmList.first;

let forEach (f: 'a => unit) ({ list }: stack 'a): unit =>
  list |> ImmList.forEach f;

let fromList (list: list 'a): (stack 'a) =>
  { count: list |> ImmList.count, list };

let fromSeqReversed (values: Seq.t 'a): (stack 'a) => empty |> addFirstAll values;

let hash ({ list }: stack 'a): int =>
  ImmList.hash list;

let hashWith (valueHash: Hash.t 'a) ({ list }: stack 'a): int =>
  ImmList.hashWith valueHash list;

let isEmpty ({ list }: stack 'a): bool =>
  list |> ImmList.isEmpty;

let isNotEmpty ({ list }: stack 'a): bool =>
  list |> ImmList.isNotEmpty;

let mapReverse (f: 'a => 'b) ({ count, list }: stack 'a): (stack 'b) => {
  count,
  list: list |> ImmList.mapReverse f,
};

let none (f: 'a => bool) ({ list }: stack 'a): bool =>
  list |> ImmList.none f;

let reduce (f: 'acc => 'a => 'acc ) (acc: 'acc) ({ list }: stack 'a): 'acc =>
  list |> ImmList.reduce f acc;

let removeAll (stack: stack 'a): (stack 'a) => empty;

let removeFirst ({ count, list }: stack 'a): (stack 'a) => ({
  count: count - 1,
  list: switch list {
    | [_, ...tail] => tail
    | [] => failwith "stack is empty"
  },
});

let return (value: 'a): (stack 'a) => {
  count: 1,
  list: [value],
};

let reverse ({ count, list }: stack 'a): (stack 'a) => {
  count,
  list: list |> ImmList.reverse,
};

let some (f: 'a => bool) ({ list }: stack 'a): bool =>
  list |> ImmList.some f;

let toList ({ list }: stack 'a): (list 'a) => list;

let toSeq ({ list }: stack 'a): (Seq.t 'a) => Seq.ofList list;

let tryFind (f: 'a => bool) ({ list }: stack 'a): (option 'a) =>
  list |> ImmList.tryFind f;

let tryFirst ({ list }: stack 'a): (option 'a) => list |> ImmList.tryFirst;
