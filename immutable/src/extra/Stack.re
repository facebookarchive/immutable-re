open Seq;

type stack 'a = {
  count: int,
  list: list 'a,
};

let addFirst (value: 'a) ({ count, list }: stack 'a): (stack 'a) => ({
  count: count + 1,
  list: [value, ...list],
});

let add = addFirst;

let addAll (values: seq 'a) ({ count, list }: stack 'a): (stack 'a) => {
  let newCount = ref count;

  let newList = values |> Seq.reduce
    (fun acc next => {
      newCount := !newCount + 1;
      [next, ...acc]
    })
    list;

  { count: !newCount, list: newList }
};

let count ({ count, list }: stack 'a): int => count;

let empty: stack 'a = {
  count: 0,
  list: [],
};

let every (f: 'a => bool) ({ list }: stack 'a): bool =>
  list |> ImmList.every f;

let first ({ list }: stack 'a): 'a => list |> ImmList.first;

let fromList (list: list 'a): (stack 'a) =>
  { count: list |> ImmList.count, list };

let fromSeq (values: seq 'a): (stack 'a) => empty |> addAll values;

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

let reverse ({ count, list }: stack 'a): (stack 'a) => {
  count,
  list: list |> ImmList.reverse,
};

let some (f: 'a => bool) ({ list }: stack 'a): bool =>
  list |> ImmList.some f;

let toList ({ list }: stack 'a): (list 'a) => list;

let toSeq ({ list }: stack 'a): (seq 'a) => Seq.ofList list;

let tryFirst ({ list }: stack 'a): (option 'a) => list |> ImmList.tryFirst;
