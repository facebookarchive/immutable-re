open Seq;

type stack 'a = {
  count: int,
  list: list 'a,
};

let add (value: 'a) ({ count, list }: stack 'a): (stack 'a) => ({
  count: count + 1,
  list: [value, ...list],
});

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

let fromList (list: list 'a): (stack 'a) =>
  { count: list |> ImmList.count, list };

let fromSeq (values: seq 'a): (stack 'a) => empty |> addAll values;

let reduce (f: 'acc => 'a => 'acc ) (acc: 'acc) ({ list }: stack 'a): 'acc =>
  list |> ImmList.reduce f acc;

let removeLast (value: 'a) ({ count, list }: stack 'a): (stack 'a) => ({
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

let toList ({ list }: stack 'a): (list 'a) => list;

let toSeq ({ list }: stack 'a): (seq 'a) => Seq.ofList list;

let tryLast ({ list }: stack 'a): (option 'a) => switch list {
  | [head, ...tail] => Some head
  | [] => None
};
