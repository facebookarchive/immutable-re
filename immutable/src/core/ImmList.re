open Comparator;
open Equality;
open Hash;
open Ordering;

let addFirst (value: 'a) (list: list 'a): (list 'a) =>
  [value, ...list];

let add = addFirst;

let rec compareWith
    (valueCompare: comparator 'a)
    (this: list 'a)
    (that: list 'a): ordering =>
  this === that ? Ordering.equal : switch (this, that) {
    | ([thisHead, ...thisTail], [thatHead, ...thatTail]) => switch (valueCompare thisHead thatHead) {
        | Equal => compareWith valueCompare thisTail thatTail
        | x => x
      }
    | ([], []) => Ordering.equal
    | (_, []) => Ordering.greaterThan
    | ([], _) => Ordering.lessThan
  };

let compare (this: list 'a) (that: list 'a): ordering =>
  compareWith Comparator.structural this that;

let containsWith (valueEquals: equality 'a) (value: 'a) (list: list 'a): bool => {
  let rec loop list => switch list {
    | [head, ..._] when valueEquals head value => true
    | [_, ...tail] => loop tail
    | [] => false
  };

  loop list;
};

let contains (value: 'a) (list: list 'a): bool =>
  containsWith Equality.structural value list;

let rec countImpl (list: list 'a) (count: int): int => switch list {
  | [head, ...tail] => countImpl tail (count + 1)
  | [] => count
};

let count (list: list 'a): int => countImpl list 0;

let empty: (list 'a) = [];

let rec equalsWith (valueEquals: equality 'a) (this: list 'a) (that: list 'a) => switch (this, that) {
  | ([thisHead, ...thisTail], [thatHead, ...thatTail]) => valueEquals thisHead thatHead
    ? equalsWith valueEquals thisTail thatTail
    : false;
  | ([], []) => true
  | _ => false
};

let equals (this: list 'a) (that: list 'a) =>
  equalsWith Equality.structural this that;

let rec every (f: 'a => bool) (list: list 'a): bool => switch list {
  | [head, ...tail] => f head ? every f tail : false
  | [] => true
};

let forEach (f: 'a => unit) (list: list 'a): unit =>
  list |> List.iter f;

let isEmpty (list: list 'a): bool => switch list {
  | [] => true
  | _ => false;
};

let isNotEmpty (list: list 'a): bool => switch list {
  | [] => false
  | _ => true;
};

let rec find (f: 'a => bool) (list: list 'a): 'a => switch list {
  | [head, ...tail] => f head ? head : find f tail
  | [] => failwith "not found"
};

let first (list: list 'a): 'a => switch list {
  | [head, ...tail] => head
  | [] => failwith "empty"
};

let rec mapReverseImpl (f: 'a => 'b) (src: list 'a) (dst: list 'b): (list 'b) => switch src {
  | [head, ...tail] => mapReverseImpl f tail [f head, ...dst]
  | [] => dst
};

let mapReverse (f: 'a => 'b) (list: list 'a): (list 'b) =>
  mapReverseImpl f list [];

let rec none (f: 'a => bool) (list: list 'a): bool => switch list {
  | [head, ...tail] => f head ? false : none f tail
  | [] => true
};

let rec reduce (f: 'acc => 'a => 'acc ) (acc: 'acc) (list: list 'a): 'acc => switch list {
  | [head, ...tail] =>
      let acc = f acc head;
      reduce f acc tail
  | [] => acc
};

let hashWith (hash: hash 'a) (list: list 'a): int =>
  list |> reduce (Hash.reducer hash) Hash.initialValue;

let hash (list: list 'a): int => hashWith Hash.structural list;

let removeAll (list: list 'a): (list 'a) => [];

let removeFirst (list: list 'a): (list 'a) => switch list {
  | [head, ...tail] => tail
  | [] => failwith "List is empty"
};

let return (value: 'a): (list 'a) => [value];

let rec reverseImpl (src: list 'a) (dst: list 'a): (list 'a) => switch src {
  | [head, ...tail] => reverseImpl tail [head, ...dst]
  | [] => dst
};

let reverse (list: list 'a): (list 'a) =>
  reverseImpl list [];

let rec some (f: 'a => bool) (list: list 'a): bool => switch list {
  | [head, ...tail] => f head || some f tail
  | [] => false
};

let rec tryFind (f: 'a => bool) (list: list 'a): (option 'a) => switch list {
  | [head, ...tail] => f head ? Some head : tryFind f tail
  | [] => None
};

let tryFirst (list: list 'a): (option 'a) => switch list {
  | [head, ...tail] => Some head
  | [] => None
};
