let addFirst (value: 'a) (list: list 'a): (list 'a) =>
  [value, ...list];

let add = addFirst;

let rec countImpl (list: list 'a) (count: int): int => switch list {
  | [head, ...tail] => countImpl tail (count + 1)
  | [] => count
};

/* Not exposing in the public API for now. I'm somewhat of the opinion,
 * that you if you need the list length, you should use a Stack.
 * This function exists so that module can compute the length of a list.
 */
let count (list: list 'a): int => countImpl list 0;

let empty: (list 'a) = [];

let rec every (f: 'a => bool) (list: list 'a): bool => switch list {
  | [head, ...tail] => f head ? every f tail : false
  | [] => true
};

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

let removeAll (list: list 'a): (list 'a) => [];

let removeFirst (list: list 'a): (list 'a) => switch list {
  | [head, ...tail] => tail
  | [] => failwith "List is empty"
};

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
