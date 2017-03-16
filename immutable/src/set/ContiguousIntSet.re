type t = {
  count: int,
  start: int,
};

let empty: t = {
  start: 0,
  count: 0,
};

let create (start: int) (count: int): t => {
  Preconditions.failIf "count must be >= 0" (count < 0);
  if (count == 0) empty
  else { start, count };
};

let contains (value: int) ({ count, start }: t): bool =>
  value >= start && value < (start + count);

let count ({ count }: t): int => count;

let equals (this: t) (that: t): bool =>
  this.start == that.start && this.count == that.count;

let every (f: int => bool) ({ count, start }: t): bool => {
  let rec recurse f start count =>
     if (count == 0) true
     else if (f start) (recurse f (start + 1) (count - 1))
     else false;

  recurse f start count;
};

let find (f: int => bool) ({ count, start }: t): int => {
  let rec recurse f start count =>
    if (count == 0) (failwith "not found")
    else if (f start) start
    else recurse f (start + 1) (count - 1);

  recurse f start count;
};

let first ({ count, start }: t): int =>
  if (count == 0) (failwith "empty")
  else start;

let forEach (f: int => unit) ({ count, start }: t) => {
  let rec recurse f start count =>
    if (count == 0) ()
    else {
      f start;
      recurse f (start + 1) (count - 1);
    };
  recurse f start count;
};

let isEmpty ({ count }: t): bool => count == 0;

let isNotEmpty ({ count }: t): bool => count != 0;

let last ({ count, start }: t): int =>
  if (count == 0) (failwith "empty")
  else start + count - 1;

let none (f: int => bool) ({ count, start }: t): bool => {
  let rec recurse f start count =>
     if (count == 0) true
     else if (f start) false
     else (recurse f (start + 1) (count - 1));

  recurse f start count;
};

let reduce (f: 'acc => int => 'acc) (acc: 'acc) ({ count, start }: t): 'acc => {
  let rec recurse f start count acc =>
    if (count == 0) acc
    else {
      let acc = f acc start;
      recurse f (start + 1) (count - 1) acc;
    };
  recurse f start count acc;
};

let reduceRight (f: 'acc => int => 'acc) (acc: 'acc) ({ count, start }: t): 'acc => {
  let rec recurse f start count acc =>
    if (count == 0) acc
    else {
      let acc = f acc start;
      recurse f (start - 1) (count - 1) acc;
    };
  recurse f (start + count - 1) count acc;
};

let hash (set: t): int =>
  set |> reduce (Hash.reducer Hash.structural) Hash.initialValue;

let some (f: int => bool) ({ count, start }: t): bool => {
  let rec recurse f start count =>
     if (count == 0) false
     else if (f start) true
     else (recurse f (start + 1) (count - 1));

  recurse f start count;
};

let toSeq ({ count, start }: t): (Seq.t int) => {
  let rec recurse start count => fun () =>
    if (count == 0) Seq.Completed
    else Seq.Next start (recurse (start + 1) (count - 1));
  recurse start count
};

let toSeqReversed ({ count, start }: t): (Seq.t int) => {
  let rec recurse start count => fun () =>
    if (count == 0) Seq.Completed
    else Seq.Next start (recurse (start - 1) (count - 1));
  recurse (start + count - 1) count
};

let tryFind (f: int => bool) ({ count, start }: t): (option int) => {
  let rec recurse f start count =>
    if (count == 0) None
    else if (f start) (Some start)
    else recurse f (start + 1) (count - 1);

  recurse f start count;
};

let toIterable (set: t): (Iterable.t int) =>
  if (isEmpty set) Iterable.empty
  else { reduce: fun f acc => reduce f acc set };

let toKeyedIterable (set: t): (KeyedIterable.t int int) =>
  if (isEmpty set) KeyedIterable.empty
  else { reduce: fun f acc => set |> reduce
    (fun acc next => f acc next next)
    acc
  };

let toSet (set: t): (ImmSet.t int) => {
  contains: fun v => contains v set,
  count: count set,
  every: fun f => set |> every f,
  find: fun f => set |> find f,
  forEach: fun f => set |> forEach f,
  none: fun f => set |> none f,
  reduce: fun f acc => set |> reduce f acc,
  some: fun f => set |> some f,
  toSeq: toSeq set,
  tryFind: fun f => set |> tryFind f,
};

let toMap (set: t): (ImmMap.t int int) =>
  set |> toSet |> ImmMap.ofSet;

let tryFirst ({ count, start }: t): (option int) =>
  if (count == 0) None
  else (Some start);

let tryLast ({ count, start }: t): (option int) =>
  if (count == 0) None
  else (start + count - 1) |> Option.return;
