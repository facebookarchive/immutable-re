open Option.Operators;

type t 'a = array 'a;

let count (arr: t 'a): int => Array.length arr;

let addFirst (item: 'a) (arr: t 'a): (t 'a) => {
  let count = count arr;

  let retval = Array.make (count + 1) item;
  Array.blit arr 0 retval 1 count;
  retval
};

let addFirstAll (seq: Seq.t 'a) (arr: t 'a): (t 'a) =>
  /* FIXME: This implemenation is particularly bad. We can improve it
   * by using dynamic array allocations.
   */
  seq |> Seq.reduce (fun acc next => acc |> addFirst next) arr;

let addLast (item: 'a) (arr: t 'a): (t 'a) => {
  let count = count arr;

  let retval = Array.make (count + 1) item;
  Array.blit arr 0 retval 0 count;
  retval
};

let addLastAll (seq: Seq.t 'a) (arr: t 'a): (t 'a) =>
  /* FIXME: This implemenation is particularly bad. We can improve it
   * by using dynamic array allocations.
   */
  seq |> Seq.reduce (fun acc next => acc |> addLast next) arr;

let compareWith
    (valueCompare: Comparator.t 'a)
    (this: t 'a)
    (that: t 'a): Ordering.t => {
  let thisCount = count this;
  let thatCount = count that;

  let loopCount = min thisCount thatCount;

  let rec loop index =>
    index < loopCount ? {
      let cmp = valueCompare this.(index) that.(index);

      if (cmp === Ordering.equal) (loop (index + 1))
      else cmp
    } :
    index < thisCount ? Ordering.greaterThan :
    index < thatCount ? Ordering.lessThan :
    Ordering.equal;
  loop 0;
};

let compare (this: t 'a) (that: t 'a): Ordering.t =>
  compareWith Comparator.structural this that;

let empty: (t 'a) = [||];

let equalsWith
    (valueEquals: Equality.t 'a)
    (this: t 'a)
    (that: t 'a): bool => {
  let thisCount = count this;
  let thatCount = count that;

  let loopCount = min thisCount thatCount;

  let rec loop index => index < loopCount
    ? (valueEquals this.(index) that.(index) ? loop (index + 1) : false)
    : true;

  this === that ? true :
  thisCount != thatCount ? false :
  loop 0;
};

let equals (this: t 'a) (that: t 'a): bool =>
  equalsWith Equality.structural this that;

let every (f: 'a => bool) (arr: t 'a): bool => {
  let arrCount = count arr;
  let rec loop index =>
    index >= arrCount ? true :
    (f arr.(index)) ? loop (index + 1) :
    false;

  loop 0;
};

let everyWithIndex (f: int => 'a => bool) (arr: t 'a): bool => {
  let arrCount = count arr;
  let rec loop index =>
    index >= arrCount ? true :
    (f index arr.(index)) ? loop (index + 1) :
    false;

  loop 0;
};

let find (f: 'a => bool) (arr: t 'a): 'a => {
  let arrCount = count arr;

  let rec loop index => index < arrCount ? {
    let v = arr.(index);
    f v ? v : loop (index + 1)
  } : failwith "not found";

  loop 0;
};

let findWithIndex (f: int => 'a => bool) (arr: t 'a): 'a => {
  let arrCount = count arr;

  let rec loop index => index < arrCount ? {
    let v = arr.(index);
    f index v ? v : loop (index + 1)
  } : failwith "not found";

  loop 0;
};

let first (arr: t 'a): 'a => arr.(0);

let forEach (f: 'a => unit) (arr: t 'a): 'acc =>
  arr |> Array.iter f;

let forEachWithIndex (f: int => 'a => unit) (arr: t 'a): 'acc =>
  arr |> Array.iteri f;

let fromSeq (seq: Seq.t 'a): (t 'a) =>
  [||] |> addLastAll seq;

let fromSeqReversed (seq: Seq.t 'a): (t 'a) =>
  [||] |> addFirstAll seq;

let get (index: int) (arr: t 'a): 'a => arr.(index);

let lastIndex (arr: t 'a): int => count arr - 1;

let last (arr: t 'a): 'a => arr.(lastIndex arr);

let indexOf (f: 'a => bool) (arr: t 'a): int => {
  let arrCount = count arr;

  let rec loop index => index < arrCount ? {
    let v = arr.(index);
    f v ? index : loop (index + 1)
  } : failwith "not found";

  loop 0;
};

let indexOfWithIndex (f: int => 'a => bool) (arr: t 'a): int => {
  let arrCount = count arr;

  let rec loop index => index < arrCount ? {
    let v = arr.(index);
    f index v ? index : loop (index + 1)
  } : failwith "not found";

  loop 0;
};

let init = Array.init;

let insertAt (index: int) (item: 'a) (arr: t 'a): (t 'a) => {
  let count = count arr;

  Preconditions.failIfOutOfRange (count + 1) index;

  let retval = Array.make (count + 1) item;
  Array.blit arr 0 retval 0 index;
  Array.blit arr index retval (index + 1) (count - index);

  retval;
};

let isEmpty (arr: t 'a): bool => (count arr) == 0;

let isNotEmpty (arr: t 'a): bool => (count arr) != 0;

let concat (arrays: list (array 'a)): (array 'a) => {
  let newCount = arrays |> ImmList.reduce (fun acc i => acc + count i) 0;

  newCount == 0 ? [||] : {
    let retval = Array.make newCount (ImmList.find isNotEmpty arrays).(0);

    ImmList.reduce (fun index next => {
      let countNext = count next;
      Array.blit next 0 retval index countNext;
      index + countNext;
    }) 0 arrays |> ignore;

    retval;
  };
};

let none (f: 'a => bool) (arr: t 'a): bool => {
  let arrCount = count arr;
  let rec loop index =>
    index >= arrCount ? true :
    (f arr.(index)) ? false :
    loop (index + 1);

  loop 0;
};

let noneWithIndex (f: int => 'a => bool) (arr: t 'a): bool => {
  let arrCount = count arr;
  let rec loop index =>
    index >= arrCount ? true :
    (f index arr.(index)) ? false :
    loop (index + 1);

  loop 0;
};

let ofUnsafe (arr: array 'a): (t 'a) => arr;

let range
    (startIndex: int)
    (newCount: option int)
    (arr: t 'a): (t 'a) => {
  let newCount = newCount |? (count arr) - startIndex;
  startIndex == 0 && newCount == (count arr)
    ? arr
    : Array.sub arr startIndex newCount;
};

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) (arr: t 'a): 'acc =>
  Array.fold_left f acc arr;

let reduceWithIndex (f: 'acc => int => 'a => 'acc) (acc: 'acc) (arr: t 'a): 'acc => {
  let arrCount = count arr;
  let rec loop acc index => index < arrCount ? {
    let acc = f acc index arr.(index);
    loop acc (index + 1);
  } : acc;

  loop acc 0;
};

let reduceRight (f: 'acc => 'a => 'acc) (acc: 'acc) (arr: t 'a): 'acc =>
  Array.fold_right (Functions.flip f) arr acc;

let forEachReverse (f: 'a => unit) (arr: t 'a): unit =>
  arr |> reduceRight (fun _ next => f next) ();

let reduceRightWithIndex (f: 'acc => int => 'a => 'acc) (acc: 'acc) (arr: t 'a): 'acc => {
  let arrLastIndex = lastIndex arr;
  let rec loop acc index => index >= 0 ? {
    let acc = f acc index arr.(index);
    loop acc (index - 1);
  } : acc;

  loop acc arrLastIndex;
};

let forEachReverseWithIndex (f: int => 'a => unit) (arr: t 'a): unit =>
  arr |> reduceRightWithIndex (fun _ index next => f index next) ();

let hashWith (hash: Hash.t 'a) (arr: t 'a): int =>
  arr |> reduce (Hash.reducer hash) Hash.initialValue;

let hash (arr: t 'a): int =>
  hashWith Hash.structural arr;

let map (f: 'a => 'b) (arr: t 'a): (t 'b) => isNotEmpty arr
  ? {
    let initialValue = f arr.(0);
    let retval = Array.make (count arr) initialValue;
    arr |> reduce (fun acc next => { retval.(acc) = f next; acc + 1 }) 0 |> ignore;
    retval;
  }: [||];

let mapWithIndex (f: int => 'a => 'b) (arr: t 'a): (t 'b) => isNotEmpty arr
  ? {
    let initialValue = f 0 arr.(0);
    let retval = Array.make (count arr) initialValue;
    arr |> reduce (fun acc next => { retval.(acc) = f acc next; acc + 1 }) 0 |> ignore;
    retval;
  }: [||];

let mapReverse (f: 'a => 'b) (arr: t 'a): (t 'b) => isNotEmpty arr
  ? {
    let initialValue = f arr.(0);
    let retval = Array.make (count arr) initialValue;
    arr |> reduceRight (fun acc next => { retval.(acc) = f next; acc + 1 }) 0 |> ignore;
    retval;
  }: [||];

let mapReverseWithIndex (f: int => 'a => 'b) (arr: t 'a): (t 'b) => isNotEmpty arr
  ? {
    let arrCount = count arr;
    let initialValue = f 0 arr.(0);
    let retval = Array.make arrCount initialValue;
    arr |> reduce (fun acc next => { retval.(arrCount - acc - 1) = f acc next; acc + 1 }) 0 |> ignore;
    retval;
  }: [||];

let removeAll (_: t 'a): (t 'a) => empty;

let removeLast (arr: t 'a): (t 'a) => {
  let count = count arr;

  count == 0 ? failwith "Array is empty" :
  count == 1 ? [||] :
  Array.sub arr 0 (count - 1)
};

let removeAt (index: int) (arr: t 'a): (t 'a) => {
  let count = count arr;

  Preconditions.failIfOutOfRange count index;

  let newLength = count - 1;
  let anyItem = arr.(0);

  let retval = Array.make newLength anyItem;
  Array.blit arr 0 retval 0 index;
  Array.blit arr (index + 1) retval index (newLength - index);

  retval
};

let removeFirst (arr: t 'a): (t 'a) =>
  removeAt 0 arr;

let return (value: 'a): (t 'a) => [| value |];

let reverse (arr: t 'a): (t 'a) => {
  let count = count arr;
  Array.init count (fun i => arr.(count - i - 1))
};

let skip (startIndex: int) (arr: t 'a): (t 'a) => {
  let newCount = (count arr) - startIndex;
  Array.sub arr startIndex newCount;
};

let some (f: 'a => bool) (arr: t 'a): bool => {
  let arrCount = count arr;

  let rec loop index =>
    index >= arrCount ? false :
    (f arr.(index)) ? true :
    loop (index + 1);

  loop 0;
};

let containsWith (valueEquals: Equality.t 'a) (value: 'a) (arr: t 'a): bool =>
  some (valueEquals value) arr;

let contains (value: 'a) (list: t 'a): bool =>
  containsWith Equality.structural value list;

let someWithIndex (f: int => 'a => bool) (arr: t 'a): bool => {
  let arrCount = count arr;

  let rec loop index =>
    index >= arrCount ? false :
    (f index arr.(index)) ? true :
    loop (index + 1);

  loop 0;
};

let take (newCount: int) (arr: t 'a): (t 'a) =>
  Array.sub arr 0 newCount;

let toSeqReversed (arr: t 'a): (Seq.t 'a) => {
  let rec loop index => fun () => index < 0
    ? Seq.Completed
    : Seq.Next arr.(index) (loop (index - 1));
  loop (count arr - 1);
};

let toSeq (arr: t 'a): (Seq.t 'a) => {
  let arrCount = count arr;
  let rec loop index => fun () => index < arrCount
    ? Seq.Next arr.(index) (loop (index + 1))
    : Seq.Completed;
  loop 0;
};

let toSeqWithIndex (arr: t 'a): (Seq.t (int, 'a)) => {
  let arrCount = count arr;
  let rec loop index => fun () => index < arrCount
    ? Seq.Next (index, arr.(index)) (loop (index + 1))
    : Seq.Completed;
  loop 0;
};

let tryGet (index: int) (arr: t 'a): (option 'a) =>
  Preconditions.noneIfIndexOutOfRange (count arr) index (Functions.flip get arr);

let tryFind (f: 'a => bool) (arr: t 'a): (option 'a) => {
  let arrCount = count arr;

  let rec loop index => index < arrCount ? {
    let v = arr.(index);
    f v ? Some v : loop (index + 1)
  } : None;

  loop 0;
};

let tryFindWithIndex (f: int => 'a => bool) (arr: t 'a): (option 'a) => {
  let arrCount = count arr;

  let rec loop index => index < arrCount ? {
    let v = arr.(index);
    f index v ? Some v : loop (index + 1)
  } : None;

  loop 0;
};

let tryFirst (arr: t 'a): (option 'a) => tryGet 0 arr;

let tryIndexOf (f: 'a => bool) (arr: t 'a): (option int) => {
  let arrCount = count arr;

  let rec loop index => index < arrCount ? {
    let v = arr.(index);
    f v ? Some index : loop (index + 1)
  } : None;

  loop 0;
};

let tryIndexOfWithIndex (f: int => 'a => bool) (arr: t 'a): (option int) => {
  let arrCount = count arr;

  let rec loop index => index < arrCount ? {
    let v = arr.(index);
    f index v ? Some index : loop (index + 1)
  } : None;

  loop 0;
};

let tryLast (arr: t 'a): (option 'a) => tryGet ((count arr) - 1) arr;

let update (index: int) (item: 'a) (arr: t 'a): (t 'a) => {
  let arrCount = count arr;

  Preconditions.failIfOutOfRange arrCount index;

  let clone = Array.copy arr;
  clone.(index) = item;
  clone
};

let updateAll (f: int => 'a => 'a) (arr: t 'a): (t 'a) => {
  let arrCount = count arr;
  let clone = Array.copy arr;
  let rec loop index => index < arrCount ? {
    clone.(index) = f index arr.(index);
    loop (index + 1);
  }: clone;

  loop 0;
};

let updateWith (index: int) (f: 'a => 'a) (arr: t 'a): (t 'a) => {
  let count = count arr;

  Preconditions.failIfOutOfRange count index;

  let clone = Array.copy arr;
  clone.(index) = f arr.(index);
  clone
};

let toMap (arr: t 'a): (ImmMap.t int 'a) => {
  containsWith: fun equals index value => index >= 0 && index < count arr
    ? equals arr.(index) value
    : false,
  containsKey: fun index => index >= 0 && index < count arr,
  count: count arr,
  every: fun f => everyWithIndex f arr,
  find: fun f => {
    let index = indexOfWithIndex f arr;
    (index, arr.(index))
  },
  forEach: fun f => forEachWithIndex f arr,
  get: fun index => get index arr,
  none: fun f => noneWithIndex f arr,
  reduce: fun f acc => reduceWithIndex f acc arr,
  some: fun f => someWithIndex f arr,
  toSeq: toSeqWithIndex arr,
  tryFind: fun f => tryIndexOfWithIndex f arr >>| fun index => (index, arr.(index)),
  tryGet: fun i => tryGet i arr,
  values: toSeq arr,
};
