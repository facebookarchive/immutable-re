open Functions.Operators;
open Option.Operators;

type iterator 'a =
  | Next 'a (t 'a)
  | Completed

and t 'a = unit => iterator 'a;

let empty: (t 'a) = fun () => Completed;

let rec ofList (list: list 'a): (t 'a) => fun () => switch list {
  | [value] => Next value empty
  | [value, ...tail] => Next value (ofList tail)
  | [] => Completed
};

let return (value: 'a): (t 'a) => fun () =>
  Next value empty;

let module Stream = Stream.Make {
  type stream 'a = t 'a;

  let concatAll (seq: t (t 'a)): (t 'a) => {
    let rec continuedWith (continuation: t (t 'a)) (iter: (iterator 'a)): (iterator 'a) => switch (iter) {
      | Next value next =>
          Next value (next >> continuedWith continuation);
      | Completed => continuation () |> flattenIter
    }

    and flattenIter (iter: iterator (t 'a)): (iterator 'a) => switch iter {
      | Next value next => value () |> continuedWith next
      | Completed => Completed
    };

    fun () => seq () |> flattenIter;
  };

  let defer (f: unit => (t 'a)): (t 'a) => fun () => f () ();

  let empty = empty;

  let filter (f: 'a => bool) (seq: t 'a): (t 'a) => {
    let rec filterIter (iter: iterator 'a): iterator 'b => switch iter {
      | Next value next => f value
         ? Next value (next >> filterIter)
         : next () |> filterIter
      | Completed => Completed
    };

    fun () => seq () |> filterIter
  };

  let flatten (seq: t (t 'a)): (t 'a) =>
    concatAll seq;

  let rec inRangeImpl (start: int) (count: option int) (step: int): (t int) => fun () => switch count {
    | Some count when count == 0 => Completed
    | Some count => Next start (inRangeImpl (start + step) (Some (count - 1)) step)
    | None => Next start (inRangeImpl (start + step) None step)
  };

  let inRange (start: int) (count: option int) (step: int): (t int) => switch count {
    | Some count when count < 0 => failwith "Count must be great than 0"
    | _ => inRangeImpl start count step
  };

  let last (seq: t 'a): t 'a => {
    let rec loop acc seq => switch (seq ()) {
      | Next v next => next |> loop v
      | Completed => return acc
    };

    switch (seq ()) {
      | Next v next => next |> loop v
      | Completed => empty
    }
  };

  let map (f: 'a => 'b) (seq: t 'a): (t 'b) =>{
    let rec mapIter (iter: iterator 'a): (iterator 'b) => switch iter {
      | Next value next =>
          Next (f value) (fun () => next () |> mapIter)
      | Completed => Completed
    };

    fun () => seq () |> mapIter;
  };

  let ofList = ofList;

  let rec repeat (value: 'a) (count: option int): (t 'a) => switch count {
    | Some count when count > 0 => fun () => Next value (repeat value (Some (count - 1)))
    | Some count when count < 0 => failwith "count must be greater or equal to 0"
    | Some _ => empty
    | None =>
        let rec repeatForever () => Next value repeatForever;
        repeatForever
  };

  let rec scan
      (reducer: 'acc => 'a => 'acc)
      (acc: 'acc)
      (seq: t 'a): (t 'acc) => fun () => switch (seq ()) {
    | Next value next => {
        let acc = reducer acc value;
        Next acc (scan reducer acc next)
      }
    | Completed => Completed
  };

  let rec takeWhile (f: 'a => bool) (seq: t 'a): (t 'a) => fun () => switch (seq ()) {
    | Next value next =>
        (f value) ? Next value (takeWhile f next) : Completed;
    | Completed => Completed
  };

  let rec zip (seqs: list (t 'a)): (t (list 'a)) => fun () => {
    let iters = seqs |> ImmList.mapReverse Functions.call;

    let nextSeq: (t (list 'a)) = fun () =>
      (iters |> ImmList.mapReverse (fun next => switch next {
        | Next _ next => next
        | Completed => empty
      }) |> zip) ();

    iters |> ImmList.reduce (
      fun acc next  => switch (acc, next) {
        | (Some Completed         , _              ) => acc
        | (_                      , Completed      ) => Some Completed
        | (Some (Next values _), Next value _) =>
            Next [value, ...values] nextSeq |> Option.return
        | (None                   , Next value _) =>
            Next [value] nextSeq |> Option.return
      }
    ) None |? Completed;
  };
};

let buffer = Stream.buffer;

let rec compareWith (valueCompare: Comparator.t 'a) (this: t 'a) (that: t 'a): Ordering.t =>
  this === that ? Ordering.equal : switch (this (), that ()) {
    | (Next thisValue thisNext, Next thatValue thatNext) => switch (valueCompare thisValue thatValue) {
        | Equal => compareWith valueCompare thisNext thatNext
        | x => x
      }
    | (Completed, Completed) => Ordering.equal
    | (Next _ _, Completed) => Ordering.greaterThan
    | (Completed, Next _ _) => Ordering.lessThan
};

let compare (this: t 'a) (that: t 'a): Ordering.t =>
  compareWith Comparator.structural this that;

let concat = Stream.concat;

let concatAll = Stream.concatAll;

let concatMap = Stream.concatMap;

let rec containsWith (valueEquals: Equality.t 'a) (value: 'a) (seq: t 'a): bool => switch (seq ()) {
  | Next next _ when valueEquals next value => true
  | Next _ nextSeq => containsWith valueEquals value nextSeq
  | Completed => false
};

let contains (value: 'a) (seq: t 'a): bool =>
  containsWith Equality.structural value seq;

let defer = Stream.defer;

let distinctUntilChanged = Stream.distinctUntilChanged;

let distinctUntilChangedWith = Stream.distinctUntilChangedWith;

let doOnNext = Stream.doOnNext;

let mapWithIndex (f: int => 'a => 'b) (seq: t 'a): (t 'b) => seq
  |> Stream.scan (fun (i, _) v => (i, Some v)) (0, None)
  |> Stream.map (fun (i, v) => f i (Option.first v));

let rec every (f: 'a => bool) (seq: t 'a): bool => switch ( seq () ) {
  | Next value next => f value ? every f next : false
  | Completed => true
};

let equalsWith (equality: Equality.t 'a) (that: t 'a) (this: t 'a): bool =>
  (that === this) ||
  (Stream.zipLongest2 that this |> every (fun (that, this) => switch (that, this) {
    | (Some that, Some this) => equality that this
    | (Some _, None) => false
    | (None, Some _) => false
    | (None, None) => true
  }));

let equals (that: t 'a) (this: t 'a): bool =>
  equalsWith Equality.structural that this;

let isEmpty (seq: t 'a): bool => switch (seq ()) {
  | Next  _ => false
  | Completed => true
};

let isNotEmpty (seq: t 'a): bool => switch (seq ()) {
  | Next  _ => true
  | Completed => false
};

let filter = Stream.filter;

let first (seq: t 'a): 'a => switch (seq ()) {
  | Next value _ => value
  | Completed => failwith "Seq is empty"
};

let find (predicate: 'a => bool) (seq: t 'a): 'a =>
  seq |> Stream.tryFind predicate |> first;

let flatMap = Stream.flatMap;

let flatten = Stream.flatten;

let inRange = Stream.inRange;

let last (seq: t 'a): 'a =>
  seq |> Stream.last |> first;

let map = Stream.map;

let rec none (f: 'a => bool) (seq: t 'a): bool => switch ( seq () ) {
  | Next value next => try (
      f value ? false : none f next
    ){ | exn => raise exn; }
  | Completed => true
};

let ofOption (opt: option 'a): (t 'a) => switch opt {
  | Some a => return a
  | None => empty
};

let scan = Stream.scan;

let repeat = Stream.repeat;

let skip = Stream.skip;

let skipWhile = Stream.skipWhile;

let rec some (f: 'a => bool) (seq: t 'a): bool => switch (seq ()) {
  | Next value next => f value || some f next;
  | Completed => false
};

let startWith = Stream.startWith;

let take = Stream.take;

let takeWhile = Stream.takeWhile;

let tryFirst (seq: t 'a): (option 'a) => switch (seq ()) {
  | Next value _ => Some value
  | Completed => None
};

let tryFind (predicate: 'a => bool) (seq: t 'a): (option 'a) =>
  seq |> Stream.tryFind predicate |> tryFirst;

let tryFindIndex (predicate: 'a => bool) (seq: t 'a): (option int) => seq
  |> mapWithIndex Pair.create
  |> tryFind (fun (_, value) => predicate value)
  >>| fst;

let get (index: int) (seq: t 'a): 'a =>
  index < 0 ? failwith "index < 0" : seq |> Stream.skip index |> first;

let tryGet (index: int) (seq: t 'a): (option 'a) =>
  index < 0 ? None : seq |> Stream.skip index |> tryFirst;

let tryLast (seq: t 'a) => seq |> Stream.last |> tryFirst;

let forEach (onNext: 'a => unit) (seq: t 'a) =>
  seq |> Stream.doOnNext onNext |> tryLast |> ignore;

let reduce (reducer: 'acc => 'a => 'acc) (initialValue: 'acc) (seq: t 'a): 'acc =>
  seq |> Stream.reduce reducer initialValue |> tryFirst |? initialValue;

let count (seq: t 'a): int => seq |> reduce
  (fun acc _ => succ acc)
  0;

let hashWith (hash: (Hash.t 'a)) (seq: t 'a): int => seq
  |> reduce (Hash.reducer hash) Hash.initialValue;

let hash (seq: t 'a): int =>
  hashWith Hash.structural seq;

let zip = Stream.zip;

let zip2 = Stream.zip2;

let zip3 = Stream.zip3;

let zipLongest = Stream.zipLongest;

let zipLongest2 = Stream.zipLongest2;

let zipLongest3 = Stream.zipLongest3;

let listAddFirstAll (seq: t 'a) (list: list 'a): (list 'a) =>
  seq |> reduce (fun acc next => acc |> ImmList.addFirst next) list;

let listFromSeqReverse (seq: t 'a): (list 'a) =>
  [] |> listAddFirstAll seq;
