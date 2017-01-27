/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Equality;
open Functions;
open Functions.Operators;
open Hash;
open Option.Operators;
open Pair;
open Stream;

type iterator 'a =
  | Next 'a  (seq 'a)
  | Completed

and seq 'a = unit => iterator 'a;

let empty: (seq 'a) = fun () => Completed;

let return (value: 'a): seq ('a) => fun () =>
  Next value empty;

let module Stream = Stream.Make {
  type stream 'a = seq 'a;

  let concatAll (seq: seq (seq 'a)): (seq 'a) =>{
    let rec continuedWith (continuation: seq (seq 'a)) (iter: (iterator 'a)): (iterator 'a) => switch (iter) {
      | Next value next =>
          Next value (next >> continuedWith continuation);
      | Completed => continuation () |> flattenIter
    }

    and flattenIter (iter: iterator (seq 'a)): (iterator 'a) => switch (iter) {
      | Next value next => value () |> continuedWith next
      | Completed => Completed
    };

    fun () => seq () |> flattenIter;
  };

  let defer (f: unit => seq 'a): (seq 'a) => fun () => f () ();

  let empty = empty;

  let filter (f: 'a => bool) (seq: seq 'a): (seq 'a) => {
    let rec filterIter (iter: iterator 'a): iterator 'b => switch (iter) {
      | Next value next => f value
         ? Next value (next >> filterIter)
         : next () |> filterIter
      | Completed => Completed
    };

    fun () => seq () |> filterIter
  };

  let flatten (seq: seq (seq 'a)): (seq 'a) =>
    concatAll seq;

  let rec inRangeImpl (start: int) (count: option int) (step: int): (seq int) => fun () => switch count {
    | Some count when count == 0 => Completed
    | Some count => Next start (inRangeImpl (start + step) (Some (count - 1)) step)
    | None => Next start (inRangeImpl (start + step) None step)
  };

  let inRange (start: int) (count: option int) (step: int): (seq int) => switch count {
    | Some count when count < 0 => failwith "Count must be great than 0"
    | _ => inRangeImpl start count step
  };

  let last (seq: seq 'a): seq 'a => {
    let rec loop acc seq => switch (seq ()) {
      | Next v next => next |> loop v
      | Completed => return acc
    };

    switch (seq ()) {
      | Next v next => next |> loop v
      | Completed => empty
    }
  };

  let map (f: 'a => 'b) (seq: seq 'a): (seq 'b) =>{
    let rec mapIter (iter: iterator 'a): (iterator 'b) => switch (iter) {
      | Next value next =>
          Next (f value) (fun () => next () |> mapIter)
      | Completed => Completed
    };

    fun () => seq () |> mapIter;
  };

  let rec ofList (list: list 'a): (seq 'a) => fun () => switch (list) {
    | [value] => Next value empty
    | [value, ...tail] => Next value (ofList tail)
    | [] => Completed
  };

  let rec repeat (value: 'a) (count: option int): (seq 'a) => switch count {
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
      (seq: seq 'a): (seq 'acc) => fun () => switch (seq ()) {
    | Next value next => {
        let acc = reducer acc value;
        Next acc (scan reducer acc next)
      }
    | Completed => Completed
  };

  let rec takeWhile (f: 'a => bool) (seq: seq 'a): (seq 'a) => fun () => switch (seq ()) {
    | Next value next =>
        (f value) ? Next value (takeWhile f next) : Completed;
    | Completed => Completed
  };

  let rec zip (seqs: list (seq 'a)): (seq (list 'a)) => fun () => {
    let iters = seqs |> ImmList.mapReverse call;

    let nextSeq: (seq (list 'a)) = fun () =>
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

let concat = Stream.concat;

let concatAll = Stream.concatAll;

let concatMap = Stream.concatMap;

let defer = Stream.defer;

let distinctUntilChanged = Stream.distinctUntilChanged;

let distinctUntilChangedWith = Stream.distinctUntilChangedWith;

let doOnNext = Stream.doOnNext;

let mapWithIndex (f: int => 'a => 'b) (seq: seq 'a): (seq 'b) => seq
  |> Stream.scan (fun (i, _) v => (i, Some v)) (0, None)
  |> Stream.map (fun (i, v) => f i (Option.get v));

let doOnNextWithIndex (onNext: int => 'a => unit) (seq: seq 'a): (seq 'a) => seq
  |> mapWithIndex Pair.create
  |> Stream.doOnNext (fun (i, v) => onNext i v)
  |> Stream.map snd;

let rec every (f: 'a => bool) (seq: seq 'a): bool => switch ( seq () ) {
  | Next value next => f value ? every f next : false
  | Completed => true
};

let equalsWith (equality: equality 'a) (that: seq 'a) (this: seq 'a): bool =>
  (that === this) ||
  (Stream.zipLongest2 that this |> every (fun (that, this) => switch (that, this) {
    | (Some that, Some this) => equality that this
    | (Some _, None) => false
    | (None, Some _) => false
    | (None, None) => true
  }));

let equals (that: seq 'a) (this: seq 'a): bool =>
  equalsWith Equality.structural that this;

let isEmpty (seq: seq 'a): bool => switch (seq ()) {
  | Next  _ => false
  | Completed => true
};

let isNotEmpty (seq: seq 'a): bool => switch (seq ()) {
  | Next  _ => true
  | Completed => false
};

let filter = Stream.filter;

let flatMap = Stream.flatMap;

let flatten = Stream.flatten;

let rec flatMapWithIndex (f: int => 'a => seq 'b) (seq: seq 'a): (seq 'b) =>
  seq |> mapWithIndex f |> flatten;

let inRange = Stream.inRange;

let map = Stream.map;

let rec none (f: 'a => bool) (seq: seq 'a): bool => switch ( seq () ) {
  | Next value next => try (
      f value ? false : none f next
    ){ | exn => raise exn; }
  | Completed => true
};

let ofList = Stream.ofList;

let ofOption = Stream.ofOption;

let scan = Stream.scan;

let rec scanWithIndex (reducer: 'acc => int => 'a => 'acc) (acc: 'acc) (seq: seq 'a): (seq 'acc) => seq
  |> mapWithIndex Pair.create
  |> Stream.scan
    (fun acc (index, next) => reducer acc index next)
    acc;

let repeat = Stream.repeat;

let skip = Stream.skip;

let skipWhile = Stream.skipWhile;

let rec some (f: 'a => bool) (seq: seq 'a): bool => switch (seq ()) {
  | Next value next => f value || some f next;
  | Completed => false
};

let startWith = Stream.startWith;

let take = Stream.take;

let takeWhile = Stream.takeWhile;

let tryFirst (seq: seq 'a): (option 'a) => switch (seq ()) {
  | Next value _ => Some value
  | Completed => None
};

let tryFind (predicate: 'a => bool) (seq: seq 'a): (option 'a) =>
  seq |> Stream.tryFind predicate |> tryFirst;

let tryFindIndex (predicate: 'a => bool) (seq: seq 'a): (option int) => seq
  |> mapWithIndex Pair.create
  |> tryFind (fun (index, value) => predicate value)
  >>| fst;

let tryGet (index: int) (seq: seq 'a): (option 'a) =>
  index < 0 ? None : seq |> Stream.skip index |> tryFirst;

let tryLast (seq: seq 'a) => seq |> Stream.last |> tryFirst;

let rec forEach (onNext: 'a => unit) (seq: seq 'a) =>
  seq |> Stream.doOnNext onNext |> tryLast |> ignore;

let rec forEachWithIndex (onNext: int => 'a => unit) (seq: seq 'a) =>
  seq |> doOnNextWithIndex onNext |> tryLast |> ignore;

let reduce (reducer: 'acc => 'a => 'acc) (initialValue: 'acc) (seq: seq 'a): 'acc =>
  seq |> Stream.reduce reducer initialValue |> tryFirst |? initialValue;

let reduceWithIndex (reducer: 'acc => int => 'a => 'acc) (initialValue: 'acc) (seq: seq 'a): 'acc =>
  seq |> scanWithIndex reducer initialValue |> tryLast |? initialValue;

let count (seq: seq 'a): int => seq |> reduce
  (fun acc _ => succ acc)
  0;

let hash (hash: (hash 'a)) (seq: seq 'a): int => seq
  |> reduce (fun acc next => (31 * acc) + (hash next)) 17;

let toReversedList (seq: seq 'a): (list 'a) => seq |> (reduce
  (fun (acc: list 'a) next => [next, ...acc])
  []
);

let zip = Stream.zip;

let zip2 = Stream.zip2;

let zip3 = Stream.zip3;

let zipLongest = Stream.zipLongest;

let zipLongest2 = Stream.zipLongest2;

let zipLongest3 = Stream.zipLongest3;
