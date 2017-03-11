open Choice;
open Functions.Operators;
open Option.Operators;

module type StreamBase = {
  type stream 'a;

  let concatAll: (stream (stream 'a)) => (stream 'a);
  let defer: (unit => (stream 'a)) => (stream 'a);
  let empty: stream 'a;
  let filter: ('a => bool) => (stream 'a) => (stream 'a);
  let flatten: (stream (stream 'a)) => (stream 'a);
  let inRange: int => (option int) => int => (stream int);
  let last: (stream 'a) => (stream 'a);
  let map: ('a => 'b) => (stream 'a) => (stream 'b);
  let ofList: (list 'a) => (stream 'a);
  let repeat: 'a => (option int) => (stream 'a);
  let scan: ('acc => 'a => 'acc) => 'acc => (stream 'a) => (stream 'acc);
  let takeWhile: ('a => bool) => (stream 'a) => (stream 'a);
  let zip: (list (stream 'a)) => (stream (list 'a));
};

module type Stream = {
  type stream 'a;

  let buffer: int => int => (stream 'a) => (stream (list 'a));
  let concat: (list (stream 'a)) => (stream 'a);
  let concatAll: (stream (stream 'a)) => (stream 'a);
  let concatMap: ('a => (stream 'b)) => (stream 'a) => (stream 'b);
  let defer: (unit => (stream 'a)) => (stream 'a);
  let distinctUntilChanged: (stream 'a) => (stream 'a);
  let distinctUntilChangedWith: (Equality.t 'a) => (stream 'a) => (stream 'a);
  let doOnNext: ('a => unit) => (stream 'a) => (stream 'a);
  let empty: stream 'a;
  let filter: ('a => bool) => (stream 'a) => (stream 'a);
  let first: (stream 'a) => (stream 'a);
  let flatMap: ('a => (stream 'b)) => (stream 'a) => (stream 'b);
  let flatten: (stream (stream 'a)) => (stream 'a);
  let inRange: int => (option int) => int => (stream int);
  let last: (stream 'a) => (stream 'a);
  let map: ('a => 'b) => (stream 'a) => (stream 'b);
  let reduce: ('acc => 'a => 'acc) => 'acc => (stream 'a) => (stream 'acc);
  let repeat: 'a => (option int) => (stream 'a);
  let return: 'a => (stream 'a);
  let scan: ('acc => 'a => 'acc) => 'acc => (stream 'a) => (stream 'acc);
  let skip: int => (stream 'a) => (stream 'a);
  let skipWhile: ('a => bool) => (stream 'a) => (stream 'a);
  let some: ('a => bool) => (stream 'a) => (stream bool);
  let startWith: 'a => (stream 'a) => (stream 'a);
  let take: int => (stream 'a) => (stream 'a);
  let takeWhile: ('a => bool) => (stream 'a) => (stream 'a);
  let tryFind: ('a => bool) => (stream 'a) => (stream 'a);
  let zip: (list (stream 'a)) => (stream (list 'a));
  let zip2: (stream 'a) => (stream 'b) => (stream ('a, 'b));
  let zip3: (stream 'a) => (stream 'b) => (stream 'c) => (stream ('a, 'b, 'c));
  let zipLongest: (list (stream 'a)) => (stream (list (option 'a)));
  let zipLongest2: (stream 'a) => (stream 'b) => (stream (option 'a, option 'b));
  let zipLongest3: (stream 'a) => (stream 'b) => (stream 'c) => (stream (option 'a, option 'b, option 'c));
};

let rec listTake (count: int) (list: list 'a): (list 'a) =>
  count == 0 ? [] : switch list {
    | [head, ...tail] => [head, ...(tail |> listTake (count - 1))]
    | _ => failwith "list too short"
  };

let module Make = fun (X: StreamBase) => {
  type stream 'a = X.stream 'a;

  let buffer
      (count: int)
      (skip: int)
      (stream: stream 'a): (stream (list 'a)) => count <= 0 || skip <= 0
    ? failwith "out of range"
    : stream |> X.scan (
        fun (lst, counted, skipped) next =>
          counted < count && skipped < skip ? ([next, ...lst], counted + 1, skipped + 1) :
          skipped < skip ? (lst, counted, skipped + 1) :
          counted < count ? ([next, ...lst], counted + 1, skipped) :
          skip < count ? ([next, ...(listTake (count - skip) lst)], counted, skipped) :
          ([next], 1, 1)
        ) ([], 0, 0)
      |> X.filter (fun (lst, counted, skipped) => counted == count && skipped == skip)
      |> X.map (fun (lst, _, _) => lst);

  let concat (streams: list (stream 'a)): (stream 'a) =>
    streams |> X.ofList |> X.concatAll;

  let concatAll = X.concatAll;

  let concatMap (f: 'a => (stream 'b)) (stream: stream 'a): (stream 'b) =>
    stream |> X.map f |> X.concatAll;

  let defer = X.defer;

  let distinctUntilChangedWith
      (equality: Equality.t 'a)
      (stream: stream 'a): (stream 'a) => stream
    |> X.scan
      (fun (accPrev, accNext) next => (accNext, Some next))
      (None, None)
    |> X.map (fun v => switch v {
        | (Some prev, Some next) when (not @@ equality prev @@ next) => Some next
        | (None, Some next) => Some next
        | _ => None
      })
    |> X.filter Option.isNotEmpty
    |> X.map Option.first;

  let distinctUntilChanged (stream: stream 'a): (stream 'a) =>
    stream |> distinctUntilChangedWith Equality.structural;

  let doOnNext (f: 'a => unit) (stream: stream 'a): (stream 'a) =>
    stream |> X.map (fun next => { f next; next });

  let empty = X.empty;

  let filter = X.filter;

  let first (stream: stream 'a): (stream 'a) => stream
    |> X.scan
        (fun acc next => Option.isEmpty acc ? Some next : None)
        None
    |> X.takeWhile Option.isNotEmpty
    |> X.map Option.first;

  let flatMap (f: 'a => (stream 'b)) (stream: stream 'a): (stream 'b) =>
    stream |> X.map f |> X.flatten;

  let flatten = X.flatten;

  let inRange = X.inRange;

  let last = X.last;

  let map = X.map;

  let return (value: 'a): stream 'a => [ value ] |> X.ofList;

  let reduce
      (reducer: 'acc => 'a => 'acc)
      (initialValue: 'acc)
      (stream: stream 'a): (stream 'acc) =>
    stream |> X.scan reducer initialValue |> X.last;

  let repeat = X.repeat;

  let scan = X.scan;

  let rec skip (count: int) (stream: stream 'a): (stream 'a) =>
    count > 0 ? stream
      |> X.scan (fun (count, _) next => count > 0
          ? (count - 1, None)
          : (count, Some next)
        ) (count, None)
      |> X.filter (snd >> Option.isNotEmpty)
      |> X.map (snd >> Option.first) :
    count == 0 ? stream :
    failwith "count must be greater or equal to 0";

  let skipWhile (f: 'a => bool) (stream: stream 'a): (stream 'a) => stream
    |> X.scan
      (fun acc next => switch acc {
        | Some _ => Some next
        | _ => f next ? None : Some next
      }) None
    |> X.filter Option.isNotEmpty
    |> X.map Option.first;

  let some (predicate: 'a => bool) (stream: stream 'a): (stream bool) =>
    stream |> X.filter predicate |> X.map (fun _ => true) |> first;

  let startWith (value: 'a) (stream: stream 'a): (stream 'a) =>
    concat [return value, stream];

  let take (count: int) (stream: stream 'a): (stream 'a) =>
    count > 0 ? stream
      |> X.scan (fun (count, _) next => count > 0
          ? (count - 1, Some next)
          : (count, None)
          ) (count, None)
      |> X.takeWhile (snd >> Option.isNotEmpty)
      |> X.map (snd >> Option.first) :
    count == 0 ? X.empty :
    failwith "count must be greater or equal to 0";

  let takeWhile = X.takeWhile;

  let tryFind (predicate: 'a => bool) (stream: stream 'a): (stream 'a) =>
    stream |> X.filter predicate |> first;

  let zip = X.zip;

  let zip2 (a: stream 'a) (b: stream 'b): (stream ('a, 'b)) =>
    X.zip [
      a |> X.map (fun a => Choice1of2 a),
      b |> X.map (fun b => Choice2of2 b),
    ] |> X.map(fun next => switch next {
      | [Choice1of2 a, Choice2of2 b] => (a, b)
      | _ => failwith "illegal state"
    });

  let zip3 (a: stream 'a) (b: stream 'b) (c: stream 'c): (stream ('a, 'b, 'c)) =>
    X.zip [
      a |> X.map (fun a => Choice1of3 a),
      b |> X.map (fun b => Choice2of3 b),
      c |> X.map (fun c => Choice3of3 c),
    ] |> X.map(fun next => switch next {
      | [Choice1of3 a, Choice2of3 b, Choice3of3 c] => (a, b, c)
      | _ => failwith "illegal state"
    });

  let zipLongest (streams: list (stream 'a)): (stream (list (option 'a))) => streams
    |> ImmList.mapReverse (fun stream => concat [stream |> X.map Option.return, X.repeat None None ])
    |> ImmList.reverse
    |> X.zip
    |> X.takeWhile (ImmList.some Option.isNotEmpty);

  let zipLongest2 (a: stream 'a) (b: stream 'b): (stream (option 'a, option 'b)) =>
    zipLongest [
      a |> X.map (fun a => Choice1of2 a),
      b |> X.map (fun b => Choice2of2 b),
    ] |> X.map(fun next => switch next {
      | [a, b] => (
          a >>| (fun (Choice1of2 a) => a),
          b >>| (fun (Choice2of2 b) => b)
        )
      | _ => failwith "illegal state"
    });

  let zipLongest3 (a: stream 'a) (b: stream 'b) (c: stream 'c): (stream (option 'a, option 'b, option 'c)) =>
    zipLongest [
      a |> X.map (fun a => Choice1of3 a),
      b |> X.map (fun b => Choice2of3 b),
      c |> X.map (fun c => Choice3of3 c),
    ] |> X.map(fun next => switch next {
      | [a, b, c] => (
          a >>| (fun (Choice1of3 a) => a),
          b >>| (fun (Choice2of3 b) => b),
          c >>| (fun (Choice3of3 c) => c)
        )
      | _ => failwith "illegal state"
    });
};
