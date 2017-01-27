/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Equality;

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
  let distinctUntilChangedWith: (equality 'a) => (stream 'a) => (stream 'a);
  let doOnNext: ('a => unit) => (stream 'a) => (stream 'a);
  let empty: stream 'a;
  let filter: ('a => bool) => (stream 'a) => (stream 'a);
  let first: (stream 'a) => (stream 'a);
  let flatMap: ('a => (stream 'b)) => (stream 'a) => (stream 'b);
  let flatten: (stream (stream 'a)) => (stream 'a);
  let inRange: int => (option int) => int => (stream int);
  let last: (stream 'a) => (stream 'a);
  let map: ('a => 'b) => (stream 'a) => (stream 'b);
  let ofList: (list 'a) => (stream 'a);
  let ofOption: (option 'a) => (stream 'a);
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

let module Make: (X: StreamBase) => Stream with type stream 'a = X.stream 'a;
