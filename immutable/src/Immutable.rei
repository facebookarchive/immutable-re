/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

let module Hash: {
  type t 'a = 'a => int;

  /*
  let bytes: t bytes;
  let char: t char;
  let int32: t int32;
  let int64: t int64;
  let nativeInt: t nativeint;
  let string: t string;
  */

  let random: unit => t 'a;
  let structural: t 'a;
};

let module Equality: {
  type t 'a = 'a => 'a => bool;

  /* Will be available in Ocaml 4.03t
  let bytes: t bytes;
  let char: t char;
  let int32: t int32;
  let int64: t int64;
  let nativeInt: t nativeint;
  let string: t string;
  */

  let reference: t 'a;
  let structural: t 'a;
};

let module Ordering: {
  type t;

  let equal: t;
  let greaterThan: t;
  let lessThan: t;
};

let module Comparator: {
  type t 'a = 'a => 'a => Ordering.t;

  let bytes: t bytes;
  let char: t char;
  let int32: t int32;
  let int64: t int64;
  let nativeInt: t nativeint;
  let string: t string;
  let structural: t 'a;
};

let module Seq: {
  type t 'a;

  let buffer: int => int => (t 'a) => (t (list 'a));
  let compare: (Comparator.t (t 'a));
  let compareWith: (Comparator.t 'a) => (Comparator.t (t 'a));
  let concat: (list (t 'a)) => (t 'a);
  let concatAll: (t (t 'a)) => (t 'a);
  let concatMap: ('a => (t 'b)) => (t 'a) => (t 'b);
  let count: (t 'a) => int;
  let defer: (unit => t 'a) => (t 'a);
  let distinctUntilChanged: (t 'a) => (t 'a);
  let distinctUntilChangedWith: (Equality.t 'a) => (t 'a) => (t 'a);
  let doOnNext: ('a => unit) => (t 'a) => (t 'a);
  let empty: t 'a;
  let equals: (Equality.t (t 'a));
  let equalsWith: (Equality.t 'a) => (Equality.t (t 'a));
  let every: ('a => bool) => (t 'a) => bool;
  let filter: ('a => bool) => (t 'a) => (t 'a);
  let find: ('a => bool) => (t 'a) => 'a;
  let first: (t 'a) => 'a;
  let flatMap: ('a => t 'b) => (t 'a) => (t 'b);
  let flatten: (t (t 'a)) => (t 'a);
  let forEach: ('a => unit) => (t 'a) => unit;
  let hash: (Hash.t (t 'a));
  let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
  let inRange: int => (option int) => int => (t int);
  let isEmpty: t 'a => bool;
  let isNotEmpty: t 'a => bool;
  let last: (t 'a) => 'a;
  let map: ('a => 'b) => (t 'a) => (t 'b);
  let none: ('a => bool) => (t 'a) => bool;
  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let repeat: 'a => (option int) => (t 'a);
  let return: 'a => t 'a;
  let scan: ('acc => 'a => 'acc) => 'acc => (t 'a) => (t 'acc);
  let skip: int => (t 'a) => (t 'a);
  let skipWhile: ('a => bool) => (t 'a) => (t 'a);
  let some: ('a => bool) => (t 'a) => bool;
  let startWith: 'a => (t 'a) => (t 'a);
  let take: int => (t 'a) => (t 'a);
  let takeWhile: ('a => bool) => (t 'a) => (t 'a);
  let tryFind: ('a => bool) => (t 'a) => (option 'a);
  let tryFirst: (t 'a) => (option 'a);
  let tryGet: int => (t 'a) => (option 'a);
  let tryLast: t 'a => option 'a;
  let zip: (list (t 'a)) => (t (list 'a));
  let zip2: (t 'a) => (t 'b) => (t ('a, 'b));
  let zip3: (t 'a) => (t 'b) => (t 'c) => (t ('a, 'b, 'c));
  let zipLongest: (list (t 'a)) => (t (list (option 'a)));
  let zipLongest2: (t 'a) => (t 'b) => (t (option 'a, option 'b));
  let zipLongest3: (t 'a) => (t 'b) => (t 'c) => (t (option 'a, option 'b, option 'c));
};

let module Collection: {
  type t 'a;

  let contains: 'a => (t 'a) => bool;
  let count: (t 'a) => int;
  let empty: (t 'a);
  let equals: (t 'a) => (t 'a) => bool;
  let hash: (Hash.t (t 'a));
  let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
  let isEmpty: t 'a => bool;
  let isNotEmpty: t 'a => bool;
  let intersect: (t 'a) => (t 'a) => (Seq.t 'a);
  let subtractFrom: (t 'a) => (t 'a) => (Seq.t 'a);
  let toSeq: (t 'a) => (Seq.t 'a);
  let union: (t 'a) => (t 'a) => (Seq.t 'a);
};

let module Keyed: {
  type t 'k 'v;

  let contains: (Equality.t 'v) => 'k => 'v => (t 'k 'v) => bool;
  let containsKey: 'k => (t 'k 'v) => bool;
  let count: (t 'k 'v) => int;
  let empty: (t 'k 'v);
  let equals: (t 'k 'v) => (t 'k 'v) => bool;
  let equalsWith: (Equality.t 'v) => (t 'k 'v) => (t 'k 'v) => bool;
  let hash: (Hash.t (t 'k 'v));
  let hashWith: (Hash.t 'k) => (Hash.t 'v) => (Hash.t (t 'k 'v));
  let isEmpty: t 'k 'v => bool;
  let isNotEmpty: t 'k 'v => bool;
  let keys: (t 'k 'v) => (Collection.t 'k);
  let map: ('v1 => 'v2) => (t 'k 'v1) => (t 'k 'v2);
  let mapWithKey: ('k => 'v1 => 'v2) => (t 'k 'v1) => (t 'k 'v2);
  let ofCollection: (Collection.t 'a) => (t 'a 'a);
  let toCollection: (Equality.t 'v) => (t 'k 'v) => (Collection.t ('k, 'v));
  let toSeq: (t 'k 'v) => (Seq.t ('k, 'v));
  let tryGet: 'k => (t 'k 'v) => (option 'v);
};

let module Indexed: {
  type t 'a;

  let count: (t 'a) => int;
  let empty: (t 'a);
  let equals: (t 'a) => (t 'a) => bool;
  let equalsWith: (Equality.t 'a) => (t 'a) => (t 'a) => bool;
/*
  let every: ('a => bool) => (t 'a) => bool;
  let find: ('a => bool) => (t 'a) => 'a;
  let first: (t 'a) => 'a;
  let get: int => (t 'a) => 'a;
*/
  let hash: (Hash.t (t 'a));
  let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
  let isEmpty: t 'a => bool;
  let isNotEmpty: t 'a => bool;
/*
  let last: (t 'a) => 'a;
*/
  let map: ('a => 'b) => (t 'a) => (t 'b);
  /*let mapReverse: ('a => 'b) => (t 'a) => (t 'b);*/
  let mapWithIndex: (int => 'a => 'b) => (t 'a) => (t 'b);
  /*let none: ('a => bool) => (t 'a) => bool;
  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let reduceRight: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;*/
  let reverse: (t 'a) => (t 'a);
/*
  let skip: int => (t 'a) => (t 'a);
  let some: ('a => bool) => (t 'a) => bool;
  let take: int => (t 'a) => (t 'a);*/
  let toKeyed: (t 'a) => (Keyed.t int 'a);
  let toSeq: (t 'a) => (Seq.t 'a);
  let toSeqReversed: (t 'a) => (Seq.t 'a);
  /*let tryFind: ('a => bool) => (t 'a) => (option 'a); */
  let tryFirst: (t 'a) => option 'a;
  let tryGet: int => (t 'a) => (option 'a);
  let tryLast: (t 'a) => option 'a;
};

let module HashStrategy: {
  type t 'a;

  let createWithComparator: (Hash.t 'a) => (Comparator.t 'a) => (t 'a);
  let createWithEquality: (Hash.t 'a) => (Equality.t 'a) => (t 'a);
  let identity: unit => t 'a;
  let structuralCompare: unit => t 'a;
  let structuralEquality: unit => t 'a;
};

let module rec BiMap: {
  type t 'k 'v;

  let count: (t 'k 'v) => int;
  let empty: unit => (t 'k 'v);
  let emptyWith: (HashStrategy.t 'k) => (HashStrategy.t 'v) => (t 'k 'v);
  let fromSeq: (Seq.t ('k, 'v)) => (t 'k 'v);
  let fromSeqWith: (HashStrategy.t 'k) => (HashStrategy.t 'v) => (Seq.t ('k, 'v)) => (t 'k 'v);
  let inverse: (t 'k 'v) => t 'v 'k;
  let isEmpty: t 'k 'v => bool;
  let isNotEmpty: t 'k 'v => bool;
  let mutate: (t 'k 'v) => (TransientBiMap.t 'k 'v);
  let put: 'k => 'v => (t 'k 'v) => t 'k 'v;
  let putAll: (Seq.t ('k, 'v)) => (t 'k 'v) => (t 'k 'v);
  let reduce: ('acc => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceWithKey: ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let remove: 'k => (t 'k 'v) => (t 'k 'v);
  let removeAll: (t 'k 'v) => (t 'k 'v);
  let toKeyed: (t 'k 'v) => (Keyed.t 'k 'v);
  let toSeq: (t 'k 'v) => (Seq.t ('k, 'v));
  let tryGet: 'k => (t 'k 'v) => (option 'v);
  let tryPut: 'k => 'v => (t 'k 'v) => (t 'k 'v);
}

and TransientBiMap: {
  type t 'k 'v;

  let count: (t 'k 'v) => int;
  let isEmpty: t 'k 'v => bool;
  let isNotEmpty: t 'k 'v => bool;
  let persist: (t 'k 'v) => (BiMap.t 'k 'v);
  let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);
  let putAll: (Seq.t ('k, 'v)) => (t 'k 'v) => (t 'k 'v);
  let remove: 'k => (t 'k 'v) => (t 'k 'v);
  let removeAll: (t 'k 'v) => (t 'k 'v);
  let tryGet: 'k => (t 'k 'v) => (option 'v);
  let tryPut: 'k => 'v => (t 'k 'v) => (t 'k 'v);
};

let module CopyOnWriteArray: {
  type t 'a;

  let addFirst: 'a => (t 'a) => (t 'a);
  let addFirstAll: (Seq.t 'a) => (t 'a) => (t 'a);
  let addLast: 'a => (t 'a) => (t 'a);
  let addLastAll: (Seq.t 'a) => (t 'a) => (t 'a);
  let compare: (Comparator.t (t 'a));
  let compareWith: (Comparator.t 'a) => (Comparator.t (t 'a));
  let concat: (list (t 'a)) => (t 'a);
  let count: (t 'a) => int;
  let empty: (t 'a);
  let equals: (Equality.t (t 'a));
  let equalsWith: (Equality.t 'a) => (Equality.t (t 'a));
  let every: ('a => bool) => (t 'a) => bool;
  let find: ('a => bool) => (t 'a) => 'a;
  let first: (t 'a) => 'a;
  let forEach: ('a => unit) => (t 'a) => unit;
  let forEachReverse: ('a => unit) => (t 'a) => unit;
  let forEachWithIndex: (int => 'a => unit) => (t 'a) => unit;
  let forEachReverseWithIndex: (int => 'a => unit) => (t 'a) => unit;
  let fromSeq: (Seq.t 'a) => (t 'a);
  let fromSeqReversed: (Seq.t 'a) => (t 'a);
  let get: int => (t 'a) => 'a;
  let hash: (Hash.t (t 'a));
  let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
  let init: int => (int => 'a) => (t 'a);
  let insertAt: int => 'a => (t 'a) => (t 'a);
  let isEmpty: t 'a => bool;
  let isNotEmpty: t 'a => bool;
  let last: (t 'a) => 'a;
  let map: ('a => 'b) => (t 'a) => (t 'b);
  let mapWithIndex: (int => 'a => 'b) => (t 'a) => (t 'b);
  let mapReverse: ('a => 'b) => (t 'a) => (t 'b);
  let mapReverseWithIndex: (int => 'a => 'b) => (t 'a) => (t 'b);
  let none: ('a => bool) => (t 'a) => bool;
  let ofUnsafe: (array 'a) => (t 'a);
  let range: int => (option int) => (t 'a) => (t 'a);
  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let reduceWithIndex: ('acc => int => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let reduceRight: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let reduceRightWithIndex: ('acc => int => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let removeAt: int => (t 'a) => (t 'a);
  let removeAll: (t 'a) => (t 'a);
  let removeFirst: (t 'a) => (t 'a);
  let removeLast: (t 'a) => (t 'a);
  let return: 'a => (t 'a);
  let reverse: (t 'a) => (t 'a);
  let skip: int => (t 'a) => (t 'a);
  let some: ('a => bool) => (t 'a) => bool;
  let take: int => (t 'a) => (t 'a);
  let toIndexed: (t 'a) => (Indexed.t 'a);
  let toSeq: (t 'a) => (Seq.t 'a);
  let toSeqReversed: (t 'a) => (Seq.t 'a);
  let tryFind: ('a => bool) => (t 'a) => (option 'a);
  let tryFirst: (t 'a) => option 'a;
  let tryGet: int => (t 'a) => (option 'a);
  let tryLast: (t 'a) => option 'a;
  let update: int => 'a => (t 'a) => (t 'a);
};

let module rec Deque: {
  type t 'a;

  let addFirst: 'a => (t 'a) => (t 'a);
  let addFirstAll: (Seq.t 'a) => (t 'a) => (t 'a);
  let addLast: 'a => (t 'a) => (t 'a);
  let addLastAll: (Seq.t 'a) => (t 'a) => (t 'a);
  let compare: (Comparator.t (t 'a));
  let compareWith: (Comparator.t 'a) => (Comparator.t (t 'a));
  let count: (t 'a) => int;
  let empty: (t 'a);
  let equals: (Equality.t (t 'a));
  let equalsWith: (Equality.t 'a) => (Equality.t (t 'a));
  let every: ('a => bool) => (t 'a) => bool;
  let find: ('a => bool) => (t 'a) => 'a;
  let first: (t 'a) => 'a;
  let forEach: ('a => unit) => (t 'a) => unit;
  let forEachReverse: ('a => unit) => (t 'a) => unit;
  let fromSeq: (Seq.t 'a) => (t 'a);
  let fromSeqReversed: (Seq.t 'a) => (t 'a);
  let hash: (Hash.t (t 'a));
  let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
  let isEmpty: (t 'a) => bool;
  let isNotEmpty: (t 'a) => bool;
  let last: (t 'a) => 'a;
  let map: ('a => 'b) => (t 'a) => (t 'b);
  let mapReverse: ('a => 'b) => (t 'a) => (t 'b);
  let mutate: (t 'a) => (TransientDeque.t 'a);
  let none: ('a => bool) => (t 'a) => bool;
  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let reduceRight: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let removeAll: (t 'a) => (t 'a);
  let removeFirst: (t 'a) => (t 'a);
  let removeLast: (t 'a) => (t 'a);
  let return: 'a => (t 'a);
  let reverse: (t 'a) => (t 'a);
  let some: ('a => bool) => (t 'a) => bool;
  let toSeq: (t 'a) => (Seq.t 'a);
  let toSeqReversed: (t 'a) => (Seq.t 'a);
  let tryFind: ('a => bool) => (t 'a) => (option 'a);
  let tryFirst: (t 'a) => option 'a;
  let tryLast: (t 'a) => option 'a;
}

and TransientDeque: {
  type t 'a;

  let addFirst: 'a => (t 'a) => (t 'a);
  let addLast: 'a => (t 'a) => (t 'a);
  let count: (t 'a) => int;
  let empty: unit => (t 'a);
  let first: (t 'a) => 'a;
  let isEmpty: t 'a => bool;
  let isNotEmpty: t 'a => bool;
  let last: (t 'a) => 'a;
  let removeAll: (t 'a) => (t 'a);
  let removeFirst: (t 'a) => (t 'a);
  let removeLast: (t 'a) => (t 'a);
  let reverse: (t 'a) => (t 'a);
  let tryFirst: (t 'a) => option 'a;
  let tryLast: (t 'a) => option 'a;
};

let module rec HashMap: {
  type t 'k 'v;

  let count: (t 'k 'v) => int;
  let empty: unit => (t 'k 'v);
  let emptyWith: (HashStrategy.t 'k) => (t 'k 'v);
  let fromKeyed: (Keyed.t 'k 'v) => (t 'k 'v);
  let fromKeyedWith: (HashStrategy.t 'k) => (Keyed.t 'k 'v) => (t 'k 'v);
  let fromSeq: (Seq.t ('k, 'v)) => (t 'k 'v);
  let fromSeqWith: (HashStrategy.t 'k) => (Seq.t ('k, 'v)) => (t 'k 'v);
  let isEmpty: t 'k 'v => bool;
  let isNotEmpty: t 'k 'v => bool;
  let map: ('a => 'b) => (t 'k 'a) => (t 'k 'b);
  let mapWithKey: ('k => 'a => 'b) => (t 'k 'a) => (t 'k 'b);
  let merge: ('k => (option 'vAcc) => (option 'v) => (option 'vAcc)) => (Keyed.t 'k 'v) => (t 'k 'vAcc)  => (t 'k 'vAcc);
  let mutate: (t 'k 'v) => (TransientHashMap.t 'k 'v);
  let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);
  let putAll: (Seq.t ('k, 'v)) => (t 'k 'v) => (t 'k 'v);
  let reduce: ('acc => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceWithKey: ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let remove: 'k => (t 'k 'v) => (t 'k 'v);
  let removeAll: (t 'k 'v) => (t 'k 'v);
  let toKeyed: (t 'k 'v) => (Keyed.t 'k 'v);
  let toSeq: (t 'k 'v) => (Seq.t ('k, 'v));
  let tryGet: 'k => (t 'k 'v) => (option 'v);
}

and TransientHashMap: {
  type t 'k 'v;

  let count: (t 'k 'v) => int;
  let isEmpty: t 'k 'v => bool;
  let isNotEmpty: t 'k 'v => bool;
  let persist: (t 'k 'v) => (HashMap.t 'k 'v);
  let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);
  let putAll: (Seq.t ('k, 'v)) => (t 'k 'v) => (t 'k 'v);
  let remove: 'k => (t 'k 'v) => (t 'k 'v);
  let removeAll: (t 'k 'v) => (t 'k 'v);
  let tryGet: 'k => (t 'k 'v) => (option 'v);
};

let module rec HashMultiset: {
  type t 'a;

  let add: 'a => (t 'a) => (t 'a);
  let addAll: (Seq.t 'a) => (t 'a) => (t 'a);
  let contains: 'a => (t 'a) => bool;
  let count: (t 'a) => int;
  let empty: unit => (t 'a);
  let emptyWith: (HashStrategy.t 'a) => (t 'a);
  let isEmpty: t 'a => bool;
  let isNotEmpty: t 'a => bool;
  let fromSeq: (Seq.t 'a) => (t 'a);
  let fromSeqWith: (HashStrategy.t 'a) => (Seq.t 'a) => (t 'a);
  let get: 'a => (t 'a) => int;
  let mutate: (t 'a) => (TransientHashMultiset.t 'a);
  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let remove: 'a => (t 'a) => (t 'a);
  let removeAll: (t 'a) => (t 'a);
  let set: 'a => int => (t 'a) => (t 'a);
  let toKeyed: (t 'a) => (Keyed.t 'a int);
  let toSeq: (t 'a) => (Seq.t 'a);
}

and TransientHashMultiset: {
  type t 'a;

  let add: 'a => (t 'a) => (t 'a);
  let addAll: (Seq.t 'a) => (t 'a) => (t 'a);
  let contains: 'a => (t 'a) => bool;
  let count: (t 'a) => int;
  let get: 'a => (t 'a) => int;
  let isEmpty: t 'a => bool;
  let isNotEmpty: t 'a => bool;
  let persist: (t 'a) => (HashMultiset.t 'a);
  let remove: 'a => (t 'a) => (t 'a);
  let removeAll: (t 'a) => (t 'a);
  let set: 'a => int => (t 'a) => (t 'a);
};

let module rec HashSet: {
  type t 'a;

  let contains: 'a => (t 'a) => bool;
  let count: (t 'a) => int;
  let empty: unit => t 'a;
  let emptyWith: (HashStrategy.t 'a) => (t 'a);
  let fromSeq: (Seq.t 'a) => (t 'a);
  let fromSeqWith: (HashStrategy.t 'a)  => (Seq.t 'a) => (t 'a);
  let isEmpty: t 'a => bool;
  let isNotEmpty: t 'a => bool;
  let mutate: (t 'a) => (TransientHashSet.t 'a);
  let put: 'a => (t 'a) => (t 'a);
  let putAll: (Seq.t 'a) => (t 'a) => (t 'a);
  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let remove: 'a => (t 'a) => (t 'a);
  let removeAll: (t 'a) => (t 'a);
  let toCollection: (t 'a) => (Collection.t 'a);
  let toKeyed: (t 'a) => (Keyed.t 'a 'a);
  let toSeq: (t 'a) => (Seq.t 'a);
}

and TransientHashSet: {
  type t 'a;

  let contains: 'a => (t 'a) => bool;
  let count: (t 'a) => int;
  let isEmpty: t 'a => bool;
  let isNotEmpty: t 'a => bool;
  let persist: (t 'a) => (HashSet.t 'a);
  let put: 'a => (t 'a) => (t 'a);
  let putAll: (Seq.t 'a) => (t 'a) => (t 'a);
  let remove: 'a => (t 'a) => (t 'a);
  let removeAll: (t 'a) => (t 'a);
};

let module HashSetMultimap: {
  type t 'k 'v;

  let count: (t 'k 'v) => int;
  let empty: unit => (t 'k 'v);
  let emptyWith: (HashStrategy.t 'k) => (HashStrategy.t 'v)  => (t 'k 'v);
  let get: 'k => (t 'k 'v) => (HashSet.t 'v);
  let isEmpty: t 'k 'v => bool;
  let isNotEmpty: t 'k 'v => bool;
  let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);
  let reduce: ('acc => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceWithKey: ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let remove: 'k => (t 'k 'v) => (t 'k 'v);
  let removeAll: (t 'k 'v) => (t 'k 'v);
  let toKeyed: (t 'k 'v) => (Keyed.t 'k (Collection.t 'v));
  let toSeq: (t 'k 'v) => (Seq.t ('k, 'v));
};

let module rec IntMap: {
  type t 'a;

  let count: (t 'a) => int;
  let empty: t 'a;
  let fromKeyed: (Keyed.t int 'a) => (t 'a);
  let fromSeq: (Seq.t (int, 'a)) => (t 'a);
  let isEmpty: t 'a => bool;
  let isNotEmpty: t 'a => bool;
  let map: ('a => 'b) => (t 'a) => (t 'b);
  let mapWithKey: (int => 'a => 'b) => (t 'a) => (t 'b);
  let merge: (int => (option 'vAcc) => (option 'v) => (option 'vAcc)) => (Keyed.t int 'v) => (t 'vAcc)  => (t 'vAcc);
  let mutate: (t 'a) => (TransientIntMap.t 'a);
  let put: int => 'a => (t 'a) => (t 'a);
  let putAll: (Seq.t (int, 'a)) => (t 'a) => (t 'a);
  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let reduceWithKey: ('acc => int => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let remove: int => (t 'a) => (t 'a);
  let removeAll: (t 'a) => (t 'a);
  let toKeyed: (t 'a) => (Keyed.t int 'a);
  let toSeq: (t 'a) => (Seq.t ((int, 'a)));
  let tryGet: int => (t 'a) => (option 'a);
}

and TransientIntMap: {
  type t 'a;

  let count: (t 'a) => int;
  let isEmpty: t 'a => bool;
  let isNotEmpty: t 'a => bool;
  let persist: (t 'a) => (IntMap.t 'a);
  let put: int => 'a => (t 'a) => (t 'a);
  let putAll: (Seq.t (int, 'a)) => (t 'a) => (t 'a);
  let remove: int => (t 'a) => (t 'a);
  let removeAll: (t 'a) => (t 'a);
  let tryGet: int => (t 'a) => (option 'a);
};

let module List: {
  type t 'a = list 'a;

  let addFirst: 'a => (t 'a) => (t 'a);
  let addFirstAll: (Seq.t 'a) => (t 'a) => (t 'a);
  let compare: (Comparator.t (t 'a));
  let compareWith: (Comparator.t 'a) => (Comparator.t (t 'a));
  let count: (t 'a) => int;
  let empty: (t 'a);
  let equals: (Equality.t (t 'a));
  let equalsWith: (Equality.t 'a) => (Equality.t (t 'a));
  let every: ('a => bool) => (t 'a) => bool;
  let first: (t 'a) => 'a;
  let find: ('a => bool) => (t 'a) => 'a;
  let forEach: ('a => unit) => (t 'a) => unit;
  let fromSeqReversed: (Seq.t 'a) => (t 'a);
  let hash: (Hash.t (t 'a));
  let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
  let isEmpty: (t 'a) => bool;
  let isNotEmpty: (t 'a) => bool;
  let mapReverse: ('a => 'b) => (t 'a) => (t 'b);
  let none: ('a => bool) => (t 'a) => bool;
  let reduce: ('acc => 'a => 'acc ) => 'acc => (t 'a) => 'acc;
  let removeAll: (t 'a) => (t 'a);
  let removeFirst: (t 'a) => (t 'a);
  let return: 'a => (t 'a);
  let reverse: (t 'a) => (t 'a);
  let some: ('a => bool) => (t 'a) => bool;
  let toSeq: (t 'a) => (Seq.t 'a);
  let tryFind: ('a => bool) => (t 'a) => (option 'a);
  let tryFirst: (t 'a) => (option 'a);
};

let module Option: {
  type t 'a = option 'a;

  let count: (t 'a) => int;
  let empty: t 'a;
  let filter: ('a => bool) => (t 'a) => (t 'a);
  let flatMap: ('a => t 'b) => (t 'a) => (t 'b);
  let flatten: (t (t 'a)) => (t 'a);
  let forEach: ('a => unit) => (t 'a) => unit;
  let get: (option 'a) => 'a;
  let isEmpty: (t 'a) => bool;
  let isNotEmpty: (t 'a) => bool;
  let map: ('a => 'b) => (t 'a) => (t 'b);
  let none: ('a => bool) => (t 'a) => bool;
  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let return: 'a => (t 'a);
  let some: ('a => bool) => (t 'a) => bool;
  let toSeq: (t 'a) => (Seq.t 'a);
};

let module SortedMap: {
  type t 'k 'v;

  let count: (t 'k 'v) => int;
  let empty: unit => (t 'k 'v);
  let emptyWith: (Comparator.t 'k) => (t 'k 'v);
  let fromKeyed: (Keyed.t 'k 'v) => (t 'k 'v);
  let fromKeyedWith: (Comparator.t 'k) => (Keyed.t 'k 'v) => (t 'k 'v);
  let fromSeq: (Seq.t ('k, 'v)) => (t 'k 'v);
  let fromSeqWith: (Comparator.t 'k) => (Seq.t ('k, 'v)) => (t 'k 'v);
  let isEmpty: t 'k 'v => bool;
  let isNotEmpty: t 'k 'v => bool;
  let map: ('a => 'b) => (t 'k 'a) => (t 'k 'b);
  let mapWithKey: ('k => 'a => 'b) => (t 'k 'a) => (t 'k 'b);
  let merge: ('k => (option 'vAcc) => (option 'v) => (option 'vAcc)) => (Keyed.t 'k 'v) => (t 'k 'vAcc)  => (t 'k 'vAcc);
  let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);
  let putAll: (Seq.t ('k, 'v)) => (t 'k 'v) => (t 'k 'v);
  let reduce: ('acc => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceWithKey: ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceRight: ('acc => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceRightWithKey: ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let remove: 'k => (t 'k 'v) => (t 'k 'v);
  let removeAll: (t 'k 'v) => (t 'k 'v);
  let toKeyed: (t 'k 'v) => (Keyed.t 'k 'v);
  let toSeq: (t 'k 'v) => (Seq.t ('k, 'v));
  let tryGet: 'k => (t 'k 'v) => (option 'v);
};

let module SortedSet: {
  type t 'a;

  let contains: 'a => (t 'a) => bool;
  let count: (t 'a) => int;
  let empty: t 'a;
  let emptyWith: (Comparator.t 'a) => (t 'a);
  let fromSeq: (Seq.t 'a) => (t 'a);
  let fromSeqWith: (Comparator.t 'a)  => (Seq.t 'a) => (t 'a);
  let isEmpty: t 'a => bool;
  let isNotEmpty: t 'a => bool;
  let putAll: (Seq.t 'a) => (t 'a) => (t 'a);
  let put: 'a => (t 'a) => (t 'a);
  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let reduceRight: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let remove: 'a => (t 'a) => (t 'a);
  let removeAll: (t 'a) => (t 'a);
  let toCollection: (t 'a) => (Collection.t 'a);
  let toKeyed: (t 'a) => (Keyed.t 'a 'a);
  let toSeq: (t 'a) => (Seq.t 'a);
};

let module Stack: {
  type t 'a;

  let addFirst: 'a => (t 'a) => (t 'a);
  let addFirstAll: (Seq.t 'a) => (t 'a) => (t 'a);
  let compare: (Comparator.t (t 'a));
  let compareWith: (Comparator.t 'a) => (Comparator.t (t 'a));
  let count: (t 'a) => int;
  let empty: (t 'a);
  let equals: (Equality.t (t 'a));
  let equalsWith: (Equality.t 'a) => (Equality.t (t 'a));
  let every: ('a => bool) => (t 'a) => bool;
  let find: ('a => bool) => (t 'a) => 'a;
  let first: (t 'a) => 'a;
  let forEach: ('a => unit) => (t 'a) => unit;
  let fromSeqReversed: (Seq.t 'a) => (t 'a);
  let hash: (Hash.t (t 'a));
  let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
  let isEmpty: t 'a => bool;
  let isNotEmpty: t 'a => bool;
  let fromList: (list 'a) => (t 'a);
  let mapReverse: ('a => 'b) => (t 'a) => (t 'b);
  let none: ('a => bool) => (t 'a) => bool;
  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let removeAll: (t 'a) => (t 'a);
  let removeFirst: (t 'a) => (t 'a);
  let return: 'a => (t 'a);
  let reverse: (t 'a) => (t 'a);
  let some: ('a => bool) => (t 'a) => bool;
  let toList: (t 'a) => (list 'a);
  let toSeq: (t 'a) => (Seq.t 'a);
  let tryFind: ('a => bool) => (t 'a) => (option 'a);
  let tryFirst: (t 'a) => (option 'a);
};

let module StackMultimap: {
  type t 'k 'v;

  let add: 'k => 'v => (t 'k 'v) => (t 'k 'v);
  let count: (t 'k 'v) => int;
  let empty: unit => (t 'k 'v);
  let emptyWith: (HashStrategy.t 'k) => (t 'k 'v);
  let get: 'k => (t 'k 'v) => (Stack.t 'v);
  let isEmpty: t 'k 'v => bool;
  let isNotEmpty: t 'k 'v => bool;
  let reduce: ('acc => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceWithKey: ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let remove: 'k => (t 'k 'v) => (t 'k 'v);
  let removeAll: (t 'k 'v) => (t 'k 'v);
  let toKeyed: (t 'k 'v) => (Keyed.t 'k (Seq.t 'v));
  let toSeq: (t 'k 'v) => (Seq.t ('k, 'v));
};

let module Table: {
  type t 'row 'column 'value;

  let count: (t 'row 'column 'value) => int;
  let empty: unit => (t 'row 'column 'value);
  let emptyWith: (HashStrategy.t 'row) => (HashStrategy.t 'column) => (t 'row 'column 'value);
  let isEmpty: t 'row 'column 'value => bool;
  let isNotEmpty: t 'row 'column 'value => bool;
  let put: 'row => 'column => 'value => (t 'row 'column 'value) => (t 'row 'column 'value);
  let reduce: ('acc => 'value => 'acc) => 'acc => (t 'row 'column 'value) => 'acc;
  let reduceWithRowAndColumn: ('acc => 'row => 'column => 'value => 'acc) => 'acc => (t 'row 'column 'value) => 'acc;
  let remove: 'row => 'column => (t 'row 'column 'value) => (t 'row 'column 'value);
  let removeAll: (t 'row 'column 'value) => (t 'row 'column 'value);
  let removeRow: 'row => (t 'row 'column 'value) => (t 'row 'column 'value);
  let toKeyed: (t 'row 'column 'value) => (Keyed.t 'row (Keyed.t 'column 'value));
  let toSeq: (t 'row 'column 'value) => (Seq.t ('row, 'column, 'value));
  let tryGet: 'row => 'column => (t 'row 'column 'value) => (option 'value);
};

let module rec Vector: {
  type t 'a;

  let addFirst: 'a => (t 'a) => (t 'a);
  let addFirstAll: (Seq.t 'a) => (t 'a) => (t 'a);
  let addLast: 'a => (t 'a) => (t 'a);
  let addLastAll: (Seq.t 'a) => (t 'a) => (t 'a);
  /* let alter: int => ('a => 'a) => (t 'a) => (t 'a);*/
  /* let alterAll: (int => 'a => 'a) => (t 'a) => (t 'a);*/
  let compare: (Comparator.t (t 'a));
  let compareWith: (Comparator.t 'a) => (Comparator.t (t 'a));
  /*let concat: (list (t 'a)) => (t 'a);*/
  let count: (t 'a) => int;
  let empty: (t 'a);
  let equals: (Equality.t (t 'a));
  let equalsWith: (Equality.t 'a) => (Equality.t (t 'a));
  let every: ('a => bool) => (t 'a) => bool;
  let find: ('a => bool) => (t 'a) => 'a;
  let first: (t 'a) => 'a;
  let forEach: ('a => unit) => (t 'a) => unit;
  let forEachReverse: ('a => unit) => (t 'a) => unit;
  let forEachWithIndex: (int => 'a => unit) => (t 'a) => unit;
  let forEachReverseWithIndex: (int => 'a => unit) => (t 'a) => unit;
  let fromSeq: (Seq.t 'a) => (t 'a);
  let fromSeqReversed: (Seq.t 'a) => (t 'a);
  let get: int => (t 'a) => 'a;
  let hash: (Hash.t (t 'a));
  let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
  let init: int => (int => 'a) => (t 'a);
  /*let insertAt: int => 'a => (t 'a) => (t 'a);*/
  let isEmpty: t 'a => bool;
  let isNotEmpty: t 'a => bool;
  let last: (t 'a) => 'a;
  let map: ('a => 'b) => (t 'a) => (t 'b);
  let mapWithIndex: (int => 'a => 'b) => (t 'a) => (t 'b);
  let mapReverse: ('a => 'b) => (t 'a) => (t 'b);
  let mapReverseWithIndex: (int => 'a => 'b) => (t 'a) => (t 'b);
  let mutate: (t 'a) => (TransientVector.t 'a);
  let none: ('a => bool) => (t 'a) => bool;
  let range: int => (option int) => (t 'a) => (t 'a);
  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let reduceWithIndex: ('acc => int => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let reduceRight: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let reduceRightWithIndex: ('acc => int => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  /*let removeAt: int => (t 'a) => (t 'a);*/
  let removeAll: (t 'a) => (t 'a);
  let removeFirst: (t 'a) => (t 'a);
  let removeLast: (t 'a) => (t 'a);
  let return: 'a => (t 'a);
  let reverse: (t 'a) => (t 'a);
  let skip: int => (t 'a) => (t 'a);
  let some: ('a => bool) => (t 'a) => bool;
  let take: int => (t 'a) => (t 'a);
  let toIndexed: (t 'a) => (Indexed.t 'a);
  let toSeq: (t 'a) => (Seq.t 'a);
  let toSeqReversed: (t 'a) => (Seq.t 'a);
  let tryFind: ('a => bool) => (t 'a) => (option 'a);
  let tryFirst: (t 'a) => option 'a;
  let tryGet: int => (t 'a) => (option 'a);
  let tryLast: (t 'a) => option 'a;
  let update: int => 'a => (t 'a) => (t 'a);
}

and TransientVector: {
  type t 'a;

  let addFirst: 'a => (t 'a) => (t 'a);
  let addLast: 'a => (t 'a) => (t 'a);
  let count: (t 'a) => int;
  let empty: unit => (t 'a);
  let first: (t 'a) => 'a;
  let get: int => (t 'a) => 'a;
  /*let insertAt: int => 'a => (t 'a) => (t 'a);*/
  let isEmpty: t 'a => bool;
  let isNotEmpty: t 'a => bool;
  let last: (t 'a) => 'a;
  let persist: (t 'a) => (Vector.t 'a);
  /*let removeAt: int => (t 'a) => (t 'a);*/
  let removeAll: (t 'a) => (t 'a);
  let removeFirst: (t 'a) => (t 'a);
  let removeLast: (t 'a) => (t 'a);
  let reverse: (t 'a) => (t 'a);
  let tryFirst: (t 'a) => option 'a;
  let tryGet: int => (t 'a) => (option 'a);
  let tryLast: (t 'a) => option 'a;
  let update: int => 'a => (t 'a) => (t 'a);
};
