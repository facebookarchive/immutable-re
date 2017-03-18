/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

let module IteratorInternal = Iterator;
let module SequenceInternal = Sequence;

let module Hash = Hash;
let module Equality = Equality;
let module Ordering = Ordering;
let module Comparator = Comparator;
let module Comparable = Comparable;

let module HashStrategy = HashStrategy;

let module Concatable = {
  module type S1 = {
    type t 'a;

    let concat: (list (t 'a)) => (t 'a);
  };
};

let module Mappable = {
  module type S1 = {
    type t 'a;

    let map: ('a => 'b) => (t 'a) => (t 'b);
  };
};

let module ReverseMappable = {
  module type S1 = {
    type t 'a;

    let mapReverse: ('a => 'b) => (t 'a) => (t 'b);
  };
};

let module FlatMappable = {
  module type S1 = {
    type t 'a;

    let flatMap: ('a => t 'b) => (t 'a) => (t 'b);
  };
};

let module Flattenable = {
  module type S1 = {
    type t 'a;

    let flatten:  (t (t 'a)) => (t 'a);
  };
};

let module Reduceable = {
  module type S = {
    type a;
    type t;

    let forEach: (a => unit) => t => unit;
    let reduce: ('acc => a => 'acc) => 'acc => t => 'acc;
  };

  module type S1 = {
    type t 'a;

    let forEach: ('a => unit) => (t 'a) => unit;
    let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  };
};

let module ReduceableRight = {
  module type S = {
    type a;
    type t;

    let forEachRight: (a => unit) => t => unit;
    let reduceRight: ('acc => a => 'acc) => 'acc => t => 'acc;
  };

  module type S1 = {
    type t 'a;

    let forEachRight: ('a => unit) => (t 'a) => unit;
    let reduceRight: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  };
};

let module Filterable = {
  module type S1 = {
    type t 'a;

    let filter: ('a => bool) => (t 'a) => (t 'a);
  };
};

let module Iterator = Iterator;

let module Iterable = {
  module type S = {
    type a;
    type t;

    include Reduceable.S with type a := a and type t := t;

    let every: (a => bool) => t => bool;
    let find: (a => bool) => t => a;
    let isEmpty: t => bool;
    let isNotEmpty: t => bool;
    let none: (a => bool) => t => bool;
    let some: (a => bool) => t => bool;
    let toIterator: t => (Iterator.t a);
    let tryFind: (a => bool) => t => (option a);
  };

  module type S1 = {
    type t 'a;

    include Reduceable.S1 with type t 'a := t 'a;

    let every: ('a => bool) => (t 'a) => bool;
    let find: ('a => bool) => (t 'a) => 'a;
    let isEmpty: (t 'a) => bool;
    let isNotEmpty: (t 'a) => bool;
    let none: ('a => bool) => (t 'a) => bool;
    let some: ('a => bool) => (t 'a) => bool;
    let toIterator: t 'a => (Iterator.t 'a);
    let tryFind: ('a => bool) => (t 'a) => (option 'a);
  };
};

let module Sequential = {
  module type S = {
    type a;
    type t;

    include Iterable.S with type a := a and type t := t;

    let first: t => a;
    let tryFirst: t => (option a);
  };

  module type S1 = {
    type t 'a;

    include Iterable.S1 with type t 'a := t 'a;

    let first: (t 'a) => 'a;
    let tryFirst: (t 'a) => (option 'a);
  };
};

let module Sequence = Sequence;

let module List = {
  include ImmList;

  let addFirstAll = IteratorInternal.listAddFirstAll;
  let fromReverse = IteratorInternal.listFromReverse;
  let toIterator = IteratorInternal.ofList;
  let toSequence = SequenceInternal.ofList;
};

let module Collection = {
  module type S = {
    type a;
    type t;

    include Iterable.S with type a := a and type t := t;

    let count: t => int;
    let toSequence: t => (Sequence.t a);
  };

  module type S1 = {
    type t 'a;

    include Iterable.S1 with type t 'a := t 'a;

    let count: (t 'a) => int;
    let toSequence: (t 'a) => (Sequence.t 'a);
  };
};

let module TransientCollection = {
  module type S = {
    type a;
    type t;

    let count: t => int;
    let isEmpty: t => bool;
    let isNotEmpty: t => bool;
    let removeAll: t => t;
  };

  module type S1 = {
    type t 'a;

    let count: (t 'a) => int;
    let isEmpty: (t 'a) => bool;
    let isNotEmpty: (t 'a) => bool;
    let removeAll: (t 'a) => (t 'a);
  };
};

let module SequentialCollection = {
  module type S = {
    type a;
    type t;

    include Collection.S with type a := a and type t := t;
    include Sequential.S with type a := a and type t := t;
  };
  module type S1 = {
    type t 'a;

    include Collection.S1 with type t 'a := t 'a;
    include Sequential.S1 with type t 'a := t 'a;
  };
};

let module NavigableCollection = {
  module type S = {
    type a;
    type t;

    include SequentialCollection.S with type a := a and type t := t;
    include ReduceableRight.S with type a := a and type t := t;

    let last: t => a;
    let toIteratorRight: t => (Iterator.t a);
    let toSequenceRight: t => (Sequence.t a);
    let tryLast: t => (option a);
  };

  module type S1 = {
    type t 'a;

    include SequentialCollection.S1 with type t 'a := t 'a;
    include ReduceableRight.S1 with type t 'a := t 'a;

    let last: (t 'a) => 'a;
    let toIteratorRight: (t 'a) => (Iterator.t 'a);
    let toSequenceRight: (t 'a) => (Sequence.t 'a);
    let tryLast: (t 'a) => (option 'a);
  };
};

let module Stack = {
  /** A singly-linked stack with an O(1) count operation. */

  module type S1 = {
    type t 'a;

    include ReverseMappable.S1 with type t 'a := t 'a;
    include SequentialCollection.S1 with type t 'a := t 'a;

    let addFirst: 'a => (t 'a) => (t 'a);
    let addFirstAll: (Iterator.t 'a) => (t 'a) => (t 'a);
    let compare: (Comparator.t (t 'a));
    let compareWith: (Comparator.t 'a) => (Comparator.t (t 'a));
    let contains: 'a => (t 'a) => bool;
    let containsWith: (Equality.t 'a) => 'a => (t 'a) => bool;
    let empty: (t 'a);
    let equals: (Equality.t (t 'a));
    let equalsWith: (Equality.t 'a) => (Equality.t (t 'a));
    let fromReverse: (Iterator.t 'a) => (t 'a);
    let hash: (Hash.t (t 'a));
    let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
    let return: 'a => (t 'a);
    let removeAll: (t 'a) => (t 'a);
    let removeFirst: (t 'a) => (t 'a);
  };

  include Stack;
};

let module TransientStack = {
  module type S1 = {
    type t 'a;

    include TransientCollection.S1 with type t 'a := t 'a;

    let addFirst: 'a => (t 'a) => (t 'a);
    let empty: unit => (t 'a);
    let first: (t 'a) => 'a;
    let removeFirst: (t 'a) => (t 'a);
    let tryFirst: (t 'a) => option 'a;
  };
};

let module TransientDeque = {
  module type S1 = {
    type t 'a;

    include TransientStack.S1 with type t 'a := t 'a;

    let addLast: 'a => (t 'a) => (t 'a);
    let last: (t 'a) => 'a;
    let removeLast: (t 'a) => (t 'a);
    let tryLast: (t 'a) => option 'a;
  };

  include Deque.TransientDeque;
};
let module Deque = {
  module type S1 = {
    type t 'a;

    include Mappable.S1 with type t 'a := t 'a;
    include NavigableCollection.S1 with type t 'a := t 'a;
    include Stack.S1 with type t 'a := t 'a;

    let addLast: 'a => (t 'a) => (t 'a);
    let addLastAll: (Iterator.t 'a) => (t 'a) => (t 'a);
    let from: (Iterator.t 'a) => (t 'a);
    let removeLast: (t 'a) => (t 'a);
  };

  include Deque;
};

let module KeyedReduceable = {
  module type S1 = {
    type k;
    type t 'v;

    let forEach: (k => 'v => unit) => t 'v => unit;
    let reduce: ('acc => k => 'v => 'acc) => 'acc => t 'v => 'acc;
  };

  module type S2 = {
    type t 'k 'v;

    let forEach: ('k => 'v => unit) => t 'k 'v => unit;
    let reduce: ('acc => 'k => 'v => 'acc) => 'acc => t 'k 'v => 'acc;
  };
};

let module KeyedIterator = KeyedIterator;

let module KeyedIterable = {
  module type S1 = {
    type k;
    type t 'v;

    include KeyedReduceable.S1 with type k := k and type t 'v := t 'v;

    let every: (k => 'v => bool) => (t 'v) => bool;
    let find: (k => 'v => bool) => (t 'v) => (k, 'v);
    let isEmpty: (t 'v) => bool;
    let isNotEmpty: (t 'v) => bool;
    let none: (k => 'v => bool) => (t 'v) => bool;
    let some: (k => 'v => bool) => (t 'v) => bool;
    let toIterator: t 'v => Iterator.t (k, 'v);
    let toKeyedIterator: t 'v => KeyedIterator.t k 'v;
    let tryFind: (k => 'v => bool) => (t 'v) => (option (k, 'v));
  };

  module type S2 = {
    type t 'k 'v;

    include KeyedReduceable.S2 with type t 'k 'v := t 'k 'v;

    let every: ('k => 'v => bool) => (t 'k 'v) => bool;
    let find: ('k => 'v => bool) => (t 'k 'v) => ('k, 'v);
    let isEmpty: (t 'k 'v) => bool;
    let isNotEmpty: (t 'k 'v) => bool;
    let none: ('k => 'v => bool) => (t 'k 'v) => bool;
    let some: ('k => 'v => bool) => (t 'k 'v) => bool;
    let toIterator: t 'k 'v => Iterator.t ('k, 'v);
    let toKeyedIterator: t 'k 'v => KeyedIterator.t 'k 'v;
    let tryFind: ('k => 'v => bool) => (t 'k 'v) => (option ('k, 'v));
  };
};

let module KeyedCollection = {
  module type S1 = {
    type k;
    type t 'v;

    include KeyedIterable.S1 with type k := k and type t 'v := t 'v;

    let containsKey: k => t 'v => bool;
    let count: t 'v => int;
    let toSequence: (t 'v) => (Sequence.t (k, 'v));
  };

  module type S2 = {
    type t 'k 'v;

    include KeyedIterable.S2 with type t 'k 'v := t 'k 'v;

    let containsKey: 'k => t 'k 'v => bool;
    let count: t 'k 'v => int;
    let toSequence: (t 'k 'v) => (Sequence.t ('k, 'v));
  };
};

let module KeyedMappable = {
  module type S1 = {
    type k;
    type t 'v;

    let map: (k => 'a => 'b) => (t 'a) => (t 'b);
  };

  module type S2 = {
    type t 'k 'v;

    let map: ('k => 'a => 'b) => (t 'k 'a) => (t 'k 'b);
  };
};

let module Set = {
  module type S = {
    type a;
    type t;

    include Collection.S with type a := a and type t := t;

    let contains: a => t => bool;
    let equals: Equality.t t;
    let toKeyedIterator: t => KeyedIterator.t a a;
    let toMap: t => (ImmMap.t a a);
    let toSet: t => (ImmSet.t a);
  };

  module type S1 = {
    type t 'a;

    include Collection.S1 with type t 'a := t 'a;

    let contains: 'a => (t 'a) => bool;
    let equals: Equality.t (t 'a);
    let toKeyedIterator: (t 'a) => (KeyedIterator.t 'a 'a);
    let toMap: (t 'a) => (ImmMap.t 'a 'a);
    let toSet: (t 'a) => (ImmSet.t 'a);
  };

  include ImmSet;

  let toMap = ImmMap.ofSet;
};

let module Map = {
  module type S1 = {
    type k;
    type t 'v;

    include KeyedCollection.S1 with type k := k and type t 'v := t 'v;

    let get: k => (t 'v) => 'v;
    let keys: (t 'v) => (ImmSet.t k);
    let tryGet: k => (t 'v) => (option 'v);
    let values: (t 'v) => (Iterator.t 'v);
    let toMap: (t 'v) => (ImmMap.t k 'v);
  };

  module type S2 = {
    type t 'k 'v;

    include KeyedCollection.S2 with type t 'k 'v := t 'k 'v;

    let get: 'k => (t 'k 'v) => 'v;
    let keys: (t 'k 'v) => (ImmSet.t 'k);
    let tryGet: 'k => (t 'k 'v) => (option 'v);
    let values: (t 'k 'v) => (Iterator.t 'v);
    let toMap: (t 'k 'v) => (ImmMap.t 'k 'v);
  };

  include ImmMap;
};

let module Option = {
  include Option;

  let toSet = ImmSet.ofOption;
  let toSetWith = ImmSet.ofOptionWith;
  let toSequence = SequenceInternal.ofOption;
};

let module IndexedCollection = {
  module type S1 = {
    type t 'a;

    include NavigableCollection.S1 with type t 'a := t 'a;

    let everyWithIndex: (int => 'a => bool) => (t 'a) => bool;
    let findWithIndex: (int => 'a => bool) => (t 'a) => 'a;
    let forEachWithIndex: (int => 'a => unit) => (t 'a) => unit;
    let forEachRightWithIndex: (int => 'a => unit) => (t 'a) => unit;
    let get: int => (t 'a) => 'a;
    let indexOf: ('a => bool) => (t 'a) => int;
    let indexOfWithIndex: (int => 'a => bool) => (t 'a) => int;
    let noneWithIndex: (int => 'a => bool) => (t 'a) => bool;
    let reduceWithIndex: ('acc => int => 'a => 'acc) => 'acc => (t 'a) => 'acc;
    let reduceRightWithIndex: ('acc => int => 'a => 'acc) => 'acc => (t 'a) => 'acc;
    let someWithIndex: (int => 'a => bool) => (t 'a) => bool;
    let toKeyedIterator: (t 'a) => (KeyedIterator.t int 'a);
    let toKeyedIteratorRight: (t 'a) => (KeyedIterator.t int 'a);
    let toMap: (t 'a) => (Map.t int 'a);
    let tryFindWithIndex: (int => 'a => bool) => (t 'a) => (option 'a);
    let tryGet: int => (t 'a) => (option 'a);
    let tryIndexOf: ('a => bool) => (t 'a) => (option int);
    let tryIndexOfWithIndex: (int => 'a => bool) => (t 'a) => (option int);
  };
};

let module IndexedMappable = {
  module type S1 = {
    type t 'a;

    include Mappable.S1 with type t 'a := t 'a;
    include ReverseMappable.S1 with type t 'a := t 'a;

    let mapWithIndex: (int => 'a => 'b) => (t 'a) => (t 'b);
    let mapReverseWithIndex: (int => 'a => 'b) => (t 'a) => (t 'b);
  };
};

let module TransientVector = Vector.TransientVector;
let module Vector = {
  module type S1 = {
    type t 'a;

    include Concatable.S1 with type t 'a := t 'a;
    include Deque.S1 with type t 'a := t 'a;
    include IndexedCollection.S1 with type t 'a := t 'a;
    include IndexedMappable.S1 with type t 'a := t 'a;

    let init: int => (int => 'a) => (t 'a);
    let insertAt: int => 'a => (t 'a) => (t 'a);
    let range: int => (option int) => (t 'a) => (t 'a);
    let removeAt: int => (t 'a) => (t 'a);
    let skip: int => (t 'a) => (t 'a);
    let take: int => (t 'a) => (t 'a);
    let update: int => 'a => (t 'a) => (t 'a);
    let updateAll: (int => 'a => 'a) => (t 'a) => (t 'a);
    let updateWith: int => ('a => 'a) => (t 'a) => (t 'a);
  };

  include Vector;
};

let module CopyOnWriteArray = CopyOnWriteArray;

let module NavigableSet = {
  module type S = {
    type a;
    type t;

    include Set.S with type a := a and type t := t;
    include NavigableCollection.S with type a := a and type t := t;

    let toKeyedIteratorRight: t => (KeyedIterator.t a a);
  };

  module type S1 = {
    type t 'a;

    include Set.S1 with type t 'a := t 'a;
    include NavigableCollection.S1 with type t 'a := t 'a;

    let toKeyedIteratorRight: (t 'a) => (KeyedIterator.t 'a 'a);
  };
};

let module IntRange = IntRange;

let module PersistentSet = {
  module type S = {
    type a;
    type t;

    include Set.S with type a := a and type t := t;

    let add: a => t => t;
    let addAll: (Iterator.t a) => t => t;
    let intersect: t => t => t;
    let remove: a => t => t;
    let removeAll: t => t;
    let subtract: t => t => t;
    let union: t => t => t;
  };

  module type S1 = {
    type t 'a;

    include Set.S1 with type t 'a := t 'a;

    let add: 'a => (t 'a) => (t 'a);
    let addAll: (Iterator.t 'a) => (t 'a) => (t 'a);
    let intersect: (t 'a) => (t 'a) => (t 'a);
    let remove: 'a => (t 'a) => (t 'a);
    let removeAll: (t 'a) => (t 'a);
    let subtract: (t 'a) => (t 'a) => (t 'a);
    let union: (t 'a) => (t 'a) => (t 'a);
  };
};

let module PersistentNavigableSet = {
  module type S = {
    type a;
    type t;

    include NavigableSet.S with type a := a and type t := t;
    include PersistentSet.S with type a := a and type t := t;

    let removeFirst: t => t;
    let removeLast: t => t;
  };

  module type S1 = {
    type t 'a;

    include NavigableSet.S1 with type t 'a := t 'a;
    include PersistentSet.S1 with type t 'a := t 'a;

    let removeFirst: t 'a => t 'a;
    let removeLast: t 'a => t 'a;
  };
};

let module HashSet = HashSet;
let module TransientHashSet = HashSet.TransientHashSet;

let module SortedSet = SortedSet;

let module IntSet = IntSet;
let module TransientIntSet = IntSet.TransientIntSet;

let module NavigableMap = {
  module type S1 = {
    type k;
    type t 'v;

    include Map.S1 with type k := k and type t 'v := t 'v;

    let first: (t 'v) => (k, 'v);
    /** [first map] returns the first key/value pair in [set] or throws. */

    let last: (t 'v) => (k, 'v);
    /** [last map] returns the last key/value pair in [set] or throws. */

    let reduceRight: ('acc => k => 'v => 'acc) => 'acc => (t 'v) => 'acc;
    /** [reduceRight f acc map] applies the accumulator function [f] to each key/value pair in [map]
     *  in reverse order with the specified seed value [acc], returning the final accumulated value.
     */

    let toIteratorRight: (t 'v) => (Iterator.t (k, 'v));

    let toKeyedIteratorRight: (t 'v) => (KeyedIterator.t k 'v);

    let toSequenceRight: (t 'v) => (Sequence.t (k, 'v));

    let tryFirst: (t 'v) => (option (k, 'v));
    /** [tryFirst map] returns the first key/value pair in [set] or None. */

    let tryLast: (t 'v) => (option (k, 'v));
    /** [tryLast map] returns the last key/value pair in [set] or None. */
  };
};

let module PersistentMap = {
  module type S1 = {
    type k;
    type t 'v;

    include Map.S1 with type k := k and type t 'v := t 'v;
    include KeyedMappable.S1 with type k := k and type t 'v := t 'v;

    let alter: k => (option 'v => option 'v) => (t 'v) => (t 'v);
    let merge: (k => (option 'vAcc) => (option 'v) => (option 'vAcc)) => (t 'v) => (t 'vAcc) => (t 'vAcc);
    let put: k => 'v => (t 'v) => (t 'v);
    let putAll: (KeyedIterator.t k 'v) => (t 'v) => (t 'v);
    let remove: k => (t 'v) => (t 'v);
    let removeAll: (t 'v) => (t 'v);
  };

  module type S2 = {
    type t 'k 'v;

    include Map.S2 with type t 'k 'v := t 'k 'v;
    include KeyedMappable.S2 with type t 'k 'v := t 'k 'v;

    let alter: 'k => (option 'v => option 'v) => (t 'k 'v) => (t 'k 'v);
    let merge: ('k => (option 'vAcc) => (option 'v) => (option 'vAcc)) => (t 'k 'v) => (t 'k 'vAcc) => (t 'k 'vAcc);
    let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);
    let putAll: (KeyedIterator.t 'k 'v) => (t 'k 'v) => (t 'k 'v);
    let remove: 'k => (t 'k 'v) => (t 'k 'v);
    let removeAll: (t 'k 'v) => (t 'k 'v);
  };
};

let module PersistentNavigableMap = {
  module type S1 = {
    type k;
    type t 'v;

    include NavigableMap.S1 with type k := k and type t 'v := t 'v;
    include PersistentMap.S1 with type k := k and type t 'v := t 'v;

    let removeFirst: (t 'v) => (t 'v);
    /** [removeFirst map] returns a new SortedMap without the first element.
     *
     *  Complexity: O(log N)
     */

    let removeLast: (t 'v) => (t 'v);
    /** [removeLast map] returns a new SortedMap without the last element.
     *
     *  Complexity: O(log N)
     */
  };
};

let module TransientMap = {
  module type S1 = {
    type k;

    type t 'v;

    let alter: k => (option 'v => option 'v) => (t 'v) => (t 'v);
    let count: (t 'v) => int;
    let get: k => (t 'v) => 'v;
    let isEmpty: (t 'v) => bool;
    let isNotEmpty: (t 'v) => bool;
    let put: k => 'v => (t 'v) => (t 'v);
    let putAll: (KeyedIterator.t k 'v) => (t 'v) => (t 'v);
    let remove: k => (t 'v) => (t 'v);
    let removeAll: (t 'v) => (t 'v);
    let tryGet: 'k => (t 'v) => (option 'v);
  };

  module type S2 = {
    type t 'k 'v;

    let alter: 'k => (option 'v => option 'v) => (t 'k 'v) => (t 'k 'v);
    let count: (t 'k 'v) => int;
    let get: 'k => (t 'k 'v) => 'v;
    let isEmpty: (t 'k 'v) => bool;
    let isNotEmpty: (t 'k 'v) => bool;
    let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);
    let putAll: (KeyedIterator.t 'k 'v) => (t 'k 'v) => (t 'k 'v);
    let remove: 'k => (t 'k 'v) => (t 'k 'v);
    let removeAll: (t 'k 'v) => (t 'k 'v);
    let tryGet: 'k => (t 'k 'v) => (option 'v);
  };
};

let module HashMap = HashMap;
let module TransientHashMap = HashMap.TransientHashMap;

let module IntMap = IntMap;
let module TransientIntMap = IntMap.TransientIntMap;

let module SortedMap = SortedMap;
