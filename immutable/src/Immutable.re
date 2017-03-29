/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

let module Equality = Equality;

let module Ordering = Ordering;

let module Comparator = Comparator;

let module Hash = Hash;

let module Equatable = Equatable;

let module Comparable = Comparable;

let module Hashable = {
  module type S = {
    type t;

    let hash: Hash.t t;
  };
};

let module Reduceable = {
  module type S = {
    type a;
    type t;

    let reduce: while_::('acc => a => bool)? => ('acc => a => 'acc) => 'acc => t => 'acc;
  };

  module type S1 = {
    type t 'a;

    let reduce: while_::('acc => 'a => bool)? => ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  };
};

let module ReduceableRight = {
  module type S = {
    type a;
    type t;

    include Reduceable.S with type a := a and type t := t;

    let reduceRight: while_::('acc => a => bool)? => ('acc => a => 'acc) => 'acc => t => 'acc;
  };

  module type S1 = {
    type t 'a;

    include Reduceable.S1 with type t 'a := t 'a;

    let reduceRight: while_::('acc => 'a => bool)? => ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  };
};

let module Streamable = {
  module type S1 = {
    type t 'a;

    let buffer: count::int => skip::int => (t 'a) => (t (list 'a));
    let concat: (list (t 'a)) => (t 'a);
    let defer: (unit => t 'a) => (t 'a);
    let distinctUntilChangedWith: (Equality.t 'a) => (t 'a) => (t 'a);
    let doOnNext: ('a => unit) => (t 'a) => (t 'a);
    let empty: unit => (t 'a);
    let filter: ('a => bool) => (t 'a) => (t 'a);
    let flatMap: ('a => t 'b) => (t 'a) => (t 'b);
    let flatten: (t (t 'a)) => (t 'a);
    let generate: ('a => 'a) => 'a => (t 'a);
    let map: ('a => 'b) => (t 'a) => (t 'b);
    let return: 'a => (t 'a);
    let scan: ('acc => 'a => 'acc) => 'acc => (t 'a) => (t 'acc);
    let skip: int => (t 'a) => (t 'a);
    let skipWhile: ('a => bool) => (t 'a) => (t 'a);
    let startWith: 'a => (t 'a) => (t 'a);
    let take: int => (t 'a) => (t 'a);
    let takeWhile: ('a => bool) => (t 'a) => (t 'a);
  };
};

let module Reducer = Reducer;

let module Iterator = Iterator;

let module Iterable = {
  module type S = {
    type a;
    type t;

    include Reduceable.S with type a := a and type t := t;

    let toIterator: t => (Iterator.t a);
  };

  module type S1 = {
    type t 'a;

    include Reduceable.S1 with type t 'a := t 'a;

    let toIterator: t 'a => (Iterator.t 'a);
  };
};

let module Sequence = Sequence;

let module Collection = {
  include Collection;

  module type S = {
    type a;
    type t;

    include Iterable.S with type a := a and type t := t;
    include Equatable.S with type t := t;

    let count: t => int;
    let empty: t;
    let isEmpty: t => bool;
    let isNotEmpty: t => bool;
    let toCollection: t => Collection.t a;
    let toSequence: t => (Sequence.t a);
  };

  module type S1 = {
    type t 'a;

    include Iterable.S1 with type t 'a := t 'a;

    let count: (t 'a) => int;
    let isEmpty: (t 'a) => bool;
    let isNotEmpty: (t 'a) => bool;
    let toCollection: (t 'a) => (Collection.t 'a);
    let toSequence: (t 'a) => (Sequence.t 'a);
  };
};

let module PersistentCollection = {
  module type S = {
    type a;
    type t;

    include Collection.S with type a := a and type t := t;

    let removeAll: t => t;
  };

  module type S1 = {
    type t 'a;

    include Collection.S1 with type t 'a := t 'a;

    let removeAll: t 'a => t 'a;
  };
};

let module TransientCollection = {
  module type S = {
    type a;
    type t;

    let count: t => int;
    let empty: unit => t;
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

    let first: t => (option a);
    let firstOrRaise: t => a;
  };

  module type S1 = {
    type t 'a;

    include Collection.S1 with type t 'a := t 'a;

    let first: (t 'a) => (option 'a);
    let firstOrRaise: (t 'a) => 'a;
  };
};

let module PersistentSequentialCollection = {
  module type S1 = {
    type t 'a;

    include PersistentCollection.S1 with type t 'a := t 'a;
    include SequentialCollection.S1 with type t 'a := t 'a;

    let addFirst: 'a => (t 'a) => (t 'a);
    let addFirstAll: (Iterator.t 'a) => (t 'a) => (t 'a);
    let empty: unit => (t 'a);
    let fromReverse: (Iterator.t 'a) => (t 'a);
    let return: 'a => (t 'a);
    let removeFirstOrRaise: (t 'a) => (t 'a);
  };
};

let module TransientSequentialCollection = {
  module type S1 = {
    type t 'a;

    include TransientCollection.S1 with type t 'a := t 'a;

    let addFirst: 'a => (t 'a) => (t 'a);
    let addFirstAll: (Iterator.t 'a) => (t 'a) => (t 'a);
    let empty: unit => (t 'a);
    let first: (t 'a) => option 'a;
    let firstOrRaise: (t 'a) => 'a;
    let removeFirstOrRaise: (t 'a) => (t 'a);
  };
};

let module NavigableCollection = {
  module type S = {
    type a;
    type t;

    include ReduceableRight.S with type a := a and type t := t;
    include SequentialCollection.S with type a := a and type t := t;

    let last: t => (option a);
    let lastOrRaise: t => a;
    let toIteratorRight: t => (Iterator.t a);
    let toSequenceRight: t => (Sequence.t a);
  };

  module type S1 = {
    type t 'a;

    include ReduceableRight.S1 with type t 'a := t 'a;
    include SequentialCollection.S1 with type t 'a := t 'a;

    let last: (t 'a) => (option 'a);
    let lastOrRaise: (t 'a) => 'a;
    let toIteratorRight: (t 'a) => (Iterator.t 'a);
    let toSequenceRight: (t 'a) => (Sequence.t 'a);
  };
};

let module PersistentNavigableCollection = {
  module type S1 = {
    type t 'a;

    include NavigableCollection.S1 with type t 'a := t 'a;
    include PersistentSequentialCollection.S1 with type t 'a := t 'a;

    let addLast: 'a => (t 'a) => (t 'a);
    let addLastAll: (Iterator.t 'a) => (t 'a) => (t 'a);
    let from: (Iterator.t 'a) => (t 'a);
    let removeLastOrRaise: (t 'a) => (t 'a);
  };
};

let module TransientNavigableCollection = {
  module type S1 = {
    type t 'a;

    include TransientSequentialCollection.S1 with type t 'a := t 'a;

    let addLast: 'a => (t 'a) => (t 'a);
    let last: (t 'a) => option 'a;
    let lastOrRaise: (t 'a) => 'a;
    let removeLastOrRaise: (t 'a) => (t 'a);
  };
};

let module Set = {
  include ImmSet;

  module type S = {
    type a;
    type t;

    include Collection.S with type a := a and type t := t;

    let contains: a => t => bool;
    let toSet: t => ImmSet.t a;
  };

  module type S1 = {
    type t 'a;

    include Collection.S1 with type t 'a := t 'a;

    let contains: 'a => (t 'a) => bool;
    let equals: Equality.t (t 'a);
    let toSet: (t 'a) => ImmSet.t 'a;
  };
};

let module PersistentSet = {
  module type S = {
    type a;
    type t;

    include Set.S with type a := a and type t := t;
    include PersistentCollection.S with type a := a and type t := t;

    let add: a => t => t;
    let addAll: (Iterator.t a) => t => t;
    let from: (Iterator.t a) => t;
    let intersect: t => t => t;
    let remove: a => t => t;
    let subtract: t => t => t;
    let union: t => t => t;
  };

  module type S1 = {
    type t 'a;

    include Set.S1 with type t 'a := t 'a;
    include PersistentCollection.S1 with type t 'a := t 'a;

    let add: 'a => (t 'a) => (t 'a);
    let addAll: (Iterator.t 'a) => (t 'a) => (t 'a);
    let intersect: (t 'a) => (t 'a) => (t 'a);
    let remove: 'a => (t 'a) => (t 'a);
    let subtract: (t 'a) => (t 'a) => (t 'a);
    let union: (t 'a) => (t 'a) => (t 'a);
  };
};

let module TransientSet = {
  module type S = {
    type a;
    type t;

    include TransientCollection.S with type a := a and type t := t;

    let add: a => t => t;
    let addAll: (Iterator.t a) => t => t;
    let contains: a => t => bool;
    let remove: a => t => t;
  };

  module type S1 = {
    type t 'a;

    include TransientCollection.S1 with type t 'a := t 'a;

    let add: 'a => (t 'a) => (t 'a);
    let addAll: (Iterator.t 'a) => (t 'a) => (t 'a);
    let contains: 'a => (t 'a) => bool;
    let remove: 'a => (t 'a) => (t 'a);
  };
};

let module NavigableSet = {
  module type S = {
    type a;
    type t;

    include Set.S with type a := a and type t := t;
    include NavigableCollection.S with type a := a and type t := t;
  };
};

let module PersistentNavigableSet = {
  module type S = {
    type a;
    type t;

    include NavigableSet.S with type a := a and type t := t;
    include PersistentSet.S with type a := a and type t := t;

    let removeFirstOrRaise: t => t;

    let removeLastOrRaise: t => t;
  };
};

let module KeyedReduceable = {
  module type S1 = {
    type k;
    type t 'v;

    let reduce: while_::('acc => k => 'v => bool)? => ('acc => k => 'v => 'acc) => 'acc => (t 'v) => 'acc;
  };

  module type S2 = {
    type t 'k 'v;

    let reduce: while_::('acc => 'k => 'v => bool)? => ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  };
};

let module KeyedReduceableRight = {
  module type S1 = {
    type k;
    type t 'v;

    include KeyedReduceable.S1 with type k := k and type t 'v := t 'v;

    let reduceRight: while_::('acc => k => 'v => bool)? => ('acc => k => 'v => 'acc) => 'acc => (t 'v) => 'acc;
  };
};

let module KeyedReducer = KeyedReducer;

let module KeyedIterator = KeyedIterator;

let module KeyedIterable = {
  module type S1 = {
    type k;
    type t 'v;

    include KeyedReduceable.S1 with type k := k and type t 'v := t 'v;

    let toIterator: t 'v => Iterator.t (k, 'v);

    let toKeyedIterator: t 'v => KeyedIterator.t k 'v;
  };

  module type S2 = {
    type t 'k 'v;

    include KeyedReduceable.S2 with type t 'k 'v := t 'k 'v;

    let toIterator: t 'k 'v => Iterator.t ('k, 'v);

    let toKeyedIterator: t 'k 'v => KeyedIterator.t 'k 'v;
  };
};

let module KeyedCollection = {
  module type S1 = {
    type k;
    type t 'v;

    include KeyedIterable.S1 with type k := k and type t 'v := t 'v;

    let containsKey: k => t 'v => bool;
    let count: t 'v => int;
    let isEmpty: (t 'v) => bool;
    let isNotEmpty: (t 'v) => bool;
    let keys: (t 'v) => (Set.t k);
    let toSequence: (t 'v) => (Sequence.t (k, 'v));
  };

  module type S2 = {
    type t 'k 'v;

    include KeyedIterable.S2 with type t 'k 'v := t 'k 'v;

    let containsKey: 'k => t 'k 'v => bool;
    let count: t 'k 'v => int;
    let isEmpty: (t 'k 'v) => bool;
    let isNotEmpty: (t 'k 'v) => bool;
    let keys: (t 'k 'v) => (Set.t 'k);
    let toSequence: (t 'k 'v) => (Sequence.t ('k, 'v));
  };
};

let module PersistentKeyedCollection = {
  module type S1 = {
    type k;
    type t 'v;

    include KeyedCollection.S1 with type k := k and type t 'v := t 'v;

    let remove: k => (t 'v) => (t 'v);

    let removeAll: (t 'v) => (t 'v);
  };

  module type S2 = {
    type t 'k 'v;

    include KeyedCollection.S2 with  type t 'k 'v := t 'k 'v;

    let remove: 'k => (t 'k 'v) => (t 'k 'v);

    let removeAll: (t 'k 'v) => (t 'k 'v);
  };
};

let module TransientKeyedCollection = {
  module type S1 = {
    type k;
    type t 'v;

    let containsKey: k => (t 'v) => bool;

    let count: (t 'v) => int;
    let isEmpty: (t 'v) => bool;
    let isNotEmpty: (t 'v) => bool;
    let remove: k => (t 'v) => (t 'v);
    let removeAll: (t 'v) => (t 'v);
  };

  module type S2 = {
    type t 'k 'v;

    let containsKey: 'k => (t 'k 'v) => bool;

    let count: (t 'k 'v) => int;
    let isEmpty: (t 'k 'v) => bool;
    let isNotEmpty: (t 'k 'v) => bool;
    let remove: 'k => (t 'k 'v) => (t 'k 'v);
    let removeAll: (t 'k 'v) => (t 'k 'v);
  };
};

let module NavigableKeyedCollection = {
  module type S1 = {
    type k;
    type t 'v;

    include KeyedCollection.S1 with type k := k and type t 'v := t 'v;
    include KeyedReduceableRight.S1 with type k := k and type t 'v := t 'v;

    let first: (t 'v) => (option (k, 'v));
    let firstOrRaise: (t 'v) => (k, 'v);
    let last: (t 'v) => (option (k, 'v));
    let lastOrRaise: (t 'v) => (k, 'v);
    let toIteratorRight: t 'v => Iterator.t (k, 'v);
    let toKeyedIteratorRight: t 'v => KeyedIterator.t k 'v;
    let toSequenceRight: (t 'v) => (Sequence.t (k, 'v));
  };
};

let module Map = {
  include ImmMap;

  module type S1 = {
    type k;
    type t 'v;

    include KeyedCollection.S1 with type k := k and type t 'v := t 'v;

    let get: k => (t 'v) => (option 'v);
    let getOrRaise: k => (t 'v) => 'v;
    let toMap: (t 'v) => ImmMap.t k 'v;
  };

  module type S2 = {
    type t 'k 'v;

    include KeyedCollection.S2 with type t 'k 'v := t 'k 'v;

    let get: 'k => (t 'k 'v) => (option 'v);
    let getOrRaise: 'k => (t 'k 'v) => 'v;
    let toMap: (t 'k 'v) => ImmMap.t 'k 'v;
  };
};

let module PersistentMap = {
  module type S1 = {
    type k;
    type t 'v;

    include PersistentKeyedCollection.S1 with type k := k and type t 'v := t 'v;
    include Map.S1 with type k := k and type t 'v := t 'v;

    let alter: k => (option 'v => option 'v) => (t 'v) => (t 'v);

    let empty: unit => (t 'v);
    let from: (KeyedIterator.t k 'v) => (t 'v);
    let fromEntries: (Iterator.t (k, 'v)) => (t 'v);
    let merge: (k => (option 'vAcc) => (option 'v) => (option 'vAcc)) => (t 'vAcc) => (t 'v) => (t 'vAcc);
    let put: k => 'v => (t 'v) => (t 'v);
    let putAll: (KeyedIterator.t k 'v) => (t 'v) => (t 'v);
    let putAllEntries: (Iterator.t (k, 'v)) => (t 'v) => (t 'v);
  };

  module type S2 = {
    type t 'k 'v;

    include PersistentKeyedCollection.S2 with type t 'k 'v := t 'k 'v;
    include Map.S2 with type t 'k 'v := t 'k 'v;

    let alter: 'k => (option 'v => option 'v) => (t 'k 'v) => (t 'k 'v);
    let merge: ('k => (option 'vAcc) => (option 'v) => (option 'vAcc)) => (t 'k 'vAcc) => (t 'k 'v) => (t 'k 'vAcc);
    let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);
    let putAll: (KeyedIterator.t 'k 'v) => (t 'k 'v) => (t 'k 'v);
    let putAllEntries: (Iterator.t ('k, 'v)) => (t 'k 'v) => (t 'k 'v);
  };
};

let module TransientMap = {
  module type S1 = {
    type k;

    type t 'v;

    include TransientKeyedCollection.S1 with type k := k and type t 'v := t 'v;

    let alter: k => (option 'v => option 'v) => (t 'v) => (t 'v);
    let empty: unit => (t 'v);
    let get: k => (t 'v) => (option 'v);
    let getOrRaise: k => (t 'v) => 'v;
    let put: k => 'v => (t 'v) => (t 'v);
    let putAll: (KeyedIterator.t k 'v) => (t 'v) => (t 'v);
    let putAllEntries: (Iterator.t (k, 'v)) => (t 'v) => (t 'v);
  };

  module type S2 = {
    type t 'k 'v;

    include TransientKeyedCollection.S2 with type t 'k 'v := t 'k 'v;

    let alter: 'k => (option 'v => option 'v) => (t 'k 'v) => (t 'k 'v);
    let get: 'k => (t 'k 'v) => (option 'v);
    let getOrRaise: 'k => (t 'k 'v) => 'v;
    let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);
    let putAll: (KeyedIterator.t 'k 'v) => (t 'k 'v) => (t 'k 'v);
    let putAllEntries: (Iterator.t ('k, 'v)) => (t 'k 'v) => (t 'k 'v);
  };
};

let module NavigableMap = {
  module type S1 = {
    type k;
    type t 'v;

    include NavigableKeyedCollection.S1 with type k := k and type t 'v := t 'v;
    include Map.S1 with type k := k and type t 'v := t 'v;
  };
};

let module PersistentNavigableMap = {
  module type S1 = {
    type k;
    type t 'v;

    include NavigableMap.S1 with type k := k and type t 'v := t 'v;
    include PersistentMap.S1 with type k := k and type t 'v := t 'v;

    let removeFirstOrRaise: (t 'v) => (t 'v);
    let removeLastOrRaise: (t 'v) => (t 'v);
  };
};

let module Indexed = {
  module type S1 = {
    type t 'a;

    include NavigableCollection.S1 with type t 'a := t 'a;

    let get: int => (t 'a) => (option 'a);
    let getOrRaise: int => (t 'a) => 'a;
    let toKeyedIterator: (t 'a) => (KeyedIterator.t int 'a);
    let toKeyedIteratorRight: (t 'a) => (KeyedIterator.t int 'a);
    let toMap: (t 'a) => (Map.t int 'a);
  };
};

let module Deque = Deque;

let module TransientDeque = Deque.TransientDeque;

let module HashMap = HashMap;

let module TransientHashMap = HashMap.TransientHashMap;

let module HashSet = HashSet;

let module TransientHashSet = HashSet.TransientHashSet;

let module IntMap = IntMap;

let module TransientIntMap = IntMap.TransientIntMap;

let module IntRange = IntRange;

let module IntSet = IntSet;

let module TransientIntSet = IntSet.TransientIntSet;

let module List = {
  include ImmList;

  let addFirstAll = Iterator.listAddFirstAll;
  let fromReverse = Iterator.listFromReverse;
  let toIterator = Iterator.ofList;
  let toSequence = Sequence.ofList;
};

let module ReadOnlyArray = CopyOnWriteArray;

let module SortedMap = SortedMap;

let module SortedSet = SortedSet;

let module Stack = ImmStack;

let module Vector = Vector;

let module TransientVector = Vector.TransientVector;
