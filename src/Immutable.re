/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Functions.Operators;

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

let module Streamable = {
  module type S1 = {
    type t 'a;

    let defer: (unit => t 'a) => (t 'a);
    let distinctUntilChangedWith: (Equality.t 'a) => (t 'a) => (t 'a);
    let doOnNext: ('a => unit) => (t 'a) => (t 'a);
    let filter: ('a => bool) => (t 'a) => (t 'a);
    let flatMap: ('a => t 'b) => (t 'a) => (t 'b);
    let flatten: (t (t 'a)) => (t 'a);
    let map: ('a => 'b) => (t 'a) => (t 'b);
    let scan: ('acc => 'a => 'acc) => 'acc => (t 'a) => (t 'acc);
    let skip: int => (t 'a) => (t 'a);
    let skipWhile: ('a => bool) => (t 'a) => (t 'a);
    let startWith: 'a => (t 'a) => (t 'a);
    let take: int => (t 'a) => (t 'a);
    let takeWhile: ('a => bool) => (t 'a) => (t 'a);
  };
};

let module Iterable = {
  include Iterable;

  include (Iterable.MakeGeneric {
    type elt 'a = 'a;

    include Iterable;
  }: Iterable.S1 with type t 'a := t 'a);
};

let module Sequence = {
  include Sequence;

  include (Iterable.MakeGeneric {
    type elt 'a = 'a;

    include Sequence;
  }: Iterable.S1 with type t 'a := t 'a);
};

let module Collection = {
  include Collection;

  include (Collection.MakeGeneric {
    type elt 'a = 'a;

    include Collection;
  }: Collection.S1 with type t 'a := t 'a);

  let module Persistent = {
    module type S = {
      type a;
      type t;

      include S with type a := a and type t := t;

      let removeAll: t => t;
    };

    module type S1 = {
      type t 'a;

      include S1 with type t 'a := t 'a;

      let removeAll: t 'a => t 'a;
    };
  };

  let module Transient = {
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

};

let module SequentialCollection = {
  include SequentialCollection;

  include (SequentialCollection.MakeGeneric {
    type elt 'a = 'a;

    include SequentialCollection;
  }: S1 with type t 'a := t 'a);

  let module Persistent = {
    module type S1 = {
      type t 'a;

      include Collection.Persistent.S1 with type t 'a := t 'a;
      include S1 with type t 'a := t 'a;

      let addFirst: 'a => (t 'a) => (t 'a);
      let addFirstAll: (Iterable.t 'a) => (t 'a) => (t 'a);
      let removeFirstOrRaise: (t 'a) => (t 'a);
    };
  };

  let module Transient = {
    module type S1 = {
      type t 'a;

      include Collection.Transient.S1 with type t 'a := t 'a;

      let addFirst: 'a => (t 'a) => (t 'a);
      let addFirstAll: (Iterable.t 'a) => (t 'a) => (t 'a);
      let first: (t 'a) => option 'a;
      let firstOrRaise: (t 'a) => 'a;
      let removeFirstOrRaise: (t 'a) => (t 'a);
    };
  };
};

let module NavigableCollection = {
  include NavigableCollection;

  include (NavigableCollection.MakeGeneric {
    type elt 'a = 'a;

    include NavigableCollection;
  }: S1 with type t 'a := t 'a);

  let module Persistent = {
    module type S1 = {
      type t 'a;

      include S1 with type t 'a := t 'a;
      include SequentialCollection.Persistent.S1 with type t 'a := t 'a;

      let addLast: 'a => (t 'a) => (t 'a);
      let addLastAll: (Iterable.t 'a) => (t 'a) => (t 'a);
      let removeLastOrRaise: (t 'a) => (t 'a);
    };
  };

  let module Transient = {
    module type S1 = {
      type t 'a;

      include SequentialCollection.Transient.S1 with type t 'a := t 'a;

      let addLast: 'a => (t 'a) => (t 'a);
      let last: (t 'a) => option 'a;
      let lastOrRaise: (t 'a) => 'a;
      let removeLastOrRaise: (t 'a) => (t 'a);
    };
  };
};

let module Set = {
  include ImmSet;

  include (ImmSet.MakeGeneric {
    type elt 'a = 'a;

    include ImmSet;
  }: ImmSet.S1 with type t 'a := t 'a);

  let intersect (this: t 'a) (that: t 'a): (Iterable.t 'a) =>
    this |> toIterable |> Iterable.filter (Functions.flip contains that);

  let subtract (this: t 'a) (that: t 'a): (Iterable.t 'a) =>
    this |> toIterable |> Iterable.filter (Functions.flip contains that >> not);

  let union (this: t 'a) (that: t 'a): (Iterable.t 'a) => Iterable.concat [
    this |> toIterable,
    subtract that this,
  ];

  let module Persistent = {
    module type S = {
      type a;
      type t;

      include S with type a := a and type t := t;
      include Collection.Persistent.S with type a := a and type t := t;

      let add: a => t => t;
      let addAll: (Iterable.t a) => t => t;
      let intersect: t => t => t;
      let remove: a => t => t;
      let subtract: t => t => t;
      let union: t => t => t;
    };

    module type S1 = {
      type t 'a;

      include S1 with type t 'a := t 'a;
      include Collection.Persistent.S1 with type t 'a := t 'a;

      let add: 'a => (t 'a) => (t 'a);
      let addAll: (Iterable.t 'a) => (t 'a) => (t 'a);
      let intersect: (t 'a) => (t 'a) => (t 'a);
      let remove: 'a => (t 'a) => (t 'a);
      let subtract: (t 'a) => (t 'a) => (t 'a);
      let union: (t 'a) => (t 'a) => (t 'a);
    };
  };

  let module Transient = {
    module type S = {
      type a;
      type t;

      include Collection.Transient.S with type a := a and type t := t;

      let add: a => t => t;
      let addAll: (Iterable.t a) => t => t;
      let contains: a => t => bool;
      let remove: a => t => t;
    };

    module type S1 = {
      type t 'a;

      include Collection.Transient.S1 with type t 'a := t 'a;

      let add: 'a => (t 'a) => (t 'a);
      let addAll: (Iterable.t 'a) => (t 'a) => (t 'a);
      let contains: 'a => (t 'a) => bool;
      let remove: 'a => (t 'a) => (t 'a);
    };
  };
};

let module NavigableSet = {
  include NavigableSet;

  include(NavigableSet.MakeGeneric {
    type elt 'a = 'a;

    include NavigableSet;
  }: NavigableSet.S1 with type t 'a := t 'a);

  let intersect (this: t 'a) (that: t 'a): (Iterable.t 'a) =>
    this |> toIterable |> Iterable.filter (Functions.flip contains that);

  let subtract (this: t 'a) (that: t 'a): (Iterable.t 'a) =>
    this |> toIterable |> Iterable.filter (Functions.flip contains that >> not);

  let union (this: t 'a) (that: t 'a): (Iterable.t 'a) => Iterable.concat [
    this |> toIterable,
    subtract that this,
  ];

  let module Persistent = {
    module type S = {
      type a;
      type t;

      include S with type a := a and type t := t;
      include Set.Persistent.S with type a := a and type t := t;

      let removeFirstOrRaise: t => t;

      let removeLastOrRaise: t => t;
    };
  };
};

let module KeyedStreamable = {
  module type S2 = {
    type t 'k 'v;

    let defer: (unit => t 'k 'v) => (t 'k 'v);
    let distinctUntilChangedWith: keyEquals::(Equality.t 'k) => valueEquals::(Equality.t 'v) => (t 'k 'v) => (t 'k 'v);
    let doOnNext: ('k => 'v => unit) => (t 'k 'v) => (t 'k 'v);
    let filter: ('k => 'v => bool) => (t 'k 'v) => (t 'k 'v);
    let flatMap: ('kA => 'vA => t 'kB 'vB) => (t 'kA 'vA) => (t 'kB 'vB);
    let map: keyMapper::('kA => 'vA => 'kB) => valueMapper::('kA => 'vA => 'vB) => (t 'kA 'vA) => (t 'kB 'vB);
    let mapKeys: ('a => 'v => 'b) => (t 'a 'v) => (t 'b 'v);
    let mapValues: ('k => 'a => 'b) => (t 'k 'a) => (t 'k 'b);
    let skip: int => (t 'k 'v) => (t 'k 'v);
    let skipWhile: ('k => 'v => bool) => (t 'k 'v) => (t 'k 'v);
    let startWith: 'k => 'v => (t 'k 'v) => (t 'k 'v);
    let take: int => (t 'k 'v) => (t 'k 'v);
    let takeWhile: ('k => 'v => bool) => (t 'k 'v) => (t 'k 'v);
  };
};

let module KeyedIterable = {
  include KeyedIterable;

  include (KeyedIterable.MakeGeneric {
    type k 'k = 'k;
    type v 'v = 'v;

    include KeyedIterable;
  }: S2 with type t 'k 'v := t 'k 'v);
};

let module KeyedCollection = {
  include KeyedCollection;

  include (KeyedCollection.MakeGeneric {
    type k 'k = 'k;
    type v 'v = 'v;

    include KeyedCollection;
  }: S2 with type t 'k 'v := t 'k 'v);

  let module Persistent = {
    module type S1 = {
      type k;
      type t 'v;

      include S1 with type k := k and type t 'v := t 'v;

      let remove: k => (t 'v) => (t 'v);

      let removeAll: (t 'v) => (t 'v);
    };

    module type S2 = {
      type t 'k 'v;

      include S2 with  type t 'k 'v := t 'k 'v;

      let remove: 'k => (t 'k 'v) => (t 'k 'v);

      let removeAll: (t 'k 'v) => (t 'k 'v);
    };
  };

  let module Transient = {
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
};

let module NavigableKeyedCollection = {
  include NavigableKeyedCollection;

  include (NavigableKeyedCollection.MakeGeneric {
    type k 'k = 'k;
    type v 'v = 'v;

    include NavigableKeyedCollection;
  }: S2 with type t 'k 'v := t 'k 'v);
};

let module Map = {
  include ImmMap;

  include (ImmMap.MakeGeneric {
    type k 'k = 'k;
    type v 'v = 'v;

    include ImmMap;
  }: S2 with type t 'k 'v := t 'k 'v);

  let module Persistent = {
    module type S1 = {
      type k;
      type t 'v;

      include KeyedCollection.Persistent.S1 with type k := k and type t 'v := t 'v;
      include S1 with type k := k and type t 'v := t 'v;

      let alter: k => (option 'v => option 'v) => (t 'v) => (t 'v);
      let merge: (k => (option 'vAcc) => (option 'v) => (option 'vAcc)) => (t 'vAcc) => (t 'v) => (t 'vAcc);
      let put: k => 'v => (t 'v) => (t 'v);
      let putAll: (KeyedIterable.t k 'v) => (t 'v) => (t 'v);
      let putAllEntries: (Iterable.t (k, 'v)) => (t 'v) => (t 'v);
    };

    module type S2 = {
      type t 'k 'v;

      include KeyedCollection.Persistent.S2 with type t 'k 'v := t 'k 'v;
      include S2 with type t 'k 'v := t 'k 'v;

      let alter: 'k => (option 'v => option 'v) => (t 'k 'v) => (t 'k 'v);
      let merge: ('k => (option 'vAcc) => (option 'v) => (option 'vAcc)) => (t 'k 'vAcc) => (t 'k 'v) => (t 'k 'vAcc);
      let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);
      let putAll: (KeyedIterable.t 'k 'v) => (t 'k 'v) => (t 'k 'v);
      let putAllEntries: (Iterable.t ('k, 'v)) => (t 'k 'v) => (t 'k 'v);
    };
  };

  let module Transient = {
    module type S1 = {
      type k;

      type t 'v;

      include KeyedCollection.Transient.S1 with type k := k and type t 'v := t 'v;

      let alter: k => (option 'v => option 'v) => (t 'v) => (t 'v);
      let get: k => (t 'v) => (option 'v);
      let getOrDefault: default::'v => k => (t 'v) => 'v;
      let getOrRaise: k => (t 'v) => 'v;
      let put: k => 'v => (t 'v) => (t 'v);
      let putAll: (KeyedIterable.t k 'v) => (t 'v) => (t 'v);
      let putAllEntries: (Iterable.t (k, 'v)) => (t 'v) => (t 'v);
    };

    module type S2 = {
      type t 'k 'v;

      include KeyedCollection.Transient.S2 with type t 'k 'v := t 'k 'v;

      let alter: 'k => (option 'v => option 'v) => (t 'k 'v) => (t 'k 'v);
      let get: 'k => (t 'k 'v) => (option 'v);
      let getOrDefault: default::'v => 'k => (t 'k 'v) => 'v;
      let getOrRaise: 'k => (t 'k 'v) => 'v;
      let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);
      let putAll: (KeyedIterable.t 'k 'v) => (t 'k 'v) => (t 'k 'v);
      let putAllEntries: (Iterable.t ('k, 'v)) => (t 'k 'v) => (t 'k 'v);
    };
  };
};

let module NavigableMap = {
  include NavigableMap;

  include (NavigableMap.MakeGeneric {
    type k 'k = 'k;
    type v 'v = 'v;

    include NavigableMap;
  }: S2 with type t 'k 'v := t 'k 'v);

  let module Persistent = {
    module type S1 = {
      type k;
      type t 'v;

      include S1 with type k := k and type t 'v := t 'v;
      include Map.Persistent.S1 with type k := k and type t 'v := t 'v;

      let removeFirstOrRaise: (t 'v) => (t 'v);
      let removeLastOrRaise: (t 'v) => (t 'v);
    };
  };
};

let module Indexed = {
  include Indexed;

  include (Indexed.MakeGeneric {
    type elt 'a = 'a;

    include Indexed;
  }: Indexed.S1 with type t 'a := t 'a);

  let module Persistent = {
    module type S1 = {
      type t 'a;

      include S1 with type t 'a := t 'a;
      include NavigableCollection.Persistent.S1 with type t 'a := t 'a;

      let concat: (list (t 'a)) => (t 'a);
      let insertAt: int => 'a => (t 'a) => (t 'a);
      let removeAt: int => (t 'a) => (t 'a);
      let skip: int => (t 'a) => (t 'a);
      let slice: start::int? => end_::int? => (t 'a) => (t 'a);
      let take: int => (t 'a) => (t 'a);
      let update: int => 'a => (t 'a) => (t 'a);
      let updateAll: (int => 'a => 'a) => (t 'a) => (t 'a);
      let updateWith: int => ('a => 'a) => (t 'a) => (t 'a);
    };
  };

  let module Transient = {
    module type S1 = {
      type t 'a;

      include NavigableCollection.Transient.S1 with type t 'a := t 'a;

      let get: int => (t 'a) => (option 'a);
      let getOrDefault: default::'a => int => (t 'a) => 'a;
      let getOrRaise: int => (t 'a) => 'a;
      let insertAt: int => 'a => (t 'a) => (t 'a);
      let removeAt: int => (t 'a) => (t 'a);
      let update: int => 'a => (t 'a) => (t 'a);
      let updateAll: (int => 'a => 'a) => (t 'a) => (t 'a);
      let updateWith: int => ('a => 'a) => (t 'a) => (t 'a);
    };
  };
};

let module Deque = {
  include Deque;

  include (NavigableCollection.MakeGeneric {
    type elt 'a = 'a;

    include Deque;
  }: NavigableCollection.S1 with type t 'a := t 'a)
};

let module HashMap = {
  include HashMap;

  include (ImmMap.MakeGeneric {
    type k 'k = 'k;
    type v 'v = 'v;

    include HashMap;
  }: ImmMap.S2 with type t 'k 'v := t 'k 'v);

  let merge
      (f: 'k => (option 'vAcc) => (option 'v) => (option 'vAcc))
      (initialValue: t 'k 'vAcc)
      (next: t 'k 'v): (t 'k 'vAcc) => Set.union (keysSet next) (keysSet initialValue)
    |> Iterable.reduce while_::Functions.alwaysTrue2 (
        fun acc key => {
          let result = f key (initialValue |> get key) (next |> get key);
          switch result {
            | None => acc |> Transient.remove key
            | Some value => acc |> Transient.put key value
          }
        }
      ) (mutate initialValue)
    |> Transient.persist;
};

let module HashSet = {
  include HashSet;

  include (ImmSet.MakeGeneric {
    type elt 'a = 'a;

    include HashSet;
  }: ImmSet.S1 with type t 'a := t 'a);

  let addAll (iter: Iterable.t 'a) (set: t 'a): (t 'a) =>
    set |> mutate |> Transient.addAll iter |> Transient.persist;

  let fromWith
      hash::(hash: Hash.t 'a)
      comparator::(comparator: Comparator.t 'a)
      (iterable: Iterable.t 'a): (t 'a) =>
    emptyWith hash::hash comparator::comparator |> addAll iterable;

  let intersect (this: t 'a) (that: t 'a): (t 'a) =>
    /* FIXME: Makes this more efficient */
    this |> removeAll |> addAll (Set.intersect (toSet this) (toSet that));

  let subtract (this: t 'a) (that: t 'a): (t 'a) =>
    /* FIXME: Makes this more efficient */
    this |> removeAll |> addAll (Set.subtract (toSet this) (toSet that));

  let union (this: t 'a) (that: t 'a): (t 'a) =>
    /* FIXME: Makes this more efficient */
    this |> removeAll |> addAll (Set.union (toSet this) (toSet that));
};

let module IntMap = {
  include IntMap;

  include (ImmMap.MakeGeneric {
    type k 'k = int;
    type v 'v = 'v;
    type t 'k 'v = IntMap.t 'v;

    let containsKey = IntMap.containsKey;
    let count = IntMap.count;
    let get = IntMap.get;
    let getOrDefault = IntMap.getOrDefault;
    let getOrRaise = IntMap.getOrRaise;
    let reduce = IntMap.reduce;
    let reduceKeys = IntMap.reduceKeys;
    let reduceValues = IntMap.reduceValues;
    let toSequence = IntMap.toSequence;
  }: ImmMap.S1 with type t 'v := t 'v and type k := k);

  let merge
      (f: k => (option 'vAcc) => (option 'v) => (option 'vAcc))
      (initialValue: t 'vAcc)
      (next: t 'v): (t 'vAcc) => Set.union (keysSet next) (keysSet initialValue)
    |> Iterable.reduce while_::Functions.alwaysTrue2 (
        fun acc key => {
          let result = f key (initialValue |> get key) (next |> get key);
          switch result {
            | None => acc |> Transient.remove key
            | Some value => acc |> Transient.put key value
          }
        }
      ) (mutate initialValue)
    |> Transient.persist;
};

let module IntRange = {
  include IntRange;

  include (NavigableSet.MakeGeneric {
    type elt 'a = int;
    type t 'a = IntRange.t;

    let contains = IntRange.contains;
    let count = IntRange.count;
    let firstOrRaise = IntRange.firstOrRaise;
    let lastOrRaise = IntRange.lastOrRaise;
    let reduce = IntRange.reduce;
    let reduceReversed = IntRange.reduceReversed;
    let toSequence = IntRange.toSequence;
    let toSequenceReversed = IntRange.toSequenceReversed;
  }: NavigableSet.S with type t := IntRange.t and type a := int);
};

let module IntSet = {
  include IntSet;

  include (ImmSet.MakeGeneric {
    type elt 'a = int;
    type nonrec t 'a = IntSet.t;

    let contains = IntSet.contains;
    let count = IntSet.count;
    let reduce = IntSet.reduce;
    let toSequence = IntSet.toSequence;
  }: ImmSet.S with type t := IntSet.t and type a := int);

  let addAll (iter: Iterable.t int) (set: t): t =>
    set |> mutate |> Transient.addAll iter |> Transient.persist;

  let from (iter: Iterable.t int): t =>
    emptyInstance |> addAll iter;

  let intersect (this: t) (that: t): t =>
    /* FIXME: Improve this implementation */
    Set.intersect (toSet this) (toSet that) |> from;

  let subtract (this: t) (that: t): t =>
    /* FIXME: Improve this implementation */
    Set.subtract (toSet this) (toSet that) |> from;

  let union (this: t) (that: t): t =>
    /* FIXME: Improve this implementation */
    Set.union (toSet this) (toSet that) |> from;
};

let module List = {
  include ImmList;

  include (Iterable.MakeGeneric {
    type t 'a = ImmList.t 'a;
    type elt 'a  = 'a;

    let isEmpty = ImmList.isEmpty;
    let reduce = ImmList.reduceImpl;
  }: Iterable.S1 with type t 'a := ImmList.t 'a);

  let addFirstAll = Iterable.listAddFirstAll;
  let fromReverse = Iterable.listFromReverse;
  let toIterable = Iterable.ofList;
  let toSequence = Sequence.ofList;
};

let module ReadOnlyArray = {
  include CopyOnWriteArray;

  include (Indexed.MakeGeneric {
    type elt 'a = 'a;

    include CopyOnWriteArray;
  }: Indexed.S1 with type t 'a := t 'a);
};

let module SortedMap = {
  module type S1 = {
    type k;

    type t +'v;
    /** The SortedMap type. */

    include NavigableMap.Persistent.S1 with type k := k and type t 'v := t 'v;

    let empty: unit => t 'v;
    let from: KeyedIterable.t k 'v => t 'v;
    let fromEntries: Iterable.t (k, 'v) => t 'v;
  };

  let module Make1 = fun (Comparable: Comparable.S) => {
    let module SortedMapImpl = (SortedMap.Make1 Comparable);

    include SortedMapImpl;

    include (NavigableMap.MakeGeneric {
      type k 'k = SortedMapImpl.k;
      type v 'v = 'v;
      type t 'k 'v = SortedMapImpl.t 'v;

      let containsKey = SortedMapImpl.containsKey;
      let count = SortedMapImpl.count;
      let firstOrRaise = SortedMapImpl.firstOrRaise;
      let get = SortedMapImpl.get;
      let getOrDefault = SortedMapImpl.getOrDefault;
      let getOrRaise = SortedMapImpl.getOrRaise;
      let lastOrRaise = SortedMapImpl.lastOrRaise;
      let reduce = SortedMapImpl.reduce;
      let reduceReversed = SortedMapImpl.reduceReversed;
      let reduceKeys = SortedMapImpl.reduceKeys;
      let reduceKeysReversed = SortedMapImpl.reduceKeysReversed;
      let reduceValues = SortedMapImpl.reduceValues;
      let reduceValuesReversed = SortedMapImpl.reduceValuesReversed;
      let toSequence = SortedMapImpl.toSequence;
      let toSequenceReversed = SortedMapImpl.toSequenceReversed;
    }: NavigableMap.S1 with type t 'v := t 'v and type k := k);

    let putAll (iter: KeyedIterable.t k 'v) (map: t 'v): (t 'v) =>
      iter |> KeyedIterable.reduce (fun acc k v => acc |> put k v) map;

    let putAllEntries (iter: Iterable.t (k, 'v)) (map: t 'v): (t 'v) =>
      iter |> Iterable.reduce (fun acc (k, v) => acc |> put k v) map;

    let from (iter: KeyedIterable.t k 'v): (t 'v) =>
      empty () |> putAll iter;

    let fromEntries (iter: Iterable.t (k, 'v)): (t 'v) =>
      empty () |> putAllEntries iter;

    let merge
        (f: k => (option 'vAcc) => (option 'v) => (option 'vAcc))
        (acc: t 'vAcc)
        (next: t 'v): (t 'vAcc) =>  Set.union (keysSet next) (keysSet acc)
      |> Iterable.reduce while_::Functions.alwaysTrue2 (
          fun acc key => {
            let result = f key (acc |> get key) (next |> get key);
            switch result {
              | None => acc |> remove key
              | Some value => acc |> put key value
            }
          }
        ) acc;
  };
};

let module SortedSet = {
  module type S = {
    type a;
    type t;

    include Comparable.S with type t := t;
    include NavigableSet.Persistent.S with type a := a and type t := t;

    let empty: unit => t;
    let from: Iterable.t a => t;
  };

  let module Make = fun (Comparable: Comparable.S) => {
    let module SortedSetImpl = (SortedSet.Make Comparable);

    include SortedSetImpl;

    include (NavigableSet.MakeGeneric {
      type elt 'a = SortedSetImpl.a;
      type t 'a = SortedSetImpl.t;

      let contains = SortedSetImpl.contains;
      let count = SortedSetImpl.count;
      let firstOrRaise = SortedSetImpl.firstOrRaise;
      let lastOrRaise = SortedSetImpl.lastOrRaise;
      let reduce = SortedSetImpl.reduce;
      let reduceReversed = SortedSetImpl.reduceReversed;
      let toSequence = SortedSetImpl.toSequence;
      let toSequenceReversed = SortedSetImpl.toSequenceReversed;
    }: NavigableSet.S with type t:= SortedSetImpl.t and type a:= Comparable.t);

    let addAll (iter: Iterable.t a) (sortedSet: t): t => iter
      |> Iterable.reduce while_::Functions.alwaysTrue2 (fun acc next => acc |> add next) sortedSet;

    let from (iter: Iterable.t a): t =>
      emptyInstance |> addAll iter;

    let intersect (this: t) (that: t): t =>
      /* FIXME: Improve this implementation */
      Set.intersect (toSet this) (toSet that) |> from;

    let subtract (this: t) (that: t): t =>
      /* FIXME: Improve this implementation */
      Set.subtract (toSet this) (toSet that) |> from;

    let union (this: t) (that: t): t =>
      /* FIXME: Improve this implementation */
      Set.union (toSet this) (toSet that) |> from;
  };
};

let module Stack = {
  include ImmStack;

  include (SequentialCollection.MakeGeneric {
    type elt 'a = 'a;

    include ImmStack;
  }: SequentialCollection.S1 with type t 'a := ImmStack.t 'a);
};

let module Vector = {
  include Vector;

  include (Indexed.MakeGeneric {
    type elt 'a = 'a;

    include Vector;
  }: Indexed.S1 with type t 'a := t 'a);
};
