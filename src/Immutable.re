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

let module Streamable = {
  module type S1 = {
    type t 'a;

    let buffer: count::int => skip::int => (t 'a) => (t (list 'a));
    let concat: (list (t 'a)) => (t 'a);
    let defer: (unit => t 'a) => (t 'a);
    let distinctUntilChangedWith: (Equality.t 'a) => (t 'a) => (t 'a);
    let doOnNext: ('a => unit) => (t 'a) => (t 'a);
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

let module Iterable = Iterable;

let module Sequence = Sequence;

let module Collection = {
  include Collection;

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

  let module Persistent = {
    module type S1 = {
      type t 'a;

      include Collection.Persistent.S1 with type t 'a := t 'a;
      include S1 with type t 'a := t 'a;

      let addFirst: 'a => (t 'a) => (t 'a);
      let addFirstAll: (Iterable.t 'a) => (t 'a) => (t 'a);
      let return: 'a => (t 'a);
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

    let concat: (list (t 'k 'v)) => (t 'k 'v);
    let defer: (unit => t 'k 'v) => (t 'k 'v);
    let distinctUntilChangedWith: keyEquals::(Equality.t 'k) => valueEquals::(Equality.t 'v) => (t 'k 'v) => (t 'k 'v);
    let doOnNext: ('k => 'v => unit) => (t 'k 'v) => (t 'k 'v);
    let filter: ('k => 'v => bool) => (t 'k 'v) => (t 'k 'v);
    let flatMap: ('kA => 'vA => t 'kB 'vB) => (t 'kA 'vA) => (t 'kB 'vB);
    let generate: genKey::('k => 'v => 'k) => genValue::('k => 'v => 'v) => 'k => 'v => (t 'k 'v);
    let map: keyMapper::('kA => 'vA => 'kB) => valueMapper::('kA => 'vA => 'vB) => (t 'kA 'vA) => (t 'kB 'vB);
    let mapKeys: ('a => 'v => 'b) => (t 'a 'v) => (t 'b 'v);
    let mapValues: ('k => 'a => 'b) => (t 'k 'a) => (t 'k 'b);
    let return: 'k => 'v => (t 'k 'v);
    let skip: int => (t 'k 'v) => (t 'k 'v);
    let skipWhile: ('k => 'v => bool) => (t 'k 'v) => (t 'k 'v);
    let startWith: 'k => 'v => (t 'k 'v) => (t 'k 'v);
    let take: int => (t 'k 'v) => (t 'k 'v);
    let takeWhile: ('k => 'v => bool) => (t 'k 'v) => (t 'k 'v);
  };
};

let module KeyedIterable = KeyedIterable;

let module KeyedCollection = {
  include KeyedCollection;

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

let module NavigableKeyedCollection = NavigableKeyedCollection;

let module Map = {
  include ImmMap;

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
      let getOrRaise: 'k => (t 'k 'v) => 'v;
      let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);
      let putAll: (KeyedIterable.t 'k 'v) => (t 'k 'v) => (t 'k 'v);
      let putAllEntries: (Iterable.t ('k, 'v)) => (t 'k 'v) => (t 'k 'v);
    };
  };
};

let module NavigableMap = {
  include NavigableMap;

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
      let getOrRaise: int => (t 'a) => 'a;
      let insertAt: int => 'a => (t 'a) => (t 'a);
      let removeAt: int => (t 'a) => (t 'a);
      let update: int => 'a => (t 'a) => (t 'a);
      let updateAll: (int => 'a => 'a) => (t 'a) => (t 'a);
      let updateWith: int => ('a => 'a) => (t 'a) => (t 'a);
    };
  };
};

let module Deque = Deque;

let module HashMap = HashMap;

let module HashSet = HashSet;

let module IntMap = IntMap;

let module IntRange = IntRange;

let module IntSet = IntSet;

let module List = {
  include ImmList;

  let addFirstAll = Iterable.listAddFirstAll;
  let fromReverse = Iterable.listFromReverse;
  let toIterable = Iterable.ofList;
  let toSequence = Sequence.ofList;
};

let module ReadOnlyArray = CopyOnWriteArray;

let module SortedMap = SortedMap;

let module SortedSet = SortedSet;

let module Stack = ImmStack;

let module Vector = Vector;
