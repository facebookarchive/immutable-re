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

module type Equatable = Equatable.S;

module type Equatable_1 = Equatable.S1;

module type Comparable = Comparable.S;

module type Hashable = {
  type t;

  let hash: Hash.t t;
};

module type Hashable_1 = {
  type t 'a;

  let hash: Hash.t (t 'a);
};

module type FlatMappable_1 = {
  type t 'a;

  let flatMap: ('a => t 'b) => (t 'a) => (t 'b);
  let flatten: (t (t 'a)) => (t 'a);
};

module type Mappable_1 = {
  type t 'a;

  let map: ('a => 'b) => (t 'a) => (t 'b);
};

module type Reduceable = {
  type a;
  type t;

  let reduce: while_::('acc => a => bool)? => ('acc => a => 'acc) => 'acc => t => 'acc;
};

module type Reduceable_1 = {
  type t 'a;

  let reduce: while_::('acc => 'a => bool)? => ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
};

module type ReduceableRight = {
  type a;
  type t;

  include Reduceable with type a := a and type t := t;

  let reduceRight: while_::('acc => a => bool)? => ('acc => a => 'acc) => 'acc => t => 'acc;
};

module type ReduceableRight_1 = {
  type t 'a;

  include Reduceable_1 with type t 'a := t 'a;

  let reduceRight: while_::('acc => 'a => bool)? => ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
};

module type ReverseMappable_1 = {
  type t 'a;

  let mapReverse: ('a => 'b) => (t 'a) => (t 'b);
};

module type Streamable_1 = {
  type t 'a;

  include FlatMappable_1 with type t 'a := t 'a;
  include Mappable_1 with type t 'a := t 'a;

  let buffer: count::int => skip::int => (t 'a) => (t (list 'a));
  let concat: (list (t 'a)) => (t 'a);
  let defer: (unit => t 'a) => (t 'a);
  let distinctUntilChangedWith: (Equality.t 'a) => (t 'a) => (t 'a);
  let doOnNext: ('a => unit) => (t 'a) => (t 'a);
  let empty: unit => (t 'a);
  let filter: ('a => bool) => (t 'a) => (t 'a);
  let generate: ('a => 'a) => 'a => (t 'a);
  let return: 'a => (t 'a);
  let scan: ('acc => 'a => 'acc) => 'acc => (t 'a) => (t 'acc);
  let skip: int => (t 'a) => (t 'a);
  let skipWhile: ('a => bool) => (t 'a) => (t 'a);
  let startWith: 'a => (t 'a) => (t 'a);
  let take: int => (t 'a) => (t 'a);
  let takeWhile: ('a => bool) => (t 'a) => (t 'a);
};

let module Reducer = Reducer;

let module Iterator = Iterator;

module type Iterable = {
  type a;
  type t;

  include Reduceable with type a := a and type t := t;

  let toIterator: t => (Iterator.t a);
};

module type Iterable_1 = {
  type t 'a;

  include Reduceable_1 with type t 'a := t 'a;

  let toIterator: t 'a => (Iterator.t 'a);
};

let module Sequence = Sequence;

module type Collection = {
  type a;
  type t;

  include Iterable with type a := a and type t := t;

  let count: t => int;
  let empty: t;
  let isEmpty: t => bool;
  let isNotEmpty: t => bool;
  let toSequence: t => (Sequence.t a);
};

module type Collection_1 = {
  type t 'a;

  include Iterable_1 with type t 'a := t 'a;

  let count: (t 'a) => int;
  let isEmpty: (t 'a) => bool;
  let isNotEmpty: (t 'a) => bool;
  let toSequence: (t 'a) => (Sequence.t 'a);
};

module type PersistentCollection = {
  type a;
  type t;

  include Collection with type a := a and type t := t;

  let removeAll: t => t;
};

module type PersistentCollection_1 = {
  type t 'a;

  include Collection_1 with type t 'a := t 'a;

  let removeAll: t 'a => t 'a;
};

module type TransientCollection = {
  type a;
  type t;

  let count: t => int;
  let empty: unit => t;
  let isEmpty: t => bool;
  let isNotEmpty: t => bool;
  let removeAll: t => t;
};

module type TransientCollection_1 = {
  type t 'a;

  let count: (t 'a) => int;
  let isEmpty: (t 'a) => bool;
  let isNotEmpty: (t 'a) => bool;
  let removeAll: (t 'a) => (t 'a);
};

module type SequentialCollection = {
  type a;
  type t;

  include Collection with type a := a and type t := t;

  let first: t => (option a);
  let firstOrRaise: t => a;
};

module type SequentialCollection_1 = {
  type t 'a;

  include Collection_1 with type t 'a := t 'a;

  let first: (t 'a) => (option 'a);
  let firstOrRaise: (t 'a) => 'a;
};

module type PersistentSequentialCollection_1 = {
  type t 'a;

  include PersistentCollection_1 with type t 'a := t 'a;
  include ReverseMappable_1 with type t 'a := t 'a;
  include SequentialCollection_1 with type t 'a := t 'a;

  let addFirst: 'a => (t 'a) => (t 'a);
  /** [addFirst value stack] returns a new Stack with [value] prepended.
   *
   *  Complexity: O(_1)
   */

  let addFirstAll: (Iterator.t 'a) => (t 'a) => (t 'a);
  /** [addFirstAll iter stack] returns a new Stack with the values in [iter] prepended. */

  let empty: unit => (t 'a);
  /** The empty Vector. */

  let fromReverse: (Iterator.t 'a) => (t 'a);
  /** [fromReverse iter] returns a new Stack containing the values in [iter]
   *  in reverse order.
   */

  let return: 'a => (t 'a);
  /** [return value] returns a new Stack containing a single element, [value]. */

  let removeFirstOrRaise: (t 'a) => (t 'a);
  /** [removeFirstOrRaise stack] returns a new Stack without the first element.
   *
   *  Complexity: O(_1)
   */
};

module type TransientSequentialCollection_1 = {
  type t 'a;

  include TransientCollection_1 with type t 'a := t 'a;

  let addFirst: 'a => (t 'a) => (t 'a);
  /** [addFirst value transient] prepends [value] to [transient].
   *
   *  Complexity: O(_1)
   */

  let empty: unit => (t 'a);
  /** [empty ()] returns a new empty TransientDeque. */

  let first: (t 'a) => option 'a;
  /** [tryFirst transient] returns first element in [transient] or None. */

  let firstOrRaise: (t 'a) => 'a;
  /** [first transient] returns the first element in [transient] or throws. */

  let removeFirstOrRaise: (t 'a) => (t 'a);
  /** [removeFirstOrRaise transient] removes the first element from [transient].
   *
   *  Complexity: O(_1)
   */
};

module type NavigableCollection = {
  type a;
  type t;

  include ReduceableRight with type a := a and type t := t;
  include SequentialCollection with type a := a and type t := t;

  let last: t => (option a);
  let lastOrRaise: t => a;
  let toIteratorRight: t => (Iterator.t a);
  let toSequenceRight: t => (Sequence.t a);
};

module type NavigableCollection_1 = {
  type t 'a;

  include ReduceableRight_1 with type t 'a := t 'a;
  include SequentialCollection_1 with type t 'a := t 'a;

  let last: (t 'a) => (option 'a);
  let lastOrRaise: (t 'a) => 'a;
  let toIteratorRight: (t 'a) => (Iterator.t 'a);
  let toSequenceRight: (t 'a) => (Sequence.t 'a);
};

module type PersistentNavigableCollection_1 = {
  type t 'a;

  include Mappable_1 with type t 'a := t 'a;
  include NavigableCollection_1 with type t 'a := t 'a;
  include PersistentSequentialCollection_1 with type t 'a := t 'a;

  let addLast: 'a => (t 'a) => (t 'a);
  /** [addLast value deque] returns a new Deque with [value] appended.
   *
   *  Complexity: O(_1)
   */

  let addLastAll: (Iterator.t 'a) => (t 'a) => (t 'a);
  /** [addLastAll iter deque] returns a new Deque with the values in [iter] appended. */

  let from: (Iterator.t 'a) => (t 'a);
  /** [from iter] returns a new Deque containing the values in [iter].
   *
   * Complexity: O(N) the number of elements in [iter].
   */

  let removeLastOrRaise: (t 'a) => (t 'a);
  /** [removeLastOrRaise deque] returns a new Deque without the last element.
   *
   *  Complexity: O(_1)
   */
};

module type TransientNavigableCollection_1 = {
  type t 'a;

  include TransientSequentialCollection_1 with type t 'a := t 'a;

  let addLast: 'a => (t 'a) => (t 'a);
  /** [addLast value transient] appends [value] to [transient].
   *
   *  Complexity: O(_1)
   */

  let last: (t 'a) => option 'a;
  /** [tryLast transient] returns the last element in [transient] or None. */

  let lastOrRaise: (t 'a) => 'a;
  /** [last transient] returns the last element in [transient] or throws. */

  let removeLastOrRaise: (t 'a) => (t 'a);
  /** [removeLastOrRaise transient] removes the last element from [transient].
   *
   *  Complexity: O(_1)
   */
};

let module Set = {
  include ImmSet;

  module type S = {
    type a;
    type t;

    include Collection with type a := a and type t := t;
    include Equatable with type t := t;

    let contains: a => t => bool;

    let toSet: t => ImmSet.t a;
  };

  module type S1 = {
    type t 'a;

    include Collection_1 with type t 'a := t 'a;
    include Equatable_1 with type t 'a := t 'a;

    let contains: 'a => (t 'a) => bool;
    let toSet: (t 'a) => ImmSet.t 'a;
  };
};

module type PersistentSet = {
  type a;
  type t;

  include Set.S with type a := a and type t := t;
  include PersistentCollection with type a := a and type t := t;

  let add: a => t => t;
  let addAll: (Iterator.t a) => t => t;
  let from: (Iterator.t a) => t;
  let intersect: t => t => t;
  let remove: a => t => t;
  let subtract: t => t => t;
  let union: t => t => t;
};

module type PersistentSet_1 = {
  type t 'a;

  include Set.S1 with type t 'a := t 'a;
  include PersistentCollection_1 with type t 'a := t 'a;

  let add: 'a => (t 'a) => (t 'a);
  let addAll: (Iterator.t 'a) => (t 'a) => (t 'a);
  let intersect: (t 'a) => (t 'a) => (t 'a);
  let remove: 'a => (t 'a) => (t 'a);
  let subtract: (t 'a) => (t 'a) => (t 'a);
  let union: (t 'a) => (t 'a) => (t 'a);
};

module type TransientSet = {
  type a;
  type t;

  include TransientCollection with type a := a and type t := t;

  let add: a => t => t;
  let addAll: (Iterator.t a) => t => t;
  let contains: a => t => bool;
  let remove: a => t => t;
};

module type TransientSet_1 = {
  type t 'a;

  include TransientCollection_1 with type t 'a := t 'a;

  let add: 'a => (t 'a) => (t 'a);
  let addAll: (Iterator.t 'a) => (t 'a) => (t 'a);
  let contains: 'a => (t 'a) => bool;
  let remove: 'a => (t 'a) => (t 'a);
};

module type NavigableSet = {
  type a;
  type t;

  include Set.S with type a := a and type t := t;
  include NavigableCollection with type a := a and type t := t;
};

module type NavigableSet_1 = {
  type t 'a;

  include Set.S1 with type t 'a := t 'a;
  include NavigableCollection_1 with type t 'a := t 'a;
};

module type PersistentNavigableSet = {
  type a;
  type t;

  include NavigableSet with type a := a and type t := t;
  include PersistentSet with type a := a and type t := t;

  let removeFirstOrRaise: t => t;

  let removeLastOrRaise: t => t;
};

module type PersistentNavigableSet_1 = {
  type t 'a;

  include NavigableSet_1 with type t 'a := t 'a;
  include PersistentSet_1 with type t 'a := t 'a;

  let removeFirstOrRaise: t 'a => t 'a;

  let removeLastOrRaise: t 'a => t 'a;
};

module type KeyedReduceable_1 = {
  type k;
  type t 'v;

  let reduce: while_::('acc => k => 'v => bool)? => ('acc => k => 'v => 'acc) => 'acc => (t 'v) => 'acc;
};

module type KeyedReduceable_2 = {
  type t 'k 'v;

  let reduce: while_::('acc => 'k => 'v => bool)? => ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
};

module type KeyedReduceableRight_1 = {
  type k;
  type t 'v;

  include KeyedReduceable_1 with type k := k and type t 'v := t 'v;

  let reduceRight: while_::('acc => k => 'v => bool)? => ('acc => k => 'v => 'acc) => 'acc => (t 'v) => 'acc;
};

module type KeyedReduceableRight_2 = {
  type t 'k 'v;

  include KeyedReduceable_2 with type t 'k 'v := t 'k 'v;

  let reduceRight: while_::('acc => 'k => 'v => bool)? => ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
};

let module KeyedReducer = KeyedReducer;

let module KeyedIterator = KeyedIterator;

module type KeyedIterable_1 = {
  type k;
  type t 'v;

  include KeyedReduceable_1 with type k := k and type t 'v := t 'v;

  let toIterator: t 'v => Iterator.t (k, 'v);

  let toKeyedIterator: t 'v => KeyedIterator.t k 'v;
};

module type KeyedIterable_2 = {
  type t 'k 'v;

  include KeyedReduceable_2 with type t 'k 'v := t 'k 'v;

  let toIterator: t 'k 'v => Iterator.t ('k, 'v);
  let toKeyedIterator: t 'k 'v => KeyedIterator.t 'k 'v;
};

module type KeyedCollection_1 = {
  type k;
  type t 'v;

  include KeyedIterable_1 with type k := k and type t 'v := t 'v;

  let containsKey: k => t 'v => bool;
  let count: t 'v => int;
  let isEmpty: (t 'v) => bool;
  let isNotEmpty: (t 'v) => bool;
  let keys: (t 'v) => (Set.t k);
  let toSequence: (t 'v) => (Sequence.t (k, 'v));
};

module type KeyedCollection_2 = {
  type t 'k 'v;

  include KeyedIterable_2 with type t 'k 'v := t 'k 'v;

  let containsKey: 'k => t 'k 'v => bool;
  let count: t 'k 'v => int;
  let isEmpty: (t 'k 'v) => bool;
  let isNotEmpty: (t 'k 'v) => bool;
  let keys: (t 'k 'v) => (Set.t 'k);
  let toSequence: (t 'k 'v) => (Sequence.t ('k, 'v));
};

module type PersistentKeyedCollection_1 = {
  type k;
  type t 'v;

  include KeyedCollection_1 with type k := k and type t 'v := t 'v;

  let remove: k => (t 'v) => (t 'v);
  let removeAll: (t 'v) => (t 'v);
};

module type PersistentKeyedCollection_2 = {
  type t 'k 'v;

  include KeyedCollection_2 with  type t 'k 'v := t 'k 'v;

  let remove: 'k => (t 'k 'v) => (t 'k 'v);
  let removeAll: (t 'k 'v) => (t 'k 'v);
};

module type TransientKeyedCollection_1 = {
  type k;
  type t 'v;

  let containsKey: k => (t 'v) => bool;
  let count: (t 'v) => int;
  let isEmpty: (t 'v) => bool;
  let isNotEmpty: (t 'v) => bool;
  let remove: k => (t 'v) => (t 'v);
  let removeAll: (t 'v) => (t 'v);
};

module type TransientKeyedCollection_2 = {
  type t 'k 'v;

  let containsKey: 'k => (t 'k 'v) => bool;
  let count: (t 'k 'v) => int;
  let isEmpty: (t 'k 'v) => bool;
  let isNotEmpty: (t 'k 'v) => bool;
  let remove: 'k => (t 'k 'v) => (t 'k 'v);
  let removeAll: (t 'k 'v) => (t 'k 'v);
};

module type NavigableKeyedCollection_1 = {
  type k;
  type t 'v;

  include KeyedCollection_1 with type k := k and type t 'v := t 'v;
  include KeyedReduceableRight_1 with type k := k and type t 'v := t 'v;

  let first: (t 'v) => (option (k, 'v));
  let firstOrRaise: (t 'v) => (k, 'v);
  let last: (t 'v) => (option (k, 'v));
  let lastOrRaise: (t 'v) => (k, 'v);
  let toIteratorRight: t 'v => Iterator.t (k, 'v);
  let toKeyedIteratorRight: t 'v => KeyedIterator.t k 'v;
  let toSequenceRight: (t 'v) => (Sequence.t (k, 'v));
};

module type NavigableKeyedCollection_2 = {
  type t 'k 'v;

  include KeyedCollection_2 with type t 'k 'v := t  'k 'v;
  include KeyedReduceableRight_2 with type t 'k 'v := t 'k 'v;

  let first: (t 'k 'v) => (option ('k, 'v));
  let firstOrRaise: (t 'k 'v) => ('k, 'v);
  let last: (t 'k 'v) => (option ('k, 'v));
  let lastOrRaise: (t 'k 'v) => ('k, 'v);
  let toIteratorRight: t 'k 'v => Iterator.t ('k, 'v);
  let toKeyedIteratorRight: t 'k 'v => KeyedIterator.t 'k 'v;
  let toSequenceRight: (t 'k 'v) => (Sequence.t ('k, 'v));
};

let module Map = {
  include ImmMap;

  module type S1 = {
    type k;
    type t 'v;

    include KeyedCollection_1 with type k := k and type t 'v := t 'v;

    let get: k => (t 'v) => (option 'v);
    let getOrRaise: k => (t 'v) => 'v;
    let values: (t 'v) => (Iterator.t 'v);
    let toMap: (t 'v) => ImmMap.t k 'v;
  };

  module type S2 = {
    type t 'k 'v;

    include KeyedCollection_2 with type t 'k 'v := t 'k 'v;

    let get: 'k => (t 'k 'v) => (option 'v);
    let getOrRaise: 'k => (t 'k 'v) => 'v;
    let values: (t 'k 'v) => (Iterator.t 'v);
    let toMap: (t 'k 'v) => ImmMap.t 'k 'v;
  };
};

module type PersistentMap_1 = {
  type k;
  type t 'v;

  include PersistentKeyedCollection_1 with type k := k and type t 'v := t 'v;
  include Map.S1 with type k := k and type t 'v := t 'v;

  let alter: k => (option 'v => option 'v) => (t 'v) => (t 'v);
  let empty: unit => (t 'v);
  let from: (KeyedIterator.t k 'v) => (t 'v);
  let fromEntries: (Iterator.t (k, 'v)) => (t 'v);
  let map: (k => 'a => 'b) => (t 'a) => (t 'b);
  let merge: (k => (option 'vAcc) => (option 'v) => (option 'vAcc)) => (t 'vAcc) => (t 'v) => (t 'vAcc);
  let put: k => 'v => (t 'v) => (t 'v);
  let putAll: (KeyedIterator.t k 'v) => (t 'v) => (t 'v);
  let putAllEntries: (Iterator.t (k, 'v)) => (t 'v) => (t 'v);
};

module type PersistentMap_2 = {
  type t 'k 'v;

  include PersistentKeyedCollection_2 with type t 'k 'v := t 'k 'v;
  include Map.S2 with type t 'k 'v := t 'k 'v;

  let alter: 'k => (option 'v => option 'v) => (t 'k 'v) => (t 'k 'v);
  let map: ('k => 'a => 'b) => (t 'k 'a) => (t 'k 'b);
  let merge: ('k => (option 'vAcc) => (option 'v) => (option 'vAcc)) => (t 'k 'vAcc) => (t 'k 'v) => (t 'k 'vAcc);
  let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);
  let putAll: (KeyedIterator.t 'k 'v) => (t 'k 'v) => (t 'k 'v);
  let putAllEntries: (Iterator.t ('k, 'v)) => (t 'k 'v) => (t 'k 'v);
};

module type TransientMap_1 = {
  type k;

  type t 'v;

  include TransientKeyedCollection_1 with type k := k and type t 'v := t 'v;

  let alter: k => (option 'v => option 'v) => (t 'v) => (t 'v);
  let empty: unit => (t 'v);
  let get: k => (t 'v) => (option 'v);
  let getOrRaise: k => (t 'v) => 'v;
  let put: k => 'v => (t 'v) => (t 'v);
  let putAll: (KeyedIterator.t k 'v) => (t 'v) => (t 'v);
};

module type TransientMap_2 = {
  type t 'k 'v;

  include TransientKeyedCollection_2 with type t 'k 'v := t 'k 'v;

  let alter: 'k => (option 'v => option 'v) => (t 'k 'v) => (t 'k 'v);
  let get: 'k => (t 'k 'v) => (option 'v);
  let getOrRaise: 'k => (t 'k 'v) => 'v;
  let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);
  let putAll: (KeyedIterator.t 'k 'v) => (t 'k 'v) => (t 'k 'v);
};

module type NavigableMap_1 = {
  type k;
  type t 'v;

  include NavigableKeyedCollection_1 with type k := k and type t 'v := t 'v;
  include Map.S1 with type k := k and type t 'v := t 'v;
};

module type PersistentNavigableMap_1 = {
  type k;
  type t 'v;

  include NavigableMap_1 with type k := k and type t 'v := t 'v;
  include PersistentMap_1 with type k := k and type t 'v := t 'v;

  let removeFirstOrRaise: (t 'v) => (t 'v);
  let removeLastOrRaise: (t 'v) => (t 'v);
};

module type IndexedCollection_1 = {
  type t 'a;

  include NavigableCollection_1 with type t 'a := t 'a;

  let get: int => (t 'a) => (option 'a);
  let getOrRaise: int => (t 'a) => 'a;
  let toKeyedIterator: (t 'a) => (KeyedIterator.t int 'a);
  let toKeyedIteratorRight: (t 'a) => (KeyedIterator.t int 'a);
  let toMap: (t 'a) => (Map.t int 'a);
};

module type IndexedMappable_1 = {
  type t 'a;

  include Mappable_1 with type t 'a := t 'a;
  include ReverseMappable_1 with type t 'a := t 'a;

  let mapWithIndex: (int => 'a => 'b) => (t 'a) => (t 'b);
  let mapReverseWithIndex: (int => 'a => 'b) => (t 'a) => (t 'b);
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

let module Option = {
  include Option;

  let toIterator = Iterator.ofOption;
  let toSequence = Sequence.ofOption;

  let module Reducer = Reducer.Make1 {
    type t 'a = Option.t 'a;
    let reduce = reduce;
  };
};

let module ReadOnlyArray = CopyOnWriteArray;

let module SortedMap = SortedMap;

let module SortedSet = SortedSet;

let module Stack = ImmStack;

let module Vector = Vector;

let module TransientVector = Vector.TransientVector;
