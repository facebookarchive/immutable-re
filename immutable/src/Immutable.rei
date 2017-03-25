/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

let module Equality: {
  /** Equality functions for common types. */

  type t 'a = 'a => 'a => bool;
  /** The Equality function type. */

  let int: (t int);

  let reference: (t 'a);
  /** The reference equality function, analogous to === */
};

let module Ordering: {
  /** Represents the absolute ordering of a type when comparing values. */

  type t;

  let equal: t;
  let greaterThan: t;
  let lessThan: t;
};

let module Comparator: {
  /** Comparison functions for common types. */

  type t 'a = 'a => 'a => Ordering.t;
  /** The Comparator function type. */

  let bytes: t bytes;
  /** Compares bytes. */

  let char: t char;
  /** Compares chars. */

  let int: t int;

  let int32: t int32;
  /** Compares int32s. */

  let int64: t int64;
  /** Compares int64s. */

  let nativeInt: t nativeint;
  /** Compares nativeInts. */

  let string: t string;
  /** Compares strings. */

  let toEquality: (t 'a) => (Equality.t 'a);
};

let module Hash: {
  /** Hash functions for common types. */

  type t 'a = 'a => int;
  /** The Hash function type */
};

module type Equatable = {
  type t;

  let equals: Equality.t t;
};

module type Equatable_1 = {
  type t 'a;

  let equals: (Equality.t (t 'a));
};

module type Comparable = {
  type t;
  /** The type that is compared by the Comparable module. */

  include Equatable with type t := t;

  let compare: Comparator.t t;
};

module type Hashable = {
  type t;

  let hash: Hash.t t;
};

module type Hashable_1 = {
  type t 'a;

  let hash: Hash.t (t 'a);
};

module type Concatable_1 = {
  type t 'a;

  let concat: (list (t 'a)) => (t 'a);
  /* [concat concatables] concantenates */
};

module type Mappable_1 = {
  type t 'a;

  let map: ('a => 'b) => (t 'a) => (t 'b);
};

module type FlatMappable_1 = {
  type t 'a;

  let flatMap: ('a => t 'b) => (t 'a) => (t 'b);
  let flatten: (t (t 'a)) => (t 'a);
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

  let reduceRight: while_::('acc => a => bool)? => ('acc => a => 'acc) => 'acc => t => 'acc;
};

module type ReduceableRight_1 = {
  type t 'a;

  let reduceRight: while_::('acc => 'a => bool)? => ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
};

module type ReverseMappable_1 = {
  type t 'a;

  let mapReverse: ('a => 'b) => (t 'a) => (t 'b);
};

module type Skippable_1 = {
  type t 'a;

  let skip: int => (t 'a) => (t 'a);
  /** [skip count seq] returns a Sequence that skips the first [count] elements in [seq]. */
};

module type Takeable_1 = {
  type t 'a;

  let take: int => (t 'a) => (t 'a);
  /** [take count seq] returns a Sequence that includes the first [count] elements in [seq]. */
};

module type Streamable_1 = {
  type t 'a;

  include Concatable_1 with type t 'a := t 'a;
  include FlatMappable_1 with type t 'a := t 'a;
  include Mappable_1 with type t 'a := t 'a;
  include Skippable_1 with type t 'a := t 'a;
  include Takeable_1 with type t 'a := t 'a;

  let buffer: count::int => skip::int => (t 'a) => (t (list 'a));
  /** [buffer count skip seq] returns a Sequence that collects elements from [seq]
   *  into buffer lists of size [count], skipping [skip] number of elements in between
   *  creation of new buffers. The returned buffers are guaranteed to be of size [count],
   *  and elements are dropped if [seq] completes before filling the last buffer.
   */

  let defer: (unit => t 'a) => (t 'a);
  /** [defer f] returns a Streamble that invokes the function [f] whenever the Sequence is enumerated. */

  let distinctUntilChangedWith: (Equality.t 'a) => (t 'a) => (t 'a);
  /** [distinctUntilChangedWith equals seq] returns a Sequence that contains only
   *  distinct contiguous elements from [seq] using [equals] to equate elements.
   */

  let doOnNext: ('a => unit) => (t 'a) => (t 'a);

  let empty: unit => (t 'a);
  /** The empty Streamble. */

  let filter: ('a => bool) => (t 'a) => (t 'a);
  let generate: ('a => 'a) => 'a => (t 'a);
  let repeat: 'a => (t 'a);
  let return: 'a => (t 'a);
  let scan: ('acc => 'a => 'acc) => 'acc => (t 'a) => (t 'acc);
  /** [scan f acc seq] returns a Sequence of accumulated values resulting from the
   *  application of the accumulator function [f] to each element in [seq] with the
   *  specified seed value [acc].
   */
  let skipWhile: ('a => bool) => (t 'a) => (t 'a);
  let startWith: 'a => (t 'a) => (t 'a);
  /** [startWith value seq] returns a seq whose first elements is [value]. */

  let takeWhile: ('a => bool) => (t 'a) => (t 'a);
  /** [takeWhile f seq] returns a Streamble that applies the predicate [f] to each element in [seq],
   *  taking elements until [f] first returns false.
   */
};

module type Zippable_1 = {
  type t 'a;

  let zip: (list (t 'a)) => (t (list 'a));
  /** [zip seqs] merges a list of n Sequences into a Sequence of lists with n values.
   *  Elements are produce until any Sequence in [seq] completes.
   */

  let zip2With: ('a => 'b => 'c) => (t 'a) => (t 'b) => (t 'c);
  /** [zip_2With f first second] merges two Sequences into a Sequence of tuples.
   *  Elements are produce until either first or second complete.
   */

  let zip3With: ('a => 'b => 'c => 'd) => (t 'a) => (t 'b) => (t 'c) => (t 'd);
  /** [zip3With f first second third] merges two Sequences into a Sequence of triples.
   *  Elements are produce until either first, second, or third complete.
   */

  let zipLongest: (list (t 'a)) => (t (list (option 'a)));
  /** [zip seqs] merges a list of n Sequences into a Sequence of lists with n values.
   *  Elements are produce until all Sequences in [seq] complete.
   */

  let zipLongest2With:
    (option 'a => option 'b => 'c) =>
    (t 'a) =>
    (t 'b) =>
    (t 'c);
  /** [zipLongest_2With f first second] merges two Sequences into a Sequence of tuples.
   *  Elements are produce until both first and second complete.
   */

  let zipLongest3With:
    (option 'a => option 'b => option 'c => 'd) =>
    (t 'a) =>
    (t 'b) =>
    (t 'c) =>
    (t 'd);
  /** [zipLongest3With f first second third] merges two Sequence into a Sequence of triples.
   *  Elements are produce until first, second, and third all complete.
   */
};

let module Reducer: {
  module type S = {
    type a;
    type t;

    let every: (a => bool) => t => bool;
    let find: (a => bool) => t => (option a);
    let findOrRaise: (a => bool) => t => a;
    let forEach: while_::(a => bool)? => (a => unit) => t => unit;
    let none: (a => bool) => t => bool;
    let some: (a => bool) => t => bool;
  };

  module type S1 = {
    type t 'a;

    let every: ('a => bool) => (t 'a) => bool;
    let find: ('a => bool) => (t 'a) => (option 'a);
    let findOrRaise: ('a => bool) => (t 'a) => 'a;
    let forEach: while_::('a => bool)? => ('a => unit) => (t 'a) => unit;
    let none: ('a => bool) => (t 'a) => bool;
    let some: ('a => bool) => (t 'a) => bool;
  };

  let module Make: (Reduceable: Reduceable) => S with type a = Reduceable.a and type t = Reduceable.t;
  let module Make1: (Reduceable: Reduceable_1) => S1 with type t 'a = Reduceable.t 'a;
};

let module Iterator: {
  type t 'a;

  include Reduceable_1 with type t 'a := t 'a;
  include Streamable_1 with type t 'a := t 'a;

  let module Reducer: Reducer.S1 with type t 'a := t 'a;
};

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

module type Sequential = {
  type a;
  type t;

  include Iterable with type a := a and type t := t;

  let first: t => (option a);
  /** [tryFirst seq] returns first element in [seq] or None.
   *
   *  Complexity: O(_1)
   */

  let firstOrRaise: t => a;
  /** [first seq] returns the first element in [seq] or throws.
   *
   *  Complexity: O(_1)
   */
};

module type Sequential_1 = {
  type t 'a;

  include Iterable_1 with type t 'a := t 'a;

  let first: (t 'a) => (option 'a);
  /** [tryFirst seq] returns first element in [seq] or None.
   *
   *  Complexity: O(_1)
   */

  let firstOrRaise: (t 'a) => 'a;
  /** [first seq] returns the first element in [seq] or throws.
   *
   *  Complexity: O(_1)
   */
};

let module Sequence: {
  /** Functional sequences. */

  type t 'a;
  /** The Sequence type. */

  include Sequential_1 with type t 'a := t 'a;
  include Streamable_1 with type t 'a := t 'a;
  include Zippable_1 with type t 'a := t 'a;

  let module Reducer: Reducer.S1 with type t 'a := t 'a;
};

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
  /** [count transient] returns the number of elements in [transient]. */

  let empty: unit => t;

  let isEmpty: t => bool;
  /** [isEmpty transient] returns true if [transient] contains no elements. */

  let isNotEmpty: t => bool;
  /** [isNotEmpty transient] returns true if [transient] contains at least one element. */

  let removeAll: t => t;
  /** [removeAll transient] removes all elements from [transient].
   *
   *  Complexity: O(_1)
   */
};

module type TransientCollection_1 = {
  type t 'a;

  let count: (t 'a) => int;
  /** [count transient] returns the number of elements in [transient]. */

  let isEmpty: (t 'a) => bool;
  /** [isEmpty transient] returns true if [transient] contains no elements. */

  let isNotEmpty: (t 'a) => bool;
  /** [isNotEmpty transient] returns true if [transient] contains at least one element. */

  let removeAll: (t 'a) => (t 'a);
  /** [removeAll transient] removes all elements from [transient].
   *
   *  Complexity: O(_1)
   */
};

module type SequentialCollection = {
  type a;
  type t;

  include Collection with type a := a and type t := t;
  include Sequential with type a := a and type t := t;
};

module type SequentialCollection_1 = {
  type t 'a;

  include Collection_1 with type t 'a := t 'a;
  include Sequential_1 with type t 'a := t 'a;
};

module type NavigableCollection = {
  type a;
  type t;

  include SequentialCollection with type a := a and type t := t;
  include ReduceableRight with type a := a and type t := t;

  let last: t => (option a);
  let lastOrRaise: t => a;
  let toIteratorRight: t => (Iterator.t a);
  let toSequenceRight: t => (Sequence.t a);
};

module type NavigableCollection_1 = {
  type t 'a;

  include SequentialCollection_1 with type t 'a := t 'a;
  include ReduceableRight_1 with type t 'a := t 'a;

  let last: (t 'a) => (option 'a);
  let lastOrRaise: (t 'a) => 'a;
  let toIteratorRight: (t 'a) => (Iterator.t 'a);
  let toSequenceRight: (t 'a) => (Sequence.t 'a);
};

module type UniqueCollection = {
  type a;
  type t;

  include Collection with type a := a and type t := t;
  include Equatable with type t := t;

  let contains: a => t => bool;
};

module type UniqueCollection_1 = {
  type t 'a;

  include Collection_1 with type t 'a := t 'a;
  include Equatable_1 with type t 'a := t 'a;

  let contains: 'a => (t 'a) => bool;
};

let module Set: {
  /** A read only view of an underlying set of unique values. The intent of this type is to enable
   *  interop between alternative concrete implementations such as [SortedSet] and [g].
   *  The complexity of functions in this module is dependent upon the underlying concrete implementation.
   */

  type t 'a;
  /** The Set type. */

  include UniqueCollection_1 with type t 'a := t 'a;

  let empty: unit => (t 'a);
  /** The empty Set. */

  let intersect: (t 'a) => (t 'a) => (Iterator.t 'a);
  /** [intersect this that] returns an Iterator of unique elements
   *  which occur in both [this] and [that].
   */

  let subtract: (t 'a) => (t 'a) => (Iterator.t 'a);
  /** [subtract this that] returns an Iterator of unique element
   *  which occur in [this] but not in [that].
   */

  let union: (t 'a) => (t 'a) => (Iterator.t 'a);
  /** [union this that] returns an Iterator of unique elements which occur in either [this] or [that]. */

  let module Reducer: Reducer.S1 with type t 'a := t 'a;
};

module type Set = {
  type a;
  type t;

  include UniqueCollection with type a := a and type t := t;

  let toSet: t => Set.t a;
};

module type Set_1 = {
  type t 'a;

  include UniqueCollection_1 with type t 'a := t 'a;

  let toSet: t 'a => Set.t 'a;
};

module type PersistentSet = {
  type a;
  type t;

  include Set with type a := a and type t := t;
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

  include Set_1 with type t 'a := t 'a;
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

  include Set with type a := a and type t := t;
  include NavigableCollection with type a := a and type t := t;
};

module type NavigableSet_1 = {
  type t 'a;

  include Set_1 with type t 'a := t 'a;
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

  let reduceRight: while_::('acc => k => 'v => bool)? => ('acc => k => 'v => 'acc) => 'acc => (t 'v) => 'acc;
};

module type KeyedReduceableRight_2 = {
  type t 'k 'v;

  let reduceRight: while_::('acc => 'k => 'v => bool)? => ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
};

let module KeyedReducer: {
  module type S1 = {
    type k;
    type t 'v;

    let every: (k => 'v => bool) => (t 'v) => bool;
    let find: (k => 'v => bool) => (t 'v) => (option (k, 'v));
    let findOrRaise: (k => 'v => bool) => (t 'v) => (k, 'v);
    let findKey: (k => 'v => bool) => (t 'v) => (option k);
    let findKeyOrRaise: (k => 'v => bool) => (t 'v) => k;
    let findValue: (k => 'v => bool) => (t 'v) => (option 'v);
    let findValueOrRaise: (k => 'v => bool) => (t 'v) => 'v;
    let forEach: while_::(k => 'v => bool)? => (k => 'v => unit) => (t 'v) => unit;
    let none: (k => 'v => bool) => (t 'v) => bool;
    let some: (k => 'v => bool) => (t 'v) => bool;
  };

  module type S2 = {
    type t 'k 'v;

    let every: ('k => 'v => bool) => (t 'k 'v) => bool;
    let find: ('k => 'v => bool) => (t 'k 'v) => (option ('k, 'v));
    let findOrRaise: ('k => 'v => bool) => (t 'k 'v) => ('k, 'v);
    let findKey: ('k => 'v => bool) => (t 'k 'v) => (option 'k);
    let findKeyOrRaise: ('k => 'v => bool) => (t 'k 'v) => 'k;
    let findValue: ('k => 'v => bool) => (t 'k 'v) => (option 'v);
    let findValueOrRaise: ('k => 'v => bool) => (t 'k 'v) => 'v;
    let forEach: while_::('k => 'v => bool)? => ('k => 'v => unit) => (t 'k 'v) => unit;
    let none: ('k => 'v => bool) => (t 'k 'v) => bool;
    let some: ('k => 'v => bool) => (t 'k 'v) => bool;
  };

  let module Make1: (KeyedReduceable: KeyedReduceable_1) => S1 with type k = KeyedReduceable.k and type t 'v = KeyedReduceable.t 'v;
  let module Make2: (KeyedReduceable: KeyedReduceable_2) => S2 with type t 'k 'v = KeyedReduceable.t 'k 'v;
};

let module KeyedIterator: {
  type t 'k 'v;

  include KeyedReduceable_2 with type t 'k 'v := t 'k 'v;

  let concat: (list (t 'k 'v)) => (t 'k 'v);
  let defer: (unit => t 'k 'v) => (t 'k 'v);
  let distinctUntilChangedWith:
    keyEquals::(Equality.t 'k) =>
    valueEquals::(Equality.t 'v) =>
    (t 'k 'v) =>
    (t 'k 'v);
  let doOnNext: ('k => 'v => unit) => (t 'k 'v) => (t 'k 'v);
  let empty: unit => (t 'k 'v);
  let filter: ('k => 'v => bool) => (t 'k 'v) => (t 'k 'v);
  let flatMap: ('kA => 'vA => t 'kB 'vB) => (t 'kA 'vA) => (t 'kB 'vB);
  let fromEntries: Iterator.t ('k, 'v) => (t 'k 'v);
  let keys: (t 'k 'v) => (Iterator.t 'k);
  let map:
      keyMapper::('kA => 'vA => 'kB) =>
      valueMapper::('kA => 'vA => 'vB) =>
      (t 'kA 'vA) =>
      (t 'kB 'vB);
  let mapKeys: ('a => 'v => 'b) => (t 'a 'v) => (t 'b 'v);
  let mapValues: ('k => 'a => 'b) => (t 'k 'a) => (t 'k 'b);
  let repeat: 'k => 'v => (t 'k 'v);
  let return: 'k => 'v => (t 'k 'v);
  let scan: ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => (Iterator.t 'acc);
  let skip: int => (t 'k 'v) => (t 'k 'v);
  let skipWhile: ('k => 'v => bool) => (t 'k 'v) => (t 'k 'v);
  let startWith: 'k => 'v => (t 'k 'v) => (t 'k 'v);
  let take: int => (t 'k 'v) => (t 'k 'v);
  let takeWhile: ('k => 'v => bool) => (t 'k 'v) => (t 'k 'v);
  let toIterator: (t 'k 'v) => (Iterator.t ('k, 'v));
  let values: (t 'k 'v) => Iterator.t 'v;

  let module KeyedReducer: KeyedReducer.S2 with type t 'k 'v := t 'k 'v;
};

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
  /** [count map] returns the number of key/value pairs in [map]. */

  let isEmpty: (t 'v) => bool;
  /** [isEmpty map] returns true if [map] contains no key/value pairs. */

  let isNotEmpty: (t 'v) => bool;
  /** [isNotEmpty map] returns true if [map] contains at least one key/value pair. */

  let remove: k => (t 'v) => (t 'v);

  let removeAll: (t 'v) => (t 'v);
  /** [removeAll transient] removes all mappings from [transient].
   *
   *  Complexity: O(_1)
   */
};

module type TransientKeyedCollection_2 = {
  type t 'k 'v;

  let containsKey: 'k => (t 'k 'v) => bool;

  let count: (t 'k 'v) => int;
  /** [count map] returns the number of key/value pairs in [map]. */

  let isEmpty: (t 'k 'v) => bool;
  /** [isEmpty map] returns true if [map] contains no key/value pairs. */

  let isNotEmpty: (t 'k 'v) => bool;
  /** [isNotEmpty map] returns true if [map] contains at least one key/value pair. */

  let remove: 'k => (t 'k 'v) => (t 'k 'v);

  let removeAll: (t 'k 'v) => (t 'k 'v);
  /** [removeAll transient] removes all mappings from [transient].
   *
   *  Complexity: O(_1)
   */
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

module type KeyValueCollection_1 = {
  type k;
  type t 'v;

  include KeyedCollection_1 with type k := k and type t 'v := t 'v;

  let get: k => (t 'v) => (option 'v);
  let getOrRaise: k => (t 'v) => 'v;
  let map: (k => 'a => 'b) => (t 'a) => (t 'b);
  let values: (t 'v) => (Iterator.t 'v);
};

module type KeyValueCollection_2 = {
  type t 'k 'v;

  include KeyedCollection_2 with type t 'k 'v := t 'k 'v;

  let get: 'k => (t 'k 'v) => (option 'v);
  let getOrRaise: 'k => (t 'k 'v) => 'v;
  let map: ('k => 'a => 'b) => (t 'k 'a) => (t 'k 'b);
  let values: (t 'k 'v) => (Iterator.t 'v);
};

let module Map: {
  /** A read only view of an underlying set of key/value pairs. The intent of this type is to enable
   *  interop between alternative concrete implementations such as [SortedMap] and [HashMap].
   *  The complexity of functions in this module is dependent upon the underlying concrete implementation.
   */

  type t 'k 'v;
  /** The map type. */

  include KeyValueCollection_2 with type t 'k 'v := t 'k 'v;

  let empty: unit => (t 'k 'v);
  /** The empty Map. */

  let module KeyedReducer: KeyedReducer.S2 with type t 'k 'v := t 'k 'v;
};

module type Map_1 = {
  type k;
  type t 'v;

  include KeyValueCollection_1 with type k := k and type t 'v := t 'v;

  let toMap: (t 'v) => Map.t k 'v;
};

module type Map_2 = {
  type t 'k 'v;

  include KeyValueCollection_2 with type t 'k 'v := t 'k 'v;

  let toMap: (t 'k 'v) => Map.t 'k 'v;
};

module type PersistentMap_1 = {
  type k;
  type t 'v;

  include PersistentKeyedCollection_1 with type k := k and type t 'v := t 'v;
  include Map_1 with type k := k and type t 'v := t 'v;

  let alter: k => (option 'v => option 'v) => (t 'v) => (t 'v);

  let empty: unit => (t 'v);
  /** The empty SortedMap using the structural comparator. */

  let from: (KeyedIterator.t k 'v) => (t 'v);
  /** [from iter] returns a SortedMap including the key/value pairs in [iter]
   *  using the structural comparison.
   */

  let fromEntries: (Iterator.t (k, 'v)) => (t 'v);

  let merge: (k => (option 'vAcc) => (option 'v) => (option 'vAcc)) => (t 'vAcc) => (t 'v) => (t 'vAcc);

  let put: k => 'v => (t 'v) => (t 'v);

  let putAll: (KeyedIterator.t k 'v) => (t 'v) => (t 'v);

  let putAllEntries: (Iterator.t (k, 'v)) => (t 'v) => (t 'v);
};

module type PersistentMap_2 = {
  type t 'k 'v;

  include PersistentKeyedCollection_2 with type t 'k 'v := t 'k 'v;
  include Map_2 with type t 'k 'v := t 'k 'v;

  let alter: 'k => (option 'v => option 'v) => (t 'k 'v) => (t 'k 'v);

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
  /** [alter key f transient] enables efficient deep updates to an existing
   *  mapping from [key] in [transient]. If [transient] already has a mapping from [key],
   *  [f] will be called with Some, otherwise it will be called with None.
   *  If [f] returns None, alter removes any mapping from [key] in [transient].
   *  If [f] returns Some, alter returns add or updates the mapping
   *  from [key] in [transient].
   */

  let empty: unit => (t 'v);
  /** [empty ()] returns a new empty TransientIntMap. */

  let get: k => (t 'v) => (option 'v);
  /** [tryGet key transient] returns the value associated with [key] or None
   *
   *  Complexity: O(log3_2 N), effectively O(_1)
   */

  let getOrRaise: k => (t 'v) => 'v;

  let put: k => 'v => (t 'v) => (t 'v);
  /** [put key value transient] adds the mapping of [key] to [value] to [transient].
   *
   *  Complexity: O(log3_2 N), effectively O(_1)
   */

  let putAll: (KeyedIterator.t k 'v) => (t 'v) => (t 'v);
  /** [putAll iter transient] adds the key/value pairs in [iter] to [transient].
   *  Key value pairs in [iter] replace existing mappings in [transient].
   */
};

module type TransientMap_2 = {
  type t 'k 'v;

  include TransientKeyedCollection_2 with type t 'k 'v := t 'k 'v;

  let alter: 'k => (option 'v => option 'v) => (t 'k 'v) => (t 'k 'v);
  /** [alter key f transient] enables efficient deep updates to an existing
   *  mapping from [key] in [transient]. If [transient] already has a mapping from [key],
   *  [f] will be called with Some, otherwise it will be called with None.
   *  If [f] returns None, alter removes any mapping from [key] in [transient].
   *  If [f] returns Some, alter returns add or updates the mapping
   *  from [key] in [transient].
   */

  let get: 'k => (t 'k 'v) => (option 'v);
  /** [tryGet key transient] returns the value associated with [key] or None
   *
   *  Complexity: O(log3_2 N), effectively O(_1)
   */

  let getOrRaise: 'k => (t 'k 'v) => 'v;

  let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);
  /** [put key value transient] adds the mapping of [key] to [value] to [transient].
   *
   *  Complexity: O(log3_2 N), effectively O(_1)
   */

  let putAll: (KeyedIterator.t 'k 'v) => (t 'k 'v) => (t 'k 'v);
  /** [putAll iter transient] adds the key/value pairs in [iter] to [transient].
   *  Key value pairs in [iter] replace existing mappings in [transient].
   */
};

module type NavigableMap_1 = {
  type k;
  type t 'v;

  include NavigableKeyedCollection_1 with type k := k and type t 'v := t 'v;
  include Map_1 with type k := k and type t 'v := t 'v;
};

module type PersistentNavigableMap_1 = {
  type k;
  type t 'v;

  include NavigableMap_1 with type k := k and type t 'v := t 'v;
  include PersistentMap_1 with type k := k and type t 'v := t 'v;

  let removeFirstOrRaise: (t 'v) => (t 'v);
  /** [removeFirstOrRaise map] returns a new SortedMap without the first element.
   *
   *  Complexity: O(log N)
   */

  let removeLastOrRaise: (t 'v) => (t 'v);
  /** [removeLastOrRaise map] returns a new SortedMap without the last element.
   *
   *  Complexity: O(log N)
   */
};

module type Stack_1 = {
  type t 'a;

  include ReverseMappable_1 with type t 'a := t 'a;
  include SequentialCollection_1 with type t 'a := t 'a;
  include PersistentCollection_1 with type t 'a := t 'a;

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

module type TransientStack_1 = {
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

module type Deque_1 = {
  type t 'a;

  include Mappable_1 with type t 'a := t 'a;
  include NavigableCollection_1 with type t 'a := t 'a;
  include Stack_1 with type t 'a := t 'a;

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

module type TransientDeque_1 = {
  type t 'a;

  include TransientStack_1 with type t 'a := t 'a;

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

module type IndexedCollection_1 = {
  type t 'a;

  include NavigableCollection_1 with type t 'a := t 'a;

  let get: int => (t 'a) => (option 'a);
  /** [tryGet index vec] returns the element at [index] or None if [index] is out of bounds. */

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
  /** [map f vec] returns a new Vector applying the
   *  function [f] to each index/element pair in [vec].
   */

  let mapReverseWithIndex: (int => 'a => 'b) => (t 'a) => (t 'b);
  /** [mapReverseWithIndex f vec] returns a new Vector applying the
   *  function [f] to each index/element pair in [vec], reversing the result.
   */
};

module type Vector_1 = {
  type t 'a;

  include Concatable_1 with type t 'a := t 'a;
  include Deque_1 with type t 'a := t 'a;
  include IndexedCollection_1 with type t 'a := t 'a;
  include IndexedMappable_1 with type t 'a := t 'a;
  include Skippable_1 with type t 'a := t 'a;
  include Takeable_1 with type t 'a := t 'a;

  let init: int => (int => 'a) => (t 'a);
  let insertAt: int => 'a => (t 'a) => (t 'a);
  let removeAt: int => (t 'a) => (t 'a);
  let slice: start::int? => end_::int? => (t 'a) => (t 'a);
  let update: int => 'a => (t 'a) => (t 'a);
  let updateAll: (int => 'a => 'a) => (t 'a) => (t 'a);
  let updateWith: int => ('a => 'a) => (t 'a) => (t 'a);
};

module type TransientVector_1 = {
  type t 'a;
  /** The TransientVector type. */

  include TransientDeque_1 with type t 'a := t 'a;

  let get: int => (t 'a) => (option 'a);

  let getOrRaise: int => (t 'a) => 'a;

  let insertAt: int => 'a => (t 'a) => (t 'a);
  /** [insertAt index value transient] inserts value into [transient] at [index].
   *
   *  WARNING: Not implemented
   *
   *  Complexity: O(log3_2 N)
   */

  let removeAt: int => (t 'a) => (t 'a);
  /** [removeAt index transient] removes the element at [index].
   *
   *  WARNING: Not implemented
   *
   *  Complexity: O(log3_2 N)
   */

  let update: int => 'a => (t 'a) => (t 'a);
  /** [update index value transient] replaces the element at [index] with [value].
   *
   *  Complexity: O(log3_2 N)
   */

  let updateAll: (int => 'a => 'a) => (t 'a) => (t 'a);
  /** [updateAll f transient] updates each element in [transient] with result of applying
   *  the function [f] to each index/element pair.
   *
   *  Complexity: O(N)
   */

  let updateWith: int => ('a => 'a) => (t 'a) => (t 'a);
  /** [updateWith index f transient] updates the element at [index] with the result
   *  of applying the function [f] to the element.
   *
   *  Complexity: O(log3_2 N)
   */
};

let module IntRange: {
  /** Represents a contiguous Set of discrete integers */

  type a = int;
  type t;
  /** The IntRange type.*/

  include NavigableSet with type a := a and type t := t;
  include Comparable with type t := t;
  include Hashable with type t := t;

  let create: start::int => count::int => t;

  let module Reducer: Reducer.S with type a := a and type t := t;
};

let module rec HashSet: {
  /** A set implementation that utilizes hashing and comparison
   *  or equality for collision resolution.
   */

  type t 'a;
  /** The HashSet type. */

  include PersistentSet_1 with type t 'a := t 'a;
  include Hashable_1 with type t 'a := t 'a;

  let emptyWith: hash::(Hash.t 'a) => comparator::(Comparator.t 'a) => (HashSet.t 'a);
  let fromWith: hash::(Hash.t 'a) => comparator::(Comparator.t 'a) => (Iterator.t 'a) => (HashSet.t 'a);
  let mutate: (t 'a) => (TransientHashSet.t 'a);
  /** [mutate set] returns a TransientHashSet containing the same elements as [set].
   *
   *  Complexity: O(_1)
   */

  let module Reducer: Reducer.S1 with type t 'a := t 'a;
}

and TransientHashSet: {
  /** A temporarily mutable HashSet. Once persisted, any further operations on a
   *  TransientHashSet instance will throw. Intended for implementing bulk mutation
   *  operations efficiently.
   */

  type t 'a;
  /** The TransientHashSet type. */

  include TransientSet_1 with type t 'a := t 'a;

  let emptyWith: hash::(Hash.t 'a) => comparator::(Comparator.t 'a) => unit => (TransientHashSet.t 'a);
  let persist: (t 'a) => (HashSet.t 'a);
  /** [persist transient] returns a persisted HashSet. Further attempts to access or mutate [transient]
   *  will throw.
   */
};

let module rec IntSet: {
  /** A set implementation optimized for storing sparse ints. */

  type a = int;
  type t;
  /** The IntSet type. */

  include PersistentSet with type a := a and type t := t;

  let mutate: t => TransientIntSet.t;
  /** [mutate set] returns a TransientIntSet containing the same elements as [set].
   *
   *  Complexity: O(_1)
   */

  let module Reducer: Reducer.S with type a = a and type t := t;
}

and TransientIntSet: {
  /** A temporarily mutable IntSet. Once persisted, any further operations on a
   *  TransientIntSet instance will throw. Intended for implementing bulk mutation
   *  operations efficiently.
   */
  type a = int;
  type t;
  /** The TransientIntSet type. */

  include TransientSet with type a := a and type t := t;

  let empty: unit => t;
  /** [empty ()] return a new empty TransientIntSet. */

  let persist: t => IntSet.t;
  /** [persist transient] returns a persisted IntSet. Further attempts to access or mutate [transient]
   *  will throw.
   */
};

let module SortedSet: {
  /** AVL tree based Set implementation. */
  module type S = {
    type a;
    type t;
    /** The SortedSet type */

    include Comparable with type t := t;
    include PersistentNavigableSet with type a := a and type t := t;

    let module Reducer: Reducer.S with type a := a and type t := t;
  };

  let module Make: (Comparable: Comparable) => S with type a = Comparable.t;
};

let module rec HashMap: {
  /** A hashed Map. */

  type t 'k 'v;
  /** The HashMap type. */

  include PersistentMap_2 with type t 'k 'v := t 'k 'v;

  let emptyWith: hash::(Hash.t 'k) => comparator::(Comparator.t 'k) => (HashMap.t 'k 'v);
  let fromWith:
    hash::(Hash.t 'k) =>
    comparator::(Comparator.t 'k) =>
    (KeyedIterator.t 'k 'v) =>
    (HashMap.t 'k 'v);

  let fromEntriesWith:
    hash::(Hash.t 'k) =>
    comparator::(Comparator.t 'k) =>
    (Iterator.t ('k, 'v)) => (t 'k 'v);

  let mutate: (t 'k 'v) => (TransientHashMap.t 'k 'v);
  /** [mutate map] returns a TransientHashMap containing the same key/values pairs as [map].
   *
   *  Complexity: O(_1)
   */

  let module KeyedReducer: KeyedReducer.S2 with type t 'k 'v := t 'k 'v;
}

and TransientHashMap: {
  /** A temporarily mutable HashMap. Once persisted, any further operations on a
   *  TransientHashSet instance will throw. Intended for implementing bulk mutation
   *  operations efficiently.
   */

  type t 'k 'v;
  /** The TransientHashMap type. */

  include TransientMap_2 with type t 'k 'v := t 'k 'v;

  let emptyWith: hash::(Hash.t 'k) => comparator::(Comparator.t 'k) => unit => (TransientHashMap.t 'k 'v);
  let persist: (t 'k 'v) => (HashMap.t 'k 'v);
  /** [persist transient] returns a persisted HashMap. Further attempts to access or mutate [transient]
   *  will throw.
   */
};

let module rec IntMap: {
  /** A Map optimized for integer keys. */

  type k = int;
  type t 'v;
  /** The IntMap type. */

  include PersistentMap_1 with type k := k and type t 'v := t 'v;

  let mutate: (t 'v) => (TransientIntMap.t 'v);
  /** [mutate map] returns a TransientIntMap containing the same key/values pairs as [map].
   *
   *  Complexity: O(_1)
   */

  let module KeyedReducer: KeyedReducer.S1 with type k = k and type t 'v := t 'v;
}

and TransientIntMap: {
  type k = int;
  type t 'v;

  include TransientMap_1 with type k := k and type t 'v := t 'v;

  let persist: (t 'v) => (IntMap.t 'v);
  /** [persist transient] returns a persisted HashBiMap. Further attempts to access or mutate [transient]
   *  will throw.
   */
};

let module SortedMap: {
  /** AVL tree based Map. */
  module type S = {
    type k;

    type t +'v;
    /** The SortedMap type. */

    include PersistentNavigableMap_1 with type k := k and type t 'v := t 'v;

    let module KeyedReducer: KeyedReducer.S1 with type k := k and type t 'v := t 'v;
  };

  let module Make: (Comparable: Comparable) => S with type k = Comparable.t;
};

let module List: {
  /** OCaml singly-linked list */

  type t 'a = list 'a;
  /** The List type. */

  include Sequential_1 with type t 'a := t 'a;
  include ReverseMappable_1 with type t 'a := t 'a;

  let addFirst: 'a => (t 'a) => (t 'a);
  /** [addFirst value list] returns a new List with [value] prepended. */

  let addFirstAll: (Iterator.t 'a) => (t 'a) => (t 'a);
  /** [addFirstAll iter list] returns a new List with the values in [iter] prepended. */

  let empty: unit => (t 'a);
  /** The empty List. */

  let fromReverse: (Iterator.t 'a) => (t 'a);
  /** [fromReverse iter] returns a new List containing the values in [iter]
   *  in reverse order.
   *
   * Complexity: O(N) the number of elements in [iter].
   */

  let removeAll: (t 'a) => (t 'a);
  /** [removeAll list] returns the empty List.
   *
   *  Complexity: O(_1)
   */

  let removeFirstOrRaise: (t 'a) => (t 'a);
  /** [removeFirstOrRaise list] returns a new List without the first element.
   *
   *  Complexity: O(_1)
   */

  let return: 'a => (t 'a);
  /** [return value] returns a new List containing a single element, [value]. */

  let toSequence: (t 'a) => (Sequence.t 'a);
  /** [toSequence list] returns a Sequence of the elements in [list] in order. */

  let module Reducer: Reducer.S1 with type t 'a := t 'a;
};

let module Option: {
  /** OCaml option type. Can be considered a set of zero or one elements.
   *  All operations have a complexity of O(_1).
   */

  type t 'a = option 'a;
  /** The Option type. */

  include FlatMappable_1 with type t 'a := t 'a;
  include Mappable_1 with type t 'a := t 'a;
  include SequentialCollection_1 with type t 'a := t 'a;

  let empty: unit => (t 'a);
  /** The empty Option, None. */

  let return: 'a => (t 'a);
  /** [return value] returns [Some value]. */

  let module Reducer: Reducer.S1 with type t 'a := t 'a;
};

let module Stack: {
  /** A singly-linked stack with an O(_1) count operation. */

  type t 'a;
  /** The Stack type. */

  include Stack_1 with type t 'a := t 'a;

  let fromList: (list 'a) => (t 'a);
  /** [fromList list] returns a Stack backed by [list].
   *
   *  Complexity: O(N)
   */

  let toList: (t 'a) => (list 'a);
  /** [toList stack] returns the underlying List backing the stack */

  let module Reducer: Reducer.S1 with type t 'a := t 'a;
};

let module rec Deque: {
  /** A double-ended queue with efficient appends [addLast], prepends [addFirst]
   *  and removals from either end of the queue [removeFirstOrRaise] [removeLastOrRaise].
   */

  type t 'a;
  /** The Deque type. */

  include Deque_1 with type t 'a := t 'a;

  let mutate: (t 'a) => (TransientDeque.t 'a);
  /** [mutate deque] returns a TransientDeque containing the same elements as [deque].
   *
   *  Complexity: O(_1)
   */

  let reverse: (t 'a) => (t 'a);
  /** [reverse deque] returns a new Deque with [deque]'s elements reversed.
   *
   *  Complexity: O(_1)
   */

  let module Reducer: Reducer.S1 with type t 'a := t 'a;
}

and TransientDeque: {
  /** A temporarily mutable Deque. Once persisted, any further operations on a
   *  TransientDeque instance will throw. Intended for implementing bulk mutation operations efficiently.
   */

  type t 'a;

  include TransientDeque_1 with type t 'a := t 'a;
  /** The TransientDeque type. */

  let persist: (t 'a) => (Deque.t 'a);
  /** [persist transient] returns a persisted Deque. Further attempts to access or mutate [transient]
   *  will throw.
   */

  let reverse: (t 'a) => (t 'a);
  /** [reverse transient] reverse [transient]'s elements.
   *
   *  Complexity: O(_1)
   */
};

let module rec Vector: {
  /** Indexed type that supports efficient operations for
   * prepend, appends, indexing, conctentation, and splits.
   */

  type t 'a;
  /** The vector type */

  include Vector_1 with type t 'a := t 'a;

  let mutate: (t 'a) => (TransientVector.t 'a);

  let module Reducer: Reducer.S1 with type t 'a := t 'a;
}

and TransientVector: {
  /** A temporarily mutable Vector. Once persisted, any further operations on a
   *  TransientVector instance will throw. Intended for implementing bulk mutation operations efficiently.
   */

  type t 'a;

  include TransientVector_1 with type t 'a := t 'a;

  let persist: (t 'a) => (Vector.t 'a);
  /** [persist transient] returns a persisted Vector. Further attempts to access or mutate [transient]
  *  will throw.
  */
};

let module CopyOnWriteArray: {
  /** Opaque wrapper around an underlying array instance that provides copy on write semantics */

  type t 'a;
  /** The CopyOnWriteArray type. */

  include Vector_1 with type t 'a := t 'a;

  let ofUnsafe: (array 'a) => (t 'a);
  /** [unsafe arr] returns a CopyOnWriteArray backed by [arr]. Note, it is the caller's
   *  responsibility to ensure that [arr] is not subsequently mutated.
   *
   *  Complexity: O(_1)
   */

  let module Reducer: Reducer.S1 with type t 'a := t 'a;
};
