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
  /** The Equality function type.
   * [equals this that] returns [true] if [this] and [that] are equal, otherwise [false].
   */

  let bytes: t bytes;
  /** Equality for bytes. */

  let char: t char;
  /** Equality for chars. */

  let int: t int;
  /** Equality for ints. */

  let int32: t int32;
  /** Equality for int32s. */

  let int64: t int64;
  /** Equality for int64s. */

  let nativeInt: t nativeint;
  /** Equality for nativeInts. */

  let reference: t 'a;
  /** The reference equality function, analogous to === */

  let string: t string;
  /** Equality for strings. */
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
  /** The Comparator function type.
   *  By definition a [compare this that] returns:
   *    [Ordering.greaterThan] if [this] is greater than [that],
   *    [Ordering.lessThan] if [this] is less than [that],
   *    otherwise [Ordering.equals].
   */

  let bytes: t bytes;
  /** Compares bytes. */

  let char: t char;
  /** Compares chars. */

  let int: t int;
  /** Compares ints. */

  let int32: t int32;
  /** Compares int32s. */

  let int64: t int64;
  /** Compares int64s. */

  let nativeInt: t nativeint;
  /** Compares nativeInts. */

  let string: t string;
  /** Compares strings. */

  let toEquality: (t 'a) => (Equality.t 'a);
  /** Converts a Comparator function to an Equality function. */
};

let module Hash: {
  /** Hash functions for common types. */

  type t 'a = 'a => int;
  /** The Hash function type. */
};

let module Equatable: {
  /** Module types implemented by modules that support testing values for equality. */

  module type S = {
    /* Equatable module type signature for types with a parametric type arity of 0. */

    type t;

    let equals: Equality.t t;
    /** An equality function for instances of type [t]. */
  };

  module type S1 = {
    /* Equatable module type signature for types with a parametric type arity of 1 */

    type t 'a;

    let equals: Equality.t (t 'a);
    /** An equality function for instances of type [t 'a]. */
  };
};

let module Comparable: {
  /** Module types implemented by modules that support absolute ordering of values. */

  module type S = {
    /** Comparable module type signature for types with a parametric type arity of 0. */

    type t;

    include Equatable.S with type t := t;

    let compare: Comparator.t t;
    /** A comparator function for instances of type [t]. */
  };
};

let module Hashable: {
  /** Module types implemented by modules that support hashing. */

  module type S = {
    /** Hashable module type signature for types with a parametric type arity of 0. */

    type t;

    let hash: Hash.t t;
    /** An hashing function for instances of type [t]. */
  };

  module type S1 = {
    /** Hashable module type signature for types with a parametric type arity of 1. */

    type t 'a;

    let hash: Hash.t (t 'a);
    /** An hashing function for instances of type [t 'a]. */
  };
};

let module FlatMappable: {
  /** Module type implemented by modules that support the flatmap operation.
   *  Computational complexity is dependent upon whether the underlying type
   *  is evaluated eagerly, in which case the operation is O(N), or lazily,
   *  in which case the operation is O(1).
   */

  module type S1 = {
    /** FlatMappable module type signature for types with a parametric type arity of 1. */

    type t 'a;

    let flatMap: ('a => t 'b) => (t 'a) => (t 'b);
    /** [flatMap mapper flatMappable] applies the mapper to each value in
     *  [flatMappable], flattening the resulting [t 'b]'s into a new [t 'b].
     */

    let flatten: (t (t 'a)) => (t 'a);
    /** [flatten flatMappables] flattens the nested values in [flatMappables] into
     *  a new [t'a].
     */
  };
};

let module Mappable: {
  /** Module type implemented by modules that support the map operation.
   *  Computational complexity is dependent upon whether the underlying type
   *  is evaluated eagerly, in which case the operation is O(N), or lazily,
   *  in which case the operation is O(1).
   */

  module type S1 = {
    /** Mappable module type signature for types with a parametric type arity of 1. */

    type t 'a;

    let map: ('a => 'b) => (t 'a) => (t 'b);
    /** [map f mappable] Returns a [Mappable] whose values are the result of
     *  applying the function [f] to each value in [mappable].
     */
  };
};

let module Reduceable: {
  /** Module types implemented by modules that support reducing over values. */

  module type S = {
    /** Reduceable module type signature for types with a parametric type arity of 0. */

    type a;
    type t;

    let reduce: while_::('acc => a => bool)? => ('acc => a => 'acc) => 'acc => t => 'acc;
    /** [reduce while_::predicate initialValue f reduceable] applies the accumulator
     *  function [f] to each value in [reduceable], while [predicate] returns true,
     *  accumulating the result.
     */
  };

  module type S1 = {
    /** Reduceable module type signature for types with a parametric type arity of 1. */

    type t 'a;

    let reduce: while_::('acc => 'a => bool)? => ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
    /** [reduce while_::predicate initialValue f reduceable] applies the accumulator
     *  function [f] to each value in [reduceable], while [predicate] returns true,
     *  accumulating the result.
     */
  };
};

let module ReduceableRight: {
  /** Module types implemented by modules that support reducing over
   *  values in both the left to right, and right to left directions.
   */

  module type S = {
    /** ReduceableRight module type signature for types with a parametric type arity of 0. */

    type a;
    type t;

    include Reduceable.S with type a := a and type t := t;

    let reduceRight: while_::('acc => a => bool)? => ('acc => a => 'acc) => 'acc => t => 'acc;
    /** [reduceRight while_::predicate initialValue f reduceable] applies the accumulator
     *  function [f] to each value in [reduceable] while [predicate] returns true, starting
     *  from the right most value, accumulating the result.
     */
  };

  module type S1 = {
    /** ReduceableRight module type signature for types with a parametric type arity of 1. */

    type t 'a;

    include Reduceable.S1 with type t 'a := t 'a;

    let reduceRight: while_::('acc => 'a => bool)? => ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
    /** [reduceRight while_::predicate initialValue f reduceable] applies the accumulator
     *  function [f] to each value in [reduceable] while [predicate] returns true, starting
     *  from the right most value, accumulating the result.
     */
  };
};

let module ReverseMappable: {
  /** Module types implemented by modules that support the mapReverse operation.
   *  Computation complexity is dependent upon whether the underlying type
   *  is evaluated eagerly, in which case the operation is O(N), or lazily,
   *  in which case the operation is O(1).
   */

  module type S1 = {
    /** ReverseMappable module type signature for types with a parametric type arity of 1. */

    type t 'a;

    let mapReverse: ('a => 'b) => (t 'a) => (t 'b);
    /** [mapReverse f reverseMappable] Returns a  [ReverseMappable] whose values
     *  are the result of applying the function [f] to each value in [reverseMappable]
     *  and reversing the order of values
     */
  };
};

let module Stream: {
  /** Module types implemented by modules that support lazily evaluated
   *  stream functions. All functions defined in this module are O(1).
   */

  module type S1 = {
    /** Stream module type signature for types with a parametric type arity of 1. */

    type t 'a;

    include FlatMappable.S1 with type t 'a := t 'a;
    include Mappable.S1 with type t 'a := t 'a;

    let buffer: count::int => skip::int => (t 'a) => (t (list 'a));
    /** [buffer count skip stream] returns a Stream that collects values from [stream]
     *  into list buffers of size [count], skipping [skip] number of values in between the
     *  creation of new buffers. The returned buffers are guaranteed to be of size [count],
     *  and values are dropped if [stream] completes before filling the last buffer.
     */

    let concat: (list (t 'a)) => (t 'a);
    /** [concat streams] returns a Stream that lazily concatenates all the
     *  Streams in [streams]. The resulting Stream returns all the values
     *  in the first Stream, followed by all the values in the second Stream,
     *  and continues until the last Stream completes.
     */

    let defer: (unit => t 'a) => (t 'a);
    /** [defer f] returns a Stream that invokes the function [f] whenever enumerated. */

    let distinctUntilChangedWith: (Equality.t 'a) => (t 'a) => (t 'a);
    /** [distinctUntilChangedWith equals stream] returns a Stream that contains only
     *  distinct contiguous values from [stream] using [equals] to equate values.
     */

    let doOnNext: ('a => unit) => (t 'a) => (t 'a);
    /** [doOnNext f stream] returns a Stream that applies the side effect
     *  function [f] to each value in the stream as they are enumerated.
     */

    let empty: unit => (t 'a);
    /** Returns an empty Stream. */

    let filter: ('a => bool) => (t 'a) => (t 'a);
    /** [filter f stream] returns a Stream only including values from [stream]
     *  for which application of the predicate function [f] returns true.
     */

    let generate: ('a => 'a) => 'a => (t 'a);
    /** [generate f initialValue] generates the infinite Stream [x, f(x), f(f(x)), ...] */

    let return: 'a => (t 'a);
    /** [return value] returns a single value Stream containing [value]. */

    let scan: ('acc => 'a => 'acc) => 'acc => (t 'a) => (t 'acc);
    /** [scan f acc stream] returns a Stream of accumulated values resulting from the
     *  application of the accumulator function [f] to each value in [stream] with the
     *  specified initial value [acc].
     */

    let skip: int => (t 'a) => (t 'a);
    /** [skip count stream] return a Stream which skips the first [count]
     *  values in [stream].
     */

    let skipWhile: ('a => bool) => (t 'a) => (t 'a);
    /** [skipWhile f stream] return a Stream which skips values in [stream]
     *  while application of the predicate function [f] returns true, and then returns
     *  the remaining values.
     */

    let startWith: 'a => (t 'a) => (t 'a);
    /** [startWith value stream] returns a Stream whose first
     *  value is [value], followed by the values in [stream].
     */

    let take: int => (t 'a) => (t 'a);
    /** [take count stream] returns a Stream with the first [count]
     *  values in [stream].
     */

    let takeWhile: ('a => bool) => (t 'a) => (t 'a);
    /** [takeWhile f stream] returns a Stream including all values in [stream]
     *  while application of the predicate function [f] returns true, then completes.
     */
  };
};

let module Reducer: {
  /** Module functions for generating modules which provide common reduction functions for Reduceables.
   *  All functions are O(N), unless otherwise noted.
   */

  module type S = {
    type a;
    type t;

    let count: t => int;
    /** [count reduceable] returns the total number values produced by [reduceable] */

    let every: (a => bool) => t => bool;
    /** [every f reduceable] returns true if the predicate [f] returns true for all values in [reduceable].
     *  If [reduceable] is empty, returns [true].
     */

    let find: (a => bool) => t => (option a);
    /** [find f reduceable] return the Some of the first value in [reduceable] for which the
     *  the predicate f returns [true]. Otherwise None.
     */

    let findOrRaise: (a => bool) => t => a;
    /** [findOrRaise f reduceable] return the the first value in [reduceable] for which the
     *  the predicate f returns [true]. Otherwise raises an exception.
     */

    let first: t => (option a);
    /** [first reduceable] returns first value in [reduceable] or None.
     *
     *  Computational Complexity: O(1)
     */

    let firstOrRaise: t => a;
    /** [firstOrRaise reduceable] returns the first value in [reduceable] or raises an exception.
     *
     *  Computational Complexity: O(1)
     */

    let forEach: while_::(a => bool)? => (a => unit) => t => unit;
    /** [forEach while_::predicate f reduceable] iterates through [reduceable] applying the
     *  side effect function [f] to each value, while [predicate] returns true
     */

    let none: (a => bool) => t => bool;
    /** [none f reduceable] returns true if the predicate [f] returns false for all values in [reduceable].
     *  If [reduceable] is empty, returns [true].
     */

    let some: (a => bool) => t => bool;
    /** [some f reduceable] returns true if the predicate [f] returns true for at
     *  least one value in [reduceable]. If [reduceable] is empty, returns [false].
     */
  };

  module type S1 = {
    type t 'a;

    let count: t 'a => int;
    /** [count reduceable] returns the total number values produced by [reduceable] */

    let every: ('a => bool) => (t 'a) => bool;
    /** [every f reduceable] returns true if the predicate [f] returns true for all values in [reduceable].
     *  If [reduceable] is empty, returns [true].
     */

    let find: ('a => bool) => (t 'a) => (option 'a);
    /** [find f reduceable] return the Some of the first value in [reduceable] for which the
     *  the predicate f returns [true]. Otherwise None.
     */

    let findOrRaise: ('a => bool) => (t 'a) => 'a;
    /** [findOrRaise f reduceable] return the the first value in [reduceable] for which the
     *  the predicate f returns [true]. Otherwise raises an exception.
     */

    let first: t 'a => (option 'a);
    /** [first reduceable] returns first value in [reduceable] or None.
     *
     *  Computational Complexity: O(1)
     */

    let firstOrRaise: t 'a => 'a;
    /** [firstOrRaise reduceable] returns the first value in [reduceable] or raises an exception.
     *
     *  Computational Complexity: O(1)
     */

    let forEach: while_::('a => bool)? => ('a => unit) => (t 'a) => unit;
    /** [forEach while_::predicate f reduceable] iterates through [reduceable] applying the
     *  side effect function [f] to each value, while [predicate] returns true
     */

    let none: ('a => bool) => (t 'a) => bool;
    /** [none f reduceable] returns true if the predicate [f] returns false for all values in [reduceable].
     *  If [reduceable] is empty, returns [true].
     */

    let some: ('a => bool) => (t 'a) => bool;
    /** [some f reduceable] returns true if the predicate [f] returns true for at
     *  least one value in [reduceable]. If [reduceable] is empty, returns [false].
     */
  };

  let module Make: (Reduceable: Reduceable.S) => S with type a = Reduceable.a and type t = Reduceable.t;
  /** Module function to create a Reducer for a specific Reduceable type. */

  let module Make1: (Reduceable: Reduceable.S1) => S1 with type t 'a = Reduceable.t 'a;
  /** Module function to create a Reducer for a specific Reduceable type
   *  with a parametric type arity of 1.
   */
};

let module Iterator: {
  /** Functional iterators over a collection of values. Iterators are stateless and can be reused. */
  type t 'a;

  include Reduceable.S1 with type t 'a := t 'a;
  include Stream.S1 with type t 'a := t 'a;

  let module Reducer: Reducer.S1 with type t 'a := t 'a;
  /* Reducer module for Iterators. */
};

let module Iterable: {
  /** Module types implemented by modules that supporting iterating over their values. */

  module type S = {
    /** Iterable module type signature for types with a parametric type arity of 0. */

    type a;
    type t;

    include Reduceable.S with type a := a and type t := t;

    let toIterator: t => (Iterator.t a);
    /* [toIterator iterable] returns an Iterator that can be used to iterate over
     * the values in [iterable].
     */
  };

  module type S1 = {
    /** Iterable module type signature for types with a parametric type arity of 1. */

    type t 'a;

    include Reduceable.S1 with type t 'a := t 'a;

    let toIterator: t 'a => (Iterator.t 'a);
    /* [toIterator iterable] returns an Iterator that can be used to iterate over
     * the values in [iterable].
     */
  };
};

let module Sequence: {
  /** Functional pull based sequences. Sequences are generally lazy, computing values as
   *  they are pulled. Sequences are reusable and are guaranteed to produce the
   *  same values, in the same order every time they are enumerated. In addition, Sequences
   *  support eager seeking and zipping. These are their main advantage over Iterators.
   *  In general, only use Sequences when you require support for one or both of these features.
   *  Otherwise Iterators are generally more efficient.
   */

  type t 'a;
  /** The Sequence type. */

  include Iterable.S1 with type t 'a := t 'a;
  include Stream.S1 with type t 'a := t 'a;

  let seek: int => (t 'a) => (t 'a);
  /** [seek count seq] scans forward [count] values in [seq]. It is the eagerly
   *  evaluated equivalent of [skip count seq].
   *
   *  Computational complexity is O(count).
   */

  let seekWhile: ('a => bool) => (t 'a) => (t 'a);
  /** [seekWhile f seq] scans forward through [seq] while application of
   *  the predicate function [f] returns true. It is the eagerly evaluated
   *  equivalent of [skipWhile f seq].
   *
   *  Computational complexity is O(N).
   */

  let zip: (list (t 'a)) => (t (list 'a));
  /** [zip seqs] returns a Sequence which lazily zips a list of [Sequence]s
   *  into a single Sequence of lists. Values are produce until any Sequence
   *  in [seqs] completes.
   */

  let zip2With: ('a => 'b => 'c) => (t 'a) => (t 'b) => (t 'c);
  /** [zip2With zipper first second] returns a Sequence which lazily zips two Sequences,
   *  combining their values using [zipper]. Values are produce until either [first]
   *  or [second] completes.
   */

  let zip3With: ('a => 'b => 'c => 'd) => (t 'a) => (t 'b) => (t 'c) => (t 'd);
  /** [zip3With zipper first second third] returns a Sequence which lazily zips three Sequences,
   *  combining their values using [zipper]. Values are produce until either [first], [second]
   *  or [third] complete.
   */

  let zipLongest: (list (t 'a)) => (t (list (option 'a)));
  /** [zipLongest seqs] returns a Sequence which zips a list of Sequences
   *  into a single of Sequence of lists. Values are produce until all Sequences
   *  in [seqs] complete.
   */

  let zipLongest2With:
    (option 'a => option 'b => 'c) =>
    (t 'a) =>
    (t 'b) =>
    (t 'c);
  /** [zipLongest2With zipper first second] returns a Sequence which lazily zips two Sequences,
   *  combining their values using [zipper]. Values are produce until both [first]
   *  and [second] complete.
   */

  let zipLongest3With:
    (option 'a => option 'b => option 'c => 'd) =>
    (t 'a) =>
    (t 'b) =>
    (t 'c) =>
    (t 'd);
  /** [zipLongest3With zipper first second third] returns a Sequence which lazily
   *  zips three Sequences, combining their values using [zipper]. Values are produce
   *  until [first], [second] and [third] all complete.
   */

  let module Reducer: Reducer.S1 with type t 'a := t 'a;
  /* Reducer module for Sequences. */
};

let module Collection: {
  /** Module types implemented by all immutable value collections.
   *
   *  By contract, all functions have a computational complexity of O(1).
   */

  module type S = {
    /** Collection module type signature for types with a parametric type arity of 0. */

    type a;
    type t;

    include Iterable.S with type a := a and type t := t;
    include Equatable.S with type t := t;

    let count: t => int;
    /** [count collection] returns number of values contained in [collection]. */

    let empty: t;
    /** The empty collection of type [t] */

    let isEmpty: t => bool;
    /** [isEmpty collection] returns true if [collection] is empty, otherwise false. */

    let isNotEmpty: t => bool;
    /** [isNotEmpty collection] returns true if [collection] contains at
     *  least one value, otherwise false.
     */

    let toSequence: t => (Sequence.t a);
    /** [toSequence collection] returns a Sequence that can be used to enumerate the collection. */
  };

  module type S1 = {
    /** Collection module type signature for types with a parametric type arity of 1. */

    type t 'a;

    include Iterable.S1 with type t 'a := t 'a;

    let count: (t 'a) => int;
    /** [count collection] returns number of values contained in the collection. */

    let isEmpty: (t 'a) => bool;
    /** [isEmpty collection] returns true if [collection] is empty, otherwise false. */

    let isNotEmpty: (t 'a) => bool;
    /** [isNotEmpty collection] returns true if [collection] contains at
     *  least one value, otherwise false.
     */

    let toSequence: (t 'a) => (Sequence.t 'a);
    /** [toSequence collection] returns a Sequence that can be used to enumerate the collection. */
  };
};

let module PersistentCollection: {
  /** Module types implemented by collections supporting fully persistent mutations. That is to say,
   *  mutation operations on these types do not mutate the underlying collection, but instead
   *  create a new collection, with the mutation applied.

   *  By contract, all functions have a computational complexity of O(1).
   */

  module type S = {
    /** PersistentCollection module type signature for types with a parametric type arity of 0. */

    type a;
    type t;

    include Collection.S with type a := a and type t := t;

    let removeAll: t => t;
    /** [removeAll collection] return a new empty collection. Depending on the implementation,
     *  the new collection may share the same configuration as [collection]. For instance, the HashSet
     *  implementations shares the same hash and comparison functions.
     */
  };

  module type S1 = {
    /** PersistentCollection module type signature for types with a parametric type arity of 1. */

    type t 'a;

    include Collection.S1 with type t 'a := t 'a;

    let removeAll: t 'a => t 'a;
    /** [removeAll collection] return a new empty collection. Depending on the implementation,
     *  the new collection may share the same configuration as [collection]. For instance, HashSet
     *  implementations shares the same hash and comparison functions.
     */
  };
};

let module TransientCollection: {
  /** Module types implemented by transiently mutable collections. Transient collections
   *  are designed to enable fast and efficient batch operations by temporarily enabling mutation
   *  of an underlying collection type. Unlike PersistentCollection functions, TransientCollection
   *  APIs always return the same value reference passed in as an argument, with mutations applied.
   *
   *  By contract, all functions have a computational complexity of O(1).
   */

  module type S = {
    /** TransientCollection module type signature for types with a parametric type arity of 0. */

    type a;
    type t;

    let count: t => int;
    /** [count transient] returns number of values contained in [transient]. */

    let empty: unit => t;

    let isEmpty: t => bool;
    /** [isEmpty transient] returns true if [transient] is empty, otherwise false. */

    let isNotEmpty: t => bool;
    /** [isNotEmpty transient] returns true if [collection] contains at
     *  least one value, otherwise false.
     */

    let removeAll: t => t;
    /** [removeAll transient] removes all values from [transient]. */
  };

  module type S1 = {
    /** TransientCollection module type signature for types with a parametric type arity of 0. */

    type t 'a;

    let count: (t 'a) => int;
    /** [count transient] returns number of values contained in [transient]. */

    let isEmpty: (t 'a) => bool;
    /** [isEmpty transient] returns true if [transient] is empty, otherwise false. */

    let isNotEmpty: (t 'a) => bool;
    /** [isNotEmpty transient] returns true if [collection] contains at
     *  least one value, otherwise false.
     */

    let removeAll: (t 'a) => (t 'a);
    /** [removeAll transient] removes all values from [transient]. */
  };
};

let module SequentialCollection: {
  /** Module types implemented by collections that support sequential access to
   *  the left most contained values. Concrete implementations include [Stack] and [SortedSet].
   *
   *  By contract, all functions must be efficient, with no worst than O(log N) performance.
   */

  module type S = {
    /** SequentialCollection module type signature for types with a parametric type arity of 0. */

    type a;
    type t;

    include Collection.S with type a := a and type t := t;

    let first: t => (option a);
    /** [first collection] returns first element in [collection] or None. */

    let firstOrRaise: t => a;
    /** [firstOrRaise collection] returns the first element in [collection] or throws. */
  };

  module type S1 = {
    /** SequentialCollection module type signature for types with a parametric type arity of 1. */
    type t 'a;

    include Collection.S1 with type t 'a := t 'a;

    let first: (t 'a) => (option 'a);
    /** [first collection] returns first element in [collection] or None. */

    let firstOrRaise: (t 'a) => 'a;
    /** [firstOrRaise collection] returns the first element in [collection] or throws. */
  };
};

let module PersistentSequentialCollection: {
  /** Module types implemented by collections supporting persistent mutations to left
   *  side of the collection.
   *
   *  By contract, all functions must be efficient, with no worst than O(log N) performance.
   */
  module type S1 = {
    /** PersistentSequentialCollection module type signature for types with a parametric type arity of 1. */

    type t 'a;

    include PersistentCollection.S1 with type t 'a := t 'a;
    include ReverseMappable.S1 with type t 'a := t 'a;
    include SequentialCollection.S1 with type t 'a := t 'a;

    let addFirst: 'a => (t 'a) => (t 'a);
    /** [addFirst value collection] returns a PersistentSequentialCollection with [value] prepended. */

    let addFirstAll: (Iterator.t 'a) => (t 'a) => (t 'a);
    /** [addFirstAll iter collection] returns a PersistentSequentialCollection with the values in [iter] prepended. */

    let empty: unit => (t 'a);
    /** [empty ()] return an empty PersistentSequentialCollection. */

    let fromReverse: (Iterator.t 'a) => (t 'a);
    /** [fromReverse iter] returns a PersistentSequentialCollection containing the values in [iter]
     *  in reverse order.
     */

    let return: 'a => (t 'a);
    /** [return value] returns a PersistentSequentialCollection containing a single element, [value]. */

    let removeFirstOrRaise: (t 'a) => (t 'a);
    /** [removeFirstOrRaise collection] returns a PersistentSequentialCollection without
     *  the first value or raises an exception if [collection] is empty.
     */
  };
};

let module TransientSequentialCollection: {
  /** Module types implemented by transient collections supporting transient mutations to left
   *  side of the collection.
   *
   *  By contract, all functions must be efficient, with no worst than O(log N) performance.
   */

  module type S1 = {
    /** TransientSequentialCollection module type signature for types with a parametric type arity of 1. */

    type t 'a;

    include TransientCollection.S1 with type t 'a := t 'a;

    let addFirst: 'a => (t 'a) => (t 'a);
    /** [addFirst value transient] prepends [value] to [transient]. */

    let addFirstAll: (Iterator.t 'a) => (t 'a) => (t 'a);
    /** [addFirstAll iter transient] prepends all values in [iter] to [transient]. */

    let empty: unit => (t 'a);
    /** [empty ()] returns a new empty TransientSequentialCollection. */

    let first: (t 'a) => option 'a;
    /** [first transient] returns first value in [transient] or None. */

    let firstOrRaise: (t 'a) => 'a;
    /** [firstOrRaise transient] returns the first value in [transient] or raises an exception. */

    let removeFirstOrRaise: (t 'a) => (t 'a);
    /** [removeFirstOrRaise transient] removes the first value from [transient] or raises
     *  an exception if [transient] is empty.
     */
  };
};

let module NavigableCollection: {
  /** Module types implemented by collections that support sequential access to
   *  both left and right most contained values. Concrete implementations include [Deque] and [Vector].
   *
   *  By contract, all functions must be efficient, with no worst than O(log N) performance.
   */

  module type S = {
    /** NavigableCollection module type signature for types with a parametric type arity of 0. */

    type a;
    type t;

    include ReduceableRight.S with type a := a and type t := t;
    include SequentialCollection.S with type a := a and type t := t;

    let last: t => (option a);
    /** [last collection] returns last element in [collection] or None. */

    let lastOrRaise: t => a;
    /** [lastOrRaise collection] returns the first element in [collection] or raises an exception. */

    let toIteratorRight: t => (Iterator.t a);
    /* [toIteratorRight collection] returns an Iterator that can be used to iterate over
     * the values in [collection] from right to left.
     */

    let toSequenceRight: t => (Sequence.t a);
    /* [toSequenceRight collection] returns an Sequence that can be used to enumerate
     * the values in [collection] from right to left.
     */
  };

  module type S1 = {
    /** NavigableCollection module type signature for types with a parametric type arity of 1. */

    type t 'a;

    include ReduceableRight.S1 with type t 'a := t 'a;
    include SequentialCollection.S1 with type t 'a := t 'a;

    let last: (t 'a) => (option 'a);
    /** [last collection] returns last element in [collection] or None. */

    let lastOrRaise: (t 'a) => 'a;
    /** [lastOrRaise collection] returns the first element in [collection] or raises an exception. */

    let toIteratorRight: (t 'a) => (Iterator.t 'a);
    /* [toIteratorRight collection] returns an Iterator that can be used to iterate over
     * the values in [collection] from right to left.
     */

    let toSequenceRight: (t 'a) => (Sequence.t 'a);
    /* [toSequenceRight collection] returns an Sequence that can be used to enumerate
     * the values in [collection] from right to left.
     */
  };
};

let module PersistentNavigableCollection: {
  /** Module types implemented by collections supporting persistent mutations to both the left
   *  and right side of the collection.
   *
   *  By contract, all functions must be efficient, with no worst than O(log N) performance.
   */

  module type S1 = {
    /** PersistentNavigableCollection module type signature for types with a parametric type arity of 1. */

    type t 'a;

    include Mappable.S1 with type t 'a := t 'a;
    include NavigableCollection.S1 with type t 'a := t 'a;
    include PersistentSequentialCollection.S1 with type t 'a := t 'a;

    let addLast: 'a => (t 'a) => (t 'a);
    /** [addLast value collection] returns a PersistentNavigableCollection with [value] appended.
     *
     *  Complexity: O(1)
     */

    let addLastAll: (Iterator.t 'a) => (t 'a) => (t 'a);
    /** [addLastAll iter deque] returns a PersistentNavigableCollection with the values in [iter] appended. */

    let from: (Iterator.t 'a) => (t 'a);
    /** [from iter] returns a PersistentNavigableCollection containing the values in [iter].
     *
     * Complexity: O(N) the number of elements in [iter].
     */

    let removeLastOrRaise: (t 'a) => (t 'a);
    /** [removeLastOrRaise collection] returns a PersistentSequentialCollection without
     *  the last value or raises an exception if [collection] is empty.
     */
  };
};

let module TransientNavigableCollection: {
  /** Module types implemented by transient collections supporting transient mutations to both
   *  the left and rights sides of the collection.
   *
   *  By contract, all functions must be efficient, with no worst than O(log N) performance.
   */
  module type S1 = {
    /** TransientNavigableCollection module type signature for types with a parametric type arity of 1. */

    type t 'a;

    include TransientSequentialCollection.S1 with type t 'a := t 'a;

    let addLast: 'a => (t 'a) => (t 'a);
    /** [addLast value transient] appends [value] to [transient].
     *
     *  Complexity: O(1)
     */

    let last: (t 'a) => option 'a;
    /** [tryLast transient] returns the last value in [transient] or None. */

    let lastOrRaise: (t 'a) => 'a;
    /** [last transient] returns the last value in [transient] or raises an exception. */

    let removeLastOrRaise: (t 'a) => (t 'a);
    /** [removeLastOrRaise transient] removes the last value from [transient] or raises
     *  an exception if [transient] is empty.
     */
  };
};

let module rec Set: {
  /** A read only view of a unique Set of values. The intent of this type is to enable
   *  interop between alternative concrete implementations such as SortedSet and HashSet.
   *  The complexity of functions in this module is dependent upon the underlying concrete implementation.
   */

  module type S = {
    /** Set module type signature for types with a parametric type arity of 0. */

    type a;
    type t;

    include Collection.S with type a := a and type t := t;

    let contains: a => t => bool;
    /** [contains value set] returns true if [set] contains at least one instace of [value],
     *  otherwise false;
     *
     *  By contract, an implementation must be efficient, with no worst than O(log N) performance.
     */

    let toSet: t => Set.t a;
    /** [toSet set] returns a Set view of [set]. */
  };

  module type S1 = {
    /** Set module type signature for types with a parametric type arity of 0. */
    type t 'a;

    include Collection.S1 with type t 'a := t 'a;
    include Equatable.S1 with type t 'a := t 'a;

    let contains: 'a => (t 'a) => bool;

    let toSet: (t 'a) => Set.t 'a;
  };

  type t 'a;
  /** The Set type. */

  include S1 with type t 'a := t 'a;

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
  /* Reducer module for Iterators. */
};

let module PersistentSet: {
  /** Module types implemented by Set collections supporting persistent mutations.
   *
   *  By contract, all functions must be efficient, with no worst than O(log N) performance.
   */

  module type S = {
    /** PersistentSet module type signature for types with a parametric type arity of 0. */

    type a;
    type t;

    include Set.S with type a := a and type t := t;
    include PersistentCollection.S with type a := a and type t := t;

    let add: a => t => t;
    /** [add value set] returns a PersistentSet containing value. If [set] already contains [value],
     *  it is returned unmodified.
     */

    let addAll: (Iterator.t a) => t => t;
    /** [addAll iter set] returns a PersistentSet with the values in [iter] and all the values in [set]. */

    let from: (Iterator.t a) => t;
    /** [from iter] returns a PersistentSet with all the values in [iter] */

    let intersect: t => t => t;
    /** [intersect this that] returns a PersistentSet of unique elements
     *  which occur in both [this] and [that].
     */

    let remove: a => t => t;
    /** [remove value set] returns a PersistentSet that does not contain [value].
     *  If [set] does not contain [value], it is returned unmodified.
     */

    let subtract: t => t => t;
    /** [subtract this that] returns an PersistentSet of unique element
     *  which occur in [this] but not in [that].
     */

    let union: t => t => t;
    /** [union this that] returns an PersistentSet of unique elements which occur in either [this] or [that]. */
  };

  module type S1 = {
    /** PersistentSet module type signature for types with a parametric type arity of 1. */

    type t 'a;

    include Set.S1 with type t 'a := t 'a;
    include PersistentCollection.S1 with type t 'a := t 'a;

    let add: 'a => (t 'a) => (t 'a);
    /** [add value set] returns a PersistentSet containing value. If [set] already contains [value],
     *  it is returned unmodified.
     */

    let addAll: (Iterator.t 'a) => (t 'a) => (t 'a);
    /** [addAll iter set] returns a PersistentSet with the values in [iter] and all the values in [set]. */

    let intersect: (t 'a) => (t 'a) => (t 'a);
    /** [intersect this that] returns a PersistentSet of unique elements
     *  which occur in both [this] and [that].
     */

    let remove: 'a => (t 'a) => (t 'a);
    /** [remove value set] returns a PersistentSet that does not contain [value].
     *  If [set] does not contain [value], it is returned unmodified.
     */

    let subtract: (t 'a) => (t 'a) => (t 'a);
    /** [subtract this that] returns an PersistentSet of unique element
     *  which occur in [this] but not in [that].
     */

    let union: (t 'a) => (t 'a) => (t 'a);
    /** [union this that] returns an PersistentSet of unique elements which occur in either [this] or [that]. */
  };
};

let module TransientSet: {
  /** Module types implemented by transiently mutable sets.
   *
   *  By contract, all functions must be efficient, with no worst than O(log N) performance.
   */

  module type S = {
    /** TransientSet module type signature for types with a parametric type arity of 0. */

    type a;
    type t;

    include TransientCollection.S with type a := a and type t := t;

    let add: a => t => t;
    /** [add value transient] adds [value] to [transient]. If [transient] already contains [value],
     *  it is returned unmodified.
     */

    let addAll: (Iterator.t a) => t => t;
    /** [addAll iter transient] adds all values in [iter] to [transient]. */

    let contains: a => t => bool;
    /** [contains value set] returns true if [set] contains at least one instace of [value],
     *  otherwise false;
     */

    let remove: a => t => t;
    /** [remove value transient] removes [value] from [transient].
     *  If [transient] does not contain [value], it is returned unmodified.
     */
  };

  module type S1 = {
    /** TransientSet module type signature for types with a parametric type arity of 0. */

    type t 'a;

    include TransientCollection.S1 with type t 'a := t 'a;

    let add: 'a => (t 'a) => (t 'a);
    /** [add value transient] adds [value] to [transient]. If [transient] already contains [value],
     *  it is returned unmodified.
     */

    let addAll: (Iterator.t 'a) => (t 'a) => (t 'a);
    /** [addAll iter transient] adds all values in [iter] to [transient]. */

    let contains: 'a => (t 'a) => bool;
    /** [contains value set] returns true if [set] contains at least one instace of [value],
     *  otherwise false;
     */

    let remove: 'a => (t 'a) => (t 'a);
    /** [remove value transient] removes [value] from [transient].
     *  If [transient] does not contain [value], it is returned unmodified.
     */
  };
};

let module NavigableSet: {
  /*  Module types implemented by Sets that supports navigation operations. */

  module type S = {
    /** NavigableSet module type signature for types with a parametric type arity of 0. */

    type a;
    type t;

    include Set.S with type a := a and type t := t;
    include NavigableCollection.S with type a := a and type t := t;
  };
};

let module PersistentNavigableSet: {
  /** Module types implemented by NavigableSet collections supporting persistent mutations.
   *
   *  By contract, all functions must be efficient, with no worst than O(log N) performance.
   */

  module type S = {
    /** PersistentNavigableSet module type signature for types with a parametric type arity of 0. */
    type a;
    type t;

    include NavigableSet.S with type a := a and type t := t;
    include PersistentSet.S with type a := a and type t := t;

    let removeFirstOrRaise: t => t;
    /** [removeFirstOrRaise set] returns a PersistentNavigableSet without
     *  the first value or raises an exception if [set] is empty.
     */

    let removeLastOrRaise: t => t;
    /** [removeLastOrRaise set] returns a PersistentNavigableSet without
     *  the last value or raises an exception if [set] is empty.
     */
  };
};

let module KeyedReduceable: {
  /** Module types implemented by modules that support reducing over key/value pairs. */

  module type S1 = {
    /** KeyedReduceable module type signature for types with a parametric type arity of 1. */

    type k;
    type t 'v;

    let reduce: while_::('acc => k => 'v => bool)? => ('acc => k => 'v => 'acc) => 'acc => (t 'v) => 'acc;
    /** [reduce while_::predicate initialValue f reduceable] applies the accumulator
     *  function [f] to each key/value pair in [reduceable], while [predicate] returns true,
     *  accumulating the result.
     */
  };

  module type S2 = {
    /** KeyedReduceable module type signature for types with a parametric type arity of 2. */

    type t 'k 'v;

    let reduce: while_::('acc => 'k => 'v => bool)? => ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
    /** [reduce while_::predicate initialValue f reduceable] applies the accumulator
     *  function [f] to each key/value pair in [reduceable], while [predicate] returns true,
     *  accumulating the result.
     */
  };
};

let module KeyedReduceableRight: {
  /** Module types implemented by modules that support reducing over
   *  key/value pairs in both the left to right, and right to left directions.
   */

  module type S1 = {
    /** KeyedReduceableRight module type signature for types with a parametric type arity of 1. */

    type k;
    type t 'v;

    include KeyedReduceable.S1 with type k := k and type t 'v := t 'v;

    let reduceRight: while_::('acc => k => 'v => bool)? => ('acc => k => 'v => 'acc) => 'acc => (t 'v) => 'acc;
    /** [reduceRight while_::predicate initialValue f reduceable] applies the accumulator
     *  function [f] to each key/value pair in [reduceable] while [predicate] returns true, starting
     *  from the right most key/value pair, accumulating the result.
     */
  };
};

let module KeyedReducer: {
  /** Module functions for generating modules which provide common reduction functions for Reduceables.
   *  All functions are O(N), unless otherwise noted.
   */
  module type S1 = {
    type k;
    type t 'v;

    let count: (t 'v) => int;
    /** [count keyedReduceable] returns the total number key/value pairs produced by [keyedReduceable] */

    let every: (k => 'v => bool) => (t 'v) => bool;
    /** [every f keyedReduceable] returns true if the predicate [f] returns true for all
     *  key/value pairs in [keyedReduceable]. If [keyedReduceable] is empty, returns [true].
     */

    let find: (k => 'v => bool) => (t 'v) => (option (k, 'v));
    /** [find f keyedReduceable] return the Some of the first key/value pair in [keyedReduceable]
     *  for which the the predicate f returns [true]. Otherwise None.
     */

    let findOrRaise: (k => 'v => bool) => (t 'v) => (k, 'v);
    /** [findOrRaise f keyedReduceable] return the the first key/value pair in [keyedReduceable]
     *  for which the the predicate f returns [true]. Otherwise raises an exception.
     */

    let findKey: (k => 'v => bool) => (t 'v) => (option k);
    /** [findKey f keyedReduceable] return the Some of the first key in [keyedReduceable]
     *  for which the the predicate f returns [true]. Otherwise None.
     */

    let findKeyOrRaise: (k => 'v => bool) => (t 'v) => k;
    /** [findOrRaise f keyedReduceable] return the the first key in [keyedReduceable]
     *  for which the the predicate f returns [true]. Otherwise raises an exception.
     */

    let findValue: (k => 'v => bool) => (t 'v) => (option 'v);
    /** [findValue f keyedReduceable] return the Some of the first value in [keyedReduceable]
     *  for which the the predicate f returns [true]. Otherwise None.
     */

    let findValueOrRaise: (k => 'v => bool) => (t 'v) => 'v;
    /** [findOrRaise f keyedReduceable] return the the first value in [keyedReduceable]
     *  for which the the predicate f returns [true]. Otherwise raises an exception.
     */

    let first: (t 'v) => (option (k, 'v));
    /** [first keyedReduceable] returns first key/value pair in [reduceable] or None.
     *
     *  Computational Complexity: O(1)
     */

    let firstOrRaise: (t 'v) => (k, 'v);
    /** [firstOrRaise keyedReduceable] returns the first key/value pair in [keyedReduceable] or raises an exception.
     *
     *  Computational Complexity: O(1)
     */

    let forEach: while_::(k => 'v => bool)? => (k => 'v => unit) => (t 'v) => unit;
    /** [forEach while_::predicate f keyedReduceable] iterates through [keyedReduceable] applying the
     *  side effect function [f] to each key/value pair, while [predicate] returns true
     */

    let none: (k => 'v => bool) => (t 'v) => bool;
    /** [none f keyedReduceable] returns true if the predicate [f] returns false
     *  for all key/value pairs in [keyedReduceable]. If [keyedReduceable] is empty, returns [true].
     */

    let some: (k => 'v => bool) => (t 'v) => bool;
    /** [some f keyedReduceable] returns true if the predicate [f] returns true for at least
     *  one key/value pair in [keyedReduceable]. If [keyedReduceable] is empty, returns [false].
     */
  };

  module type S2 = {
    type t 'k 'v;

    let count: (t 'k 'v) => int;
    /** [count keyedReduceable] returns the total number key/value pairs produced by [keyedReduceable] */

    let every: ('k => 'v => bool) => (t 'k 'v) => bool;
    /** [every f keyedReduceable] returns true if the predicate [f] returns true for all
     *  key/value pairs in [keyedReduceable]. If [keyedReduceable] is empty, returns [true].
     */

    let find: ('k => 'v => bool) => (t 'k 'v) => (option ('k, 'v));
    /** [find f keyedReduceable] return the Some of the first key/value pair in [keyedReduceable]
     *  for which the the predicate f returns [true]. Otherwise None.
     */

    let findOrRaise: ('k => 'v => bool) => (t 'k 'v) => ('k, 'v);
    /** [findOrRaise f keyedReduceable] return the the first key/value pair in [keyedReduceable]
     *  for which the the predicate f returns [true]. Otherwise raises an exception.
     */

    let findKey: ('k => 'v => bool) => (t 'k 'v) => (option 'k);
    /** [findKey f keyedReduceable] return the Some of the first key in [keyedReduceable]
     *  for which the the predicate f returns [true]. Otherwise None.
     */

    let findKeyOrRaise: ('k => 'v => bool) => (t 'k 'v) => 'k;
    /** [findOrRaise f keyedReduceable] return the the first key in [keyedReduceable]
     *  for which the the predicate f returns [true]. Otherwise raises an exception.
     */

    let findValue: ('k => 'v => bool) => (t 'k 'v) => (option 'v);
    /** [findValue f keyedReduceable] return the Some of the first value in [keyedReduceable]
     *  for which the the predicate f returns [true]. Otherwise None.
     */

    let findValueOrRaise: ('k => 'v => bool) => (t 'k 'v) => 'v;
    /** [findOrRaise f keyedReduceable] return the the first value in [keyedReduceable]
     *  for which the the predicate f returns [true]. Otherwise raises an exception.
     */

    let first: (t 'k 'v) => (option ('k, 'v));
    /** [first keyedReduceable] returns first key/value pair in [reduceable] or None.
     *
     *  Computational Complexity: O(1)
     */

    let firstOrRaise: (t 'k 'v) => ('k, 'v);
    /** [firstOrRaise keyedReduceable] returns the first key/value pair in [keyedReduceable] or raises an exception.
     *
     *  Computational Complexity: O(1)
     */

    let forEach: while_::('k => 'v => bool)? => ('k => 'v => unit) => (t 'k 'v) => unit;
    /** [forEach while_::predicate f keyedReduceable] iterates through [keyedReduceable] applying the
     *  side effect function [f] to each key/value pair, while [predicate] returns true
     */

    let none: ('k => 'v => bool) => (t 'k 'v) => bool;
    /** [none f keyedReduceable] returns true if the predicate [f] returns false
     *  for all key/value pairs in [keyedReduceable]. If [keyedReduceable] is empty, returns [true].
     */

    let some: ('k => 'v => bool) => (t 'k 'v) => bool;
    /** [some f keyedReduceable] returns true if the predicate [f] returns true for at least
     *  one key/value pair in [keyedReduceable]. If [keyedReduceable] is empty, returns [false].
     */
  };

  let module Make1: (KeyedReduceable: KeyedReduceable.S1) => S1 with type k = KeyedReduceable.k and type t 'v = KeyedReduceable.t 'v;
  /** Module function to create a KeyedReducer for a specific KeyedReduceable type with a parametric type arity of 1. */

  let module Make2: (KeyedReduceable: KeyedReduceable.S2) => S2 with type t 'k 'v = KeyedReduceable.t 'k 'v;
  /** Module function to create a KeyedReducer for a specific KeyedReduceable type with a parametric type arity of 2. */
};

let module KeyedIterator: {
  type t 'k 'v;

  include KeyedReduceable.S2 with type t 'k 'v := t 'k 'v;

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

let module KeyedIterable: {
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

let module KeyedCollection: {
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

let module PersistentKeyedCollection: {
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

let module TransientKeyedCollection: {
  module type S1 = {
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
     *  Complexity: O(1)
     */
  };

  module type S2 = {
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
     *  Complexity: O(1)
     */
  };
};

let module NavigableKeyedCollection: {
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

let module rec Map: {
  /** A read only view of an underlying set of key/value pairs. The intent of this type is to enable
   *  interop between alternative concrete implementations such as [SortedMap] and [HashMap].
   *  The complexity of functions in this module is dependent upon the underlying concrete implementation.
   */

   module type S1 = {
     type k;
     type t 'v;

     include KeyedCollection.S1 with type k := k and type t 'v := t 'v;

     let get: k => (t 'v) => (option 'v);
     let getOrRaise: k => (t 'v) => 'v;
     let values: (t 'v) => (Iterator.t 'v);
     let toMap: (t 'v) => Map.t k 'v;
   };

   module type S2 = {
     type t 'k 'v;

     include KeyedCollection.S2 with type t 'k 'v := t 'k 'v;

     let get: 'k => (t 'k 'v) => (option 'v);
     let getOrRaise: 'k => (t 'k 'v) => 'v;
     let values: (t 'k 'v) => (Iterator.t 'v);
     let toMap: (t 'k 'v) => Map.t 'k 'v;
   };

  type t 'k 'v;
  /** The map type. */

  include S2 with type t 'k 'v := t 'k 'v;

  let empty: unit => (t 'k 'v);
  /** The empty Map. */

  let map: ('k => 'a => 'b) => (t 'k 'a) => (t 'k 'b);

  let module KeyedReducer: KeyedReducer.S2 with type t 'k 'v := t 'k 'v;
};

let module PersistentMap: {
  module type S1 = {
    type k;
    type t 'v;

    include PersistentKeyedCollection.S1 with type k := k and type t 'v := t 'v;
    include Map.S1 with type k := k and type t 'v := t 'v;

    let alter: k => (option 'v => option 'v) => (t 'v) => (t 'v);

    let empty: unit => (t 'v);
    /** The empty SortedMap using the structural comparator. */

    let from: (KeyedIterator.t k 'v) => (t 'v);
    /** [from iter] returns a SortedMap including the key/value pairs in [iter]
     *  using the structural comparison.
     */

    let fromEntries: (Iterator.t (k, 'v)) => (t 'v);

    let map: (k => 'a => 'b) => (t 'a) => (t 'b);

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

    let map: ('k => 'a => 'b) => (t 'k 'a) => (t 'k 'b);

    let merge: ('k => (option 'vAcc) => (option 'v) => (option 'vAcc)) => (t 'k 'vAcc) => (t 'k 'v) => (t 'k 'vAcc);

    let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);

    let putAll: (KeyedIterator.t 'k 'v) => (t 'k 'v) => (t 'k 'v);

    let putAllEntries: (Iterator.t ('k, 'v)) => (t 'k 'v) => (t 'k 'v);
  };
};

let module TransientMap: {
  module type S1 = {
    type k;

    type t 'v;

    include TransientKeyedCollection.S1 with type k := k and type t 'v := t 'v;

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
     *  Complexity: O(log32 N), effectively O(1)
     */

    let getOrRaise: k => (t 'v) => 'v;

    let put: k => 'v => (t 'v) => (t 'v);
    /** [put key value transient] adds the mapping of [key] to [value] to [transient].
     *
     *  Complexity: O(log32 N), effectively O(1)
     */

    let putAll: (KeyedIterator.t k 'v) => (t 'v) => (t 'v);
    /** [putAll iter transient] adds the key/value pairs in [iter] to [transient].
     *  Key value pairs in [iter] replace existing mappings in [transient].
     */
  };

  module type S2 = {
    type t 'k 'v;

    include TransientKeyedCollection.S2 with type t 'k 'v := t 'k 'v;

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
     *  Complexity: O(log32 N), effectively O(1)
     */

    let getOrRaise: 'k => (t 'k 'v) => 'v;

    let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);
    /** [put key value transient] adds the mapping of [key] to [value] to [transient].
     *
     *  Complexity: O(log32 N), effectively O(1)
     */

    let putAll: (KeyedIterator.t 'k 'v) => (t 'k 'v) => (t 'k 'v);
    /** [putAll iter transient] adds the key/value pairs in [iter] to [transient].
     *  Key value pairs in [iter] replace existing mappings in [transient].
     */
  };
};

let module NavigableMap: {
  module type S1 = {
    type k;
    type t 'v;

    include NavigableKeyedCollection.S1 with type k := k and type t 'v := t 'v;
    include Map.S1 with type k := k and type t 'v := t 'v;
  };
};

let module PersistentNavigableMap: {
  module type S1 = {
    type k;
    type t 'v;

    include NavigableMap.S1 with type k := k and type t 'v := t 'v;
    include PersistentMap.S1 with type k := k and type t 'v := t 'v;

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
};

let module IndexedCollection: {
  module type S1 = {
    type t 'a;

    include NavigableCollection.S1 with type t 'a := t 'a;

    let get: int => (t 'a) => (option 'a);
    /** [tryGet index vec] returns the element at [index] or None if [index] is out of bounds. */

    let getOrRaise: int => (t 'a) => 'a;

    let toKeyedIterator: (t 'a) => (KeyedIterator.t int 'a);

    let toKeyedIteratorRight: (t 'a) => (KeyedIterator.t int 'a);

    let toMap: (t 'a) => (Map.t int 'a);
  };
};

let module IndexedMappable: {
  module type S1 = {
    type t 'a;

    include Mappable.S1 with type t 'a := t 'a;
    include ReverseMappable.S1 with type t 'a := t 'a;

    let mapWithIndex: (int => 'a => 'b) => (t 'a) => (t 'b);
    /** [map f vec] returns a new Vector applying the
     *  function [f] to each index/element pair in [vec].
     */

    let mapReverseWithIndex: (int => 'a => 'b) => (t 'a) => (t 'b);
    /** [mapReverseWithIndex f vec] returns a new Vector applying the
     *  function [f] to each index/element pair in [vec], reversing the result.
     */
  };
};

let module rec Deque: {
  /** A double-ended queue with efficient appends [addLast], prepends [addFirst]
   *  and removals from either end of the queue [removeFirstOrRaise] [removeLastOrRaise].
   */

  type t 'a;
  /** The Deque type. */

  include PersistentNavigableCollection.S1 with type t 'a := t 'a;

  let mutate: (t 'a) => (TransientDeque.t 'a);
  /** [mutate deque] returns a TransientDeque containing the same elements as [deque].
   *
   *  Complexity: O(1)
   */

  let reverse: (t 'a) => (t 'a);
  /** [reverse deque] returns a new Deque with [deque]'s elements reversed.
   *
   *  Complexity: O(1)
   */

  let module ReducerRight: Reducer.S1 with type t 'a := t 'a;
  let module Reducer: Reducer.S1 with type t 'a := t 'a;
}

and TransientDeque: {
  /** A temporarily mutable Deque. Once persisted, any further operations on a
   *  TransientDeque instance will throw. Intended for implementing bulk mutation operations efficiently.
   */

  type t 'a;

  include TransientNavigableCollection.S1 with type t 'a := t 'a;
  /** The TransientDeque type. */

  let persist: (t 'a) => (Deque.t 'a);
  /** [persist transient] returns a persisted Deque. Further attempts to access or mutate [transient]
   *  will throw.
   */

  let reverse: (t 'a) => (t 'a);
  /** [reverse transient] reverse [transient]'s elements.
   *
   *  Complexity: O(1)
   */
};

let module rec HashMap: {
  /** A hashed Map. */

  type t 'k 'v;
  /** The HashMap type. */

  include PersistentMap.S2 with type t 'k 'v := t 'k 'v;

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
   *  Complexity: O(1)
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

  include TransientMap.S2 with type t 'k 'v := t 'k 'v;

  let emptyWith: hash::(Hash.t 'k) => comparator::(Comparator.t 'k) => unit => (TransientHashMap.t 'k 'v);
  let persist: (t 'k 'v) => (HashMap.t 'k 'v);
  /** [persist transient] returns a persisted HashMap. Further attempts to access or mutate [transient]
   *  will throw.
   */
};

let module rec HashSet: {
  /** A set implementation that utilizes hashing and comparison
   *  or equality for collision resolution.
   */

  type t 'a;
  /** The HashSet type. */

  include PersistentSet.S1 with type t 'a := t 'a;
  include Hashable.S1 with type t 'a := t 'a;

  let emptyWith: hash::(Hash.t 'a) => comparator::(Comparator.t 'a) => (HashSet.t 'a);
  let fromWith: hash::(Hash.t 'a) => comparator::(Comparator.t 'a) => (Iterator.t 'a) => (HashSet.t 'a);
  let mutate: (t 'a) => (TransientHashSet.t 'a);
  /** [mutate set] returns a TransientHashSet containing the same elements as [set].
   *
   *  Complexity: O(1)
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

  include TransientSet.S1 with type t 'a := t 'a;

  let emptyWith: hash::(Hash.t 'a) => comparator::(Comparator.t 'a) => unit => (TransientHashSet.t 'a);
  let persist: (t 'a) => (HashSet.t 'a);
  /** [persist transient] returns a persisted HashSet. Further attempts to access or mutate [transient]
   *  will throw.
   */
};

let module rec IntMap: {
  /** A Map optimized for integer keys. */

  type k = int;
  type t 'v;
  /** The IntMap type. */

  include PersistentMap.S1 with type k := k and type t 'v := t 'v;

  let mutate: (t 'v) => (TransientIntMap.t 'v);
  /** [mutate map] returns a TransientIntMap containing the same key/values pairs as [map].
   *
   *  Complexity: O(1)
   */

  let module KeyedReducer: KeyedReducer.S1 with type k = k and type t 'v := t 'v;
}

and TransientIntMap: {
  type k = int;
  type t 'v;

  include TransientMap.S1 with type k := k and type t 'v := t 'v;

  let persist: (t 'v) => (IntMap.t 'v);
  /** [persist transient] returns a persisted HashBiMap. Further attempts to access or mutate [transient]
   *  will throw.
   */
};

let module IntRange: {
  /** Represents a contiguous Set of discrete integers */

  type a = int;
  type t;
  /** The IntRange type.*/

  include NavigableSet.S with type a := a and type t := t;
  include Comparable.S with type t := t;
  include Hashable.S with type t := t;

  let create: start::int => count::int => t;

  let module ReducerRight: Reducer.S with type a := a and type t := t;
  let module Reducer: Reducer.S with type a := a and type t := t;
};

let module rec IntSet: {
  /** A set implementation optimized for storing sparse ints. */

  type a = int;
  type t;
  /** The IntSet type. */

  include PersistentSet.S with type a := a and type t := t;

  let mutate: t => TransientIntSet.t;
  /** [mutate set] returns a TransientIntSet containing the same elements as [set].
   *
   *  Complexity: O(1)
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

  include TransientSet.S with type a := a and type t := t;

  let empty: unit => t;
  /** [empty ()] return a new empty TransientIntSet. */

  let persist: t => IntSet.t;
  /** [persist transient] returns a persisted IntSet. Further attempts to access or mutate [transient]
   *  will throw.
   */
};

let module List: {
  /** OCaml singly-linked list */

  type t 'a = list 'a;
  /** The List type. */

  include Iterable.S1 with type t 'a := t 'a;
  include ReverseMappable.S1 with type t 'a := t 'a;

  let addFirst: 'a => (t 'a) => (t 'a);
  /** [addFirst value list] returns a new List with [value] prepended. */

  let addFirstAll: (Iterator.t 'a) => (t 'a) => (t 'a);
  /** [addFirstAll iter list] returns a new List with the values in [iter] prepended. */

  let empty: unit => (t 'a);
  /** The empty List. */

  let first: t 'a => (option 'a);
  /** [tryFirst seq] returns first element in [seq] or None.
   *
   *  Complexity: O(1)
   */

  let firstOrRaise: t 'a => 'a;
  /** [first seq] returns the first element in [seq] or throws.
   *
   *  Complexity: O(1)
   */

  let fromReverse: (Iterator.t 'a) => (t 'a);
  /** [fromReverse iter] returns a new List containing the values in [iter]
   *  in reverse order.
   *
   * Complexity: O(N) the number of elements in [iter].
   */

  let removeAll: (t 'a) => (t 'a);
  /** [removeAll list] returns the empty List.
   *
   *  Complexity: O(1)
   */

  let removeFirstOrRaise: (t 'a) => (t 'a);
  /** [removeFirstOrRaise list] returns a new List without the first element.
   *
   *  Complexity: O(1)
   */

  let return: 'a => (t 'a);
  /** [return value] returns a new List containing a single element, [value]. */

  let toSequence: (t 'a) => (Sequence.t 'a);
  /** [toSequence list] returns a Sequence of the elements in [list] in order. */

  let module Reducer: Reducer.S1 with type t 'a := t 'a;
};

let module Option: {
  /** OCaml option type. Can be considered a set of zero or one elements.
   *  All operations have a complexity of O(1).
   */

  type t 'a = option 'a;
  /** The Option type. */

  include FlatMappable.S1 with type t 'a := t 'a;
  include Mappable.S1 with type t 'a := t 'a;
  include SequentialCollection.S1 with type t 'a := t 'a;

  let empty: unit => (t 'a);
  /** The empty Option, None. */

  let return: 'a => (t 'a);
  /** [return value] returns [Some value]. */

  let module Reducer: Reducer.S1 with type t 'a := t 'a;
};

let module ReadOnlyArray: {
  /** Opaque wrapper around an underlying array instance that provides read only semantics */

  type t 'a;
  /** The CopyOnWriteArray type. */

  include IndexedCollection.S1 with type t 'a := t 'a;
  include IndexedMappable.S1 with type t 'a := t 'a;

  let empty: unit => t 'a;
  let init: int => (int => 'a) => (t 'a);
  let ofUnsafe: (array 'a) => (t 'a);
  /** [unsafe arr] returns a ReadOnlyArray backed by [arr]. Note, it is the caller's
   *  responsibility to ensure that [arr] is not subsequently mutated.
   *
   *  Complexity: O(1)
   */

  let module ReducerRight: Reducer.S1 with type t 'a := t 'a;
  let module Reducer: Reducer.S1 with type t 'a := t 'a;
};

let module Stack: {
  /** A singly-linked stack with an O(1) count operation. */

  type t 'a;
  /** The Stack type. */

  include PersistentSequentialCollection.S1 with type t 'a := t 'a;

  let fromList: (list 'a) => (t 'a);
  /** [fromList list] returns a Stack backed by [list].
   *
   *  Complexity: O(N)
   */

  let toList: (t 'a) => (list 'a);
  /** [toList stack] returns the underlying List backing the stack */

  let module Reducer: Reducer.S1 with type t 'a := t 'a;
};

let module SortedMap: {
  /** AVL tree based Map. */
  module type S1 = {
    type k;

    type t +'v;
    /** The SortedMap type. */

    include PersistentNavigableMap.S1 with type k := k and type t 'v := t 'v;

    let module KeyedReducerRight: KeyedReducer.S1 with type k := k and type t 'v := t 'v;
    let module KeyedReducer: KeyedReducer.S1 with type k := k and type t 'v := t 'v;
  };

  let module Make1: (Comparable: Comparable.S) => S1 with type k = Comparable.t;
};

let module SortedSet: {
  /** AVL tree based Set implementation. */
  module type S = {
    type a;
    type t;
    /** The SortedSet type */

    include Comparable.S with type t := t;
    include PersistentNavigableSet.S with type a := a and type t := t;

    let module ReducerRight: Reducer.S with type a:= a and type t:= t;
    let module Reducer: Reducer.S with type a := a and type t := t;
  };

  let module Make: (Comparable: Comparable.S) => S with type a = Comparable.t;
};

let module rec Vector: {
  /** An IndexedCollection supporting efficient prepend, appends, indexing, conctentation, and splits.
   */

  type t 'a;
  /** The vector type */

  include PersistentNavigableCollection.S1 with type t 'a := t 'a;
  include IndexedCollection.S1 with type t 'a := t 'a;
  include IndexedMappable.S1 with type t 'a := t 'a;

  let concat: (list (t 'a)) => (t 'a);
  let init: int => (int => 'a) => (t 'a);
  let insertAt: int => 'a => (t 'a) => (t 'a);
  let removeAt: int => (t 'a) => (t 'a);
  let skip: int => (t 'a) => (t 'a);
  let slice: start::int? => end_::int? => (t 'a) => (t 'a);
  let take: int => (t 'a) => (t 'a);
  let update: int => 'a => (t 'a) => (t 'a);
  let updateAll: (int => 'a => 'a) => (t 'a) => (t 'a);
  let updateWith: int => ('a => 'a) => (t 'a) => (t 'a);

  let mutate: (t 'a) => (TransientVector.t 'a);

  let module ReducerRight: Reducer.S1 with type t 'a := t 'a;
  let module Reducer: Reducer.S1 with type t 'a := t 'a;
}

and TransientVector: {
  /** A temporarily mutable Vector. Once persisted, any further operations on a
   *  TransientVector instance will throw. Intended for implementing bulk mutation operations efficiently.
   */
  type t 'a;
  /** The TransientVector type. */

  include TransientNavigableCollection.S1 with type t 'a := t 'a;

  let get: int => (t 'a) => (option 'a);

  let getOrRaise: int => (t 'a) => 'a;

  let insertAt: int => 'a => (t 'a) => (t 'a);
  /** [insertAt index value transient] inserts value into [transient] at [index].
   *
   *  WARNING: Not implemented
   *
   *  Complexity: O(log32 N)
   */

  let removeAt: int => (t 'a) => (t 'a);
  /** [removeAt index transient] removes the element at [index].
   *
   *  WARNING: Not implemented
   *
   *  Complexity: O(log32 N)
   */

  let update: int => 'a => (t 'a) => (t 'a);
  /** [update index value transient] replaces the element at [index] with [value].
   *
   *  Complexity: O(log32 N)
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
   *  Complexity: O(log32 N)
   */

  let persist: (t 'a) => (Vector.t 'a);
  /** [persist transient] returns a persisted Vector. Further attempts to access or mutate [transient]
  *  will throw.
  */
};
