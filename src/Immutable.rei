/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

let module rec Equality: {
  /** Equality functions for common types. */

  type t 'a = 'a => 'a => bool;
  /** The Equality function type.
   * [equals this that] returns [true] if [this] and [that] are equal, otherwise [false].
   */

  let bytes: Equality.t bytes;
  /** Equality for bytes. */

  let char: Equality.t char;
  /** Equality for chars. */

  let int: Equality.t int;
  /** Equality for ints. */

  let int32: Equality.t int32;
  /** Equality for int32s. */

  let int64: Equality.t int64;
  /** Equality for int64s. */

  let nativeInt: Equality.t nativeint;
  /** Equality for nativeInts. */

  let reference: Equality.t 'a;
  /** The reference equality function, analogous to === */

  let string: Equality.t string;
  /** Equality for strings. */
};

let module rec Ordering: {
  /** Represents the absolute ordering of a type when comparing values. */

  type t;

  let equal: Ordering.t;
  let greaterThan: Ordering.t;
  let lessThan: Ordering.t;
};

let module rec Comparator: {
  /** Comparison functions for common types. */

  type t 'a = 'a => 'a => Ordering.t;
  /** The Comparator function type.
   *  By definition a [compare this that] returns:
   *    [Ordering.greaterThan] if [this] is greater than [that],
   *    [Ordering.lessThan] if [this] is less than [that],
   *    otherwise [Ordering.equals].
   */

  let bytes: Comparator.t bytes;
  /** Compares bytes. */

  let char: Comparator.t char;
  /** Compares chars. */

  let int: Comparator.t int;
  /** Compares ints. */

  let int32: Comparator.t int32;
  /** Compares int32s. */

  let int64: Comparator.t int64;
  /** Compares int64s. */

  let nativeInt: Comparator.t nativeint;
  /** Compares nativeInts. */

  let string: Comparator.t string;
  /** Compares strings. */

  let toEquality: (Comparator.t 'a) => (Equality.t 'a);
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
};

let module rec Streamable: {
  /** Module types implemented by modules that support lazily evaluated
   *  stream functions. All functions defined in this module are O(1).
   */

  module type S1 = {
    /** Streamable module type signature for types with a parametric type arity of 1. */

    type t 'a;

    let buffer: count::int => skip::int => (t 'a) => (t (list 'a));
    /** [buffer count skip stream] returns a Streamable that collects values from [stream]
     *  into list buffers of size [count], skipping [skip] number of values in between the
     *  creation of new buffers. The returned buffers are guaranteed to be of size [count],
     *  and values are dropped if [stream] completes before filling the last buffer.
     */

    let concat: (list (t 'a)) => (t 'a);
    /** [concat streams] returns a Streamable that lazily concatenates all the
     *  Streamables in [streams]. The resulting Streamable returns all the values
     *  in the first Streamable, followed by all the values in the second Streamable,
     *  and continues until the last Streamable completes.
     */

    let defer: (unit => t 'a) => (t 'a);
    /** [defer f] returns a Streamable that invokes the function [f] whenever enumerated. */

    let distinctUntilChangedWith: (Equality.t 'a) => (t 'a) => (t 'a);
    /** [distinctUntilChangedWith equals stream] returns a Streamable that contains only
     *  distinct contiguous values from [stream] using [equals] to equate values.
     */

    let doOnNext: ('a => unit) => (t 'a) => (t 'a);
    /** [doOnNext f stream] returns a Streamable that applies the side effect
     *  function [f] to each value in the stream as they are enumerated.
     */

    let filter: ('a => bool) => (t 'a) => (t 'a);
    /** [filter f stream] returns a Streamable only including values from [stream]
     *  for which application of the predicate function [f] returns true.
     */

    let flatMap: ('a => t 'b) => (t 'a) => (t 'b);
    /** [flatMap mapper stream] applies the mapper to each value in
     *  [stream], flattening the resulting Streams into a new Stream.
     */

    let flatten: (t (t 'a)) => (t 'a);
    /** [flatten stream] flattens the nested values in [streams] into
     *  a new [stream].
     */

    let generate: ('a => 'a) => 'a => (t 'a);
    /** [generate f initialValue] generates the infinite Streamable [x, f(x), f(f(x)), ...] */

    let map: ('a => 'b) => (t 'a) => (t 'b);
    /** [map f stream] Returns a Stream whose values are the result of
     *  applying the function [f] to each value in [stream].
     */

    let return: 'a => (t 'a);
    /** [return value] returns a single value Streamable containing [value]. */

    let scan: ('acc => 'a => 'acc) => 'acc => (t 'a) => (t 'acc);
    /** [scan f acc stream] returns a Streamable of accumulated values resulting from the
     *  application of the accumulator function [f] to each value in [stream] with the
     *  specified initial value [acc].
     */

    let skip: int => (t 'a) => (t 'a);
    /** [skip count stream] return a Streamable which skips the first [count]
     *  values in [stream].
     */

    let skipWhile: ('a => bool) => (t 'a) => (t 'a);
    /** [skipWhile f stream] return a Streamable which skips values in [stream]
     *  while application of the predicate function [f] returns true, and then returns
     *  the remaining values.
     */

    let startWith: 'a => (t 'a) => (t 'a);
    /** [startWith value stream] returns a Streamable whose first
     *  value is [value], followed by the values in [stream].
     */

    let take: int => (t 'a) => (t 'a);
    /** [take count stream] returns a Streamable with the first [count]
     *  values in [stream].
     */

    let takeWhile: ('a => bool) => (t 'a) => (t 'a);
    /** [takeWhile f stream] returns a Streamable including all values in [stream]
     *  while application of the predicate function [f] returns true, then completes.
     */
  };
};

let module rec Iterable: {
  /** Functional iterators over a collection of values. Iterables are stateless and can be reused. */

  module type S = {
    /** Iterable module type signature for types with a parametric type arity of 0. */

    type a;
    type t;

    let reduce: while_::('acc => a => bool)? => ('acc => a => 'acc) => 'acc => t => 'acc;
    /** [reduce while_::predicate initialValue f iterable] applies the accumulator
     *  function [f] to each value in [iterable], while [predicate] returns true,
     *  accumulating the result.
     */

    let toIterable: t => (Iterable.t a);
    /** [toIterable iterable] returns an Iterable that can be used to iterate over
     * the values in [iterable].
     */
  };

  module type S1 = {
    /** Iterable module type signature for types with a parametric type arity of 1. */

    type t 'a;

    let reduce: while_::('acc => 'a => bool)? => ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
    /** [reduce while_::predicate initialValue f iterable] applies the accumulator
     *  function [f] to each value in [iterable], while [predicate] returns true,
     *  accumulating the result.
     */

    let toIterable: t 'a => (Iterable.t 'a);
    /** [toIterable iterable] returns an Iterable that can be used to iterate over
     * the values in [iterable].
     */
  };

  type t 'a;

  include Streamable.S1 with type t 'a := Iterable.t 'a;
  include S1 with type t 'a := Iterable.t 'a;

  let count: t 'a => int;
  /** [count iterable] returns the total number values produced by [iterable] */

  let empty: unit => (Iterable.t 'a);
  /** Returns an empty Iterable. */

  let every: ('a => bool) => (t 'a) => bool;
  /** [every f iterable] returns true if the predicate [f] returns true for all values in [iterable].
   *  If [iterable] is empty, returns [true].
   */

  let find: ('a => bool) => (t 'a) => (option 'a);
  /** [find f iterable] return the Some of the first value in [iterable] for which the
   *  the predicate f returns [true]. Otherwise None.
   */

  let findOrRaise: ('a => bool) => (t 'a) => 'a;
  /** [findOrRaise f iterable] return the the first value in [iterable] for which the
   *  the predicate f returns [true]. Otherwise raises an exception.
   */

  let first: t 'a => (option 'a);
  /** [first iterable] returns first value in [iterable] or None.
   *
   *  Computational Complexity: O(1)
   */

  let firstOrRaise: t 'a => 'a;
  /** [firstOrRaise iterable] returns the first value in [iterable] or raises an exception.
   *
   *  Computational Complexity: O(1)
   */

  let forEach: while_::('a => bool)? => ('a => unit) => (t 'a) => unit;
  /** [forEach while_::predicate f iterable] iterates through [iterable] applying the
   *  side effect function [f] to each value, while [predicate] returns true
   */

  let none: ('a => bool) => (t 'a) => bool;
  /** [none f iterable] returns true if the predicate [f] returns false for all values in [iterable].
   *  If [iterable] is empty, returns [true].
   */

  let some: ('a => bool) => (t 'a) => bool;
  /** [some f iterable] returns true if the predicate [f] returns true for at
   *  least one value in [iterable]. If [iterable] is empty, returns [false].
   */
};

let module rec Sequence: {
  /** Functional pull based sequences. Sequences are generally lazy, computing values as
   *  they are pulled. Sequences are reusable and are guaranteed to produce the
   *  same values, in the same order every time they are enumerated. In addition, Sequences
   *  support eager seeking and zipping. These are their main advantage over Iterables.
   *  In general, only use Sequences when you require support for one or both of these features.
   *  Otherwise Iterables are generally more efficient.
   */

  type t 'a;
  /** The Sequence type. */

  include Iterable.S1 with type t 'a := t 'a;
  include Streamable.S1 with type t 'a := t 'a;

  let empty: unit => (Sequence.t 'a);
  /** Returns an empty Sequence. */

  let seek: int => (Sequence.t 'a) => (Sequence.t 'a);
  /** [seek count seq] scans forward [count] values in [seq]. It is the eagerly
   *  evaluated equivalent of [skip count seq].
   *
   *  Computational complexity: O(count).
   */

  let seekWhile: ('a => bool) => (Sequence.t 'a) => (Sequence.t 'a);
  /** [seekWhile f seq] scans forward through [seq] while application of
   *  the predicate function [f] returns true. It is the eagerly evaluated
   *  equivalent of [skipWhile f seq].
   *
   *  Computational complexity: O(N).
   */

  let zip: (list (Sequence.t 'a)) => (Sequence.t (list 'a));
  /** [zip seqs] returns a Sequence which lazily zips a list of [Sequence]s
   *  into a single Sequence of lists. Values are produce until any Sequence
   *  in [seqs] completes.
   */

  let zip2With: ('a => 'b => 'c) => (Sequence.t 'a) => (Sequence.t 'b) => (Sequence.t 'c);
  /** [zip2With zipper first second] returns a Sequence which lazily zips two Sequences,
   *  combining their values using [zipper]. Values are produce until either [first]
   *  or [second] completes.
   */

  let zip3With: ('a => 'b => 'c => 'd) => (Sequence.t 'a) => (Sequence.t 'b) => (Sequence.t 'c) => (Sequence.t 'd);
  /** [zip3With zipper first second third] returns a Sequence which lazily zips three Sequences,
   *  combining their values using [zipper]. Values are produce until either [first], [second]
   *  or [third] complete.
   */

  let zipLongest: (list (Sequence.t 'a)) => (Sequence.t (list (option 'a)));
  /** [zipLongest seqs] returns a Sequence which zips a list of Sequences
   *  into a single of Sequence of lists. Values are produce until all Sequences
   *  in [seqs] complete.
   */

  let zipLongest2With:
    (option 'a => option 'b => 'c) =>
    (Sequence.t 'a) =>
    (Sequence.t 'b) =>
    (Sequence.t 'c);
  /** [zipLongest2With zipper first second] returns a Sequence which lazily zips two Sequences,
   *  combining their values using [zipper]. Values are produce until both [first]
   *  and [second] complete.
   */

  let zipLongest3With:
    (option 'a => option 'b => option 'c => 'd) =>
    (Sequence.t 'a) =>
    (Sequence.t 'b) =>
    (Sequence.t 'c) =>
    (Sequence.t 'd);
  /** [zipLongest3With zipper first second third] returns a Sequence which lazily
   *  zips three Sequences, combining their values using [zipper]. Values are produce
   *  until [first], [second] and [third] all complete.
   */
};

let module rec Collection: {
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

    let toCollection: t => (Collection.t a);

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

    let toCollection: (t 'a) => (Collection.t 'a);

    let toSequence: (t 'a) => (Sequence.t 'a);
    /** [toSequence collection] returns a Sequence that can be used to enumerate the collection. */
  };

  type t 'a;

  include S1 with type t 'a := Collection.t 'a;

  let empty: unit => (Collection.t 'a);
  /** [empty ()] returns the empty collection */

  let module Persistent: {
    /** Module types implemented by collections supporting fully persistent mutations.
     *  Mutation operations on these types do not mutate the underlying collection, but instead
     *  create a new collection, with the mutation applied.

     *  By contract, all functions have a computational complexity of O(1).
     */

    module type S = {
      /** Persistent Collection module type signature for types with a parametric type arity of 0. */

      type a;
      type t;

      include S with type a := a and type t := t;

      let removeAll: t => t;
      /** [removeAll collection] return an empty Persistent Collection. Depending on the implementation,
       *  the new collection may share the same configuration as [collection]. For instance, the HashSet
       *  implementations shares the same hash and comparison functions.
       */
    };

    module type S1 = {
      /** PersistentCollection module type signature for types with a parametric type arity of 1. */

      type t 'a;

      include S1 with type t 'a := t 'a;

      let removeAll: t 'a => t 'a;
      /** [removeAll collection] return an empty Persistent Collection. Depending on the implementation,
       *  the new collection may share the same configuration as [collection]. For instance, HashSet
       *  implementations shares the same hash and comparison functions.
       */
    };
  };

  let module Transient: {
    /** Module types implemented by transiently mutable Collections. Transient collections
     *  are designed to enable fast and efficient batch operations by temporarily enabling mutation
     *  of an underlying collection type. Unlike Persistent Collection functions, Transient Collection
     *  APIs always return the same value reference passed in as an argument, with mutations applied.
     *
     *  By contract, all functions have a computational complexity of O(1).
     */

    module type S = {
      /** Transient Collection module type signature for types with a parametric type arity of 0. */

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
      /** Transient Collection module type signature for types with a parametric type arity of 0. */

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
};

let module rec SequentialCollection: {
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
    /** [first collection] returns first value in [collection] or None. */

    let firstOrRaise: t => a;
    /** [firstOrRaise collection] returns the first value in [collection] or throws. */

    let toSequentialCollection: t => (SequentialCollection.t a);
  };

  module type S1 = {
    /** SequentialCollection module type signature for types with a parametric type arity of 1. */
    type t 'a;

    include Collection.S1 with type t 'a := t 'a;

    let first: (t 'a) => (option 'a);
    /** [first collection] returns first value in [collection] or None. */

    let firstOrRaise: (t 'a) => 'a;
    /** [firstOrRaise collection] returns the first value in [collection] or throws. */

    let toSequentialCollection: (t 'a) => (SequentialCollection.t 'a);
  };

  type t 'a;

  include S1 with type t 'a := SequentialCollection.t 'a;

  let empty: unit => (SequentialCollection.t 'a);
  /** [empty ()] returns the empty SequentialCollection */

  let module Persistent: {
    /** Module types implemented by collections supporting persistent mutations to left
     *  side of the collection.
     *
     *  By contract, all functions must be efficient, with no worst than O(log N) performance.
     */
    module type S1 = {
      /** SequentialCollection.Persistent module type signature for types with a parametric type arity of 1. */

      type t 'a;

      include Collection.Persistent.S1 with type t 'a := t 'a;
      include S1 with type t 'a := t 'a;

      let addFirst: 'a => (t 'a) => (t 'a);
      /** [addFirst value collection] returns a SequentialCollection.Persistent with [value] prepended. */

      let addFirstAll: (Iterable.t 'a) => (t 'a) => (t 'a);
      /** [addFirstAll iter collection] returns a SequentialCollection.Persistent with the values in [iter] prepended. */

      let empty: unit => (t 'a);
      /** [empty ()] return an empty SequentialCollection.Persistent. */

      let fromReverse: (Iterable.t 'a) => (t 'a);
      /** [fromReverse iter] returns a SequentialCollection.Persistent containing the values in [iter]
       *  in reverse order.
       */

      let return: 'a => (t 'a);
      /** [return value] returns a SequentialCollection.Persistent containing a single value, [value]. */

      let removeFirstOrRaise: (t 'a) => (t 'a);
      /** [removeFirstOrRaise collection] returns a SequentialCollection.Persistent without
       *  the first value or raises an exception if [collection] is empty.
       */
    };
  };

  let module Transient: {
    /** Module types implemented by transient collections supporting transient mutations to left
     *  side of the collection.
     *
     *  By contract, all functions must be efficient, with no worst than O(log N) performance.
     */

    module type S1 = {
      /** SequentialCollection.Transient module type signature for types with a parametric type arity of 1. */

      type t 'a;

      include Collection.Transient.S1 with type t 'a := t 'a;

      let addFirst: 'a => (t 'a) => (t 'a);
      /** [addFirst value transient] prepends [value] to [transient]. */

      let addFirstAll: (Iterable.t 'a) => (t 'a) => (t 'a);
      /** [addFirstAll iter transient] prepends all values in [iter] to [transient]. */

      let empty: unit => (t 'a);
      /** [empty ()] returns a new empty SequentialCollection.Transient. */

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
};

let module rec NavigableCollection: {
  /** Module types implemented by Collections that are ordered or sorted and support
   *  navigation operations.
   *
   *  By contract, all functions must be efficient, with no worst than O(log N) performance.
   */

  module type S = {
    /** NavigableCollection module type signature for types with a parametric type arity of 0. */

    type a;
    type t;

    include SequentialCollection.S with type a := a and type t := t;

    let last: t => (option a);
    /** [last collection] returns last value in [collection] or None.
     *
     *  By contract, implementations are efficient with no worst than O(log N) performance.
     */

    let lastOrRaise: t => a;
    /** [lastOrRaise collection] returns the last value in [collection] or raises an exception.
     *
     *  By contract, implementations are efficient with no worst than O(log N) performance.
     */

    let reduceReversed: while_::('acc => a => bool)? => ('acc => a => 'acc) => 'acc => t => 'acc;
    /** [reduceReversed while_::predicate initialValue f iterable] applies the accumulator
     *  function [f] to each value in [iterable] while [predicate] returns true, starting
     *  from the right most value, accumulating the result.
     */

    let toIterableReversed: t => (Iterable.t a);

    let toNavigableCollection: t => (NavigableCollection.t a);

    let toSequenceReversed: t => (Sequence.t a);
    /* [toSequenceReversed collection] returns an Sequence that can be used to enumerate
     * the values in [collection] from right to left.
     */
  };

  module type S1 = {
    /** NavigableCollection module type signature for types with a parametric type arity of 1. */

    type t 'a;

    include SequentialCollection.S1 with type t 'a := t 'a;

    let last: (t 'a) => (option 'a);
    /** [last collection] returns last value in [collection] or None. */

    let lastOrRaise: (t 'a) => 'a;
    /** [lastOrRaise collection] returns the first value in [collection] or raises an exception. */

    let reduceReversed: while_::('acc => 'a => bool)? => ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
    /** [reduceReversed while_::predicate initialValue f iterable] applies the accumulator
     *  function [f] to each value in [iterable] while [predicate] returns true, starting
     *  from the right most value, accumulating the result.
     */

    let toIterableReversed: t 'a => (Iterable.t 'a);

    let toNavigableCollection: (t 'a) => (NavigableCollection.t 'a);

    let toSequenceReversed: (t 'a) => (Sequence.t 'a);
    /* [toSequenceReversed collection] returns an Sequence that can be used to enumerate
     * the values in [collection] from right to left.
     */
  };

  type t 'a;

  include S1 with type t 'a := NavigableCollection.t 'a;

  let empty: unit => (NavigableCollection.t 'a);
  /** [empty ()] returns the empty NavigableCollection */

  let module Persistent: {
    /** Module types implemented by collections supporting persistent mutations to both the left
     *  and right side of the collection.
     *
     *  By contract, all functions must be efficient, with no worst than O(log N) performance.
     */

    module type S1 = {
      /** PersistentNavigableCollection module type signature for types with a parametric type arity of 1. */

      type t 'a;

      include S1 with type t 'a := t 'a;
      include SequentialCollection.Persistent.S1 with type t 'a := t 'a;

      let addLast: 'a => (t 'a) => (t 'a);
      /** [addLast value collection] returns a Persistent NavigableCollection with [value] appended.
       *
       *  Complexity: O(1)
       */

      let addLastAll: (Iterable.t 'a) => (t 'a) => (t 'a);
      /** [addLastAll iter collection] returns a Persistent NavigableCollection with the values in [iter] appended. */

      let from: (Iterable.t 'a) => (t 'a);
      /** [from iter] returns a Persistent NavigableCollection containing the values in [iter].
       *
       * Complexity: O(N) the number of values in [iter].
       */

      let removeLastOrRaise: (t 'a) => (t 'a);
      /** [removeLastOrRaise collection] returns a SequentialCollection.Persistent without
       *  the last value or raises an exception if [collection] is empty.
       */
    };
  };

  let module Transient: {
    /** Module types implemented by transient collections supporting transient mutations to both
     *  the left and rights sides of the collection.
     *
     *  By contract, all functions must be efficient, with no worst than O(log N) performance.
     */
    module type S1 = {
      /** Transient NavigableCollection module type signature for types with a parametric type arity of 1. */

      type t 'a;

      include SequentialCollection.Transient.S1 with type t 'a := t 'a;

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

    let contains: 'a => (t 'a) => bool;
    /** [contains value set] returns true if [set] contains at least one instace of [value],
     *  otherwise false;
     *
     *  By contract, an implementation must be efficient, with no worst than O(log N) performance.
     */

    let equals: Equality.t (t 'a);
    /** An equality function for Set.S1 instances. */

    let toSet: (t 'a) => Set.t 'a;
    /** [toSet set] returns a Set view of [set]. */
  };

  type t 'a;
  /** The Set type. */

  include S1 with type t 'a := Set.t 'a;

  let empty: unit => (Set.t 'a);
  /** The empty Set. */

  let intersect: (Set.t 'a) => (Set.t 'a) => (Iterable.t 'a);
  /** [intersect this that] returns an Iterable of unique values
   *  which occur in both [this] and [that].
   */

  let subtract: (Set.t 'a) => (Set.t 'a) => (Iterable.t 'a);
  /** [subtract this that] returns an Iterable of unique value
   *  which occur in [this] but not in [that].
   */

  let union: (Set.t 'a) => (Set.t 'a) => (Iterable.t 'a);
  /** [union this that] returns an Iterable of unique values which occur in either [this] or [that]. */

  let module Persistent: {
    /** Module types implemented by Set collections supporting persistent mutations.
     *
     *  By contract, all functions must be efficient, with no worst than O(log N) performance.
     */

    module type S = {
      /** Persistent Set module type signature for types with a parametric type arity of 0. */

      type a;
      type t;

      include S with type a := a and type t := t;
      include Collection.Persistent.S with type a := a and type t := t;

      let add: a => t => t;
      /** [add value set] returns a Persistent Set containing value. If [set] already contains [value],
       *  it is returned unmodified.
       */

      let addAll: (Iterable.t a) => t => t;
      /** [addAll iter set] returns a Persistent Set with the values in [iter] and all the values in [set]. */

      let from: (Iterable.t a) => t;
      /** [from iter] returns a Persistent Set with all the values in [iter] */

      let intersect: t => t => t;
      /** [intersect this that] returns a Persistent Set of unique values
       *  which occur in both [this] and [that].
       */

      let remove: a => t => t;
      /** [remove value set] returns a Persistent Set that does not contain [value].
       *  If [set] does not contain [value], it is returned unmodified.
       */

      let subtract: t => t => t;
      /** [subtract this that] returns an Persistent Set of unique value
       *  which occur in [this] but not in [that].
       */

      let union: t => t => t;
      /** [union this that] returns an Persistent Set of unique values which occur in either [this] or [that]. */
    };

    module type S1 = {
      /** Persistent Set module type signature for types with a parametric type arity of 1. */

      type t 'a;

      include S1 with type t 'a := t 'a;
      include Collection.Persistent.S1 with type t 'a := t 'a;

      let add: 'a => (t 'a) => (t 'a);
      /** [add value set] returns a Persistent Set containing value. If [set] already contains [value],
       *  it is returned unmodified.
       */

      let addAll: (Iterable.t 'a) => (t 'a) => (t 'a);
      /** [addAll iter set] returns a Persistent Set with the values in [iter] and all the values in [set]. */

      let intersect: (t 'a) => (t 'a) => (t 'a);
      /** [intersect this that] returns a Persistent Set of unique values
       *  which occur in both [this] and [that].
       */

      let remove: 'a => (t 'a) => (t 'a);
      /** [remove value set] returns a Persistent Set that does not contain [value].
       *  If [set] does not contain [value], it is returned unmodified.
       */

      let subtract: (t 'a) => (t 'a) => (t 'a);
      /** [subtract this that] returns an Persistent Set of unique values
       *  which occur in [this] but not in [that].
       */

      let union: (t 'a) => (t 'a) => (t 'a);
      /** [union this that] returns an Persistent Set of unique values which occur in either [this] or [that]. */
    };
  };

  let module Transient: {
    /** Module types implemented by transiently mutable sets.
     *
     *  By contract, all functions must be efficient, with no worst than O(log N) performance.
     */

    module type S = {
      /** Transient Set module type signature for types with a parametric type arity of 0. */

      type a;
      type t;

      include Collection.Transient.S with type a := a and type t := t;

      let add: a => t => t;
      /** [add value transient] adds [value] to [transient]. If [transient] already contains [value],
       *  it is returned unmodified.
       */

      let addAll: (Iterable.t a) => t => t;
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
      /** Transient Set module type signature for types with a parametric type arity of 0. */

      type t 'a;

      include Collection.Transient.S1 with type t 'a := t 'a;

      let add: 'a => (t 'a) => (t 'a);
      /** [add value transient] adds [value] to [transient]. If [transient] already contains [value],
       *  it is returned unmodified.
       */

      let addAll: (Iterable.t 'a) => (t 'a) => (t 'a);
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
};

let module rec NavigableSet: {
  /*  Module types implemented by Sets that supports navigation operations. */

  module type S = {
    /** NavigableSet module type signature for types with a parametric type arity of 0. */

    type a;
    type t;

    include Set.S with type a := a and type t := t;
    include NavigableCollection.S with type a := a and type t := t;

    let toNavigableSet: t => NavigableSet.t a;
    /** [toNavigableSet set] returns a NavigableSet view of [set]. */
  };

  module type S1 = {
    /** NavigableSet module type signature for types with a parametric type arity of 1. */

    type t 'a;

    include Set.S1 with type t 'a := t 'a;
    include NavigableCollection.S1 with type t 'a := t 'a;

    let toNavigableSet: (t 'a) => NavigableSet.t 'a;
    /** [toNavigableSet set] returns a NavigableSet view of [set]. */
  };

  type t 'a;
  /** The Set type. */

  include S1 with type t 'a := NavigableSet.t 'a;

  let empty: unit => (NavigableSet.t 'a);
  /** The empty Set. */

  let module Persistent: {
    /** Module types implemented by NavigableSet collections supporting persistent mutations.
     *
     *  By contract, all functions must be efficient, with no worst than O(log N) performance.
     */

    module type S = {
      /** Persistent NavigableSet module type signature for types with a parametric type arity of 0. */
      type a;
      type t;

      include S with type a := a and type t := t;
      include Set.Persistent.S with type a := a and type t := t;

      let removeFirstOrRaise: t => t;
      /** [removeFirstOrRaise set] returns a Persistent NavigableSet without
       *  the first value or raises an exception if [set] is empty.
       */

      let removeLastOrRaise: t => t;
      /** [removeLastOrRaise set] returns a Persistent NavigableSet without
       *  the last value or raises an exception if [set] is empty.
       */
    };
  };
};

let module KeyedStreamable: {
  module type S2 = {
    type t 'k 'v;

    let concat: (list (t 'k 'v)) => (t 'k 'v);
    /** [concat stream] returns a KeyedStreamable that lazily concatenates all the
     *  KeyedStreamables in [stream]. The resulting KeyedStreamable returns all the key/value pairs
     *  in the first KeyedStreamable, followed by all the key/value pairs in the second KeyedStreamable,
     *  and continues until the last KeyedStreamable completes.
     */

    let defer: (unit => t 'k 'v) => (t 'k 'v);
    /** [defer f] returns a KeyedStreamable that invokes the function [f] whenever iterated. */

    let distinctUntilChangedWith: keyEquals::(Equality.t 'k) => valueEquals::(Equality.t 'v) => (t 'k 'v) => (t 'k 'v);
    /** [distinctUntilChangedWith equals stream] returns a KeyedStreamable that contains only
     *  distinct contiguous key/value pairs from [stream] using [keyEquals] and [valueEquals] to
     *  equate key/value pairs.
     */

    let doOnNext: ('k => 'v => unit) => (t 'k 'v) => (t 'k 'v);
    /** [doOnNext f stream] returns a KeyedStreamable that applies the side effect
     *  function [f] to each key/value pair they are iterated.
     */

    let filter: ('k => 'v => bool) => (t 'k 'v) => (t 'k 'v);
    /** [filter f stream] returns a KeyedStreamable only including key/value pairs from [stream]
     *  for which application of the predicate function [f] returns true.
     */

    let flatMap: ('kA => 'vA => t 'kB 'vB) => (t 'kA 'vA) => (t 'kB 'vB);
    /** [flatMap mapper stream] returns a KeyedStreamable which applies [mapper] to each value in
     *  [stream], flattening the results.
     */

    let generate: genKey::('k => 'v => 'k) => genValue::('k => 'v => 'v) => 'k => 'v => (t 'k 'v);
    /** [generate genKey gen value k v] generates an infinite KeyedStreamable
     *  where the keys are [k, genKey(k v), genKey(genKey(k v), v1), ...]
     *  and values are [v, genValue(k, v), genValue(k1, genValue(k, v)), ...]
     */

    let map: keyMapper::('kA => 'vA => 'kB) => valueMapper::('kA => 'vA => 'vB) => (t 'kA 'vA) => (t 'kB 'vB);
    /** [map keyMapper::keyMapper valueMapper::valueMapper stream] returns a KeyedStreamable
     *  whose keys are the result applying [keyMapper] to each key, and whose values are the result
     *  of applying [valueMapper] to each value in [stream].
     */

    let mapKeys: ('a => 'v => 'b) => (t 'a 'v) => (t 'b 'v);
    /** [mapKeys mapper stream] returns a KeyedStreamable with mapper applied
     *  to each key in [stream].
     */

    let mapValues: ('k => 'a => 'b) => (t 'k 'a) => (t 'k 'b);
    /** [mapValues mapper stream] returns a KeyedStreamable with mapper applied
     *  to each value in [stream].
     */

    let return: 'k => 'v => (t 'k 'v);
    /** [return key value] returns a KeyedStreamable containing the pair ([key], [value]). */

    let skip: int => (t 'k 'v) => (t 'k 'v);
    /** [skip count stream] return a KeyedStreamable which skips the first [count]
     *  values in [stream].
     */

    let skipWhile: ('k => 'v => bool) => (t 'k 'v) => (t 'k 'v);
    /** [skipWhile f stream] return a KeyedStreamable which skips key/value pairs in [stream]
     *  while application of the predicate function [f] returns true, and then returns
     *  the remaining values.
     */

    let startWith: 'k => 'v => (t 'k 'v) => (t 'k 'v);
    /** [startWith key value stream] returns a KeyedStreamable whose first
     *  pair is (key, value), followed by the key/value pairs in [stream].
     */

    let take: int => (t 'k 'v) => (t 'k 'v);
    /** [take count stream] returns a KeyedStreamable with the first [count]
     *  key/value pairs in [stream].
     */

    let takeWhile: ('k => 'v => bool) => (t 'k 'v) => (t 'k 'v);
    /** [takeWhile f stream] returns a KeyedStreamable including all values in [stream]
     *  while application of the predicate function [f] returns true, then completes.
     */
  };
};

let module rec KeyedIterable: {
  /** Functional iterators over a collection of key/value pairs. KeyedIterable are stateless and can be reused. */

  module type S1 = {
    /** KeyedIterable module type signature for types with a parametric type arity of 1. */

    type k;
    type t 'v;

    let keys: (t 'v) => (Iterable.t k);
    /** [keys keyedIter] returns an Iterable view of the keys in [keyedIter] */

    let reduce: while_::('acc => k => 'v => bool)? => ('acc => k => 'v => 'acc) => 'acc => (t 'v) => 'acc;
    /** [reduce while_::predicate initialValue f keyedIterable] applies the accumulator
     *  function [f] to each key/value pair in [keyedIterable], while [predicate] returns true,
     *  accumulating the result.
     */

    let toIterable: t 'v => Iterable.t (k, 'v);
    /** [toIterable keyedIterable] returns an Iterable that can be used to iterate over
     *  the key/value pairs in [keyedIterable] as tuples.
     */

    let toKeyedIterable: t 'v => KeyedIterable.t k 'v;
    /** [toKeyedIterable keyedIterable] returns a KeyedIterable that can be used to iterate over
     *  the key/value pairs in [keyedIterable].
     */

    let values: (t 'v) => Iterable.t 'v;
    /** [values keyedIter] returns an Iterable view of the values in [keyedIter] */
  };

  module type S2 = {
    /** KeyedIterable module type signature for types with a parametric type arity of 2. */

    type t 'k 'v;

    let keys: (t 'k 'v) => (Iterable.t 'k);
    /** [keys keyedIter] returns an Iterable view of the keys in [keyedIter] */

    let reduce: while_::('acc => 'k => 'v => bool)? => ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
    /** [reduce while_::predicate initialValue f keyedIterable] applies the accumulator
     *  function [f] to each key/value pair in [keyedIterable], while [predicate] returns true,
     *  accumulating the result.
     */

    let toIterable: t 'k 'v => Iterable.t ('k, 'v);
    /** [toIterable keyedIterable] returns an Iterable that can be used to iterate over
     *  the key/value pairs in [keyedIterable] as tuples.
     */

    let toKeyedIterable: t 'k 'v => KeyedIterable.t 'k 'v;
    /** [toKeyedIterable keyedIterable] returns a KeyedIterable that can be used to iterate over
     *  the key/value pairs in [keyedIterable].
     */

    let values: (t 'k 'v) => Iterable.t 'v;
    /** [values keyedIter] returns an Iterable view of the values in [keyedIter] */
  };

  type t 'k 'v;

  include KeyedStreamable.S2 with type t 'k 'v := KeyedIterable.t 'k 'v;
  include S2 with type t 'k 'v := KeyedIterable.t 'k 'v;

  let count: (t 'k 'v) => int;
  /** [count keyedIterable] returns the total number key/value pairs produced by [keyedIterable] */

  let empty: unit => (KeyedIterable.t 'k 'v);
  /** The empty KeyedCollection. */

  let every: ('k => 'v => bool) => (t 'k 'v) => bool;
  /** [every f keyedIterable] returns true if the predicate [f] returns true for all
   *  key/value pairs in [keyedIterable]. If [keyedIterable] is empty, returns [true].
   */

  let find: ('k => 'v => bool) => (t 'k 'v) => (option ('k, 'v));
  /** [find f keyedIterable] return the Some of the first key/value pair in [keyedIterable]
   *  for which the the predicate f returns [true]. Otherwise None.
   */

  let findOrRaise: ('k => 'v => bool) => (t 'k 'v) => ('k, 'v);
  /** [findOrRaise f keyedIterable] return the the first key/value pair in [keyedIterable]
   *  for which the the predicate f returns [true]. Otherwise raises an exception.
   */

  let findKey: ('k => 'v => bool) => (t 'k 'v) => (option 'k);
  /** [findKey f keyedIterable] return the Some of the first key in [keyedIterable]
   *  for which the the predicate f returns [true]. Otherwise None.
   */

  let findKeyOrRaise: ('k => 'v => bool) => (t 'k 'v) => 'k;
  /** [findOrRaise f keyedIterable] return the the first key in [keyedIterable]
   *  for which the the predicate f returns [true]. Otherwise raises an exception.
   */

  let findValue: ('k => 'v => bool) => (t 'k 'v) => (option 'v);
  /** [findValue f keyedIterable] return the Some of the first value in [keyedIterable]
   *  for which the the predicate f returns [true]. Otherwise None.
   */

  let findValueOrRaise: ('k => 'v => bool) => (t 'k 'v) => 'v;
  /** [findOrRaise f keyedIterable] return the the first value in [keyedIterable]
   *  for which the the predicate f returns [true]. Otherwise raises an exception.
   */

  let first: (t 'k 'v) => (option ('k, 'v));
  /** [first keyedIterable] returns first key/value pair in [keyedIterable] or None.
   *
   *  Computational Complexity: O(1)
   */

  let firstOrRaise: (t 'k 'v) => ('k, 'v);
  /** [firstOrRaise keyedIterable] returns the first key/value pair in [keyedIterable] or raises an exception.
   *
   *  Computational Complexity: O(1)
   */

  let forEach: while_::('k => 'v => bool)? => ('k => 'v => unit) => (t 'k 'v) => unit;
  /** [forEach while_::predicate f keyedIterable] iterates through [keyedIterable] applying the
   *  side effect function [f] to each key/value pair, while [predicate] returns true
   */

  let fromEntries: Iterable.t ('k, 'v) => (KeyedIterable.t 'k 'v);
  /** [fromEntries iter] returns a KeyedIterable view of key/value tuples in [iter]. */

  let none: ('k => 'v => bool) => (t 'k 'v) => bool;
  /** [none f keyedIterable] returns true if the predicate [f] returns false
   *  for all key/value pairs in [keyedIterable]. If [keyedIterable] is empty, returns [true].
   */

  let scan: ('acc => 'k => 'v => 'acc) => 'acc => (KeyedIterable.t 'k 'v) => (Iterable.t 'acc);
  /** [scan f acc stream] returns a KeyedStreamable of accumulated values resulting from the
   *  application of the accumulator function [f] to each value in [stream] with the
   *  specified initial value [acc].
   */

  let some: ('k => 'v => bool) => (t 'k 'v) => bool;
  /** [some f keyedIterable] returns true if the predicate [f] returns true for at least
   *  one key/value pair in [keyedIterable]. If [keyedIterable] is empty, returns [false].
   */
};

let module rec KeyedCollection: {
  /** Module types implemented by all immutable keyed collections. This module
   *  signature does not impose any restrictions on the relationship between
   *  keys and associated values.
   *
   *  By contract, all functions have a computational complexity of O(1),
   *  unless otherwise noted.
   */

  module type S1 = {
     /** KeyedCollection module type signature for types with a parametric type arity of 1. */

    type k;
    type t 'v;

    include KeyedIterable.S1 with type k := k and type t 'v := t 'v;

    let containsKey: k => t 'v => bool;
    /** [containsKey key keyed] returns true if [keyed] contains an association from [key] to
     *  one or more values, otherwise false.
     *
     *  By contract, [containsKey] is efficient with no worst than O(log N) performance.
     */

    let count: t 'v => int;
    /** [count keyed] returns number of key/value pairs contained in [keyed]. */

    let isEmpty: (t 'v) => bool;
    /** [isEmpty keyed] returns true if [keyed] is empty, otherwise false. */

    let isNotEmpty: (t 'v) => bool;
    /** [isNotEmpty keyed] returns true if [keyed] contains at
     *  least one value, otherwise false.
     */

    let toKeyedCollection: (t 'v) => (KeyedCollection.t k 'v);
    /* [toKeyedCollection keyed] returns KeyedCollection view. */

    let toSequence: (t 'v) => (Sequence.t (k, 'v));
    /* [toSequence keyed] returns an Sequence that can be used to enumerate
     * the key/value pairs in [keyed] as tuples.
     */
  };

  module type S2 = {
    /** KeyedCollection module type signature for types with a parametric type arity of 2. */

    type t 'k 'v;

    include KeyedIterable.S2 with type t 'k 'v := t 'k 'v;

    let containsKey: 'k => t 'k 'v => bool;
    /** [containsKey key keyed] returns true if [keyed] contains an association from [key] to
     *  one or more values, otherwise false.
     *
     *  By contract, [containsKey] is efficient with no worst than O(log N) performance.
     */

    let count: t 'k 'v => int;
    /** [count keyed] returns number of key/value pairs contained in [keyed]. */

    let isEmpty: (t 'k 'v) => bool;
    /** [isEmpty keyed] returns true if [keyed] is empty, otherwise false. */

    let isNotEmpty: (t 'k 'v) => bool;
    /** [isNotEmpty keyed] returns true if [keyed] contains at
     *  least one value, otherwise false.
     */

    let toKeyedCollection: (t 'k 'v) => (KeyedCollection.t 'k 'v);
    /* [toKeyedCollection keyed] returns KeyedCollection view. */

    let toSequence: (t 'k 'v) => (Sequence.t ('k, 'v));
    /* [toSequence keyed] returns an Sequence that can be used to enumerate
     * the key/value pairs in [keyed] as tuples.
     */
  };

  type t 'k 'v;

  include S2 with type t 'k 'v := KeyedCollection.t 'k 'v;

  let empty: unit => (KeyedCollection.t 'k 'v);
  /** The empty KeyedCollection. */

  let module Persistent: {
    /** Module types implemented by KeyedCollections supporting fully persistent mutations.
     *  Mutation operations on these types do not mutate the underlying collection, but instead
     *  create a new collection with the mutation applied.
     */

    module type S1 = {
      /** Persistent KeyedCollection module type signature for types with a parametric type arity of 1. */

      type k;
      type t 'v;

      include S1 with type k := k and type t 'v := t 'v;

      let remove: k => (t 'v) => (t 'v);
      /** [remove key keyed] removes all values associated with [key] from [keyed]
       *
       *  By contract, [remove] is efficient with no worst than O(log N) performance.
       */

      let removeAll: (t 'v) => (t 'v);
      /** [removeAll keyed] return an empty Persistent KeyedCollection. Depending on the implementation,
       *  the new Persistent KeyedCollection may share the same configuration as [keyed]. For instance,
       *  the HashMap implementation shares the same hash and comparison functions.
       *
       *  Computational complexity: O(1)
       */
    };

    module type S2 = {
      /** Persistent KeyedCollection module type signature for types with a parametric type arity of 2. */

      type t 'k 'v;

      include S2 with  type t 'k 'v := t 'k 'v;

      let remove: 'k => (t 'k 'v) => (t 'k 'v);
      /** [remove key keyed] removes all values associated with [key] from [keyed]
       *
       *  By contract, [remove] is efficient with no worst than O(log N) performance.
       */

      let removeAll: (t 'k 'v) => (t 'k 'v);
      /** [removeAll keyed] return an empty Persistent KeyedCollection. Depending on the implementation,
       *  the new Persistent KeyedCollection may share the same configuration as [keyed]. For instance,
       *  the HashMap implementation shares the same hash and comparison functions.
       *
       *  Computational complexity: O(1)
       */
    };
  };

  let module Transient: {
    /** Module types implemented by transiently mutable KeyedCollections. Transient Collections
    *  are designed to enable fast and efficient batch operations by temporarily enabling mutation
    *  of an underlying collection type. Unlike Persistent KeyedCollection functions, Transient KeyedCollection
    *  APIs always return the same value reference passed in as an argument, with mutations applied.
    *
    *  By contract, all functions have a computational complexity of O(1), unless otherwise noted.
    */

    module type S1 = {
      /** Transient KeyedCollection module type signature for types with a parametric type arity of 1. */

      type k;
      type t 'v;

      let containsKey: k => (t 'v) => bool;
      /** [containsKey key transient] returns true if [transient] contains an association from [key] to
       *  one or more values, otherwise false.
       *
       *  By contract, [containsKey] is efficient with no worst than O(log N) performance.
       */

      let count: (t 'v) => int;
      /** [count transient] returns number of key/value pairs contained in [transient]. */

      let isEmpty: (t 'v) => bool;
      /** [isEmpty transient] returns true if [transient] is empty, otherwise false. */

      let isNotEmpty: (t 'v) => bool;
      /** [isNotEmpty transient] returns true if [transient] contains at
       *  least one value, otherwise false.
       */

      let remove: k => (t 'v) => (t 'v);
      /** [remove key transient] removes all values associated with [key] from [transient]
       *
       *  By contract, [remove] is efficient with no worst than O(log N) performance.
       */

      let removeAll: (t 'v) => (t 'v);
      /** [removeAll transient] removes all key/value pairs from [transient]. */
    };

    module type S2 = {
      /** Transient KeyedCollection module type signature for types with a parametric type arity of 1. */

      type t 'k 'v;

      let containsKey: 'k => (t 'k 'v) => bool;
      /** [containsKey key transient] returns true if [transient] contains an association from [key] to
       *  one or more values, otherwise false.
       *
       *  By contract, [containsKey] is efficient with no worst than O(log N) performance.
       */

      let count: (t 'k 'v) => int;
      /** [count transient] returns number of key/value pairs contained in [transient]. */

      let isEmpty: (t 'k 'v) => bool;
      /** [isEmpty transient] returns true if [transient] is empty, otherwise false. */

      let isNotEmpty: (t 'k 'v) => bool;
      /** [isNotEmpty transient] returns true if [transient] contains at
       *  least one value, otherwise false.
       */

      let remove: 'k => (t 'k 'v) => (t 'k 'v);
      /** [remove key transient] removes all values associated with [key] from [transient]
       *
       *  By contract, [remove] is efficient with no worst than O(log N) performance.
       */

      let removeAll: (t 'k 'v) => (t 'k 'v);
      /** [removeAll transient] removes all key/value pairs from [transient]. */
    };
  };
};

let module rec NavigableKeyedCollection: {
  /** Module types implemented by KeyedCollections that are ordered or sorted and support
   *  navigation operations.
   */

  module type S1 = {
    /** NavigableKeyedCollection module type signature for types with a parametric type arity of 1. */

    type k;
    type t 'v;

    include KeyedCollection.S1 with type k := k and type t 'v := t 'v;

    let first: (t 'v) => (option (k, 'v));
    /** [first keyed] returns first value in [keyed] or None.
     *
     *  By contract, no worst than O(log N) performance.
     */

    let firstOrRaise: (t 'v) => (k, 'v);
    /** [firstOrRaise keyed] returns first value in [keyed] or raises an exception.
     *
     *  By contract, no worst than O(log N) performance.
     */

    let firstKey: (t 'v) => (option k);

    let firstKeyOrRaise: (t 'v) => k;

    let firstValue: (t 'v) => (option 'v);

    let firstValueOrRaise: (t 'v) => 'v;

    let keysReversed: (t 'v) => Iterable.t k;

    let last: (t 'v) => (option (k, 'v));
    /** [last keyed] returns last value in [keyed] or None.
     *
     *  By contract, no worst than O(log N) performance.
     */

    let lastOrRaise: (t 'v) => (k, 'v);
    /** [lastOrRaise keyed] returns last value in [keyed] or raises an exception.
     *
     *  By contract, no worst than O(log N) performance.
     */

    let lastKey: (t 'v) => (option k);

    let lastKeyOrRaise: (t 'v) => k;

    let lastValue: (t 'v) => (option 'v);

    let lastValueOrRaise: (t 'v) => 'v;

    let reduceReversed: while_::('acc => k => 'v => bool)? => ('acc => k => 'v => 'acc) => 'acc => (t 'v) => 'acc;
    /** [reduceReversed while_::predicate initialValue f keyedIterable] applies the accumulator
     *  function [f] to each key/value pair in [keyedIterable] while [predicate] returns true, starting
     *  from the right most key/value pair, accumulating the result.
     */

    let toIterableReversed: t 'v => Iterable.t (k, 'v);
    /** [toIterableReversed keyedIterable] returns an Iterable that can be used to iterate over
     *  the key/value pairs in [keyedIterable] as tuples.
     */

    let toKeyedIterableReversed: t 'v => KeyedIterable.t k 'v;
    /** [toKeyedIterableReversed keyedIterable] returns a KeyedIterable that can be used to iterate over
     *  the key/value pairs in [keyedIterable].
     */

    let toNavigableKeyedCollection: t 'v => NavigableKeyedCollection.t k 'v;

    let toSequenceReversed: (t 'v) => (Sequence.t (k, 'v));
    /* [toSequenceReversed keyed] returns an Sequence that can be used to enumerate
     * the key/value pairs in [keyed] as tuples from right to left.
     */

    let valuesReversed: (t 'v) => Iterable.t 'v;
  };

  module type S2 = {
    /** NavigableKeyedCollection module type signature for types with a parametric type arity of 2. */

    type t 'k 'v;

    include KeyedCollection.S2 with type t 'k 'v := t 'k 'v;

    let first: (t 'k 'v) => (option ('k, 'v));
    /** [first keyed] returns first value in [keyed] or None.
     *
     *  By contract, no worst than O(log N) performance.
     */

    let firstOrRaise: (t 'k 'v) => ('k, 'v);
    /** [firstOrRaise keyed] returns first value in [keyed] or raises an exception.
     *
     *  By contract, no worst than O(log N) performance.
     */

    let firstKey: (t 'k 'v) => (option 'k);

    let firstKeyOrRaise: (t 'k 'v) => 'k;

    let firstValue: (t 'k 'v) => (option 'v);

    let firstValueOrRaise: (t 'k 'v) => 'v;

    let keysReversed: (t 'k 'v) => Iterable.t 'k;

    let last: (t 'k 'v) => (option ('k, 'v));
    /** [last keyed] returns last value in [keyed] or None.
     *
     *  By contract, no worst than O(log N) performance.
     */

    let lastOrRaise: (t 'k 'v) => ('k, 'v);
    /** [lastOrRaise keyed] returns last value in [keyed] or raises an exception.
     *
     *  By contract, no worst than O(log N) performance.
     */

    let lastKey: (t 'k 'v) => (option 'k);

    let lastKeyOrRaise: (t 'k 'v) => 'k;

    let lastValue: (t 'k 'v) => (option 'v);

    let lastValueOrRaise: (t 'k 'v) => 'v;

    let reduceReversed: while_::('acc => 'k => 'v => bool)? => ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
    /** [reduceReversed while_::predicate initialValue f keyedIterable] applies the accumulator
     *  function [f] to each key/value pair in [keyedIterable] while [predicate] returns true, starting
     *  from the right most key/value pair, accumulating the result.
     */

    let toIterableReversed: t 'k 'v => Iterable.t ('k, 'v);
    /** [toIterableReversed keyedIterable] returns an Iterable that can be used to iterate over
     *  the key/value pairs in [keyedIterable] as tuples.
     */

    let toKeyedIterableReversed: t 'k 'v => KeyedIterable.t 'k 'v;
    /** [toKeyedIterableReversed keyedIterable] returns a KeyedIterable that can be used to iterate over
     *  the key/value pairs in [keyedIterable].
     */

    let toNavigableKeyedCollection: t 'k 'v => NavigableKeyedCollection.t 'k 'v;

    let toSequenceReversed: (t 'k 'v) => (Sequence.t ('k, 'v));
    /* [toSequenceReversed keyed] returns an Sequence that can be used to enumerate
     * the key/value pairs in [keyed] as tuples from right to left.
     */

    let valuesReversed: (t 'k 'v) => Iterable.t 'v;
  };

  type t 'k 'v;

  include S2 with type t 'k 'v := NavigableKeyedCollection.t 'k 'v;

  let empty: unit => (NavigableKeyedCollection.t 'k 'v);
  /** The empty Map. */
};

let module rec Map: {
  /** A read only view of a mappings keys to values. The intent of this type is to enable
   *  interop between alternative concrete implementations such as SortedMap and HashMap.
   *  The complexity of functions in this module is dependent upon the underlying concrete implementation.
   */

   module type S1 = {
     /** Map module type signature for types with a parametric type arity of 1. */

     type k;
     type t 'v;

     include KeyedCollection.S1 with type k := k and type t 'v := t 'v;

     let get: k => (t 'v) => (option 'v);
     /** [get key map] returns the value associated with [key] in [map] or None */

     let getOrRaise: k => (t 'v) => 'v;
     /** [getOrRaise key map] returns the value associated with [key] in [map] or raises an exception */

     let keySet: (t 'v) => (Set.t k);
     /** [keys keyed] return a Set view of the keys in [keyed]. */

     let toMap: (t 'v) => Map.t k 'v;
     /** [toMap map] returns a Map view of [map] */
   };

   module type S2 = {
     /** Map module type signature for types with a parametric type arity of 1. */

     type t 'k 'v;

     include KeyedCollection.S2 with type t 'k 'v := t 'k 'v;

     let get: 'k => (t 'k 'v) => (option 'v);
     /** [get key map] returns the value associated with [key] in [map] or None */

     let getOrRaise: 'k => (t 'k 'v) => 'v;
     /** [getOrRaise key map] returns the value associated with [key] in [map] or raises an exception */

     let keySet: (t 'k 'v) => (Set.t 'k);
     /** [keys keyed] return a Set view of the keys in [keyed]. */

     let toMap: (t 'k 'v) => Map.t 'k 'v;
     /** [toMap map] returns a Map view of [map] */
   };

  type t 'k 'v;
  /** The map type. */

  include S2 with type t 'k 'v := Map.t 'k 'v;

  let empty: unit => (Map.t 'k 'v);
  /** The empty Map. */

  let module Persistent: {
    /** Module types implemented by Map collections supporting persistent mutations.
     *
     */

    module type S1 = {
      /** Persistent Map module type signature for types with a parametric type arity of 1. */

      type k;
      type t 'v;

      include KeyedCollection.Persistent.S1 with type k := k and type t 'v := t 'v;
      include S1 with type k := k and type t 'v := t 'v;

      let alter: k => (option 'v => option 'v) => (t 'v) => (t 'v);
      /** [alter key f map] return a Persistent Map applying the mutation function to the value
       *  associated with key or None if no association exists. If [f] returns Some, the value
       *  associated with key is either added or updated. If [f] returns None,
       *  the value associated with key is removed if an association previously existed.
       *
       *  By contract, [alter] is efficient with no worst than O(log N) performance.
       */

      let empty: unit => (t 'v);
      /** [empty ()] Return an empty Persistent Map. */

      let from: (KeyedIterable.t k 'v) => (t 'v);
      /** [from keyedIterable] returns a Persistent Map including the key/value pairs in [keyedIterable].
       *
       *  By contract, [from] is efficient with no worst than O(N log N) performance.
       */

      let fromEntries: (Iterable.t (k, 'v)) => (t 'v);
      /** [fromEntries iter] returns a Persistent Map including the key/value pairs in [iter].
       *
       *  By contract, [fromEntries] is efficient with no worst than O(N log N) performance.
       */

      let merge: (k => (option 'vAcc) => (option 'v) => (option 'vAcc)) => (t 'vAcc) => (t 'v) => (t 'vAcc);
      /** [merge f acc next] return a Persistent Map that is the result of reducing [acc] with [next].
       *  The callback [f] is applied to the union of keys from [acc] and [next], with the values
       *  associated with each key, or None. If [f] returns None, the associated key/value pair is
       *  removed from the accumulator. If [f] returns Some, the associated key/value pair is
       *  added or update to the accumulator.
       *
       *  By contract, [merge] is efficient with no worst than O(N log N) performance.
       */

      let put: k => 'v => (t 'v) => (t 'v);
      /** [put key value map] returns a Persistent Map with an association
       *  from [key] to [value] added [map].
       *
       *  By contract, [put] is efficient with no worst than O(log N) performance.
       */

      let putAll: (KeyedIterable.t k 'v) => (t 'v) => (t 'v);
      /** [putAll keyedIter map] returns a Persistent Map, adding associations from all key/value pairs
       *  in [keyedIter] to [map],
       *
       *  By contract, [putAll] is efficient with no worst than O(N log N) performance.
       */

      let putAllEntries: (Iterable.t (k, 'v)) => (t 'v) => (t 'v);
      /** [putAllEntries iter map] returns a Persistent Map, adding associations from all key/value pairs
       *  in [iter] to [map].
       *
       *  By contract, [putAllEntries] is efficient with no worst than O(N log N) performance.
       */
    };

    module type S2 = {
      /** Persistent Map module type signature for types with a parametric type arity of 1. */

      type t 'k 'v;

      include KeyedCollection.Persistent.S2 with type t 'k 'v := t 'k 'v;
      include S2 with type t 'k 'v := t 'k 'v;

      let alter: 'k => (option 'v => option 'v) => (t 'k 'v) => (t 'k 'v);
      /** [alter key f map] return a Persistent Map applying the mutation function to the value
       *  associated with key or None if no association exists. If [f] returns Some, the value
       *  associated with key is either added or updated. If [f] returns None,
       *  the value associated with key is removed if an association previously existed.
       *
       *  By contract, [alter] is efficient with no worst than O(log N) performance.
       */

      let merge: ('k => (option 'vAcc) => (option 'v) => (option 'vAcc)) => (t 'k 'vAcc) => (t 'k 'v) => (t 'k 'vAcc);
      /** [merge f acc next] return a Persistent Map that is the result of reducing [acc] with [next].
       *  The callback [f] is applied to the union of keys from [acc] and [next], with the values
       *  associated with each key, or None. If [f] returns None, the associated key/value pair is
       *  removed from the accumulator. If [f] returns Some, the associated key/value pair is
       *  added or update to the accumulator.
       *
       *  By contract, [merge] is efficient with no worst than O(N log N) performance.
       */

      let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);
      /** [put key value map] returns a Persistent Map with an association
       *  from [key] to [value] added [map].
       *
       *  By contract, [put] is efficient with no worst than O(log N) performance.
       */

      let putAll: (KeyedIterable.t 'k 'v) => (t 'k 'v) => (t 'k 'v);
      /** [putAll keyedIter map] returns a Persistent Map, adding associations from all key/value pairs
       *  in [keyedIter] to [map],
       *
       *  By contract, [putAll] is efficient with no worst than O(N log N) performance.
       */

      let putAllEntries: (Iterable.t ('k, 'v)) => (t 'k 'v) => (t 'k 'v);
      /** [putAllEntries iter map] returns a Persistent Map, adding associations from all key/value pairs
       *  in [iter] to [map].
       *
       *  By contract, [putAllEntries] is efficient with no worst than O(N log N) performance.
       */
    };
  };

  let module Transient: {
    /** Module types implemented by transiently mutable maps. */

    module type S1 = {
      /** Transient Map module type signature for types with a parametric type arity of 1. */

      type k;
      type t 'v;

      include KeyedCollection.Transient.S1 with type k := k and type t 'v := t 'v;

      let alter: k => (option 'v => option 'v) => (t 'v) => (t 'v);
      /** [alter key f transient] applies the mutation function to the value
       *  associated with key or None if no association exists. If [f] returns Some, the value
       *  associated with key is either added or updated to [transient]. If [f] returns None,
       *  the value associated with key is removed if an association previously existed in [transient].
       *
       *  By contract, [alter] is efficient with no worst than O(log N) performance.
       */

      let empty: unit => (t 'v);
      /** [empty ()] returns a new empty Transient Map. */

      let get: k => (t 'v) => (option 'v);
      /** [get key transient] returns the value associated with [key] in [transient] or None */

      let getOrRaise: k => (t 'v) => 'v;
      /** [getOrRaise key transient] returns the value associated with [key] in [transient] or raises an exception */

      let put: k => 'v => (t 'v) => (t 'v);
      /** [put key value transient] adds or replaces an association [key] to [value] in [transient].
       *
       *  By contract, [put] is efficient with no worst than O(log N) performance.
       */

      let putAll: (KeyedIterable.t k 'v) => (t 'v) => (t 'v);
      /** [putAll keyedIter transient] adds associations from all key/value pairs in [keyedIter] to [transient].
       *
       *  By contract, [putAll] is efficient with no worst than O(N log N) performance.
       */

      let putAllEntries: (Iterable.t (k, 'v)) => (t 'v) => (t 'v);
      /** [putAll keyedIter transient] adds associations from all key/value pairs in [keyedIter] to [transient].
       *
       *  By contract, [putAll] is efficient with no worst than O(N log N) performance.
       */
    };

    module type S2 = {
      type t 'k 'v;

      include KeyedCollection.Transient.S2 with type t 'k 'v := t 'k 'v;

      let alter: 'k => (option 'v => option 'v) => (t 'k 'v) => (t 'k 'v);
      /** [alter key f transient] applies the mutation function to the value
       *  associated with key or None if no association exists. If [f] returns Some, the value
       *  associated with key is either added or updated to [transient]. If [f] returns None,
       *  the value associated with key is removed if an association previously existed in [transient].
       *
       *  By contract, [alter] is efficient with no worst than O(log N) performance.
       */

      let get: 'k => (t 'k 'v) => (option 'v);
      /** [get key transient] returns the value associated with [key] in [transient] or None */

      let getOrRaise: 'k => (t 'k 'v) => 'v;
      /** [getOrRaise key transient] returns the value associated with [key] in [transient] or raises an exception */

      let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);
      /** [put key value transient] adds or replaces an association [key] to [value] in [transient].
       *
       *  By contract, [put] is efficient with no worst than O(log N) performance.
       */

      let putAll: (KeyedIterable.t 'k 'v) => (t 'k 'v) => (t 'k 'v);
      /** [putAll keyedIter transient] adds associations from all key/value pairs in [keyedIter] to [transient].
       *
       *  By contract, [putAll] is efficient with no worst than O(N log N) performance.
       */

      let putAllEntries: (Iterable.t ('k, 'v)) => (t 'k 'v) => (t 'k 'v);
      /** [putAll keyedIter transient] adds associations from all key/value pairs in [keyedIter] to [transient].
       *
       *  By contract, [putAll] is efficient with no worst than O(N log N) performance.
       */
    };
  };
};

let module rec NavigableMap: {
  /*  Module types implemented by NavigableMap that supports navigation operations. */

  module type S1 = {
    /** NavigableMap module type signature for types with a parametric type arity of 1. */

    type k;
    type t 'v;

    include NavigableKeyedCollection.S1 with type k := k and type t 'v := t 'v;
    include Map.S1 with type k := k and type t 'v := t 'v;

    let navigableKeySet: (t 'v) => (NavigableSet.t k);
    /** [keys keyed] return a NavigableSet view of the keys in [keyed]. */

    let toNavigableMap: (t 'v) => NavigableMap.t k 'v;
  };

  module type S2 = {
    /** NavigableMap module type signature for types with a parametric type arity of 1. */

    type t 'k 'v;

    include NavigableKeyedCollection.S2 with type t 'k 'v := t 'k 'v;
    include Map.S2 with type t 'k 'v := t 'k 'v;

    let navigableKeySet: (t 'k 'v) => (NavigableSet.t 'k);
    /** [keys keyed] return a NavigableSet view of the keys in [keyed]. */

    let toNavigableMap: (t 'k 'v) => NavigableMap.t 'k 'v;
  };

  type t 'k 'v;
  /** The map type. */

  include S2 with type t 'k 'v := NavigableMap.t 'k 'v;

  let module Persistent: {
    /** Module types implemented by NavigableMaps supporting persistent mutations.
     *
     *  By contract, all functions must be efficient, with no worst than O(log N) performance.
     */

    module type S1 = {
      /** Persistent NavigableMap module type signature for types with a parametric type arity of 1. */

      type k;
      type t 'v;

      include S1 with type k := k and type t 'v := t 'v;
      include Map.Persistent.S1 with type k := k and type t 'v := t 'v;

      let removeFirstOrRaise: (t 'v) => (t 'v);
      /** [removeFirstOrRaise map] returns a Persistent NavigableMap without
       *  the first value or raises an exception if [map] is empty.
       */

      let removeLastOrRaise: (t 'v) => (t 'v);
      /** [removeLastOrRaise map] returns a Persistent NavigableMap without
       *  the last value or raises an exception if [map] is empty.
       */
    };
  };
};

let module rec Indexed: {
  /** Collections that support efficient indexed access to values.
   *
   *  By contract, all functions must be efficient, with no worst than O(log N) performance.
   */

  module type S1 = {
    /** Indexed module type signature for types with a parametric type arity of 1. */

    type t 'a;

    include NavigableCollection.S1 with type t 'a := t 'a;

    let get: int => (t 'a) => (option 'a);
    /** [get index indexed] returns the value at [index] or None if [index] is out of bounds. */

    let getOrRaise: int => (t 'a) => 'a;
    /** [getOrRaise index indexed] returns the value at [index] or
     *  raises an exception if [index] is out of bounds.
     */

    let toKeyedCollection: (t 'a) => (KeyedCollection.t int 'a);
    /** [toKeyedCollection indexed] returns a KeyedCollection view of
     *  the index/value pairs in [indexed].
     */

    let toKeyedIterable: (t 'a) => (KeyedIterable.t int 'a);
    /** [toKeyedIterable indexed] returns a KeyedIterable that can be used to iterate over
     *  the index/value pairs in [indexed].
     */

    let toKeyedIterableReversed: (t 'a) => (KeyedIterable.t int 'a);
    /* [toKeyedIterableReversed indexed] returns an KeyedIterable that can be used to iterate over
     * the index/value pairs in [indexed] from right to left.
     */

    let toMap: (t 'a) => (Map.t int 'a);
    /** [toMap indexed] returns a Map view of [indexed]. */

    let toNavigableKeyedCollection: (t 'a) => (NavigableKeyedCollection.t int 'a);

    let toNavigableMap: (t 'a) => (NavigableMap.t int 'a);
  };

  type t 'a;

  include S1 with type t 'a := Indexed.t 'a;

  let module Persistent: {
    /** An Indexed collection supporting persistent mutations. */

    module type S1 = {
      type t 'a;

      include S1 with type t 'a := t 'a;
      include NavigableCollection.Persistent.S1 with type t 'a := t 'a;

      let concat: (list (t 'a)) => (t 'a);
      /** [concat indexed] returns a persistent Indexed collection concatenating
       *  together the in [indexed].
       */

      let insertAt: int => 'a => (t 'a) => (t 'a);
      /** [insertAt index value indexed] returns a persistent Indexed collection with
       *  [value] inserted at [index].
       */

      let removeAt: int => (t 'a) => (t 'a);
      /** [removeAt index indexed] returns a persistent Indexed collection with the
       *  value at [index] removed.
       */

      let skip: int => (t 'a) => (t 'a);
      /** [skip count indexed] returns a persistent Indexed collection that removes the
       *  first [count] values in [indexed].
       */

      let slice: start::int? => end_::int? => (t 'a) => (t 'a);

      let take: int => (t 'a) => (t 'a);
      /** [take count indexed] returns a Vector that includes the first [count] values in [indexed]. */

      let update: int => 'a => (t 'a) => (t 'a);
      /** [update index value indexed] returns a persistent Indexed collection with [value]
       *  replacing the value at [index].
       */

      let updateAll: (int => 'a => 'a) => (t 'a) => (t 'a);
      /** [updateAll f indexed] returns a persistent Indexed collection updating each value
       *  in [indexed] with result of applying the function [f] to each index/value pair.
       *
       *  Complexity: O(N)
       */

      let updateWith: int => ('a => 'a) => (t 'a) => (t 'a);
      /** [updateWith index f indexed] returns a persistent Indexed collection updating the value
       *  at [index] with the result of applying the function [f] to the value.
       */
    };
  };

  let module Transient: {
    /** A temporarily mutable Indexed collection. Once persisted, any further operations on a
     *  Transient instance will throw. Intended for implementing bulk mutation
     *  operations efficiently.
     */

    module type S1 = {
      type t 'a;

      include NavigableCollection.Transient.S1 with type t 'a := t 'a;

      let get: int => (t 'a) => (option 'a);
      /** [get index transient] returns the value at [index] or None if [index] is out of bounds. */

      let getOrRaise: int => (t 'a) => 'a;
      /** [getOrRaise index transient] returns the value at [index] or
       *  raises an exception if [index] is out of bounds.
       */

      let insertAt: int => 'a => (t 'a) => (t 'a);
      /** [insertAt index value transient] inserts value into [transient] at [index]. */

      let removeAt: int => (t 'a) => (t 'a);
      /** [removeAt index transient] removes the value at [index]. */

      let update: int => 'a => (t 'a) => (t 'a);
      /** [update index value transient] replaces the value at [index] with [value]. */

      let updateAll: (int => 'a => 'a) => (t 'a) => (t 'a);
      /** [updateAll f transient] updates each value in [transient] with result of applying
       *  the function [f] to each index/value pair.
       */

      let updateWith: int => ('a => 'a) => (t 'a) => (t 'a);
      /** [updateWith index f transient] updates the value at [index] with the result
       *  of applying the function [f] to the value.
       */
    };
  };
};

let module rec Deque: {
  /** A double-ended queue with efficient appends [addLast], prepends [addFirst]
   *  and removals from either end of the queue [removeFirstOrRaise] [removeLastOrRaise].
   */

  type t 'a;

  include NavigableCollection.Persistent.S1 with type t 'a := Deque.t 'a;

  let reverse: (Deque.t 'a) => (Deque.t 'a);
  /** [reverse deque] returns a new Deque with [deque]'s values reversed.
   *
   *  Complexity: O(1)
   */

  let module Transient: {
    /** A temporarily mutable Deque. Once persisted, any further operations on a
     *  Transient Deque instance will raise exceptions. Intended for implementing bulk
     *  mutation operations efficiently.
     */

    type t 'a;

    include NavigableCollection.Transient.S1 with type t 'a := Deque.Transient.t 'a;
    /** The Transient Deque type. */

    let persist: (Deque.Transient.t 'a) => (Deque.t 'a);
    /** [persist transient] persists [transient] returning a Deque. Further attempts
     *  to access or mutate [transient] will raise exceptions.
     */

    let reverse: (Deque.Transient.t 'a) => (Deque.Transient.t 'a);
    /** [reverse transient] reverse [transient]'s values.
     *
     *  Complexity: O(1)
     */
  };

  let mutate: (Deque.t 'a) => (Deque.Transient.t 'a);
  /** [mutate deque] returns a Transient Deque containing the same values as [deque]. */
};

let module rec HashMap: {
  /** A Persistent Map implemented using hashing and a comparator function to resolve hash conflicts.
   *  HashMap is implemented as a bitmapped trie of AVLTrees. Most map operations have a computational
   *  complexity of O(log32 N).
   */

  type t 'k 'v;

  include Map.Persistent.S2 with type t 'k 'v := HashMap.t 'k 'v;

  let emptyWith: hash::(Hash.t 'k) => comparator::(Comparator.t 'k) => (HashMap.t 'k 'v);
  /** [emptyWith hash comparator] returns an empty HashMap which uses [hash] to hash
   *  keys, and [comparator] to resolve collisions.
   */

  let fromWith: hash::(Hash.t 'k) => comparator::(Comparator.t 'k) => (KeyedIterable.t 'k 'v) => (HashMap.t 'k 'v);
  /** [fromWith hash comparator keyedIter] returns a HashMap containing all the key/value
   *  pairs in [keyedIter]. The returned HashMap uses [hash] to hash keys, and [comparator]
   *  to resolve collisions.
   */

  let fromEntriesWith: hash::(Hash.t 'k) => comparator::(Comparator.t 'k) => (Iterable.t ('k, 'v)) => (HashMap.t 'k 'v);
  /** [fromEntriesWith hash comparator iter] returns a HashMap containing all the key/value
   *  pairs in [iter]. The returned HashMap uses [hash] to hash keys, and [comparator]
   *  to resolve collisions.
   */

  let module Transient: {
    /** A temporarily mutable HashMap. Once persisted, any further operations on a
     *  Transient HashMap instance will throw. Intended for implementing bulk mutation
     *  operations efficiently.
     */

    type t 'k 'v;

    include Map.Transient.S2 with type t 'k 'v := t 'k 'v;

    let emptyWith: hash::(Hash.t 'k) => comparator::(Comparator.t 'k) => unit => (HashMap.Transient.t 'k 'v);
    /** [emptyWith hash comparator ()] returns an empty Transient HashMap which uses [hash] to hash
     *  keys, and [comparator] to resolve collisions.
     */

    let persist: (t 'k 'v) => (HashMap.t 'k 'v);
    /** [persist transient] persists [transient] returning a HashMap. Further attempts
     *  to access or mutate [transient] will raise exceptions.
     */
  };

  let mutate: (HashMap.t 'k 'v) => (HashMap.Transient.t 'k 'v);
  /** [mutate map] returns a Transient HashMap containing the same key/values pairs as [map]. */
};

let module rec HashSet: {
  /** A Persistent Set implemented using hashing and a comparator function to resolve hash conflicts.
   *  HashSet are implemented as bitmapped tries. Most set operations have a computational
   *  complexity of O(log32 N).
   */

  type t 'a;
  /** The HashSet type. */

  include Set.Persistent.S1 with type t 'a := HashSet.t 'a;

  let emptyWith: hash::(Hash.t 'a) => comparator::(Comparator.t 'a) => (HashSet.t 'a);
  /** [emptyWith hash comparator] returns an empty HashSet which uses [hash] to hash
   *  keys, and [comparator] to resolve collisions.
   */

  let fromWith: hash::(Hash.t 'a) => comparator::(Comparator.t 'a) => (Iterable.t 'a) => (HashSet.t 'a);
  /** [fromWith hash comparator iter] returns a HashSet containing all the values in [iter].
   *  The returned HashSet uses [hash] to hash keys, and [comparator] to resolve collisions.
   */

  let hash: Hash.t (HashSet.t 'a);
  /** An hashing function for HashSet instances. */

  let module Transient: {
    /** A temporarily mutable HashSet. Once persisted, any further operations on a
     *  Transient HashMap instance will throw. Intended for implementing bulk mutation
     *  operations efficiently.
     */

    type t 'a;

    include Set.Transient.S1 with type t 'a := HashSet.Transient.t 'a;

    let emptyWith: hash::(Hash.t 'a) => comparator::(Comparator.t 'a) => unit => (HashSet.Transient.t 'a);
    /** [emptyWith hash comparator ()] returns an empty Transient HashSet which uses [hash] to hash
     *  keys, and [comparator] to resolve collisions.
     */

    let persist: (HashSet.Transient.t 'a) => (HashSet.t 'a);
    /** [persist transient] persists [transient] returning a HashSet. Further attempts
     *  to access or mutate [transient] will raise exceptions.
     */
  };

  let mutate: (HashSet.t 'a) => (HashSet.Transient.t 'a);
  /** [mutate set] returns a Transient HashSet containing the same values as [set]. */
};

let module rec IntMap: {
  /** A Map optimized for integer keys. IntMap is implemented as a bitmapped trie.
   *  Most map operations have a computational complexity of O(log32 N).
   */

  type k = int;
  type t 'v;

  include Map.Persistent.S1 with type k := IntMap.k and type t 'v := IntMap.t 'v;

  let module Transient: {
    type k = int;
    type t 'v;

    include Map.Transient.S1 with type k := IntMap.Transient.k and type t 'v := IntMap.Transient.t 'v;

    let persist: (t 'v) => (IntMap.t 'v);
    /** [persist transient] persists [transient] returning a IntMap. Further attempts
     *  to access or mutate [transient] will raise exceptions.
     */
  };

  let mutate: (t 'v) => (IntMap.Transient.t 'v);
  /** [mutate map] returns a Transient IntMap containing the same key/values pairs as [map]. */
};

let module rec IntRange: {
  /** A contiguous Set of discrete integers */

  type a = int;
  type t;

  include NavigableSet.S with type a := IntRange.a and type t := IntRange.t;
  include Comparable.S with type t := IntRange.t;
  include Hashable.S with type t := IntRange.t;

  let create: start::int => count::int => IntRange.t;
  /** [create start count] returns an IntRange startint at [start] with [count].
   *  [start] may be any positive or negative integer. [count] must be greater
   *  than or equal to 0.
   */
};

let module rec IntSet: {
  /** A Persistent Set optimized for integer values. IntSets are implemented as
   *  bitmapped tries. Most set operations have a computational complexity of O(log32 N).
   */

  type a = int;
  type t;
  /** The IntSet type. */

  include Set.Persistent.S with type a := IntSet.a and type t := IntSet.t;

  let module Transient: {
    /** A temporarily mutable IntSet. Once persisted, any further operations on a
     *  Transient IntSet instance will throw. Intended for implementing bulk mutation
     *  operations efficiently.
     */

    type a = int;
    type t;
    /** The Transient IntSet type. */

    include Set.Transient.S with type a := IntSet.Transient.a and type t := IntSet.Transient.t;

    let empty: unit => IntSet.Transient.t;
    /** [empty ()] return a new empty Transient IntSet. */

    let persist: t => IntSet.t;
    /** [persist transient] returns a persisted IntSet. Further attempts to access or mutate [transient]
     *  will throw.
     */
  };

  let mutate: t => IntSet.Transient.t;
  /** [mutate set] returns a Transient IntSet containing the same values as [set]. */
};

let module rec List: {
  /** OCaml singly-linked list */

  type t 'a = list 'a;

  include Iterable.S1 with type t 'a := t 'a;

  let addFirst: 'a => (List.t 'a) => (List.t 'a);
  /** [addFirst value list] returns a List with [value] prepended.
   *
   *  Complexity: O(1)
   */

  let addFirstAll: (Iterable.t 'a) => (List.t 'a) => (List.t 'a);
  /** [addFirstAll iter list] returns a List with the values in [iter] prepended. */

  let empty: unit => (List.t 'a);
  /** [empty ()] returns an empty List. */

  let first: t 'a => (option 'a);
  /** [first list] returns first value in [list] or None.
   *
   *  Complexity: O(1)
   */

  let firstOrRaise: List.t 'a => 'a;
  /** [first seq] returns the first value in [list] or raises an exception.
   *
   *  Complexity: O(1)
   */

  let fromReverse: (Iterable.t 'a) => (List.t 'a);
  /** [fromReverse iter] returns a new List containing the values in [iter]
   *  in reverse order.
   *
   * Complexity: O(N) the number of values in [iter].
   */

  let removeAll: (List.t 'a) => (List.t 'a);
  /** [removeAll list] returns the empty List.
   *
   *  Complexity: O(1)
   */

  let removeFirstOrRaise: (List.t 'a) => (List.t 'a);
  /** [removeFirstOrRaise list] returns a List without the first value.
   *
   *  Complexity: O(1)
   */

  let return: 'a => (List.t 'a);
  /** [return value] returns a new List containing a single value, [value]. */

  let toSequence: (List.t 'a) => (Sequence.t 'a);
  /** [toSequence list] returns a Sequence of the values in [list] in order. */
};

let module rec ReadOnlyArray: {
  /** Opaque wrapper around an underlying array instance that provides read only semantics */

  type t 'a;

  include Indexed.S1 with type t 'a := ReadOnlyArray.t 'a;

  let empty: unit => ReadOnlyArray.t 'a;
  /* [empty ()] returns an empty ReadOnlyArray. */

  let init: int => (int => 'a) => (ReadOnlyArray.t 'a);
  /* [init count f] returns a ReadOnlyArray with size [count]. The callback [f] is called
   * for each index to initialize the value at the respective index.
   */

  let ofUnsafe: (array 'a) => (ReadOnlyArray.t 'a);
  /** [unsafe arr] returns a ReadOnlyArray backed by [arr]. Note, it is the caller's
   *  responsibility to ensure that [arr] is not subsequently mutated.
   */
};

let module rec Stack: {
  /** A singly-linked stack with an O(1) count operation. */

  type t 'a;

  include SequentialCollection.Persistent.S1 with type t 'a := Stack.t 'a;

  let fromList: (list 'a) => (Stack.t 'a);
  /** [fromList list] returns a Stack backed by [list].
   *
   *  Complexity: O(N)
   */

  let toList: (Stack.t 'a) => (list 'a);
  /** [toList stack] returns the underlying List backing the stack */
};

let module SortedMap: {
  /** Sorted map implemented as an AVL tree. Most set operations
   *  have a computational complexity of O(log N).
   */

  module type S1 = {
    type k;

    type t +'v;
    /** The SortedMap type. */

    include NavigableMap.Persistent.S1 with type k := k and type t 'v := t 'v;
  };

  let module Make1: (Comparable: Comparable.S) => S1 with type k = Comparable.t;
  /** Module function to create a SortedMap. */
};

let module SortedSet: {
  /** Sorted set implemented as an AVL tree. Most set operations
   *  have a computational complexity of O(log N).
   */
  module type S = {
    type a;
    type t;

    include Comparable.S with type t := t;
    include NavigableSet.Persistent.S with type a := a and type t := t;
  };

  let module Make: (Comparable: Comparable.S) => S with type a = Comparable.t;
  /** Module function to create a SortedSet. */
};

let module rec Vector: {
  /** An Indexed supporting efficient prepend, appends, indexing, conctentation,
   *  and splits. Vectors are implemented as relaxed radix balanced trees. Computational
   *  is O(log32 N) for most operations, with optimizations for effection O(1) access to
   *  first and last values.
   */

  type t 'a;

  include Indexed.Persistent.S1 with type t 'a := Vector.t 'a;

  let init: int => (int => 'a) => (Vector.t 'a);
  /* [init count f] returns a Vector with size [count]. The callback [f] is called
   * for each index to initialize the value at the respective index.
   */

  let module Transient: {
    /** A temporarily mutable Vector. Once persisted, any further operations on a
     *  Transient Vector instance will throw. Intended for implementing bulk mutation
     *  operations efficiently.
     */

    type t 'a;

    include Indexed.Transient.S1 with type t 'a := Vector.Transient.t 'a;

    let persist: (Vector.Transient.t 'a) => (Vector.t 'a);
    /** [persist transient] returns a persisted Vector. Further attempts to access or mutate [transient]
    *  will throw.
    */
  };

  let mutate: (Vector.t 'a) => (Vector.Transient.t 'a);
  /** [mutate vector] returns a Transient Vector containing the same values as [set]. */
};
