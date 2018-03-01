/***
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

/*** Equality functions for common types. */
module rec Equality: {

  /*** The Equality function type.
   * [equals this that] returns [true] if [this] and [that] are equal, otherwise [false].
   */
  type t('a) = ('a, 'a) => bool;

  /*** Equality for bytes. */
  let bytes: Equality.t(bytes);

  /*** Equality for chars. */
  let char: Equality.t(char);

  /*** Equality for ints. */
  let int: Equality.t(int);

  /*** Equality for int32s. */
  let int32: Equality.t(int32);

  /*** Equality for int64s. */
  let int64: Equality.t(int64);

  /*** Equality for nativeInts. */
  let nativeInt: Equality.t(nativeint);

  /*** The reference equality function, analogous to === */
  let reference: Equality.t('a);

  /*** Equality for strings. */
  let string: Equality.t(string);
};

/*** Represents the absolute ordering of a type when comparing values. */
module rec Ordering: {
  type t;
  let equal: Ordering.t;
  let greaterThan: Ordering.t;
  let lessThan: Ordering.t;
};

/*** Comparison functions for common types. */
module rec Comparator: {

  /*** The Comparator function type.
   *  By definition a [compare this that] returns:
   *    [Ordering.greaterThan] if [this] is greater than [that],
   *    [Ordering.lessThan] if [this] is less than [that],
   *    otherwise [Ordering.equals].
   */
  type t('a) = ('a, 'a) => Ordering.t;

  /*** Compares bytes. */
  let bytes: Comparator.t(bytes);

  /*** Compares chars. */
  let char: Comparator.t(char);

  /*** Compares ints. */
  let int: Comparator.t(int);

  /*** Compares int32s. */
  let int32: Comparator.t(int32);

  /*** Compares int64s. */
  let int64: Comparator.t(int64);

  /*** Compares nativeInts. */
  let nativeInt: Comparator.t(nativeint);

  /*** Compares strings. */
  let string: Comparator.t(string);

  /*** Converts a Comparator function to an Equality function. */
  let toEquality: Comparator.t('a) => Equality.t('a);
};

/*** Hash functions for common types. */
module Hash: {

  /*** The Hash function type. */
  type t('a) = 'a => int;
};

/*** Module types implemented by modules that support testing values for equality. */
module Equatable: {

  /* Equatable module type signature for types with a parametric type arity of 0. */
  module type S = {
    type t;

    /*** An equality function for instances of type [t]. */
    let equals: Equality.t(t);
  };
};

/*** Module types implemented by modules that support absolute ordering of values. */
module Comparable: {
  /*** Comparable module type signature for types with a parametric type arity of 0. */
  module type S = {
    type t;

    include Equatable.S with type t := t;

    /*** A comparator function for instances of type [t]. */
    let compare: Comparator.t(t);
  };
};

/*** Module types implemented by modules that support hashing. */
module Hashable: {

  /*** Hashable module type signature for types with a parametric type arity of 0. */
  module type S = {
    type t;

    /*** An hashing function for instances of type [t]. */
    let hash: Hash.t(t);
  };
};

/*** Module types implemented by modules that support lazily evaluated
 *  stream functions. All functions defined in this module are O(1).
 */
module rec Streamable: {

  /*** Streamable module type signature for types with a parametric type arity of 1. */
  module type S1 = {
    type t('a);

    /*** [defer f] returns a Streamable that invokes the function [f] whenever enumerated. */
    let defer: (unit => t('a)) => t('a);

    /*** [distinctUntilChangedWith equals stream] returns a Streamable that contains only
     *  distinct contiguous values from [stream] using [equals] to equate values.
     */
    let distinctUntilChangedWith: (Equality.t('a), t('a)) => t('a);

    /*** [doOnNext f stream] returns a Streamable that applies the side effect
     *  function [f] to each value in the stream as they are enumerated.
     */
    let doOnNext: ('a => unit, t('a)) => t('a);

    /*** [filter f stream] returns a Streamable only including values from [stream]
     *  for which application of the predicate function [f] returns true.
     */
    let filter: ('a => bool, t('a)) => t('a);

    /*** [flatMap mapper stream] applies the mapper to each value in
     *  [stream], flattening the resulting Streams into a new Stream.
     */
    let flatMap: ('a => t('b), t('a)) => t('b);

    /*** [flatten stream] flattens the nested values in [streams] into
     *  a new [stream].
     */
    let flatten: t(t('a)) => t('a);

    /*** [map f stream] Returns a Stream whose values are the result of
     *  applying the function [f] to each value in [stream].
     */
    let map: ('a => 'b, t('a)) => t('b);

    /*** [scan f acc stream] returns a Streamable of accumulated values resulting from the
     *  application of the accumulator function [f] to each value in [stream] with the
     *  specified initial value [acc].
     */
    let scan: (('acc, 'a) => 'acc, 'acc, t('a)) => t('acc);

    /*** [skip count stream] return a Streamable which skips the first [count]
     *  values in [stream].
     */
    let skip: (int, t('a)) => t('a);

    /*** [skipWhile f stream] return a Streamable which skips values in [stream]
     *  while application of the predicate function [f] returns true, and then returns
     *  the remaining values.
     */
    let skipWhile: ('a => bool, t('a)) => t('a);

    /*** [startWith value stream] returns a Streamable whose first
     *  value is [value], followed by the values in [stream].
     */
    let startWith: ('a, t('a)) => t('a);

    /*** [take count stream] returns a Streamable with the first [count]
     *  values in [stream].
     */
    let take: (int, t('a)) => t('a);

    /*** [takeWhile f stream] returns a Streamable including all values in [stream]
     *  while application of the predicate function [f] returns true, then completes.
     */
    let takeWhile: ('a => bool, t('a)) => t('a);
  };
};

/*** Functional iterators over a collection of values. Iterables are stateless and can be reused. */
module rec Iterable: {
  /*** Iterable module type signature for types with a parametric type arity of 0. */
  module type S = {
    type a;
    type t;

    /*** [every f iterable] returns true if the predicate [f] returns true for all values in [iterable].
     *  If [iterable] is empty, returns [true].
     */
    let every: (a => bool, t) => bool;

    /*** [find f iterable] return the Some of the first value in [iterable] for which the
     *  the predicate f returns [true]. Otherwise None.
     */
    let find: (a => bool, t) => option(a);

    /*** [findOrRaise f iterable] return the the first value in [iterable] for which the
     *  the predicate f returns [true]. Otherwise raises an exception.
     */
    let findOrRaise: (a => bool, t) => a;

    /*** [forEach while_::predicate f iterable] iterates through [iterable] applying the
     *  side effect function [f] to each value, while [predicate] returns true
     */
    let forEach: (~while_: a => bool=?, a => unit, t) => unit;

    /*** [none f iterable] returns true if the predicate [f] returns false for all values in [iterable].
     *  If [iterable] is empty, returns [true].
     */
    let none: (a => bool, t) => bool;

    /*** [reduce while_::predicate initialValue f iterable] applies the accumulator
     *  function [f] to each value in [iterable], while [predicate] returns true,
     *  accumulating the result.
     */
    let reduce: (~while_: ('acc, a) => bool=?, ('acc, a) => 'acc, 'acc, t) => 'acc;

    /*** [some f iterable] returns true if the predicate [f] returns true for at
     *  least one value in [iterable]. If [iterable] is empty, returns [false].
     */
    let some: (a => bool, t) => bool;

    /*** [toIterable iterable] returns an Iterable that can be used to iterate over
     * the values in [iterable].
     */
    let toIterable: t => Iterable.t(a);
  };

  /*** Iterable module type signature for types with a parametric type arity of 1. */
  module type S1 = {
    type t('a);

    /*** [every f iterable] returns true if the predicate [f] returns true for all values in [iterable].
     *  If [iterable] is empty, returns [true].
     */
    let every: ('a => bool, t('a)) => bool;

    /*** [find f iterable] return the Some of the first value in [iterable] for which the
     *  the predicate f returns [true]. Otherwise None.
     */
    let find: ('a => bool, t('a)) => option('a);

    /*** [findOrRaise f iterable] return the the first value in [iterable] for which the
     *  the predicate f returns [true]. Otherwise raises an exception.
     */
    let findOrRaise: ('a => bool, t('a)) => 'a;

    /*** [forEach while_::predicate f iterable] iterates through [iterable] applying the
     *  side effect function [f] to each value, while [predicate] returns true
     */
    let forEach: (~while_: 'a => bool=?, 'a => unit, t('a)) => unit;

    /*** [none f iterable] returns true if the predicate [f] returns false for all values in [iterable].
     *  If [iterable] is empty, returns [true].
     */
    let none: ('a => bool, t('a)) => bool;

    /*** [reduce while_::predicate initialValue f iterable] applies the accumulator
     *  function [f] to each value in [iterable], while [predicate] returns true,
     *  accumulating the result.
     */
    let reduce: (~while_: ('acc, 'a) => bool=?, ('acc, 'a) => 'acc, 'acc, t('a)) => 'acc;

    /*** [some f iterable] returns true if the predicate [f] returns true for at
     *  least one value in [iterable]. If [iterable] is empty, returns [false].
     */
    let some: ('a => bool, t('a)) => bool;

    /*** [toIterable iterable] returns an Iterable that can be used to iterate over
     * the values in [iterable].
     */
    let toIterable: t('a) => Iterable.t('a);
  };
  type t('a);
  include Streamable.S1 with type t('a) := Iterable.t('a);
  include S1 with type t('a) := Iterable.t('a);

  /*** [concat iters] returns an Iterable that lazily concatenates all the
   *  Iterables in [iters]. The resulting Iterable returns all the values
   *  in the first Iterable, followed by all the values in the second Iterable,
   *  and continues until the last Iterable completes.
   */
  let concat: list(t('a)) => t('a);

  /*** Returns an empty Iterable. */
  let empty: unit => Iterable.t('a);

  /*** [generate f initialValue] generates the infinite Iterable [x, f(x), f(f(x)), ...] */
  let generate: ('a => 'a, 'a) => t('a);

  /*** [return value] returns a single value Iterable containing [value]. */
  let return: 'a => t('a);
};

/*** Functional pull based sequences. Sequences are generally lazy, computing values as
 *  they are pulled. Sequences are reusable and are guaranteed to produce the
 *  same values, in the same order every time they are enumerated. In addition, Sequences
 *  support eager seeking and zipping. These are their main advantage over Iterables.
 *  In general, only use Sequences when you require support for one or both of these features.
 *  Otherwise Iterables are generally more efficient.
 */
module rec Sequence: {
  /*** The Sequence type. */
  type t('a);

  include Iterable.S1 with type t('a) := t('a);
  include Streamable.S1 with type t('a) := t('a);

  /*** [concat seqs] returns an Sequence that lazily concatenates all the
   *  Sequence in [seqs]. The resulting Sequence returns all the values
   *  in the first Sequence, followed by all the values in the second Sequence,
   *  and continues until the last Sequence completes.
   */
  let concat: list(t('a)) => t('a);

  /*** Returns an empty Sequence. */
  let empty: unit => Sequence.t('a);

  /*** [first seq] returns first value in [seq] or None.
   *
   *  Computational Complexity: O(1)
   */
  let first: t('a) => option('a);

  /*** [firstOrRaise seq] returns the first value in [seq] or raises an exception.
   *
   *  Computational Complexity: O(1)
   */
  let firstOrRaise: t('a) => 'a;

  /*** [generate f initialValue] generates the infinite Sequence [x, f(x), f(f(x)), ...] */
  let generate: ('a => 'a, 'a) => t('a);

  /*** [return value] returns a single value Sequence containing [value]. */
  let return: 'a => t('a);

  /*** [seek count seq] scans forward [count] values in [seq]. It is the eagerly
   *  evaluated equivalent of [skip count seq].
   *
   *  Computational complexity: O(count).
   */
  let seek: (int, Sequence.t('a)) => Sequence.t('a);

  /*** [seekWhile f seq] scans forward through [seq] while application of
   *  the predicate function [f] returns true. It is the eagerly evaluated
   *  equivalent of [skipWhile f seq].
   *
   *  Computational complexity: O(N).
   */
  let seekWhile: ('a => bool, Sequence.t('a)) => Sequence.t('a);

  /*** [zip seqs] returns a Sequence which lazily zips a list of [Sequence]s
   *  into a single Sequence of lists. Values are produce until any Sequence
   *  in [seqs] completes.
   */
  let zip: list(Sequence.t('a)) => Sequence.t(list('a));

  /*** [zip2 zipper first second] returns a Sequence which lazily zips two Sequences,
   *  combining their values using [zipper]. Values are produce until either [first]
   *  or [second] completes.
   */
  let zip2With: (~zipper: ('a, 'b) => 'c, Sequence.t('a), Sequence.t('b)) => Sequence.t('c);

  /*** [zip3With zipper first second third] returns a Sequence which lazily zips three Sequences,
   *  combining their values using [zipper]. Values are produce until either [first], [second]
   *  or [third] complete.
   */
  let zip3With:
    (~zipper: ('a, 'b, 'c) => 'd, Sequence.t('a), Sequence.t('b), Sequence.t('c)) => Sequence.t('d);

  /*** [zipLongest seqs] returns a Sequence which zips a list of Sequences
   *  into a single of Sequence of lists. Values are produce until all Sequences
   *  in [seqs] complete.
   */
  let zipLongest: list(Sequence.t('a)) => Sequence.t(list(option('a)));

  /*** [zipLongest2With zipper first second] returns a Sequence which lazily zips two Sequences,
   *  combining their values using [zipper]. Values are produce until both [first]
   *  and [second] complete.
   */
  let zipLongest2With:
    (~zipper: (option('a), option('b)) => 'c, Sequence.t('a), Sequence.t('b)) => Sequence.t('c);

  /*** [zipLongest3With zipper first second third] returns a Sequence which lazily
   *  zips three Sequences, combining their values using [zipper]. Values are produce
   *  until [first], [second] and [third] all complete.
   */
  let zipLongest3With:
    (
      ~zipper: (option('a), option('b), option('c)) => 'd,
      Sequence.t('a),
      Sequence.t('b),
      Sequence.t('c)
    ) =>
    Sequence.t('d);

};

/*** Module types implemented by all immutable value collections.
 *
 *  By contract, all functions have a computational complexity of O(1).
 */
module rec Collection: {

  /*** Collection module type signature for types with a parametric type arity of 0. */
  module type S = {
    type a;
    type t;
    include Iterable.S with type a := a and type t := t;

    /*** [count collection] returns number of values contained in [collection]. */
    let count: t => int;

    /*** [isEmpty collection] returns true if [collection] is empty, otherwise false. */
    let isEmpty: t => bool;

    /*** [isNotEmpty collection] returns true if [collection] contains at
     *  least one value, otherwise false.
     */
    let isNotEmpty: t => bool;

    let toCollection: t => Collection.t(a);

    /*** [toSequence collection] returns a Sequence that can be used to enumerate the collection. */
    let toSequence: t => Sequence.t(a);

  };

  /*** Collection module type signature for types with a parametric type arity of 1. */
  module type S1 = {
    type t('a);
    include Iterable.S1 with type t('a) := t('a);

    /*** [count collection] returns number of values contained in the collection. */
    let count: t('a) => int;

    /*** [isEmpty collection] returns true if [collection] is empty, otherwise false. */
    let isEmpty: t('a) => bool;

    /*** [isNotEmpty collection] returns true if [collection] contains at
     *  least one value, otherwise false.
     */
    let isNotEmpty: t('a) => bool;

    let toCollection: t('a) => Collection.t('a);

    /*** [toSequence collection] returns a Sequence that can be used to enumerate the collection. */
    let toSequence: t('a) => Sequence.t('a);
  };
  type t('a);
  include S1 with type t('a) := Collection.t('a);

  /*** [empty ()] returns the empty collection */
  let empty: unit => Collection.t('a);

  /*** Module types implemented by collections supporting fully persistent mutations.
   *  Mutation operations on these types do not mutate the underlying collection, but instead
   *  create a new collection, with the mutation applied.
   *
   *  By contract, all functions have a computational complexity of O(1).
   */
  module Persistent: {

    /*** Persistent Collection module type signature for types with a parametric type arity of 0. */
    module type S = {
      type a;
      type t;
      include S with type a := a and type t := t;

      /*** [removeAll collection] return an empty Persistent Collection. Depending on the implementation,
       *  the new collection may share the same configuration as [collection]. For instance, the HashSet
       *  implementations shares the same hash and comparison functions.
       */
      let removeAll: t => t;
    };

    /*** PersistentCollection module type signature for types with a parametric type arity of 1. */
    module type S1 = {
      type t('a);
      include S1 with type t('a) := t('a);

      /*** [removeAll collection] return an empty Persistent Collection. Depending on the implementation,
       *  the new collection may share the same configuration as [collection]. For instance, HashSet
       *  implementations shares the same hash and comparison functions.
       */
      let removeAll: t('a) => t('a);
    };
  };

  /*** Module types implemented by transiently mutable Collections. Transient collections
   *  are designed to enable fast and efficient batch operations by temporarily enabling mutation
   *  of an underlying collection type. Unlike Persistent Collection functions, Transient Collection
   *  APIs always return the same value reference passed in as an argument, with mutations applied.
   *
   *  By contract, all functions have a computational complexity of O(1).
   */
  module Transient: {

    /*** Transient Collection module type signature for types with a parametric type arity of 0. */
    module type S = {
      type a;
      type t;

      /*** [count transient] returns number of values contained in [transient]. */
      let count: t => int;

      /*** [isEmpty transient] returns true if [transient] is empty, otherwise false. */
      let isEmpty: t => bool;

      /*** [isNotEmpty transient] returns true if [collection] contains at
       *  least one value, otherwise false.
       */
      let isNotEmpty: t => bool;

      /*** [removeAll transient] removes all values from [transient]. */
      let removeAll: t => t;
    };

    /*** Transient Collection module type signature for types with a parametric type arity of 0. */
    module type S1 = {
      type t('a);

      /*** [count transient] returns number of values contained in [transient]. */
      let count: t('a) => int;

      /*** [isEmpty transient] returns true if [transient] is empty, otherwise false. */
      let isEmpty: t('a) => bool;

      /*** [isNotEmpty transient] returns true if [collection] contains at
       *  least one value, otherwise false.
       */
      let isNotEmpty: t('a) => bool;

      /*** [removeAll transient] removes all values from [transient]. */
      let removeAll: t('a) => t('a);
    };
  };
};

/*** Module types implemented by collections that support sequential access to
 *  the left most contained values. Concrete implementations include [Stack] and [SortedSet].
 *
 *  By contract, all functions must be efficient, with no worst than O(log N) performance.
 */
module rec SequentialCollection: {

  /*** SequentialCollection module type signature for types with a parametric type arity of 0. */
  module type S = {
    type a;
    type t;
    include Collection.S with type a := a and type t := t;

    /*** [first collection] returns first value in [collection] or None. */
    let first: t => option(a);

    /*** [firstOrRaise collection] returns the first value in [collection] or throws. */
    let firstOrRaise: t => a;

    let toSequentialCollection: t => SequentialCollection.t(a);
  };

  /*** SequentialCollection module type signature for types with a parametric type arity of 1. */
  module type S1 = {
    type t('a);
    include Collection.S1 with type t('a) := t('a);

    /*** [first collection] returns first value in [collection] or None. */
    let first: t('a) => option('a);

    /*** [firstOrRaise collection] returns the first value in [collection] or throws. */
    let firstOrRaise: t('a) => 'a;

    let toSequentialCollection: t('a) => SequentialCollection.t('a);
  };
  type t('a);
  include S1 with type t('a) := SequentialCollection.t('a);

  /*** [empty ()] returns the empty SequentialCollection */
  let empty: unit => SequentialCollection.t('a);

  /*** Module types implemented by collections supporting persistent mutations to left
   *  side of the collection.
   *
   *  By contract, all functions must be efficient, with no worst than O(log N) performance.
   */
  module Persistent: {

    /*** SequentialCollection.Persistent module type signature for types with a parametric type arity of 1. */
    module type S1 = {
      type t('a);
      include Collection.Persistent.S1 with type t('a) := t('a);
      include S1 with type t('a) := t('a);

      /*** [addFirst value collection] returns a SequentialCollection.Persistent with [value] prepended. */
      let addFirst: ('a, t('a)) => t('a);

      /*** [addFirstAll iter collection] returns a SequentialCollection.Persistent with the values in [iter] prepended. */
      let addFirstAll: (Iterable.t('a), t('a)) => t('a);

      /*** [removeFirstOrRaise collection] returns a SequentialCollection.Persistent without
       *  the first value or raises an exception if [collection] is empty.
       */
      let removeFirstOrRaise: t('a) => t('a);
    };
  };

  /*** Module types implemented by transient collections supporting transient mutations to left
   *  side of the collection.
   *
   *  By contract, all functions must be efficient, with no worst than O(log N) performance.
   */
  module Transient: {

    /*** SequentialCollection.Transient module type signature for types with a parametric type arity of 1. */
    module type S1 = {
      type t('a);
      include Collection.Transient.S1 with type t('a) := t('a);

      /*** [addFirst value transient] prepends [value] to [transient]. */
      let addFirst: ('a, t('a)) => t('a);

      /*** [addFirstAll iter transient] prepends all values in [iter] to [transient]. */
      let addFirstAll: (Iterable.t('a), t('a)) => t('a);

      /*** [first transient] returns first value in [transient] or None. */
      let first: t('a) => option('a);

      /*** [firstOrRaise transient] returns the first value in [transient] or raises an exception. */
      let firstOrRaise: t('a) => 'a;

      /*** [removeFirstOrRaise transient] removes the first value from [transient] or raises
       *  an exception if [transient] is empty.
       */
      let removeFirstOrRaise: t('a) => t('a);
    };
  };
};

/*** Module types implemented by Collections that are ordered or sorted and support
 *  navigation operations.
 *
 *  By contract, all functions must be efficient, with no worst than O(log N) performance.
 */
module rec NavigableCollection: {

  /*** NavigableCollection module type signature for types with a parametric type arity of 0. */
  module type S = {
    type a;
    type t;
    include SequentialCollection.S with type a := a and type t := t;

    /*** [last collection] returns last value in [collection] or None.
     *
     *  By contract, implementations are efficient with no worst than O(log N) performance.
     */
    let last: t => option(a);

    /*** [lastOrRaise collection] returns the last value in [collection] or raises an exception.
     *
     *  By contract, implementations are efficient with no worst than O(log N) performance.
     */
    let lastOrRaise: t => a;

    /*** [reduceReversed while_::predicate initialValue f iterable] applies the accumulator
     *  function [f] to each value in [iterable] while [predicate] returns true, starting
     *  from the right most value, accumulating the result.
     */
    let reduceReversed: (~while_: ('acc, a) => bool=?, ('acc, a) => 'acc, 'acc, t) => 'acc;

    let toCollectionReversed: t => Collection.t(a);
    let toIterableReversed: t => Iterable.t(a);
    let toNavigableCollection: t => NavigableCollection.t(a);
    let toNavigableCollectionReversed: t => NavigableCollection.t(a);

    /* [toSequenceReversed collection] returns an Sequence that can be used to enumerate
     * the values in [collection] from right to left.
     */
    let toSequenceReversed: t => Sequence.t(a);

    let toSequentialCollectionReversed: t => SequentialCollection.t(a);
  };

  /*** NavigableCollection module type signature for types with a parametric type arity of 1. */
  module type S1 = {
    type t('a);
    include SequentialCollection.S1 with type t('a) := t('a);

    /*** [last collection] returns last value in [collection] or None. */
    let last: t('a) => option('a);

    /*** [lastOrRaise collection] returns the first value in [collection] or raises an exception. */
    let lastOrRaise: t('a) => 'a;

    /*** [reduceReversed while_::predicate initialValue f iterable] applies the accumulator
     *  function [f] to each value in [iterable] while [predicate] returns true, starting
     *  from the right most value, accumulating the result.
     */
    let reduceReversed: (~while_: ('acc, 'a) => bool=?, ('acc, 'a) => 'acc, 'acc, t('a)) => 'acc;

    let toCollectionReversed: t('a) => Collection.t('a);
    let toIterableReversed: t('a) => Iterable.t('a);
    let toNavigableCollection: t('a) => NavigableCollection.t('a);
    let toNavigableCollectionReversed: t('a) => NavigableCollection.t('a);

    /* [toSequenceReversed collection] returns an Sequence that can be used to enumerate
     * the values in [collection] from right to left.
     */
    let toSequenceReversed: t('a) => Sequence.t('a);

    let toSequentialCollectionReversed: t('a) => SequentialCollection.t('a);
  };
  type t('a);
  include S1 with type t('a) := NavigableCollection.t('a);

  /*** [empty ()] returns the empty NavigableCollection */
  let empty: unit => NavigableCollection.t('a);

  /*** Module types implemented by collections supporting persistent mutations to both the left
   *  and right side of the collection.
   *
   *  By contract, all functions must be efficient, with no worst than O(log N) performance.
   */
  module Persistent: {

    /*** PersistentNavigableCollection module type signature for types with a parametric type arity of 1. */
    module type S1 = {
      type t('a);
      include S1 with type t('a) := t('a);
      include SequentialCollection.Persistent.S1 with type t('a) := t('a);

      /*** [addLast value collection] returns a Persistent NavigableCollection with [value] appended.
       *
       *  Complexity: O(1)
       */
      let addLast: ('a, t('a)) => t('a);

      /*** [addLastAll iter collection] returns a Persistent NavigableCollection with the values in [iter] appended. */
      let addLastAll: (Iterable.t('a), t('a)) => t('a);

      /*** [removeLastOrRaise collection] returns a SequentialCollection.Persistent without
       *  the last value or raises an exception if [collection] is empty.
       */
      let removeLastOrRaise: t('a) => t('a);
    };
  };

  /*** Module types implemented by transient collections supporting transient mutations to both
   *  the left and rights sides of the collection.
   *
   *  By contract, all functions must be efficient, with no worst than O(log N) performance.
   */
  module Transient: {

    /*** Transient NavigableCollection module type signature for types with a parametric type arity of 1. */
    module type S1 = {
      type t('a);
      include SequentialCollection.Transient.S1 with type t('a) := t('a);

      /*** [addLast value transient] appends [value] to [transient].
       *
       *  Complexity: O(1)
       */
      let addLast: ('a, t('a)) => t('a);

      let addLastAll: (Iterable.t('a), t('a)) => t('a);

      /*** [last transient] returns the last value in [transient] or None. */
      let last: t('a) => option('a);

      /*** [lastOrRaise transient] returns the last value in [transient] or raises an exception. */
      let lastOrRaise: t('a) => 'a;

      /*** [removeLastOrRaise transient] removes the last value from [transient] or raises
       *  an exception if [transient] is empty.
       */
      let removeLastOrRaise: t('a) => t('a);
    };
  };
};

/*** A read only view of a unique Set of values. The intent of this type is to enable
 *  interop between alternative concrete implementations such as SortedSet and HashSet.
 *  The complexity of functions in this module is dependent upon the underlying concrete implementation.
 */
module rec Set: {

  /*** Set module type signature for types with a parametric type arity of 0. */
  module type S = {
    type a;
    type t;
    include Collection.S with type a := a and type t := t;
    include Equatable.S with type t := t;

    /*** [contains value set] returns true if [set] contains at least one instace of [value],
     *  otherwise false;
     *
     *  By contract, an implementation must be efficient, with no worst than O(log N) performance.
     */
    let contains: (a, t) => bool;

    /*** [toSet set] returns a Set view of [set]. */
    let toSet: t => Set.t(a);
  };

  /*** Set module type signature for types with a parametric type arity of 0. */
  module type S1 = {
    type t('a);
    include Collection.S1 with type t('a) := t('a);

    /*** [contains value set] returns true if [set] contains at least one instace of [value],
     *  otherwise false;
     *
     *  By contract, an implementation must be efficient, with no worst than O(log N) performance.
     */
    let contains: ('a, t('a)) => bool;

    /*** An equality function for Set.S1 instances. */
    let equals: Equality.t(t('a));

    /*** [toSet set] returns a Set view of [set]. */
    let toSet: t('a) => Set.t('a);
  };

  /*** The Set type. */
  type t('a);

  include S1 with type t('a) := Set.t('a);

  /*** The empty Set. */
  let empty: unit => Set.t('a);

  /*** [intersect this that] returns an Iterable of unique values
   *  which occur in both [this] and [that].
   */
  let intersect: (Set.t('a), Set.t('a)) => Iterable.t('a);

  /*** [subtract this that] returns an Iterable of unique value
   *  which occur in [this] but not in [that].
   */
  let subtract: (Set.t('a), Set.t('a)) => Iterable.t('a);

  /*** [union this that] returns an Iterable of unique values which occur in either [this] or [that]. */
  let union: (Set.t('a), Set.t('a)) => Iterable.t('a);

  /*** Module types implemented by Set collections supporting persistent mutations.
   *
   *  By contract, all functions must be efficient, with no worst than O(log N) performance.
   */
  module Persistent: {

    /*** Persistent Set module type signature for types with a parametric type arity of 0. */
    module type S = {
      type a;
      type t;
      include S with type a := a and type t := t;
      include Collection.Persistent.S with type a := a and type t := t;

      /*** [add value set] returns a Persistent Set containing value. If [set] already contains [value],
       *  it is returned unmodified.
       */
      let add: (a, t) => t;

      /*** [addAll iter set] returns a Persistent Set with the values in [iter] and all the values in [set]. */
      let addAll: (Iterable.t(a), t) => t;

      /*** [intersect this that] returns a Persistent Set of unique values
       *  which occur in both [this] and [that].
       */
      let intersect: (t, t) => t;

      /*** [remove value set] returns a Persistent Set that does not contain [value].
       *  If [set] does not contain [value], it is returned unmodified.
       */
      let remove: (a, t) => t;

      /*** [subtract this that] returns an Persistent Set of unique value
       *  which occur in [this] but not in [that].
       */
      let subtract: (t, t) => t;

      /*** [union this that] returns an Persistent Set of unique values which occur in either [this] or [that]. */
      let union: (t, t) => t;
    };

    /*** Persistent Set module type signature for types with a parametric type arity of 1. */
    module type S1 = {
      type t('a);
      include S1 with type t('a) := t('a);
      include Collection.Persistent.S1 with type t('a) := t('a);

      /*** [add value set] returns a Persistent Set containing value. If [set] already contains [value],
       *  it is returned unmodified.
       */
      let add: ('a, t('a)) => t('a);

      /*** [addAll iter set] returns a Persistent Set with the values in [iter] and all the values in [set]. */
      let addAll: (Iterable.t('a), t('a)) => t('a);

      /*** [intersect this that] returns a Persistent Set of unique values
       *  which occur in both [this] and [that].
       */
      let intersect: (t('a), t('a)) => t('a);

      /*** [remove value set] returns a Persistent Set that does not contain [value].
       *  If [set] does not contain [value], it is returned unmodified.
       */
      let remove: ('a, t('a)) => t('a);

      /*** [subtract this that] returns an Persistent Set of unique values
       *  which occur in [this] but not in [that].
       */
      let subtract: (t('a), t('a)) => t('a);

      /*** [union this that] returns an Persistent Set of unique values which occur in either [this] or [that]. */
      let union: (t('a), t('a)) => t('a);
    };
  };

  /*** Module types implemented by transiently mutable sets.
   *
   *  By contract, all functions must be efficient, with no worst than O(log N) performance.
   */
  module Transient: {

    /*** Transient Set module type signature for types with a parametric type arity of 0. */
    module type S = {
      type a;
      type t;
      include Collection.Transient.S with type a := a and type t := t;

      /*** [add value transient] adds [value] to [transient]. If [transient] already contains [value],
       *  it is returned unmodified.
       */
      let add: (a, t) => t;

      /*** [addAll iter transient] adds all values in [iter] to [transient]. */
      let addAll: (Iterable.t(a), t) => t;

      /*** [contains value set] returns true if [set] contains at least one instace of [value],
       *  otherwise false;
       */
      let contains: (a, t) => bool;

      /*** [remove value transient] removes [value] from [transient].
       *  If [transient] does not contain [value], it is returned unmodified.
       */
      let remove: (a, t) => t;
    };

    /*** Transient Set module type signature for types with a parametric type arity of 0. */
    module type S1 = {
      type t('a);
      include Collection.Transient.S1 with type t('a) := t('a);

      /*** [add value transient] adds [value] to [transient]. If [transient] already contains [value],
       *  it is returned unmodified.
       */
      let add: ('a, t('a)) => t('a);

      /*** [addAll iter transient] adds all values in [iter] to [transient]. */
      let addAll: (Iterable.t('a), t('a)) => t('a);

      /*** [contains value set] returns true if [set] contains at least one instace of [value],
       *  otherwise false;
       */
      let contains: ('a, t('a)) => bool;

      /*** [remove value transient] removes [value] from [transient].
       *  If [transient] does not contain [value], it is returned unmodified.
       */
      let remove: ('a, t('a)) => t('a);
    };
  };
};

/*  Module types implemented by Sets that supports navigation operations. */
module rec NavigableSet: {

  /*** NavigableSet module type signature for types with a parametric type arity of 0. */
  module type S = {
    type a;
    type t;
    include Set.S with type a := a and type t := t;
    include NavigableCollection.S with type a := a and type t := t;

    /*** [toNavigableSet set] returns a NavigableSet view of [set]. */
    let toNavigableSet: t => NavigableSet.t(a);
    let toNavigableSetReversed: t => NavigableSet.t(a);
    let toSetReversed: t => Set.t(a);
  };

  /*** NavigableSet module type signature for types with a parametric type arity of 1. */
  module type S1 = {
    type t('a);
    include Set.S1 with type t('a) := t('a);
    include NavigableCollection.S1 with type t('a) := t('a);

    /*** [toNavigableSet set] returns a NavigableSet view of [set]. */
    let toNavigableSet: t('a) => NavigableSet.t('a);
    let toNavigableSetReversed: t('a) => NavigableSet.t('a);
    let toSetReversed: t('a) => Set.t('a);
  };

  /*** The Set type. */
  type t('a);

  include S1 with type t('a) := NavigableSet.t('a);

  /*** The empty Set. */
  let empty: unit => NavigableSet.t('a);

  /*** Module types implemented by NavigableSet collections supporting persistent mutations.
   *
   *  By contract, all functions must be efficient, with no worst than O(log N) performance.
   */
  module Persistent: {

    /*** Persistent NavigableSet module type signature for types with a parametric type arity of 0. */
    module type S = {
      type a;
      type t;
      include S with type a := a and type t := t;
      include Set.Persistent.S with type a := a and type t := t;

      /*** [removeFirstOrRaise set] returns a Persistent NavigableSet without
       *  the first value or raises an exception if [set] is empty.
       */
      let removeFirstOrRaise: t => t;

      /*** [removeLastOrRaise set] returns a Persistent NavigableSet without
       *  the last value or raises an exception if [set] is empty.
       */
      let removeLastOrRaise: t => t;
    };
  };
};

module KeyedStreamable: {
  module type S2 = {
    type t('k, 'v);

    /*** [defer f] returns a KeyedStreamable that invokes the function [f] whenever iterated. */
    let defer: (unit => t('k, 'v)) => t('k, 'v);

    /*** [distinctUntilChangedWith equals stream] returns a KeyedStreamable that contains only
     *  distinct contiguous key/value pairs from [stream] using [keyEquals] and [valueEquals] to
     *  equate key/value pairs.
     */
    let distinctUntilChangedWith:
      (~keyEquals: Equality.t('k), ~valueEquals: Equality.t('v), t('k, 'v)) => t('k, 'v);

    /*** [doOnNext f stream] returns a KeyedStreamable that applies the side effect
     *  function [f] to each key/value pair they are iterated.
     */
    let doOnNext: (('k, 'v) => unit, t('k, 'v)) => t('k, 'v);

    /*** [filter f stream] returns a KeyedStreamable only including key/value pairs from [stream]
     *  for which application of the predicate function [f] returns true.
     */
    let filter: (('k, 'v) => bool, t('k, 'v)) => t('k, 'v);

    /*** [flatMap mapper stream] returns a KeyedStreamable which applies [mapper] to each value in
     *  [stream], flattening the results.
     */
    let flatMap: (('kA, 'vA) => t('kB, 'vB), t('kA, 'vA)) => t('kB, 'vB);

    /*** [map keyMapper::keyMapper valueMapper::valueMapper stream] returns a KeyedStreamable
     *  whose keys are the result applying [keyMapper] to each key, and whose values are the result
     *  of applying [valueMapper] to each value in [stream].
     */
    let map:
      (~keyMapper: ('kA, 'vA) => 'kB, ~valueMapper: ('kA, 'vA) => 'vB, t('kA, 'vA)) => t('kB, 'vB);

    /*** [mapKeys mapper stream] returns a KeyedStreamable with mapper applied
     *  to each key in [stream].
     */
    let mapKeys: (('a, 'v) => 'b, t('a, 'v)) => t('b, 'v);

    /*** [mapValues mapper stream] returns a KeyedStreamable with mapper applied
     *  to each value in [stream].
     */
    let mapValues: (('k, 'a) => 'b, t('k, 'a)) => t('k, 'b);

    /*** [skip count stream] return a KeyedStreamable which skips the first [count]
     *  values in [stream].
     */
    let skip: (int, t('k, 'v)) => t('k, 'v);

    /*** [skipWhile f stream] return a KeyedStreamable which skips key/value pairs in [stream]
     *  while application of the predicate function [f] returns true, and then returns
     *  the remaining values.
     */
    let skipWhile: (('k, 'v) => bool, t('k, 'v)) => t('k, 'v);

    /*** [startWith key value stream] returns a KeyedStreamable whose first
     *  pair is (key, value), followed by the key/value pairs in [stream].
     */
    let startWith: ('k, 'v, t('k, 'v)) => t('k, 'v);

    /*** [take count stream] returns a KeyedStreamable with the first [count]
     *  key/value pairs in [stream].
     */
    let take: (int, t('k, 'v)) => t('k, 'v);

    /*** [takeWhile f stream] returns a KeyedStreamable including all values in [stream]
     *  while application of the predicate function [f] returns true, then completes.
     */
    let takeWhile: (('k, 'v) => bool, t('k, 'v)) => t('k, 'v);
  };
};

/*** Functional iterators over a collection of key/value pairs. KeyedIterable are stateless and can be reused. */
module rec KeyedIterable: {

  /*** KeyedIterable module type signature for types with a parametric type arity of 1. */
  module type S1 = {
    type k;
    type t('v);

    /*** [every f keyedIterable] returns true if the predicate [f] returns true for all
     *  key/value pairs in [keyedIterable]. If [keyedIterable] is empty, returns [true].
     */
    let every: ((k, 'v) => bool, t('v)) => bool;

    /*** [find f keyedIterable] return the Some of the first key/value pair in [keyedIterable]
     *  for which the the predicate f returns [true]. Otherwise None.
     */
    let find: (~selector: (k, 'v) => 'c, (k, 'v) => bool, t('v)) => option('c);

    /*** [findOrRaise f keyedIterable] return the the first key/value pair in [keyedIterable]
     *  for which the the predicate f returns [true]. Otherwise raises an exception.
     */
    let findOrRaise: (~selector: (k, 'v) => 'c, (k, 'v) => bool, t('v)) => 'c;

    /*** [forEach while_::predicate f keyedIterable] iterates through [keyedIterable] applying the
     *  side effect function [f] to each key/value pair, while [predicate] returns true
     */
    let forEach: (~while_: (k, 'v) => bool=?, (k, 'v) => unit, t('v)) => unit;

    /*** [keys keyedIter] returns an Iterable view of the keys in [keyedIter] */
    let keys: t('v) => Iterable.t(k);

    /*** [none f keyedIterable] returns true if the predicate [f] returns false
     *  for all key/value pairs in [keyedIterable]. If [keyedIterable] is empty, returns [true].
     */
    let none: ((k, 'v) => bool, t('v)) => bool;

    /*** [reduce while_::predicate initialValue f keyedIterable] applies the accumulator
     *  function [f] to each key/value pair in [keyedIterable], while [predicate] returns true,
     *  accumulating the result.
     */
    let reduce: (~while_: ('acc, k, 'v) => bool=?, ('acc, k, 'v) => 'acc, 'acc, t('v)) => 'acc;

    let reduceKeys: (~while_: ('acc, k) => bool=?, ('acc, k) => 'acc, 'acc, t('v)) => 'acc;
    let reduceValues: (~while_: ('acc, 'v) => bool=?, ('acc, 'v) => 'acc, 'acc, t('v)) => 'acc;

    /*** [some f keyedIterable] returns true if the predicate [f] returns true for at least
     *  one key/value pair in [keyedIterable]. If [keyedIterable] is empty, returns [false].
     */
    let some: ((k, 'v) => bool, t('v)) => bool;

    /*** [toIterable keyedIterable] returns an Iterable that can be used to iterate over
     *  the key/value pairs in [keyedIterable] as tuples.
     */
    let toIterable: ((k, 'v) => 'c, t('v)) => Iterable.t('c);

    /*** [toKeyedIterable keyedIterable] returns a KeyedIterable that can be used to iterate over
     *  the key/value pairs in [keyedIterable].
     */
    let toKeyedIterable: t('v) => KeyedIterable.t(k, 'v);

    /*** [values keyedIter] returns an Iterable view of the values in [keyedIter] */
    let values: t('v) => Iterable.t('v);
  };

  /*** KeyedIterable module type signature for types with a parametric type arity of 2. */
  module type S2 = {
    type t('k, 'v);

    /*** [every f keyedIterable] returns true if the predicate [f] returns true for all
     *  key/value pairs in [keyedIterable]. If [keyedIterable] is empty, returns [true].
     */
    let every: (('k, 'v) => bool, t('k, 'v)) => bool;

    /*** [find f keyedIterable] return the Some of the first key/value pair in [keyedIterable]
     *  for which the the predicate f returns [true]. Otherwise None.
     */
    let find: (~selector: ('k, 'v) => 'c, ('k, 'v) => bool, t('k, 'v)) => option('c);

    /*** [findOrRaise f keyedIterable] return the the first key/value pair in [keyedIterable]
     *  for which the the predicate f returns [true]. Otherwise raises an exception.
     */
    let findOrRaise: (~selector: ('k, 'v) => 'c, ('k, 'v) => bool, t('k, 'v)) => 'c;

    /*** [forEach while_::predicate f keyedIterable] iterates through [keyedIterable] applying the
     *  side effect function [f] to each key/value pair, while [predicate] returns true
     */
    let forEach: (~while_: ('k, 'v) => bool=?, ('k, 'v) => unit, t('k, 'v)) => unit;

    /*** [keys keyedIter] returns an Iterable view of the keys in [keyedIter] */
    let keys: t('k, 'v) => Iterable.t('k);

    /*** [none f keyedIterable] returns true if the predicate [f] returns false
     *  for all key/value pairs in [keyedIterable]. If [keyedIterable] is empty, returns [true].
     */
    let none: (('k, 'v) => bool, t('k, 'v)) => bool;

    /*** [reduce while_::predicate initialValue f keyedIterable] applies the accumulator
     *  function [f] to each key/value pair in [keyedIterable], while [predicate] returns true,
     *  accumulating the result.
     */
    let reduce:
      (~while_: ('acc, 'k, 'v) => bool=?, ('acc, 'k, 'v) => 'acc, 'acc, t('k, 'v)) => 'acc;

    let reduceKeys: (~while_: ('acc, 'k) => bool=?, ('acc, 'k) => 'acc, 'acc, t('k, 'v)) => 'acc;
    let reduceValues: (~while_: ('acc, 'v) => bool=?, ('acc, 'v) => 'acc, 'acc, t('k, 'v)) => 'acc;

    /*** [some f keyedIterable] returns true if the predicate [f] returns true for at least
     *  one key/value pair in [keyedIterable]. If [keyedIterable] is empty, returns [false].
     */
    let some: (('k, 'v) => bool, t('k, 'v)) => bool;

    /*** [toIterable keyedIterable] returns an Iterable that can be used to iterate over
     *  the key/value pairs in [keyedIterable] as tuples.
     */
    let toIterable: (('k, 'v) => 'c, t('k, 'v)) => Iterable.t('c);

    /*** [toKeyedIterable keyedIterable] returns a KeyedIterable that can be used to iterate over
     *  the key/value pairs in [keyedIterable].
     */
    let toKeyedIterable: t('k, 'v) => KeyedIterable.t('k, 'v);

    /*** [values keyedIter] returns an Iterable view of the values in [keyedIter] */
    let values: t('k, 'v) => Iterable.t('v);
  };
  type t('k, 'v);
  include KeyedStreamable.S2 with type t('k, 'v) := KeyedIterable.t('k, 'v);
  include S2 with type t('k, 'v) := KeyedIterable.t('k, 'v);

  /*** [concat stream] returns a KeyedIterable that lazily concatenates all the
   *  KeyedStreamables in [stream]. The resulting KeyedIterable returns all the key/value pairs
   *  in the first KeyedIterable, followed by all the key/value pairs in the second KeyedIterable,
   *  and continues until the last KeyedIterable completes.
   */
  let concat: list(t('k, 'v)) => t('k, 'v);

  /*** The empty KeyedCollection. */
  let empty: unit => KeyedIterable.t('k, 'v);

  /*** [fromEntries iter] returns a KeyedIterable view of key/value tuples in [iter]. */
  let fromEntries: Iterable.t(('k, 'v)) => KeyedIterable.t('k, 'v);

  /*** [generate genKey genValue k v] generates an infinite KeyedIterable
   *  where the keys are [k, genKey(k v), genKey(genKey(k v), v1), ...]
   *  and values are [v, genValue(k, v), genValue(k1, genValue(k, v)), ...]
   */
  let generate: (~genKey: ('k, 'v) => 'k, ~genValue: ('k, 'v) => 'v, 'k, 'v) => t('k, 'v);

  /*** [return key value] returns a KeyedIterable containing the pair ([key], [value]). */
  let return: ('k, 'v) => t('k, 'v);

  /*** [scan f acc stream] returns a KeyedStreamable of accumulated values resulting from the
   *  application of the accumulator function [f] to each value in [stream] with the
   *  specified initial value [acc].
   */
  let scan: (('acc, 'k, 'v) => 'acc, 'acc, KeyedIterable.t('k, 'v)) => Iterable.t('acc);
};

/*** Module types implemented by all immutable keyed collections. This module
 *  signature does not impose any restrictions on the relationship between
 *  keys and associated values.
 *
 *  By contract, all functions have a computational complexity of O(1),
 *  unless otherwise noted.
 */
module rec KeyedCollection: {

  /*** KeyedCollection module type signature for types with a parametric type arity of 1. */
  module type S1 = {
    type k;
    type t('v);
    include KeyedIterable.S1 with type k := k and type t('v) := t('v);

    /*** [containsKey key keyed] returns true if [keyed] contains an association from [key] to
     *  one or more values, otherwise false.
     *
     *  By contract, [containsKey] is efficient with no worst than O(log N) performance.
     */
    let containsKey: (k, t('v)) => bool;

    /*** [count keyed] returns number of key/value pairs contained in [keyed]. */
    let count: t('v) => int;

    /*** [isEmpty keyed] returns true if [keyed] is empty, otherwise false. */
    let isEmpty: t('v) => bool;

    /*** [isNotEmpty keyed] returns true if [keyed] contains at
     *  least one value, otherwise false.
     */
    let isNotEmpty: t('v) => bool;

    let keysCollection: t('v) => Collection.t(k);
    let keysSequence: t('v) => Sequence.t(k);

    /* [toKeyedCollection keyed] returns KeyedCollection view. */
    let toKeyedCollection: t('v) => KeyedCollection.t(k, 'v);

    /* [toSequence keyed] returns an Sequence that can be used to enumerate
     * the key/value pairs in [keyed] as tuples.
     */
    let toSequence: ((k, 'v) => 'c, t('v)) => Sequence.t('c);

    let valuesCollection: t('v) => Collection.t('v);
    let valuesSequence: t('v) => Sequence.t('v);
  };

  /*** KeyedCollection module type signature for types with a parametric type arity of 2. */
  module type S2 = {
    type t('k, 'v);
    include KeyedIterable.S2 with type t('k, 'v) := t('k, 'v);

    /*** [containsKey key keyed] returns true if [keyed] contains an association from [key] to
     *  one or more values, otherwise false.
     *
     *  By contract, [containsKey] is efficient with no worst than O(log N) performance.
     */
    let containsKey: ('k, t('k, 'v)) => bool;

    /*** [count keyed] returns number of key/value pairs contained in [keyed]. */
    let count: t('k, 'v) => int;

    /*** [isEmpty keyed] returns true if [keyed] is empty, otherwise false. */
    let isEmpty: t('k, 'v) => bool;

    /*** [isNotEmpty keyed] returns true if [keyed] contains at
     *  least one value, otherwise false.
     */
    let isNotEmpty: t('k, 'v) => bool;

    let keysCollection: t('k, 'v) => Collection.t('k);
    let keysSequence: t('k, 'v) => Sequence.t('k);

    /* [toKeyedCollection keyed] returns KeyedCollection view. */
    let toKeyedCollection: t('k, 'v) => KeyedCollection.t('k, 'v);

    /* [toSequence keyed] returns an Sequence that can be used to enumerate
     * the key/value pairs in [keyed] as tuples.
     */
    let toSequence: (('k, 'v) => 'c, t('k, 'v)) => Sequence.t('c);
    let valuesCollection: t('k, 'v) => Collection.t('v);
    let valuesSequence: t('k, 'v) => Sequence.t('v);
  };
  type t('k, 'v);
  include S2 with type t('k, 'v) := KeyedCollection.t('k, 'v);

  /*** The empty KeyedCollection. */
  let empty: unit => KeyedCollection.t('k, 'v);

  /*** Module types implemented by KeyedCollections supporting fully persistent mutations.
   *  Mutation operations on these types do not mutate the underlying collection, but instead
   *  create a new collection with the mutation applied.
   */
  module Persistent: {

    /*** Persistent KeyedCollection module type signature for types with a parametric type arity of 1. */
    module type S1 = {
      type k;
      type t('v);
      include S1 with type k := k and type t('v) := t('v);

      /*** [remove key keyed] removes all values associated with [key] from [keyed]
       *
       *  By contract, [remove] is efficient with no worst than O(log N) performance.
       */
      let remove: (k, t('v)) => t('v);

      /*** [removeAll keyed] return an empty Persistent KeyedCollection. Depending on the implementation,
       *  the new Persistent KeyedCollection may share the same configuration as [keyed]. For instance,
       *  the HashMap implementation shares the same hash and comparison functions.
       *
       *  Computational complexity: O(1)
       */
      let removeAll: t('v) => t('v);
    };

    /*** Persistent KeyedCollection module type signature for types with a parametric type arity of 2. */
    module type S2 = {
      type t('k, 'v);
      include S2 with type t('k, 'v) := t('k, 'v);

      /*** [remove key keyed] removes all values associated with [key] from [keyed]
       *
       *  By contract, [remove] is efficient with no worst than O(log N) performance.
       */
      let remove: ('k, t('k, 'v)) => t('k, 'v);

      /*** [removeAll keyed] return an empty Persistent KeyedCollection. Depending on the implementation,
       *  the new Persistent KeyedCollection may share the same configuration as [keyed]. For instance,
       *  the HashMap implementation shares the same hash and comparison functions.
       *
       *  Computational complexity: O(1)
       */
      let removeAll: t('k, 'v) => t('k, 'v);
    };
  };

  /*** Module types implemented by transiently mutable KeyedCollections. Transient Collections
   *  are designed to enable fast and efficient batch operations by temporarily enabling mutation
   *  of an underlying collection type. Unlike Persistent KeyedCollection functions, Transient KeyedCollection
   *  APIs always return the same value reference passed in as an argument, with mutations applied.
   *
   *  By contract, all functions have a computational complexity of O(1), unless otherwise noted.
   */
  module Transient: {
    /*** Transient KeyedCollection module type signature for types with a parametric type arity of 1. */
    module type S1 = {
      type k;
      type t('v);

      /*** [containsKey key transient] returns true if [transient] contains an association from [key] to
       *  one or more values, otherwise false.
       *
       *  By contract, [containsKey] is efficient with no worst than O(log N) performance.
       */
      let containsKey: (k, t('v)) => bool;

      /*** [count transient] returns number of key/value pairs contained in [transient]. */
      let count: t('v) => int;

      /*** [isEmpty transient] returns true if [transient] is empty, otherwise false. */
      let isEmpty: t('v) => bool;

      /*** [isNotEmpty transient] returns true if [transient] contains at
       *  least one value, otherwise false.
       */
      let isNotEmpty: t('v) => bool;

      /*** [remove key transient] removes all values associated with [key] from [transient]
       *
       *  By contract, [remove] is efficient with no worst than O(log N) performance.
       */
      let remove: (k, t('v)) => t('v);

      /*** [removeAll transient] removes all key/value pairs from [transient]. */
      let removeAll: t('v) => t('v);
    };

    /*** Transient KeyedCollection module type signature for types with a parametric type arity of 1. */
    module type S2 = {
      type t('k, 'v);

      /*** [containsKey key transient] returns true if [transient] contains an association from [key] to
       *  one or more values, otherwise false.
       *
       *  By contract, [containsKey] is efficient with no worst than O(log N) performance.
       */
      let containsKey: ('k, t('k, 'v)) => bool;

      /*** [count transient] returns number of key/value pairs contained in [transient]. */
      let count: t('k, 'v) => int;

      /*** [isEmpty transient] returns true if [transient] is empty, otherwise false. */
      let isEmpty: t('k, 'v) => bool;

      /*** [isNotEmpty transient] returns true if [transient] contains at
       *  least one value, otherwise false.
       */
      let isNotEmpty: t('k, 'v) => bool;

      /*** [remove key transient] removes all values associated with [key] from [transient]
       *
       *  By contract, [remove] is efficient with no worst than O(log N) performance.
       */
      let remove: ('k, t('k, 'v)) => t('k, 'v);

      /*** [removeAll transient] removes all key/value pairs from [transient]. */
      let removeAll: t('k, 'v) => t('k, 'v);
    };
  };
};

/*** Module types implemented by KeyedCollections that are ordered or sorted and support
 *  navigation operations.
 */
module rec NavigableKeyedCollection: {

  /*** NavigableKeyedCollection module type signature for types with a parametric type arity of 1. */
  module type S1 = {
    type k;
    type t('v);
    include KeyedCollection.S1 with type k := k and type t('v) := t('v);

    /*** [first keyed] returns first value in [keyed] or None.
     *
     *  By contract, no worst than O(log N) performance.
     */
    let first: ((k, 'v) => 'c, t('v)) => option('c);

    /*** [firstOrRaise keyed] returns first value in [keyed] or raises an exception.
     *
     *  By contract, no worst than O(log N) performance.
     */
    let firstOrRaise: ((k, 'v) => 'c, t('v)) => 'c;

    let keysCollectionReversed: t('v) => Collection.t(k);
    let keysNavigableCollection: t('v) => NavigableCollection.t(k);
    let keysNavigableCollectionReversed: t('v) => NavigableCollection.t(k);
    let keysReversed: t('v) => Iterable.t(k);
    let keysSequentialCollection: t('v) => SequentialCollection.t(k);
    let keysSequentialCollectionReversed: t('v) => SequentialCollection.t(k);
    let keysSequenceReversed: t('v) => Sequence.t(k);

    /*** [last keyed] returns last value in [keyed] or None.
     *
     *  By contract, no worst than O(log N) performance.
     */
    let last: ((k, 'v) => 'c, t('v)) => option('c);

    /*** [lastOrRaise keyed] returns last value in [keyed] or raises an exception.
     *
     *  By contract, no worst than O(log N) performance.
     */
    let lastOrRaise: ((k, 'v) => 'c, t('v)) => 'c;

    /*** [reduceReversed while_::predicate initialValue f keyedIterable] applies the accumulator
     *  function [f] to each key/value pair in [keyedIterable] while [predicate] returns true, starting
     *  from the right most key/value pair, accumulating the result.
     */
    let reduceReversed:
      (~while_: ('acc, k, 'v) => bool=?, ('acc, k, 'v) => 'acc, 'acc, t('v)) => 'acc;

    let reduceKeysReversed: (~while_: ('acc, k) => bool=?, ('acc, k) => 'acc, 'acc, t('v)) => 'acc;
    let reduceValuesReversed:
      (~while_: ('acc, 'v) => bool=?, ('acc, 'v) => 'acc, 'acc, t('v)) => 'acc;

    /*** [toIterableReversed keyedIterable] returns an Iterable that can be used to iterate over
     *  the key/value pairs in [keyedIterable] as tuples.
     */
    let toIterableReversed: ((k, 'v) => 'c, t('v)) => Iterable.t('c);

    let toKeyedCollectionReversed: t('v) => KeyedCollection.t(k, 'v);

    /*** [toKeyedIterableReversed keyedIterable] returns a KeyedIterable that can be used to iterate over
     *  the key/value pairs in [keyedIterable].
     */
    let toKeyedIterableReversed: t('v) => KeyedIterable.t(k, 'v);

    let toNavigableKeyedCollection: t('v) => NavigableKeyedCollection.t(k, 'v);
    let toNavigableKeyedCollectionReversed: t('v) => NavigableKeyedCollection.t(k, 'v);

    /* [toSequenceReversed keyed] returns an Sequence that can be used to enumerate
     * the key/value pairs in [keyed] as tuples from right to left.
     */
    let toSequenceReversed: ((k, 'v) => 'c, t('v)) => Sequence.t('c);

    let valuesCollectionReversed: t('v) => Collection.t('v);
    let valuesNavigableCollection: t('v) => NavigableCollection.t('v);
    let valuesNavigableCollectionReversed: t('v) => NavigableCollection.t('v);
    let valuesReversed: t('v) => Iterable.t('v);
    let valuesSequentialCollection: t('v) => SequentialCollection.t('v);
    let valuesSequentialCollectionReversed: t('v) => SequentialCollection.t('v);
    let valuesSequenceReversed: t('v) => Sequence.t('v);
  };

  /*** NavigableKeyedCollection module type signature for types with a parametric type arity of 2. */
  module type S2 = {
    type t('k, 'v);
    include KeyedCollection.S2 with type t('k, 'v) := t('k, 'v);

    /*** [first keyed] returns first value in [keyed] or None.
     *
     *  By contract, no worst than O(log N) performance.
     */
    let first: (('k, 'v) => 'c, t('k, 'v)) => option('c);

    /*** [firstOrRaise keyed] returns first value in [keyed] or raises an exception.
     *
     *  By contract, no worst than O(log N) performance.
     */
    let firstOrRaise: (('k, 'v) => 'c, t('k, 'v)) => 'c;

    let keysCollectionReversed: t('k, 'v) => Collection.t('k);
    let keysNavigableCollection: t('k, 'v) => NavigableCollection.t('k);
    let keysNavigableCollectionReversed: t('k, 'v) => NavigableCollection.t('k);
    let keysReversed: t('k, 'v) => Iterable.t('k);
    let keysSequentialCollection: t('k, 'v) => SequentialCollection.t('k);
    let keysSequentialCollectionReversed: t('k, 'v) => SequentialCollection.t('k);
    let keysSequenceReversed: t('k, 'v) => Sequence.t('k);

    /*** [last keyed] returns last value in [keyed] or None.
     *
     *  By contract, no worst than O(log N) performance.
     */
    let last: (('k, 'v) => 'c, t('k, 'v)) => option('c);

    /*** [lastOrRaise keyed] returns last value in [keyed] or raises an exception.
     *
     *  By contract, no worst than O(log N) performance.
     */
    let lastOrRaise: (('k, 'v) => 'c, t('k, 'v)) => 'c;

    /*** [reduceReversed while_::predicate initialValue f keyedIterable] applies the accumulator
     *  function [f] to each key/value pair in [keyedIterable] while [predicate] returns true, starting
     *  from the right most key/value pair, accumulating the result.
     */
    let reduceReversed:
      (~while_: ('acc, 'k, 'v) => bool=?, ('acc, 'k, 'v) => 'acc, 'acc, t('k, 'v)) => 'acc;

    let reduceKeysReversed:
      (~while_: ('acc, 'k) => bool=?, ('acc, 'k) => 'acc, 'acc, t('k, 'v)) => 'acc;
    let reduceValuesReversed:
      (~while_: ('acc, 'v) => bool=?, ('acc, 'v) => 'acc, 'acc, t('k, 'v)) => 'acc;

    /*** [toIterableReversed keyedIterable] returns an Iterable that can be used to iterate over
     *  the key/value pairs in [keyedIterable] as tuples.
     */
    let toIterableReversed: (('k, 'v) => 'c, t('k, 'v)) => Iterable.t('c);

    let toKeyedCollectionReversed: t('k, 'v) => KeyedCollection.t('k, 'v);

    /*** [toKeyedIterableReversed keyedIterable] returns a KeyedIterable that can be used to iterate over
     *  the key/value pairs in [keyedIterable].
     */
    let toKeyedIterableReversed: t('k, 'v) => KeyedIterable.t('k, 'v);

    let toNavigableKeyedCollection: t('k, 'v) => NavigableKeyedCollection.t('k, 'v);
    let toNavigableKeyedCollectionReversed: t('k, 'v) => NavigableKeyedCollection.t('k, 'v);

    /* [toSequenceReversed keyed] returns an Sequence that can be used to enumerate
     * the key/value pairs in [keyed] as tuples from right to left.
     */
    let toSequenceReversed: (('k, 'v) => 'c, t('k, 'v)) => Sequence.t('c);

    let valuesCollectionReversed: t('k, 'v) => Collection.t('v);
    let valuesNavigableCollection: t('k, 'v) => NavigableCollection.t('v);
    let valuesNavigableCollectionReversed: t('k, 'v) => NavigableCollection.t('v);
    let valuesReversed: t('k, 'v) => Iterable.t('v);
    let valuesSequentialCollection: t('k, 'v) => SequentialCollection.t('v);
    let valuesSequentialCollectionReversed: t('k, 'v) => SequentialCollection.t('v);
    let valuesSequenceReversed: t('k, 'v) => Sequence.t('v);
  };
  type t('k, 'v);
  include S2 with type t('k, 'v) := NavigableKeyedCollection.t('k, 'v);

  /*** The empty NavigableKeyedCollection. */
  let empty: unit => NavigableKeyedCollection.t('k, 'v);

};

/*** A read only view of a mappings keys to values. The intent of this type is to enable
 *  interop between alternative concrete implementations such as SortedMap and HashMap.
 *  The complexity of functions in this module is dependent upon the underlying concrete implementation.
 */
module rec Map: {

  /*** Map module type signature for types with a parametric type arity of 1. */
  module type S1 = {
    type k;
    type t('v);
    include KeyedCollection.S1 with type k := k and type t('v) := t('v);

    /*** [get key map] returns the value associated with [key] in [map] or None */
    let get: (k, t('v)) => option('v);

    let getOrDefault: (~default: 'v, k, t('v)) => 'v;

    /*** [getOrRaise key map] returns the value associated with [key] in [map] or raises an exception */
    let getOrRaise: (k, t('v)) => 'v;

    /*** [keysSet keyed] return a Set view of the keys in [keyed]. */
    let keysSet: t('v) => Set.t(k);

    /*** [toMap map] returns a Map view of [map] */
    let toMap: t('v) => Map.t(k, 'v);
  };

  /*** Map module type signature for types with a parametric type arity of 1. */
  module type S2 = {
    type t('k, 'v);
    include KeyedCollection.S2 with type t('k, 'v) := t('k, 'v);

    /*** [get key map] returns the value associated with [key] in [map] or None */
    let get: ('k, t('k, 'v)) => option('v);

    let getOrDefault: (~default: 'v, 'k, t('k, 'v)) => 'v;

    /*** [getOrRaise key map] returns the value associated with [key] in [map] or raises an exception */
    let getOrRaise: ('k, t('k, 'v)) => 'v;

    /*** [keysSet keyed] return a Set view of the keys in [keyed]. */
    let keysSet: t('k, 'v) => Set.t('k);

    /*** [toMap map] returns a Map view of [map] */
    let toMap: t('k, 'v) => Map.t('k, 'v);
  };

  /*** The map type. */
  type t('k, 'v);

  include S2 with type t('k, 'v) := Map.t('k, 'v);

  /*** The empty Map. */
  let empty: unit => Map.t('k, 'v);

  /*** Module types implemented by Map collections supporting persistent mutations.
   *
   */
  module Persistent: {

    /*** Persistent Map module type signature for types with a parametric type arity of 1. */
    module type S1 = {
      type k;
      type t('v);
      include KeyedCollection.Persistent.S1 with type k := k and type t('v) := t('v);
      include S1 with type k := k and type t('v) := t('v);

      /*** [alter key f map] return a Persistent Map applying the mutation function to the value
       *  associated with key or None if no association exists. If [f] returns Some, the value
       *  associated with key is either added or updated. If [f] returns None,
       *  the value associated with key is removed if an association previously existed.
       *
       *  By contract, [alter] is efficient with no worst than O(log N) performance.
       */
      let alter: (k, option('v) => option('v), t('v)) => t('v);

      /*** [merge f acc next] return a Persistent Map that is the result of reducing [acc] with [next].
       *  The callback [f] is applied to the union of keys from [acc] and [next], with the values
       *  associated with each key, or None. If [f] returns None, the associated key/value pair is
       *  removed from the accumulator. If [f] returns Some, the associated key/value pair is
       *  added or update to the accumulator.
       *
       *  By contract, [merge] is efficient with no worst than O(N log N) performance.
       */
      let merge: ((k, option('vAcc), option('v)) => option('vAcc), t('vAcc), t('v)) => t('vAcc);

      /*** [put key value map] returns a Persistent Map with an association
       *  from [key] to [value] added [map].
       *
       *  By contract, [put] is efficient with no worst than O(log N) performance.
       */
      let put: (k, 'v, t('v)) => t('v);

      /*** [putAll keyedIter map] returns a Persistent Map, adding associations from all key/value pairs
       *  in [keyedIter] to [map],
       *
       *  By contract, [putAll] is efficient with no worst than O(N log N) performance.
       */
      let putAll: (KeyedIterable.t(k, 'v), t('v)) => t('v);

      /*** [putAllEntries iter map] returns a Persistent Map, adding associations from all key/value pairs
       *  in [iter] to [map].
       *
       *  By contract, [putAllEntries] is efficient with no worst than O(N log N) performance.
       */
      let putAllEntries: (Iterable.t((k, 'v)), t('v)) => t('v);
    };

    /*** Persistent Map module type signature for types with a parametric type arity of 1. */
    module type S2 = {
      type t('k, 'v);
      include KeyedCollection.Persistent.S2 with type t('k, 'v) := t('k, 'v);
      include S2 with type t('k, 'v) := t('k, 'v);

      /*** [alter key f map] return a Persistent Map applying the mutation function to the value
       *  associated with key or None if no association exists. If [f] returns Some, the value
       *  associated with key is either added or updated. If [f] returns None,
       *  the value associated with key is removed if an association previously existed.
       *
       *  By contract, [alter] is efficient with no worst than O(log N) performance.
       */
      let alter: ('k, option('v) => option('v), t('k, 'v)) => t('k, 'v);

      /*** [merge f acc next] return a Persistent Map that is the result of reducing [acc] with [next].
       *  The callback [f] is applied to the union of keys from [acc] and [next], with the values
       *  associated with each key, or None. If [f] returns None, the associated key/value pair is
       *  removed from the accumulator. If [f] returns Some, the associated key/value pair is
       *  added or update to the accumulator.
       *
       *  By contract, [merge] is efficient with no worst than O(N log N) performance.
       */
      let merge:
        (('k, option('vAcc), option('v)) => option('vAcc), t('k, 'vAcc), t('k, 'v)) => t('k, 'vAcc);

      /*** [put key value map] returns a Persistent Map with an association
       *  from [key] to [value] added [map].
       *
       *  By contract, [put] is efficient with no worst than O(log N) performance.
       */
      let put: ('k, 'v, t('k, 'v)) => t('k, 'v);

      /*** [putAll keyedIter map] returns a Persistent Map, adding associations from all key/value pairs
       *  in [keyedIter] to [map],
       *
       *  By contract, [putAll] is efficient with no worst than O(N log N) performance.
       */
      let putAll: (KeyedIterable.t('k, 'v), t('k, 'v)) => t('k, 'v);

      /*** [putAllEntries iter map] returns a Persistent Map, adding associations from all key/value pairs
       *  in [iter] to [map].
       *
       *  By contract, [putAllEntries] is efficient with no worst than O(N log N) performance.
       */
      let putAllEntries: (Iterable.t(('k, 'v)), t('k, 'v)) => t('k, 'v);
    };
  };

  /*** Module types implemented by transiently mutable maps. */
  module Transient: {

    /*** Transient Map module type signature for types with a parametric type arity of 1. */
    module type S1 = {
      type k;
      type t('v);
      include KeyedCollection.Transient.S1 with type k := k and type t('v) := t('v);

      /*** [alter key f transient] applies the mutation function to the value
       *  associated with key or None if no association exists. If [f] returns Some, the value
       *  associated with key is either added or updated to [transient]. If [f] returns None,
       *  the value associated with key is removed if an association previously existed in [transient].
       *
       *  By contract, [alter] is efficient with no worst than O(log N) performance.
       */
      let alter: (k, option('v) => option('v), t('v)) => t('v);

      /*** [get key transient] returns the value associated with [key] in [transient] or None */
      let get: (k, t('v)) => option('v);

      let getOrDefault: (~default: 'v, k, t('v)) => 'v;

      /*** [getOrRaise key transient] returns the value associated with [key] in [transient] or raises an exception */
      let getOrRaise: (k, t('v)) => 'v;

      /*** [put key value transient] adds or replaces an association [key] to [value] in [transient].
       *
       *  By contract, [put] is efficient with no worst than O(log N) performance.
       */
      let put: (k, 'v, t('v)) => t('v);

      /*** [putAll keyedIter transient] adds associations from all key/value pairs in [keyedIter] to [transient].
       *
       *  By contract, [putAll] is efficient with no worst than O(N log N) performance.
       */
      let putAll: (KeyedIterable.t(k, 'v), t('v)) => t('v);

      /*** [putAll keyedIter transient] adds associations from all key/value pairs in [keyedIter] to [transient].
       *
       *  By contract, [putAll] is efficient with no worst than O(N log N) performance.
       */
      let putAllEntries: (Iterable.t((k, 'v)), t('v)) => t('v);
    };

    module type S2 = {
      type t('k, 'v);
      include KeyedCollection.Transient.S2 with type t('k, 'v) := t('k, 'v);

      /*** [alter key f transient] applies the mutation function to the value
       *  associated with key or None if no association exists. If [f] returns Some, the value
       *  associated with key is either added or updated to [transient]. If [f] returns None,
       *  the value associated with key is removed if an association previously existed in [transient].
       *
       *  By contract, [alter] is efficient with no worst than O(log N) performance.
       */
      let alter: ('k, option('v) => option('v), t('k, 'v)) => t('k, 'v);

      /*** [get key transient] returns the value associated with [key] in [transient] or None */
      let get: ('k, t('k, 'v)) => option('v);

      let getOrDefault: (~default: 'v, 'k, t('k, 'v)) => 'v;

      /*** [getOrRaise key transient] returns the value associated with [key] in [transient] or raises an exception */
      let getOrRaise: ('k, t('k, 'v)) => 'v;

      /*** [put key value transient] adds or replaces an association [key] to [value] in [transient].
       *
       *  By contract, [put] is efficient with no worst than O(log N) performance.
       */
      let put: ('k, 'v, t('k, 'v)) => t('k, 'v);

      /*** [putAll keyedIter transient] adds associations from all key/value pairs in [keyedIter] to [transient].
       *
       *  By contract, [putAll] is efficient with no worst than O(N log N) performance.
       */
      let putAll: (KeyedIterable.t('k, 'v), t('k, 'v)) => t('k, 'v);

      /*** [putAll keyedIter transient] adds associations from all key/value pairs in [keyedIter] to [transient].
       *
       *  By contract, [putAll] is efficient with no worst than O(N log N) performance.
       */
      let putAllEntries: (Iterable.t(('k, 'v)), t('k, 'v)) => t('k, 'v);
    };
  };
};

/*  Module types implemented by NavigableMap that supports navigation operations. */
module rec NavigableMap: {

  /*** NavigableMap module type signature for types with a parametric type arity of 1. */
  module type S1 = {
    type k;
    type t('v);
    include NavigableKeyedCollection.S1 with type k := k and type t('v) := t('v);
    include Map.S1 with type k := k and type t('v) := t('v);
    let keysNavigableSet: t('v) => NavigableSet.t(k);
    let keysNavigableSetReversed: t('v) => NavigableSet.t(k);
    let keysSet: t('v) => ImmSet.t(k);
    let toMapReversed: t('v) => ImmMap.t(k, 'v);
    let toNavigableMap: t('v) => NavigableMap.t(k, 'v);
    let toNavigableMapReversed: t('v) => NavigableMap.t(k, 'v);
  };

  /*** NavigableMap module type signature for types with a parametric type arity of 1. */
  module type S2 = {
    type t('k, 'v);
    include NavigableKeyedCollection.S2 with type t('k, 'v) := t('k, 'v);
    include Map.S2 with type t('k, 'v) := t('k, 'v);
    let keysNavigableSet: t('k, 'v) => NavigableSet.t('k);
    let keysNavigableSetReversed: t('k, 'v) => NavigableSet.t('k);
    let keysSet: t('k, 'v) => ImmSet.t('k);
    let toMapReversed: t('k, 'v) => ImmMap.t('k, 'v);
    let toNavigableMap: t('k, 'v) => NavigableMap.t('k, 'v);
    let toNavigableMapReversed: t('k, 'v) => NavigableMap.t('k, 'v);
  };

  /*** The NavigableMap type. */
  type t('k, 'v);

  include S2 with type t('k, 'v) := NavigableMap.t('k, 'v);

  /*** Module types implemented by NavigableMaps supporting persistent mutations.
   *
   *  By contract, all functions must be efficient, with no worst than O(log N) performance.
   */
  module Persistent: {

    /*** Persistent NavigableMap module type signature for types with a parametric type arity of 1. */
    module type S1 = {
      type k;
      type t('v);
      include S1 with type k := k and type t('v) := t('v);
      include Map.Persistent.S1 with type k := k and type t('v) := t('v);

      /*** [removeFirstOrRaise map] returns a Persistent NavigableMap without
       *  the first value or raises an exception if [map] is empty.
       */
      let removeFirstOrRaise: t('v) => t('v);

      /*** [removeLastOrRaise map] returns a Persistent NavigableMap without
       *  the last value or raises an exception if [map] is empty.
       */
      let removeLastOrRaise: t('v) => t('v);
    };
  };
};

/*** Collections that support efficient indexed access to values.
 *
 *  By contract, all functions must be efficient, with no worst than O(log N) performance.
 */
module rec Indexed: {

  /*** Indexed module type signature for types with a parametric type arity of 1. */
  module type S1 = {
    type t('a);
    include NavigableCollection.S1 with type t('a) := t('a);

    /*** [get index indexed] returns the value at [index] or None if [index] is out of bounds. */
    let get: (int, t('a)) => option('a);

    let getOrDefault: (~default: 'a, int, t('a)) => 'a;

    /*** [getOrRaise index indexed] returns the value at [index] or
     *  raises an exception if [index] is out of bounds.
     */
    let getOrRaise: (int, t('a)) => 'a;

    let toIndexed: t('a) => Indexed.t('a);
    let toIndexedReversed: t('a) => Indexed.t('a);

    /*** [toKeyedCollection indexed] returns a KeyedCollection view of
     *  the index/value pairs in [indexed].
     */
    let toKeyedCollection: t('a) => KeyedCollection.t(int, 'a);

    let toKeyedCollectionReversed: t('a) => KeyedCollection.t(int, 'a);

    /*** [toKeyedIterable indexed] returns a KeyedIterable that can be used to iterate over
     *  the index/value pairs in [indexed].
     */
    let toKeyedIterable: t('a) => KeyedIterable.t(int, 'a);

    /* [toKeyedIterableReversed indexed] returns an KeyedIterable that can be used to iterate over
     * the index/value pairs in [indexed] from right to left.
     */
    let toKeyedIterableReversed: t('a) => KeyedIterable.t(int, 'a);

    /*** [toMap indexed] returns a Map view of [indexed]. */
    let toMap: t('a) => Map.t(int, 'a);

    let toMapReversed: t('a) => Map.t(int, 'a);
    let toNavigableKeyedCollection: t('a) => NavigableKeyedCollection.t(int, 'a);
    let toNavigableKeyedCollectionReversed: t('a) => NavigableKeyedCollection.t(int, 'a);
    let toNavigableMap: t('a) => NavigableMap.t(int, 'a);
    let toNavigableMapReversed: t('a) => NavigableMap.t(int, 'a);
  };
  type t('a);
  include S1 with type t('a) := Indexed.t('a);

  /*** An Indexed collection supporting persistent mutations. */
  module Persistent: {
    module type S1 = {
      type t('a);
      include S1 with type t('a) := t('a);
      include NavigableCollection.Persistent.S1 with type t('a) := t('a);

      /*** [concat indexed] returns a persistent Indexed collection concatenating
       *  together the in [indexed].
       */
      let concat: list(t('a)) => t('a);

      /*** [insertAt index value indexed] returns a persistent Indexed collection with
       *  [value] inserted at [index].
       */
      let insertAt: (int, 'a, t('a)) => t('a);

      /*** [removeAt index indexed] returns a persistent Indexed collection with the
       *  value at [index] removed.
       */
      let removeAt: (int, t('a)) => t('a);

      /*** [skip count indexed] returns a persistent Indexed collection that removes the
       *  first [count] values in [indexed].
       */
      let skip: (int, t('a)) => t('a);

      let slice: (~start: int=?, ~end_: int=?, t('a)) => t('a);

      /*** [take count indexed] returns a Vector that includes the first [count] values in [indexed]. */
      let take: (int, t('a)) => t('a);

      /*** [update index value indexed] returns a persistent Indexed collection with [value]
       *  replacing the value at [index].
       */
      let update: (int, 'a, t('a)) => t('a);

      /*** [updateAll f indexed] returns a persistent Indexed collection updating each value
       *  in [indexed] with result of applying the function [f] to each index/value pair.
       *
       *  Complexity: O(N)
       */
      let updateAll: ((int, 'a) => 'a, t('a)) => t('a);

      /*** [updateWith index f indexed] returns a persistent Indexed collection updating the value
       *  at [index] with the result of applying the function [f] to the value.
       */
      let updateWith: (int, 'a => 'a, t('a)) => t('a);
    };
  };

  /*** A temporarily mutable Indexed collection. Once persisted, any further operations on a
   *  Transient instance will throw. Intended for implementing bulk mutation
   *  operations efficiently.
   */
  module Transient: {
    module type S1 = {
      type t('a);
      include NavigableCollection.Transient.S1 with type t('a) := t('a);

      /*** [get index transient] returns the value at [index] or None if [index] is out of bounds. */
      let get: (int, t('a)) => option('a);

      let getOrDefault: (~default: 'a, int, t('a)) => 'a;

      /*** [getOrRaise index transient] returns the value at [index] or
       *  raises an exception if [index] is out of bounds.
       */
      let getOrRaise: (int, t('a)) => 'a;

      /*** [insertAt index value transient] inserts value into [transient] at [index]. */
      let insertAt: (int, 'a, t('a)) => t('a);

      /*** [removeAt index transient] removes the value at [index]. */
      let removeAt: (int, t('a)) => t('a);

      /*** [update index value transient] replaces the value at [index] with [value]. */
      let update: (int, 'a, t('a)) => t('a);

      /*** [updateAll f transient] updates each value in [transient] with result of applying
       *  the function [f] to each index/value pair.
       */
      let updateAll: ((int, 'a) => 'a, t('a)) => t('a);

      /*** [updateWith index f transient] updates the value at [index] with the result
       *  of applying the function [f] to the value.
       */
      let updateWith: (int, 'a => 'a, t('a)) => t('a);
    };
  };
};

/*** A double-ended queue with efficient appends [addLast], prepends [addFirst]
 *  and removals from either end of the queue [removeFirstOrRaise] [removeLastOrRaise].
 */
module rec Deque: {
  type t('a);
  include NavigableCollection.Persistent.S1 with type t('a) := Deque.t('a);

  /*** [reverse deque] returns a new Deque with [deque]'s values reversed.
   *
   *  Complexity: O(1)
   */
  let reverse: Deque.t('a) => Deque.t('a);

  /*** A temporarily mutable Deque. Once persisted, any further operations on a
   *  Transient Deque instance will raise exceptions. Intended for implementing bulk
   *  mutation operations efficiently.
   */
  module Transient: {

    /*** The Transient Deque type. */
    type t('a);
    include NavigableCollection.Transient.S1 with type t('a) := Deque.Transient.t('a);

    let empty: unit => Deque.Transient.t('a);

    /*** [persist transient] persists [transient] returning a Deque. Further attempts
     *  to access or mutate [transient] will raise exceptions.
     */
    let persist: Deque.Transient.t('a) => Deque.t('a);

    /*** [reverse transient] reverse [transient]'s values.
     *
     *  Complexity: O(1)
     */
    let reverse: Deque.Transient.t('a) => Deque.Transient.t('a);
  };
  let empty: unit => Deque.t('a);
  let from: Iterable.t('a) => Deque.t('a);
  let fromReverse: Iterable.t('a) => Deque.t('a);

  /*** [mutate deque] returns a Transient Deque containing the same values as [deque]. */
  let mutate: Deque.t('a) => Deque.Transient.t('a);

  /*** [return value] returns a Deque containing a single value, [value]. */
  let return: 'a => t('a);
};

/*** A Persistent Map implemented using hashing and a comparator function to resolve hash conflicts.
 *  HashMap is implemented as a bitmapped trie of AVLTrees. Most map operations have a computational
 *  complexity of O(log32 N).
 */
module rec HashMap: {

  type t('k, 'v);
  include Map.Persistent.S2 with type t('k, 'v) := HashMap.t('k, 'v);

  /*** [emptyWith hash comparator] returns an empty HashMap which uses [hash] to hash
   *  keys, and [comparator] to resolve collisions.
   */
  let emptyWith: (~hash: Hash.t('k), ~comparator: Comparator.t('k)) => HashMap.t('k, 'v);

  /*** [fromWith hash comparator keyedIter] returns a HashMap containing all the key/value
   *  pairs in [keyedIter]. The returned HashMap uses [hash] to hash keys, and [comparator]
   *  to resolve collisions.
   */
  let fromWith:
    (~hash: Hash.t('k), ~comparator: Comparator.t('k), KeyedIterable.t('k, 'v)) =>
    HashMap.t('k, 'v);

  /*** [fromEntriesWith hash comparator iter] returns a HashMap containing all the key/value
   *  pairs in [iter]. The returned HashMap uses [hash] to hash keys, and [comparator]
   *  to resolve collisions.
   */
  let fromEntriesWith:
    (~hash: Hash.t('k), ~comparator: Comparator.t('k), Iterable.t(('k, 'v))) => HashMap.t('k, 'v);

  /*** A temporarily mutable HashMap. Once persisted, any further operations on a
   *  Transient HashMap instance will throw. Intended for implementing bulk mutation
   *  operations efficiently.
   */
  module Transient: {
    type t('k, 'v);
    include Map.Transient.S2 with type t('k, 'v) := t('k, 'v);

    /*** [emptyWith hash comparator ()] returns an empty Transient HashMap which uses [hash] to hash
     *  keys, and [comparator] to resolve collisions.
     */
    let emptyWith:
      (~hash: Hash.t('k), ~comparator: Comparator.t('k), unit) => HashMap.Transient.t('k, 'v);

    /*** [persist transient] persists [transient] returning a HashMap. Further attempts
     *  to access or mutate [transient] will raise exceptions.
     */
    let persist: t('k, 'v) => HashMap.t('k, 'v);
  };

  /*** [mutate map] returns a Transient HashMap containing the same key/values pairs as [map]. */
  let mutate: HashMap.t('k, 'v) => HashMap.Transient.t('k, 'v);
};

/*** A Persistent Set implemented using hashing and a comparator function to resolve hash conflicts.
 *  HashSet are implemented as bitmapped tries. Most set operations have a computational
 *  complexity of O(log32 N).
 */
module rec HashSet: {

  /*** The HashSet type. */
  type t('a);

  include Set.Persistent.S1 with type t('a) := HashSet.t('a);

  /*** [emptyWith hash comparator] returns an empty HashSet which uses [hash] to hash
   *  keys, and [comparator] to resolve collisions.
   */
  let emptyWith: (~hash: Hash.t('a), ~comparator: Comparator.t('a)) => HashSet.t('a);

  /*** [fromWith hash comparator iter] returns a HashSet containing all the values in [iter].
   *  The returned HashSet uses [hash] to hash keys, and [comparator] to resolve collisions.
   */
  let fromWith:
    (~hash: Hash.t('a), ~comparator: Comparator.t('a), Iterable.t('a)) => HashSet.t('a);

  /*** An hashing function for HashSet instances. */
  let hash: Hash.t(HashSet.t('a));

  /*** A temporarily mutable HashSet. Once persisted, any further operations on a
   *  Transient HashMap instance will throw. Intended for implementing bulk mutation
   *  operations efficiently.
   */
  module Transient: {
    type t('a);
    include Set.Transient.S1 with type t('a) := HashSet.Transient.t('a);

    /*** [emptyWith hash comparator ()] returns an empty Transient HashSet which uses [hash] to hash
     *  keys, and [comparator] to resolve collisions.
     */
    let emptyWith:
      (~hash: Hash.t('a), ~comparator: Comparator.t('a), unit) => HashSet.Transient.t('a);

    /*** [persist transient] persists [transient] returning a HashSet. Further attempts
     *  to access or mutate [transient] will raise exceptions.
     */
    let persist: HashSet.Transient.t('a) => HashSet.t('a);
  };

  /*** [mutate set] returns a Transient HashSet containing the same values as [set]. */
  let mutate: HashSet.t('a) => HashSet.Transient.t('a);
};

/*** A Map optimized for integer keys. IntMap is implemented as a bitmapped trie.
 *  Most map operations have a computational complexity of O(log32 N).
 */
module rec IntMap: {
  type k = int;
  type t('v);
  include Map.Persistent.S1 with type k := IntMap.k and type t('v) := IntMap.t('v);
  module Transient: {
    type k = int;
    type t('v);
    include
      Map.Transient.S1 with type k := IntMap.Transient.k and type t('v) := IntMap.Transient.t('v);
    let empty: unit => IntMap.Transient.t('v);

    /*** [persist transient] persists [transient] returning a IntMap. Further attempts
     *  to access or mutate [transient] will raise exceptions.
     */
    let persist: t('v) => IntMap.t('v);
  };
  let empty: unit => IntMap.t('v);
  let from: KeyedIterable.t(k, 'v) => IntMap.t('v);
  let fromEntries: Iterable.t((k, 'v)) => IntMap.t('v);

  /*** [mutate map] returns a Transient IntMap containing the same key/values pairs as [map]. */
  let mutate: t('v) => IntMap.Transient.t('v);
};

/*** A contiguous Set of discrete integers */
module rec IntRange: {
  type a = int;
  type t;
  include NavigableSet.S with type a := IntRange.a and type t := IntRange.t;
  include Comparable.S with type t := IntRange.t;
  include Hashable.S with type t := IntRange.t;

  /*** [create start count] returns an IntRange startint at [start] with [count].
   *  [start] may be any positive or negative integer. [count] must be greater
   *  than or equal to 0.
   */
  let create: (~start: int, ~count: int) => IntRange.t;

  let empty: unit => IntRange.t;
};

/*** A Persistent Set optimized for integer values. IntSets are implemented as
 *  bitmapped tries. Most set operations have a computational complexity of O(log32 N).
 */
module rec IntSet: {
  type a = int;

  /*** The IntSet type. */
  type t;

  include Set.Persistent.S with type a := IntSet.a and type t := IntSet.t;

  /*** A temporarily mutable IntSet. Once persisted, any further operations on a
   *  Transient IntSet instance will throw. Intended for implementing bulk mutation
   *  operations efficiently.
   */
  module Transient: {
    type a = int;

    /*** The Transient IntSet type. */
    type t;

    include Set.Transient.S with type a := IntSet.Transient.a and type t := IntSet.Transient.t;

    /*** [empty ()] return a new empty Transient IntSet. */
    let empty: unit => IntSet.Transient.t;

    /*** [persist transient] returns a persisted IntSet. Further attempts to access or mutate [transient]
     *  will throw.
     */
    let persist: t => IntSet.t;
  };
  let empty: unit => t;
  let from: Iterable.t(int) => t;

  /*** [mutate set] returns a Transient IntSet containing the same values as [set]. */
  let mutate: t => IntSet.Transient.t;
};

/*** OCaml singly-linked list */
module rec List: {
  type t('a) = list('a);
  include Iterable.S1 with type t('a) := t('a);

  /*** [addFirst value list] returns a List with [value] prepended.
   *
   *  Complexity: O(1)
   */
  let addFirst: ('a, List.t('a)) => List.t('a);

  /*** [addFirstAll iter list] returns a List with the values in [iter] prepended. */
  let addFirstAll: (Iterable.t('a), List.t('a)) => List.t('a);

  /*** [empty ()] returns an empty List. */
  let empty: unit => List.t('a);

  /*** [first list] returns first value in [list] or None.
   *
   *  Complexity: O(1)
   */
  let first: t('a) => option('a);

  /*** [firstOrRaise seq] returns the first value in [list] or raises an exception.
   *
   *  Complexity: O(1)
   */
  let firstOrRaise: List.t('a) => 'a;

  /*** [fromReverse iter] returns a new List containing the values in [iter]
   *  in reverse order.
   *
   * Complexity: O(N) the number of values in [iter].
   */
  let fromReverse: Iterable.t('a) => List.t('a);

  /*** [removeAll list] returns the empty List.
   *
   *  Complexity: O(1)
   */
  let removeAll: List.t('a) => List.t('a);

  /*** [removeFirstOrRaise list] returns a List without the first value.
   *
   *  Complexity: O(1)
   */
  let removeFirstOrRaise: List.t('a) => List.t('a);

  /*** [return value] returns a new List containing a single value, [value]. */
  let return: 'a => List.t('a);

  /*** [toSequence list] returns a Sequence of the values in [list] in order. */
  let toSequence: List.t('a) => Sequence.t('a);
};

/*** Opaque wrapper around an underlying array instance that provides read only semantics */
module rec ReadOnlyArray: {
  type t('a);
  include Indexed.S1 with type t('a) := ReadOnlyArray.t('a);

  /* [empty ()] returns an empty ReadOnlyArray. */
  let empty: unit => ReadOnlyArray.t('a);

  /* [init count f] returns a ReadOnlyArray with size [count]. The callback [f] is called
   * for each index to initialize the value at the respective index.
   */
  let init: (int, int => 'a) => ReadOnlyArray.t('a);

  /*** [unsafe arr] returns a ReadOnlyArray backed by [arr]. Note, it is the caller's
   *  responsibility to ensure that [arr] is not subsequently mutated.
   */
  let ofUnsafe: array('a) => ReadOnlyArray.t('a);
};

/*** A singly-linked stack with an O(1) count operation. */
module rec Stack: {
  type t('a);
  include SequentialCollection.Persistent.S1 with type t('a) := Stack.t('a);
  let empty: unit => Stack.t('a);

  /*** [fromList list] returns a Stack backed by [list].
   *
   *  Complexity: O(N)
   */
  let fromList: list('a) => Stack.t('a);

  let fromReverse: Iterable.t('a) => Stack.t('a);

  /*** [return value] returns a Stack containing a single value, [value]. */
  let return: 'a => t('a);

  /*** [toList stack] returns the underlying List backing the stack */
  let toList: Stack.t('a) => list('a);
};

/*** Sorted map implemented as an AVL tree. Most set operations
 *  have a computational complexity of O(log N).
 */
module SortedMap: {
  module type S1 = {
    type k;

    /*** The SortedMap type. */
    type t(+'v);

    include NavigableMap.Persistent.S1 with type k := k and type t('v) := t('v);
    let empty: unit => t('v);
    let from: KeyedIterable.t(k, 'v) => t('v);
    let fromEntries: Iterable.t((k, 'v)) => t('v);
  };

  /*** Module function to create a SortedMap. */
  module Make1: (Comparable: Comparable.S) => S1 with type k = Comparable.t;
};

/*** Sorted set implemented as an AVL tree. Most set operations
 *  have a computational complexity of O(log N).
 */
module SortedSet: {
  module type S = {
    type a;
    type t;
    include Comparable.S with type t := t;
    include NavigableSet.Persistent.S with type a := a and type t := t;
    let empty: unit => t;
    let from: Iterable.t(a) => t;
  };

  /*** Module function to create a SortedSet. */
  module Make: (Comparable: Comparable.S) => S with type a = Comparable.t;
};

/*** An Indexed supporting efficient prepend, appends, indexing, conctentation,
 *  and splits. Vectors are implemented as relaxed radix balanced trees. Computational
 *  is O(log32 N) for most operations, with optimizations for effection O(1) access to
 *  first and last values.
 */
module rec Vector: {
  type t('a);
  include Indexed.Persistent.S1 with type t('a) := Vector.t('a);

  /* [init count f] returns a Vector with size [count]. The callback [f] is called
   * for each index to initialize the value at the respective index.
   */
  let init: (int, int => 'a) => Vector.t('a);

  /*** A temporarily mutable Vector. Once persisted, any further operations on a
   *  Transient Vector instance will throw. Intended for implementing bulk mutation
   *  operations efficiently.
   */
  module Transient: {
    type t('a);
    include Indexed.Transient.S1 with type t('a) := Vector.Transient.t('a);
    let empty: unit => Vector.Transient.t('a);

    /*** [persist transient] returns a persisted Vector. Further attempts to access or mutate [transient]
     *  will throw.
     */
    let persist: Vector.Transient.t('a) => Vector.t('a);
  };
  let empty: unit => Vector.t('a);
  let from: Iterable.t('a) => Vector.t('a);
  let fromReverse: Iterable.t('a) => Vector.t('a);

  /*** [mutate vector] returns a Transient Vector containing the same values as [set]. */
  let mutate: Vector.t('a) => Vector.Transient.t('a);

  /*** [return value] returns a Vector containing a single value, [value]. */
  let return: 'a => t('a);
};
