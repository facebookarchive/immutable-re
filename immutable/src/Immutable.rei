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

module type Equatable = {
  /** Module type implemented by modules that support testing values for equality. */

  type t;

  let equals: Equality.t t;
  /** An equality function for instances of type [t]. */
};

module type Equatable_1 = {
  /** Module type implemented by modules that support testing values for equality. */

  type t 'a;

  let equals: Equality.t (t 'a);
  /** An equality function for instances of type [t 'a]. */
};

module type Comparable = {
  /** Module type implemented by modules that support absolute ordering of values. */

  type t;

  include Equatable with type t := t;

  let compare: Comparator.t t;
  /** A comparator function for instances of type [t]. */
};

module type Hashable = {
  /** Module type implemented by modules that support hashing. */

  type t;

  let hash: Hash.t t;
  /** An hashing function for instances of type [t]. */
};

module type Hashable_1 = {
  /** Module type implemented by modules that support hashing. */

  type t 'a;

  let hash: Hash.t (t 'a);
  /** An hashing function for instances of type [t 'a]. */
};

module type FlatMappable_1 = {
  /** Module type implemented by modules that support the flatmap operation.
   *  Computational complexity is dependent upon whether the underlying type
   *  is evaluated eagerly, in which case the operation is O(N), or lazily,
   *  in which case the operation is O(1).
   */

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

module type Mappable_1 = {
  /** Module type implemented by modules that support the map operation.
   *  Computational complexity is dependent upon whether the underlying type
   *  is evaluated eagerly, in which case the operation is O(N), or lazily,
   *  in which case the operation is O(1).
   */

  type t 'a;

  let map: ('a => 'b) => (t 'a) => (t 'b);
  /** [map f mappable] Returns a [Mappable_1] whose values are the result of
   *  applying the function [f] to each value in [mappable].
   */
};

module type Reduceable = {
  /** Module type implemented by modules that support reducing over
   *  values contained by a container.
   */

  type a;
  type t;

  let reduce: while_::('acc => a => bool)? => ('acc => a => 'acc) => 'acc => t => 'acc;
  /** [reduce while_::predicate initialValue f reduceable] applies the accumulator
   *  function [f] to each value in [reduceable], while [predicate] returns true,
   *  accumulating the result.
   */
};

module type Reduceable_1 = {
  /** Module type implemented by modules that support reducing over
   *  values contained by a container.
   */

  type t 'a;

  let reduce: while_::('acc => 'a => bool)? => ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  /** [reduce while_::predicate initialValue f reduceable] applies the accumulator
   *  function [f] to each value in [reduceable], while [predicate] returns true,
   *  accumulating the result.
   */
};

module type ReduceableRight = {
  /** Module type implemented by modules that support reducing over
   *  values contained by a container in both the left to right,
   *  and right to left directions.
   */

  type a;
  type t;

  include Reduceable with type a := a and type t := t;

  let reduceRight: while_::('acc => a => bool)? => ('acc => a => 'acc) => 'acc => t => 'acc;
  /** [reduceRight while_::predicate initialValue f reduceable] applies the accumulator
   *  function [f] to each value in [reduceable] while [predicate] returns true, starting
   *  from the right most value, accumulating the result.
   */
};

module type ReduceableRight_1 = {
  /** Module type implemented by modules that support reducing over
   *  values contained by a container in both the left to right,
   *  and right to left directions.
   */

  type t 'a;

  include Reduceable_1 with type t 'a := t 'a;

  let reduceRight: while_::('acc => 'a => bool)? => ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  /** [reduceRight while_::predicate initialValue f reduceable] applies the accumulator
   *  function [f] to each value in [reduceable] while [predicate] returns true, starting
   *  from the right most value, accumulating the result.
   */
};

module type ReverseMappable_1 = {
  /** Module type implemented by modules that support the mapReverse operation.
   *  Computation complexity is dependent upon whether the underlying type
   *  is evaluated eagerly, in which case the operation is O(N), or lazily,
   *  in which case the operation is O(1).
   */

  type t 'a;

  let mapReverse: ('a => 'b) => (t 'a) => (t 'b);
  /** [mapReverse f reverseMappable] Returns a  [ReverseMappable_1] whose values
   *  are the result of applying the function [f] to each value in [reverseMappable]
   *  and reversing the order of values
   */
};

module type Streamable_1 = {
  /** Module type implemented by modules that implement lazily evaluated
   *  stream functions. All functions defined in this module are O(1).
   */

  type t 'a;

  include FlatMappable_1 with type t 'a := t 'a;
  include Mappable_1 with type t 'a := t 'a;

  let buffer: count::int => skip::int => (t 'a) => (t (list 'a));
  /** [buffer count skip stream] returns a [Streamable_1] that collects values from [stream]
   *  into buffer lists of size [count], skipping [skip] number of values in between the
   *  creation of new buffers. The returned buffers are guaranteed to be of size [count],
   *  and values are dropped if [stream] completes before filling the last buffer.
   */

  let concat: (list (t 'a)) => (t 'a);
  /** [concat streams] returns a [Streamable_1] that lazily concatenates all the
   *  [Streamable_1]s in [streams]. The resulting [Streamable_1] returns all the values
   *  in the first [Streamable_1], followed by all the values in the second [Streamable_1],
   *  and continues until the last [Streamable_1] completes.
   */

  let defer: (unit => t 'a) => (t 'a);
  /** [defer f] returns a [Streamable_1] that invokes the function [f] whenever enumerated. */

  let distinctUntilChangedWith: (Equality.t 'a) => (t 'a) => (t 'a);
  /** [distinctUntilChangedWith equals stream] returns a [Streamable_1] that contains only
   *  distinct contiguous values from [stream] using [equals] to equate values.
   */

  let doOnNext: ('a => unit) => (t 'a) => (t 'a);
  /** [doOnNext f stream] returns a [Streamable_1] that applies the side effect
   *  function [f] to each value in the stream as they are enumerated.
   */

  let empty: unit => (t 'a);
  /** Returns an empty [Streamable_1]. */

  let filter: ('a => bool) => (t 'a) => (t 'a);
  /** [filter f stream] returns a [Streamable_1] only including values from [stream]
   *  for which application of the predicate function [f] returns true.
   */

  let generate: ('a => 'a) => 'a => (t 'a);
  /** [generate f initialValue] generates the infinite [Streamable_1] [x, f(x), f(f(x)), ...] */

  let return: 'a => (t 'a);
  /** [return value] returns a single value [Streamable_1] containing [value]. */

  let scan: ('acc => 'a => 'acc) => 'acc => (t 'a) => (t 'acc);
  /** [scan f acc stream] returns a [Streamable_1] of accumulated values resulting from the
   *  application of the accumulator function [f] to each value in [stream] with the
   *  specified initial value [acc].
   */

  let skip: int => (t 'a) => (t 'a);
  /** [skip count stream] return a [Streamable_1] which skips the first [count]
   *  values in [stream].
   */

  let skipWhile: ('a => bool) => (t 'a) => (t 'a);
  /** [skipWhile f stream] return a [Streamable_1] which skips values in [stream]
   *  while application of the predicate function [f] returns true, and then returns
   *  the remaining values.
   */

  let startWith: 'a => (t 'a) => (t 'a);
  /** [startWith value stream] returns a [Streamable_1] whose first
   *  value is [value], followed by the values in [stream].
   */

  let take: int => (t 'a) => (t 'a);
  /** [take count stream] returns a [Streamable_1] with the first [count]
   *  values in [stream].
   */

  let takeWhile: ('a => bool) => (t 'a) => (t 'a);
  /** [takeWhile f stream] returns a [Streamable_1] including all values in [stream]
   *  while application of the predicate function [f] returns true, then completes.
   */
};

let module Reducer: {
  /** Module functions for generating modules which provide common reduction functions for [Reduceable]s.
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
    /** [firstOrRaise reduceable] returns the first element in [reduceable] or raises an exception.
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
    /** [firstOrRaise reduceable] returns the first element in [reduceable] or raises an exception.
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

  let module Make: (Reduceable: Reduceable) => S with type a = Reduceable.a and type t = Reduceable.t;
  /** Module function to create a [Reducer] for a specific [Reduceable] type. */

  let module Make1: (Reduceable: Reduceable_1) => S1 with type t 'a = Reduceable.t 'a;
  /** Module function to create a [Reducer] for a specific [Reduceable] type
   *  with a parametric type arity of 1.
   */
};

let module Iterator: {
  /** Functional iterator over a collection of values. Iterators are stateless and can be reused. */
  type t 'a;

  include Reduceable_1 with type t 'a := t 'a;
  include Streamable_1 with type t 'a := t 'a;

  let module Reducer: Reducer.S1 with type t 'a := t 'a;
};

module type Iterable = {
  /** Module type implemented by modules that supporting iterating over their values. */

  type a;
  type t;

  include Reduceable with type a := a and type t := t;

  let toIterator: t => (Iterator.t a);
  /* [toIterator iterable] returns an [Iterator] that can be used to iterate over
   * the values in [iterable].
   */
};

module type Iterable_1 = {
  /** Module type implemented by modules that supporting iterating over their values. */

  type t 'a;

  include Reduceable_1 with type t 'a := t 'a;

  let toIterator: t 'a => (Iterator.t 'a);
  /* [toIterator iterable] returns an [Iterator] that can be used to iterate over
   * the values in [iterable].
   */
};

let module Sequence: {
  /** Functional pull based sequences. */

  type t 'a;
  /** The Sequence type. */

  include Iterable_1 with type t 'a := t 'a;
  include Streamable_1 with type t 'a := t 'a;

  let seek: int => (t 'a) => (t 'a);
  /** [seek count seq] scans forward [count] times through [seq]. It is the eagerly
   *  evaluated equivalent of [skip count seq]. Computational complexity is O(count).
   */

  let seekWhile: ('a => bool) => (t 'a) => (t 'a);
  /** [seekWhile f seq] scans forward through [seq] while application of
   *  the predicate function [f] returns true. It is the eagerly evaluated
   *  equivalent of [skipWhile f seq]. Computational complexity is O(N).
   */

  let zip: (list (t 'a)) => (t (list 'a));
  /** [zip seq] returns a [Sequence] which zips a list of [Sequence]s
   *  into a single [Sequence.t (list 'a)]. Values are produce until any [Sequence]
   *  in [seq] completes.
   */

  let zip2With: ('a => 'b => 'c) => (t 'a) => (t 'b) => (t 'c);
  /** [zip2With zipper first second] returns a [Sequence] which zips two [Sequence]s,
   *  combining values using [zipper]. Values are produce until either [first]
   *  or [second] complete.
   */

  let zip3With: ('a => 'b => 'c => 'd) => (t 'a) => (t 'b) => (t 'c) => (t 'd);
  /** [zip3With zipper first second third] returns a [Sequence] which zips three [Sequence]s,
   *  combining values using [zipper]. Values are produce until either [first], [second]
   *  or [third] complete.
   */

  let zipLongest: (list (t 'a)) => (t (list (option 'a)));
  /** [zipLongest seq] returns a [Sequence] which zips a list of [Sequence]s
   *  into a single of [Sequence.t (list (option 'a))]. Values are produce until all [Sequence]s
   *  in [seq] complete.
   */

  let zipLongest2With:
    (option 'a => option 'b => 'c) =>
    (t 'a) =>
    (t 'b) =>
    (t 'c);
  /** [zipLongest2With zipper first second] returns a [Sequence] which zips two [Sequence]s,
   *  combining values using [zipper]. Values are produce until both [first]
   *  and [second] complete.
   */

  let zipLongest3With:
    (option 'a => option 'b => option 'c => 'd) =>
    (t 'a) =>
    (t 'b) =>
    (t 'c) =>
    (t 'd);
  /** [zipLongest3With zipper first second third] returns a [Sequence] which
   *  zips three [Sequence]s, combining values using [zipper]. Values are produce
   *  until [first], [second] and [third] all complete.
   */

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

module type SequentialCollection_1 = {
  type t 'a;

  include Collection_1 with type t 'a := t 'a;

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

let module rec Set: {
  /** A read only view of an underlying set of unique values. The intent of this type is to enable
   *  interop between alternative concrete implementations such as [SortedSet] and [g].
   *  The complexity of functions in this module is dependent upon the underlying concrete implementation.
   */

   module type S = {
     type a;
     type t;

     include Collection with type a := a and type t := t;
     include Equatable with type t := t;

     let contains: a => t => bool;

     let toSet: t => Set.t a;
   };

   module type S1 = {
     type t 'a;

     include Collection_1 with type t 'a := t 'a;
     include Equatable_1 with type t 'a := t 'a;

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

let module KeyedReducer: {
  module type S1 = {
    type k;
    type t 'v;

    let count: (t 'v) => int;
    let every: (k => 'v => bool) => (t 'v) => bool;
    let find: (k => 'v => bool) => (t 'v) => (option (k, 'v));
    let findOrRaise: (k => 'v => bool) => (t 'v) => (k, 'v);
    let findKey: (k => 'v => bool) => (t 'v) => (option k);
    let findKeyOrRaise: (k => 'v => bool) => (t 'v) => k;
    let findValue: (k => 'v => bool) => (t 'v) => (option 'v);
    let findValueOrRaise: (k => 'v => bool) => (t 'v) => 'v;
    let first: (t 'v) => (option (k, 'v));
    let firstOrRaise: (t 'v) => (k, 'v);
    let forEach: while_::(k => 'v => bool)? => (k => 'v => unit) => (t 'v) => unit;
    let none: (k => 'v => bool) => (t 'v) => bool;
    let some: (k => 'v => bool) => (t 'v) => bool;
  };

  module type S2 = {
    type t 'k 'v;

    let count: (t 'k 'v) => int;
    let every: ('k => 'v => bool) => (t 'k 'v) => bool;
    let find: ('k => 'v => bool) => (t 'k 'v) => (option ('k, 'v));
    let findOrRaise: ('k => 'v => bool) => (t 'k 'v) => ('k, 'v);
    let findKey: ('k => 'v => bool) => (t 'k 'v) => (option 'k);
    let findKeyOrRaise: ('k => 'v => bool) => (t 'k 'v) => 'k;
    let findValue: ('k => 'v => bool) => (t 'k 'v) => (option 'v);
    let findValueOrRaise: ('k => 'v => bool) => (t 'k 'v) => 'v;
    let first: (t 'k 'v) => (option ('k, 'v));
    let firstOrRaise: (t 'k 'v) => ('k, 'v);
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

let module rec Map: {
  /** A read only view of an underlying set of key/value pairs. The intent of this type is to enable
   *  interop between alternative concrete implementations such as [SortedMap] and [HashMap].
   *  The complexity of functions in this module is dependent upon the underlying concrete implementation.
   */

   module type S1 = {
     type k;
     type t 'v;

     include KeyedCollection_1 with type k := k and type t 'v := t 'v;

     let get: k => (t 'v) => (option 'v);
     let getOrRaise: k => (t 'v) => 'v;
     let values: (t 'v) => (Iterator.t 'v);
     let toMap: (t 'v) => Map.t k 'v;
   };

   module type S2 = {
     type t 'k 'v;

     include KeyedCollection_2 with type t 'k 'v := t 'k 'v;

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

module type PersistentMap_1 = {
  type k;
  type t 'v;

  include PersistentKeyedCollection_1 with type k := k and type t 'v := t 'v;
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
  include Map.S1 with type k := k and type t 'v := t 'v;
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

let module rec Deque: {
  /** A double-ended queue with efficient appends [addLast], prepends [addFirst]
   *  and removals from either end of the queue [removeFirstOrRaise] [removeLastOrRaise].
   */

  type t 'a;
  /** The Deque type. */

  include PersistentNavigableCollection_1 with type t 'a := t 'a;

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

  include TransientNavigableCollection_1 with type t 'a := t 'a;
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

let module List: {
  /** OCaml singly-linked list */

  type t 'a = list 'a;
  /** The List type. */

  include Iterable_1 with type t 'a := t 'a;
  include ReverseMappable_1 with type t 'a := t 'a;

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

let module ReadOnlyArray: {
  /** Opaque wrapper around an underlying array instance that provides read only semantics */

  type t 'a;
  /** The CopyOnWriteArray type. */

  include IndexedCollection_1 with type t 'a := t 'a;
  include IndexedMappable_1 with type t 'a := t 'a;

  let empty: unit => t 'a;
  let init: int => (int => 'a) => (t 'a);
  let ofUnsafe: (array 'a) => (t 'a);
  /** [unsafe arr] returns a ReadOnlyArray backed by [arr]. Note, it is the caller's
   *  responsibility to ensure that [arr] is not subsequently mutated.
   *
   *  Complexity: O(1)
   */

  let module Reducer: Reducer.S1 with type t 'a := t 'a;
};

let module Stack: {
  /** A singly-linked stack with an O(_1) count operation. */

  type t 'a;
  /** The Stack type. */

  include PersistentSequentialCollection_1 with type t 'a := t 'a;

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

    include PersistentNavigableMap_1 with type k := k and type t 'v := t 'v;

    let module KeyedReducer: KeyedReducer.S1 with type k := k and type t 'v := t 'v;
  };

  let module Make1: (Comparable: Comparable) => S1 with type k = Comparable.t;
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

let module rec Vector: {
  /** Indexed type that supports efficient operations for
   * prepend, appends, indexing, conctentation, and splits.
   */

  type t 'a;
  /** The vector type */

  include PersistentNavigableCollection_1 with type t 'a := t 'a;
  include IndexedCollection_1 with type t 'a := t 'a;
  include IndexedMappable_1 with type t 'a := t 'a;

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

  let module Reducer: Reducer.S1 with type t 'a := t 'a;
}

and TransientVector: {
  /** A temporarily mutable Vector. Once persisted, any further operations on a
   *  TransientVector instance will throw. Intended for implementing bulk mutation operations efficiently.
   */
  type t 'a;
  /** The TransientVector type. */

  include TransientNavigableCollection_1 with type t 'a := t 'a;

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

  let persist: (t 'a) => (Vector.t 'a);
  /** [persist transient] returns a persisted Vector. Further attempts to access or mutate [transient]
  *  will throw.
  */
};
