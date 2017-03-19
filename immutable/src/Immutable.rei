/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

let module Hash: {
  /** Hash functions for common types. */

  type t 'a = 'a => int;
  /** The Hash function type */

  /*
  let bytes: t bytes;
  let char: t char;
  let int32: t int32;
  let int64: t int64;
  let nativeInt: t nativeint;
  let string: t string;
  */

  let random: unit => (t 'a);
  /** Creates a random structural hash function. */

  let structural: (t 'a);
  /** The default ocaml hash function. */
};

let module Equality: {
  /** Equality functions for common types. */

  type t 'a = 'a => 'a => bool;
  /** The Equality function type. */

  /* Will be available in Ocaml 4.03
  let bytes: t bytes;
  let char: t char;
  let int32: t int32;
  let int64: t int64;
  let nativeInt: t nativeint;
  let string: t string;
  */

  let reference: (t 'a);
  /** The reference equality function, analogous to === */

  let structural: (t 'a);
  /** The structural equality function, analogous to == */
};

let module Ordering: {
  /** Represents the absolute ordering of a type */

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

  let int32: t int32;
  /** Compares int32s. */

  let int64: t int64;
  /** Compares int64s. */

  let nativeInt: t nativeint;
  /** Compares nativeInts. */

  let string: t string;
  /** Compares strings. */

  let structural: (t 'a);
  /** The default structural comparison function. */
};

let module Comparable: {
  module type S = {
    type t;

    let compare: Comparator.t t;
  };
};

let module HashStrategy: {
  /** Strategies for hashing values and resolving conflicts either using equality or comparison. */

  type t 'a;
  /** The HashStrategy type. */

  let createWithComparator: (Hash.t 'a) => (Comparator.t 'a) => (t 'a);
  /** [createWithComparator hash comparator] returns a HashStrategy using the
   *  provided hash and comparator functions.
   */

  let createWithEquality: (Hash.t 'a) => (Equality.t 'a) => (t 'a);
  /** [createWithEquality hash equality] returns a HashStrategy using the
   *  provided hash and equality functions.
   */

  let identity: (t 'a);
  /** A HashStrategy using structural hashing and reference equality. */

  let structuralCompare: (t 'a);
  /** A HashStrategy using structural hashing and structural comparison. */

  let structuralEquality: (t 'a);
  /** A HashStrategy using structural hashing and structural equality. */
};

let module Concatable: {
  module type S1 = {
    type t 'a;

    let concat: (list (t 'a)) => (t 'a);
  };
};

let module Mappable: {
  module type S1 = {
    type t 'a;

    let map: ('a => 'b) => (t 'a) => (t 'b);
  };
};

let module Filterable: {
  module type S1 = {
    type t 'a;

    let filter: ('a => bool) => (t 'a) => (t 'a);
  };
};

let module FlatMappable: {
  module type S1 = {
    type t 'a;

    let flatMap: ('a => t 'b) => (t 'a) => (t 'b);
  };
};

let module Flattenable: {
  module type S1 = {
    type t 'a;

    let flatten: (t (t 'a)) => (t 'a);
  };
};

let module Reduceable: {
  module type S = {
    type a;
    type t;

    let every: (a => bool) => t => bool;
    /** [every f seq] returns true if the predicate [f] returns true for every
     *  element in [seq], otherwise false. If [seq] is empty, returns true.
     *
     *  Complexity: O(N)
     */

    let find: (a => bool) => t => (option a);
    /** [tryFind f seq] returns the first value for which the predicate [f] returns true or None.
     *
     *  Complexity: O(N)
     */

    let findOrRaise: (a => bool) => t => a;
    /** [find f seq] returns the first value for which the predicate [f] returns true.
     *  If no value is found, an exception is thrown.
     *
     *  Complexity: O(N)
     */

    let forEach: (a => unit) => t => unit;

    let forEachWhile: (a => bool) => (a => unit) => t => unit;

    let isEmpty: t => bool;
    /** [isEmpty seq] returns true if [seq] contains no elements.
     *
     *  Complexity: O(1)
     */

    let isNotEmpty: t => bool;
    /** [isNotEmpty seq] returns true if [seq] contains at least one element.
     *
     *  Complexity: O(1)
     */

    let none: (a => bool) => t => bool;
    /** [none f seq] returns true if the predicate [f] returns false for every
     *  elements in [seq], otherwise true. If [seq] is empty, returns true.
     *
     *  Complexity: O(N)
     */

    let reduce: ('acc => a => 'acc) => 'acc => t => 'acc;

    let reduceWhile: ('acc => a => bool) => ('acc => a => 'acc) => 'acc => t => 'acc;

    let some: (a => bool) => t => bool;
    /** [some f seq] returns true if the predicate [f] returns true for any element in [seq], otherwise false.
     *  If [seq] is empty, returns false.
     *
     *  Complexity: O(N)
     */
  };

  module type S1 = {
    type t 'a;

    let every: ('a => bool) => (t 'a) => bool;
    /** [every f seq] returns true if the predicate [f] returns true for every
     *  element in [seq], otherwise false. If [seq] is empty, returns true.
     *
     *  Complexity: O(N)
     */

    let find: ('a => bool) => (t 'a) => (option 'a);
    /** [tryFind f seq] returns the first value for which the predicate [f] returns true or None.
     *
     *  Complexity: O(N)
     */

    let findOrRaise: ('a => bool) => (t 'a) => 'a;
    /** [find f seq] returns the first value for which the predicate [f] returns true.
     *  If no value is found, an exception is thrown.
     *
     *  Complexity: O(N)
     */

    let forEach: ('a => unit) => (t 'a) => unit;

    let forEachWhile: ('a => bool) => ('a => unit) => (t 'a) => unit;

    let isEmpty: (t 'a) => bool;
    /** [isEmpty seq] returns true if [seq] contains no elements.
     *
     *  Complexity: O(1)
     */

    let isNotEmpty: (t 'a) => bool;
    /** [isNotEmpty seq] returns true if [seq] contains at least one element.
     *
     *  Complexity: O(1)
     */

    let none: ('a => bool) => (t 'a) => bool;
    /** [none f seq] returns true if the predicate [f] returns false for every
     *  elements in [seq], otherwise true. If [seq] is empty, returns true.
     *
     *  Complexity: O(N)
     */

    let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;

    let reduceWhile: ('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;

    let some: ('a => bool) => (t 'a) => bool;
    /** [some f seq] returns true if the predicate [f] returns true for any element in [seq], otherwise false.
     *  If [seq] is empty, returns false.
     *
     *  Complexity: O(N)
     */
  };
};

let module ReduceableRight: {
  module type S = {
    type a;
    type t;

    let forEachRight: (a => unit) => t => unit;
    let forEachRightWhile: (a => bool) => (a => unit) => t => unit;
    let reduceRight: ('acc => a => 'acc) => 'acc => t => 'acc;
    let reduceRightWhile: ('acc => a => bool) => ('acc => a => 'acc) => 'acc => t => 'acc;
  };

  module type S1 = {
    type t 'a;

    let forEachRight: ('a => unit) => (t 'a) => unit;
    let forEachRightWhile: ('a => bool) => ('a => unit) => (t 'a) => unit;
    let reduceRight: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
    let reduceRightWhile: ('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  };
};

let module ReverseMappable: {
  module type S1 = {
    type t 'a;

    let mapReverse: ('a => 'b) => (t 'a) => (t 'b);
  };
};

let module Iterator: {
  type t 'a;

  include Concatable.S1 with type t 'a := t 'a;
  include Mappable.S1 with type t 'a := t 'a;
  include Filterable.S1 with type t 'a := t 'a;
  include FlatMappable.S1 with type t 'a := t 'a;
  include Flattenable.S1 with type t 'a := t 'a;
  include Reduceable.S1 with type t 'a := t 'a;

  let doOnNext: ('a => unit) => (t 'a) => (t 'a);
  let empty: (t 'a);
  let return: 'a => (t 'a);
};

let module Iterable: {
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

let module Sequential: {
  module type S = {
    type a;
    type t;

    include Iterable.S with type a := a and type t := t;

    let first: t => (option a);
    /** [tryFirst seq] returns first element in [seq] or None.
     *
     *  Complexity: O(1)
     */

    let firstOrRaise: t => a;
    /** [first seq] returns the first element in [seq] or throws.
     *
     *  Complexity: O(1)
     */
  };

  module type S1 = {
    type t 'a;

    include Iterable.S1 with type t 'a := t 'a;

    let first: (t 'a) => (option 'a);
    /** [tryFirst seq] returns first element in [seq] or None.
     *
     *  Complexity: O(1)
     */

    let firstOrRaise: (t 'a) => 'a;
    /** [first seq] returns the first element in [seq] or throws.
     *
     *  Complexity: O(1)
     */
  };
};

let module Sequence: {
  /** Functional sequences. */

  type t 'a;
  /** The Sequence type. */

  include Concatable.S1 with type t 'a := t 'a;
  include Filterable.S1 with type t 'a := t 'a;
  include FlatMappable.S1 with type t 'a := t 'a;
  include Flattenable.S1 with type t 'a := t 'a;
  include Mappable.S1 with type t 'a := t 'a;
  include Sequential.S1 with type t 'a := t 'a;

  let buffer: int => int => (t 'a) => (t (list 'a));
  /** [buffer count skip seq] returns a Sequence that collects elements from [seq]
   *  into buffer lists of size [count], skipping [skip] number of elements in between
   *  creation of new buffers. The returned buffers are guaranteed to be of size [count],
   *  and elements are dropped if [seq] completes before filling the last buffer.
   */

  let contains: 'a => (t 'a) => bool;
  /** [contains value seq] returns true if any element in [seq] is equal to [value]
   *  using structural equality, otherwise false.
   *
   *  Complexity: O(N)
   */

  let containsWith: (Equality.t 'a) => 'a => (t 'a) => bool;
  /** [containsWith equals seq] returns true if any element in [seq] is equal to [a]
   *  using the equality function [equals], otherwise false.
   *
   *  Complexity: O(N)
   */

  let defer: (unit => t 'a) => (t 'a);
  /** [defer f] returns a Sequence that invokes the function [f] whenever the Sequence is iterated. */

  let distinctUntilChanged: (t 'a) => (t 'a);
  /** [distinctUntilChanged seq] returns a Sequence that contains only
   *  distinct contiguous elements from [seq] using structural equality.
   */

  let distinctUntilChangedWith: (Equality.t 'a) => (t 'a) => (t 'a);
  /** [distinctUntilChangedWith equals seq] returns a Sequence that contains only
   *  distinct contiguous elements from [seq] using [equals] to equate elements.
   */

  let doOnNext: ('a => unit) => (t 'a) => (t 'a);
  /** [doOnNext f seq] returns a Sequence that invokes the function [f] on every element produced by [seq]. */

  let empty: (t 'a);
  /** The empty Sequence. */

  let generate: ('acc => 'acc) => 'acc => (t 'acc);
  /** [generate f acc] generates the infinite sequence x, f(x), f(f(x)), ...*/

  let repeat: 'a => (t 'a);
  /** [repeat value] returns a Sequence that repeats [value] forever. */

  let return: 'a => (t 'a);
  /** [return value] returns a single element Sequence containing [value]. */

  let scan: ('acc => 'a => 'acc) => 'acc => (t 'a) => (t 'acc);
  /** [scan f acc seq] returns a Sequence of accumulated values resulting from the
   *  application of the accumulator function [f] to each element in [seq] with the
   *  specified seed value [acc].
   */

  let skip: int => (t 'a) => (t 'a);
  /** [skip count seq] returns a Sequence that skips the first [count] elements in [seq]. */

  let skipWhile: ('a => bool) => (t 'a) => (t 'a);
  /** [skipWhile f seq] returns a Sequence that applies the predicate [f] to each element in [seq],
   *  skipping elements until [f] first returns false.
   */

  let startWith: 'a => (t 'a) => (t 'a);
  /** [startWith value seq] returns a seq whose first elements is [value]. */

  let take: int => (t 'a) => (t 'a);
  /** [take count seq] returns a Sequence that includes the first [count] elements in [seq]. */

  let takeWhile: ('a => bool) => (t 'a) => (t 'a);
  /** [takeWhile f seq] returns a Sequence that applies the predicate [f] to each element in [seq],
   *  taking elements until [f] first returns false.
   */

  let zip: (list (t 'a)) => (t (list 'a));
  /** [zip seqs] merges a list of n Sequences into a Sequence of lists with n values.
   *  Elements are produce until any Sequence in [seq] completes.
   */

  let zip2: (t 'a) => (t 'b) => (t ('a, 'b));
  /** [zip2 first second] merges two Sequences into a Sequence of tuples.
   *  Elements are produce until either first or second complete.
   */

  let zip3: (t 'a) => (t 'b) => (t 'c) => (t ('a, 'b, 'c));
  /** [zip3 first second third] merges two Sequences into a Sequence of triples.
   *  Elements are produce until either first, second, or third complete.
   */

  let zipLongest: (list (t 'a)) => (t (list (option 'a)));
  /** [zip seqs] merges a list of n Sequences into a Sequence of lists with n values.
   *  Elements are produce until all Sequences in [seq] complete.
   */

  let zipLongest2: (t 'a) => (t 'b) => (t (option 'a, option 'b));
  /** [zip2 first second] merges two Sequences into a Sequence of tuples.
   *  Elements are produce until both first and second complete.
   */

  let zipLongest3: (t 'a) => (t 'b) => (t 'c) => (t (option 'a, option 'b, option 'c));
  /** [zip3 first second third] merges two Sequence into a Sequence of triples.
   *  Elements are produce until first, second, and third all complete.
   */
};

let module List: {
  /** OCaml singly-linked list */

  type t 'a = list 'a;
  /** The List type. */

  include Sequential.S1 with type t 'a := t 'a;
  include ReverseMappable.S1 with type t 'a := t 'a;

  let addFirst: 'a => (t 'a) => (t 'a);
  /** [addFirst value list] returns a new List with [value] prepended.
   *
   *  Complexity: O(1)
   */

  let addFirstAll: (Iterator.t 'a) => (t 'a) => (t 'a);
  /** [addFirstAll iter list] returns a new List with the values in [iter] prepended. */

  let compare: (Comparator.t (t 'a));
  /** A comparator that compares two Lists
   *  using structural comparison to compare elements.
   */

  let compareWith: (Comparator.t 'a) => (Comparator.t (t 'a));
  /** [compareWith comparator] returns a Comparator that compares two Lists
   *  using [comparator] to compare elements.
   */

  let contains: 'a => (t 'a) => bool;
  /** [contains value list] returns true if any element in [list] is equal to [value]
   *  using structural equality, otherwise false.
   *
   *  Complexity: O(N)
   */

  let containsWith: (Equality.t 'a) => 'a => (t 'a) => bool;
  /** [containsWith equals list] returns true if any element in [list] is equal to [a]
   *  using the equality function [equals], otherwise false.
   *
   *  Complexity: O(N)
   */

  let empty: (t 'a);
  /** The empty List. */

  let equals: (Equality.t (t 'a));
  /** [equals this that] compares [this] and [that] for equality using structural equality to equate elements.
   *
   *  Complexity: O(N)
   */

  let equalsWith: (Equality.t 'a) => (Equality.t (t 'a));
  /** [equalsWith equals this that] compares [this] and [that] for equality using [equals] to equate elements.
   *
   *  Complexity: O(N)
   */

  let fromReverse: (Iterator.t 'a) => (t 'a);
  /** [fromReverse iter] returns a new List containing the values in [iter]
   *  in reverse order.
   *
   * Complexity: O(N) the number of elements in [iter].
   */

  let hash: (Hash.t (t 'a));
  /** [hash list] hashes [list], hashing elements using structural hashing. */

  let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
  /** [hashWith hash list] hashes [list], hashing elements using [hash]. */

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
};

let module Collection: {
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

let module TransientCollection: {
  module type S = {
    type a;
    type t;

    let count: t => int;
    /** [count transient] returns the number of elements in [transient]. */

    let isEmpty: t => bool;
    /** [isEmpty transient] returns true if [transient] contains no elements. */

    let isNotEmpty: t => bool;
    /** [isNotEmpty transient] returns true if [transient] contains at least one element. */

    let removeAll: t => t;
    /** [removeAll transient] removes all elements from [transient].
     *
     *  Complexity: O(1)
     */
  };

  module type S1 = {
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
     *  Complexity: O(1)
     */
  };
};

let module SequentialCollection: {
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

let module NavigableCollection: {
  module type S = {
    type a;
    type t;

    include SequentialCollection.S with type a := a and type t := t;
    include ReduceableRight.S with type a := a and type t := t;

    let last: t => (option a);
    let lastOrRaise: t => a;
    let toIteratorRight: t => (Iterator.t a);
    let toSequenceRight: t => (Sequence.t a);
  };

  module type S1 = {
    type t 'a;

    include SequentialCollection.S1 with type t 'a := t 'a;
    include ReduceableRight.S1 with type t 'a := t 'a;

    let last: (t 'a) => (option 'a);
    let lastOrRaise: (t 'a) => 'a;
    let toIteratorRight: (t 'a) => (Iterator.t 'a);
    let toSequenceRight: (t 'a) => (Sequence.t 'a);
  };
};

let module Stack: {
  /** A singly-linked stack with an O(1) count operation. */

  module type S1 = {
    type t 'a;

    include ReverseMappable.S1 with type t 'a := t 'a;
    include SequentialCollection.S1 with type t 'a := t 'a;

    let addFirst: 'a => (t 'a) => (t 'a);
    /** [addFirst value stack] returns a new Stack with [value] prepended.
     *
     *  Complexity: O(1)
     */

    let addFirstAll: (Iterator.t 'a) => (t 'a) => (t 'a);
    /** [addFirstAll iter stack] returns a new Stack with the values in [iter] prepended. */

    let compare: (Comparator.t (t 'a));
    /** A comparator that compares two Vectors
     *  using structural comparison to compare elements.
     */

    let compareWith: (Comparator.t 'a) => (Comparator.t (t 'a));
    /** [compareWith comparator] returns a Comparator that compares two Vectors
     *  using [comparator] to compare elements.
     */

    let contains: 'a => (t 'a) => bool;
    /** [contains value vec] returns true if any element in [vec] is equal to [value]
     *  using structural equality, otherwise false.
     *
     *  Complexity: O(N)
     */

    let containsWith: (Equality.t 'a) => 'a => (t 'a) => bool;
    /** [containsWith equals vec] returns true if any element in [vec] is equal to [a]
     *  using the equality function [equals], otherwise false.
     *
     *  Complexity: O(N)
     */

    let empty: (t 'a);
    /** The empty Vector. */

    let equals: (Equality.t (t 'a));
    /** [equals this that] compares [this] and [that] for equality using structural equality to equate elements.
     *
     *  Complexity: O(N)
     */

    let equalsWith: (Equality.t 'a) => (Equality.t (t 'a));
    /** [equalsWith equals this that] compares [this] and [that] for equality using [equals] to equate elements.
     *
     *  Complexity: O(N)
     */

    let fromReverse: (Iterator.t 'a) => (t 'a);
    /** [fromReverse iter] returns a new Stack containing the values in [iter]
     *  in reverse order.
     */

    let hash: (Hash.t (t 'a));
    /** [hash stack] hashes [stack], hashing elements using structural hashing. */

    let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
    /** [hashWith hash stack] hashes [stack], hashing elements using [hash]. */

    let return: 'a => (t 'a);
    /** [return value] returns a new Stack containing a single element, [value]. */

    let removeAll: (t 'a) => (t 'a);
    /** [removeAll stack] returns the empty Stack.
     *
     *  Complexity: O(1)
     */

    let removeFirstOrRaise: (t 'a) => (t 'a);
    /** [removeFirstOrRaise stack] returns a new Stack without the first element.
     *
     *  Complexity: O(1)
     */
  };

  type t 'a;
  /** The Stack type. */

  include S1 with type t 'a := t 'a;

  let fromList: (list 'a) => (t 'a);
  /** [fromList list] returns a Stack backed by [list].
   *
   *  Complexity: O(N)
   */

  let toList: (t 'a) => (list 'a);
  /** [toList stack] returns the underlying List backing the stack */
};

let module TransientStack: {
  module type S1 = {
    type t 'a;

    include TransientCollection.S1 with type t 'a := t 'a;

    let addFirst: 'a => (t 'a) => (t 'a);
    /** [addFirst value transient] prepends [value] to [transient].
     *
     *  Complexity: O(1)
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
     *  Complexity: O(1)
     */
  };
};

let module rec Deque: {
  /** A double-ended queue with efficient appends [addLast], prepends [addFirst]
   *  and removals from either end of the queue [removeFirstOrRaise] [removeLastOrRaise].
   */

  module type S1 = {
    type t 'a;

    include Mappable.S1 with type t 'a := t 'a;
    include NavigableCollection.S1 with type t 'a := t 'a;
    include Stack.S1 with type t 'a := t 'a;

    let addLast: 'a => (t 'a) => (t 'a);
    /** [addLast value deque] returns a new Deque with [value] appended.
     *
     *  Complexity: O(1)
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
     *  Complexity: O(1)
     */
  };

  type t 'a;
  /** The Deque type. */

  include S1 with type t 'a := t 'a;

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
}

and TransientDeque: {
  /** A temporarily mutable Deque. Once persisted, any further operations on a
   *  TransientDeque instance will throw. Intended for implementing bulk mutation operations efficiently.
   */

  module type S1 = {
    type t 'a;

    include TransientStack.S1 with type t 'a := t 'a;

    let addLast: 'a => (t 'a) => (t 'a);
    /** [addLast value transient] appends [value] to [transient].
     *
     *  Complexity: O(1)
     */

    let last: (t 'a) => option 'a;
    /** [tryLast transient] returns the last element in [transient] or None. */

    let lastOrRaise: (t 'a) => 'a;
    /** [last transient] returns the last element in [transient] or throws. */

    let removeLastOrRaise: (t 'a) => (t 'a);
    /** [removeLastOrRaise transient] removes the last element from [transient].
     *
     *  Complexity: O(1)
     */
  };

  type t 'a;

  include S1 with type t 'a := t 'a;
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

let module KeyedMappable: {
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

let module KeyedReduceable: {
  module type S1 = {
    type k;
    type t 'v;

    let every: (k => 'v => bool) => (t 'v) => bool;

    let find: (k => 'v => bool) => (t 'v) => (option (k, 'v));

    let findOrRaise: (k => 'v => bool) => (t 'v) => (k, 'v);

    let forEach: (k => 'v => unit) => (t 'v) => unit;

    let forEachWhile: (k => 'v => bool) => (k => 'v => unit) => (t 'v) => unit;

    let isEmpty: (t 'v) => bool;

    let isNotEmpty: (t 'v) => bool;

    let none: (k => 'v => bool) => (t 'v) => bool;

    let reduce: ('acc => k => 'v => 'acc) => 'acc => (t 'v) => 'acc;

    let reduceWhile: ('acc => k => 'v => bool) => ('acc => k => 'v => 'acc) => 'acc => (t 'v) => 'acc;

    let some: (k => 'v => bool) => (t 'v) => bool;
  };

  module type S2 = {
    type t 'k 'v;

    let every: ('k => 'v => bool) => (t 'k 'v) => bool;

    let find: ('k => 'v => bool) => (t 'k 'v) => (option ('k, 'v));

    let findOrRaise: ('k => 'v => bool) => (t 'k 'v) => ('k, 'v);

    let forEach: ('k => 'v => unit) => (t 'k 'v) => unit;

    let forEachWhile: ('k => 'v => bool) => ('k => 'v => unit) => (t 'k 'v) => unit;

    let isEmpty: (t 'k 'v) => bool;

    let isNotEmpty: (t 'k 'v) => bool;

    let none: ('k => 'v => bool) => (t 'k 'v) => bool;

    let reduce: ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;

    let reduceWhile: ('acc => 'k => 'v => bool) => ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;

    let some: ('k => 'v => bool) => (t 'k 'v) => bool;
  };
};

let module KeyedIterator: {
  type t 'k 'v;

  include KeyedReduceable.S2 with type t 'k 'v := t 'k 'v;
  include KeyedMappable.S2 with type t 'k 'v := t 'k 'v;

  let concat: (list (t 'k 'v)) => (t 'k 'v);
  let doOnNext: ('k => 'v => unit) => (t 'k 'v) => (t 'k 'v);
  let empty: (t 'k 'v);
  let filter: ('k => 'v => bool) => (t 'k 'v) => (t 'k 'v);
  let flatMap: ('kA => 'vA => t 'kB 'vB) => (t 'kA 'vA) => (t 'kB 'vB);
  let keys: (t 'k 'v) => (Iterator.t 'k);
  let toIterator: (t 'k 'v) => (Iterator.t ('k, 'v));
  let values: (t 'k 'v) => Iterator.t 'v;
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

let module rec Set: {
  /** A read only view of an underlying set of unique values. The intent of this type is to enable
   *  interop between alternative concrete implementations such as [SortedSet] and [HashSet].
   *  The complexity of functions in this module is dependent upon the underlying concrete implementation.
   */

  type t 'a;
  /** The Set type. */

  module type S = {
    type a;
    type t;

    include Collection.S with type a := a and type t := t;

    let contains: a => t => bool;
    let equals: Equality.t t;
    let toKeyedIterator: t => KeyedIterator.t a a;
    let toMap: t => (Map.t a a);
    let toSet: t => (Set.t a);
  };

  module type S1 = {
    type t 'a;

    include Collection.S1 with type t 'a := t 'a;

    let contains: 'a => (t 'a) => bool;
    let equals: Equality.t (t 'a);
    let toKeyedIterator: (t 'a) => (KeyedIterator.t 'a 'a);
    let toMap: (t 'a) => (Map.t 'a 'a);
    let toSet: (t 'a) => (Set.t 'a);
  };

  include S1 with type t 'a := t 'a;

  let empty: (t 'a);
  /** The empty Set. */

  let hash: (Hash.t (t 'a));
  /** [hash set] hashes [set], hashing elements using structural hashing. */

  let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
  /** [hashWith hash set] hashes [set], hashing elements using [hash]. */

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
}

and Map: {
  /** A read only view of an underlying set of key/value pairs. The intent of this type is to enable
   *  interop between alternative concrete implementations such as [SortedMap] and [HashMap].
   *  The complexity of functions in this module is dependent upon the underlying concrete implementation.
   */

  type t 'k 'v;
  /** The map type. */

  module type S1 = {
    type k;
    type t 'v;

    include KeyedCollection.S1 with type k := k and type t 'v := t 'v;

    let get: k => (t 'v) => (option 'v);
    let getOrRaise: k => (t 'v) => 'v;
    let keys: (t 'v) => (Set.t k);
    let values: (t 'v) => (Iterator.t 'v);
    let toMap: (t 'v) => (Map.t k 'v);
  };

  module type S2 = {
    type t 'k 'v;

    include KeyedCollection.S2 with type t 'k 'v := t 'k 'v;

    let get: 'k => (t 'k 'v) => (option 'v);
    let getOrRaise: 'k => (t 'k 'v) => 'v;
    let keys: (t 'k 'v) => (Set.t 'k);
    let values: (t 'k 'v) => (Iterator.t 'v);
    let toMap: (t 'k 'v) => (Map.t 'k 'v);
  };

  include S2 with type t 'k 'v := t 'k 'v;
  include KeyedMappable.S2 with type t 'k 'v := t 'k 'v;

  let contains: 'k => 'v => (t 'k 'v) => bool;
  /** [contains key value map] returns true if [map] contains the [key] [value] pair,
   *  using structural equality to equate [value].
   */

  let containsWith: (Equality.t 'v) => 'k => 'v => (t 'k 'v) => bool;
  /** [containsWith equals key value map] returns true if [map] contains the [key] [value] pair,
   *  using [equals] to equate [value].
   */

  let empty: (t 'k 'v);
  /** The empty Map. */

  let equals: (t 'k 'v) => (t 'k 'v) => bool;
  /** [equals this that] equates [this] and [that] ensuring that the Maps have the same count
   *  and contains the same key/value pairs. Structural equality is used to equate values.
   */

  let equalsWith: (Equality.t 'v) => (t 'k 'v) => (t 'k 'v) => bool;
  /** [equalsWith equals this that] equates [this] and [that] ensuring that the Maps
   *  have the same count, and contains the same key/value pairs. [equals] is used to equate values.
   */

  let hash: (Hash.t (t 'k 'v));
  /** [hash map] hashes [map], hashing keys and values using structural hashing. */

  let hashWith: (Hash.t 'k) => (Hash.t 'v) => (Hash.t (t 'k 'v));
  /** [hashWith keyHash valueHash map] hashes [map], hashing keys using [keyHash]
   *  and values using [valueHash].
   */

  let toSet: (t 'k 'v) => (Set.t ('k, 'v));
  /** [toSet map] returns a Set view of key/value pairs in [map], using structural equality
   *  to equate values.
   */

  let toSetWith: (Equality.t 'v) => (t 'k 'v) => (Set.t ('k, 'v));
  /** [toSetWith equals map] returns a Set view of key/value pairs in [map],
   *  using [equals] to equate values.
   */
};

let module Option: {
  /** OCaml option type. Can be considered a set of zero or one elements.
   *  All operations have a complexity of O(1).
   */

  type t 'a = option 'a;
  /** The Option type. */

  include FlatMappable.S1 with type t 'a := t 'a;
  include Filterable.S1 with type t 'a := t 'a;
  include Flattenable.S1 with type t 'a := t 'a;
  include Mappable.S1 with type t 'a := t 'a;
  include SequentialCollection.S1 with type t 'a := t 'a;

  let compare: (Comparator.t (t 'a));
  /** A comparator that compares two Options using structural comparison to compare elements. */

  let compareWith: (Comparator.t 'a) => (Comparator.t (t 'a));
  /** [compareWith comparator] returns a Comparator that compares two Deques
   *  using [comparator] to compare elements.
   */

  let contains: 'a => (t 'a) => bool;
  /** [contains value option] returns true if any element in [option] is equal to [value]
   *  using structural equality, otherwise false.
   */

  let containsWith: (Equality.t 'a) => 'a => (t 'a) => bool;
  /** [containsWith equals option] returns true if any element in [option] is equal to [a]
   *  using the equality function [equals], otherwise false.
   *
   *  Complexity: O(N)
   */

  let empty: (t 'a);
  /** The empty Option, None. */

  let equals: (Equality.t (t 'a));
  /** [equals this that] compares [this] and [that] for equality using
   *  structural equality to equate elements.
   */

  let equalsWith: (Equality.t 'a) => (Equality.t (t 'a));
  /** [equalsWith equals this that] compares [this] and [that] for equality using [equals] to equate elements.
   *
   *  Complexity: O(N)
   */

  let hash: (Hash.t (t 'a));
  /** [hash option] hashes [option], hashing elements using structural hashing. */

  let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
  /** [hashWith hash option] hashes [option], hashing elements using [hash]. */

  let return: 'a => (t 'a);
  /** [return value] returns [Some value]. */

  let toSet: (option 'a) => (Set.t 'a);
  /* [toSet option] returns a Set view of the option using structural equality equate values. */

  let toSetWith: (Equality.t 'a) => (option 'a) => (Set.t 'a);
  /* [toSetWith equals option] returns a Set view of the option using [equals] equate values. */
};

let module IndexedCollection: {
  module type S1 = {
    type t 'a;

    include NavigableCollection.S1 with type t 'a := t 'a;

    let everyWithIndex: (int => 'a => bool) => (t 'a) => bool;

    let findWithIndex: (int => 'a => bool) => (t 'a) => (option 'a);
    /** [tryFindWithIndex f vec] returns the first value for which the predicate [f] returns true or None. */

    let findWithIndexOrRaise: (int => 'a => bool) => (t 'a) => 'a;

    let forEachWithIndex: (int => 'a => unit) => (t 'a) => unit;

    let forEachRightWithIndex: (int => 'a => unit) => (t 'a) => unit;

    let get: int => (t 'a) => (option 'a);
    /** [tryGet index vec] returns the element at [index] or None if [index] is out of bounds. */

    let getOrRaise: int => (t 'a) => 'a;

    let indexOf: ('a => bool) => (t 'a) => (option int);
    /** [tryIndexOf f vec] returns the index of the first element for which the predicate [f] returns true.
     *  If no value is found, returns None.
     */

    let indexOfOrRaise: ('a => bool) => (t 'a) => int;

    let indexOfWithIndex: (int => 'a => bool) => (t 'a) => (option int);
    /** [indexOfWithIndex f vec] returns the index of the first index/element pair in [vec] for
     *  which the predicate [f] returns true. If no value is found, returns None.
     */

    let indexOfWithIndexOrRaise: (int => 'a => bool) => (t 'a) => int;

    let noneWithIndex: (int => 'a => bool) => (t 'a) => bool;

    let reduceWithIndex: ('acc => int => 'a => 'acc) => 'acc => (t 'a) => 'acc;

    let reduceRightWithIndex: ('acc => int => 'a => 'acc) => 'acc => (t 'a) => 'acc;

    let someWithIndex: (int => 'a => bool) => (t 'a) => bool;

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

let module rec Vector: {
  /** Indexed type that supports efficient operations for
   * prepend, appends, indexing, conctentation, and splits.
   */

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

  type t 'a;
  /** The vector type */

  include S1 with type t 'a := t 'a;

  let mutate: (t 'a) => (TransientVector.t 'a);
}

and TransientVector: {
  /** A temporarily mutable Vector. Once persisted, any further operations on a
   *  TransientVector instance will throw. Intended for implementing bulk mutation operations efficiently.
   */
  type t 'a;
  /** The TransientVector type. */

  include TransientDeque.S1 with type t 'a := t 'a;

  let get: int => (t 'a) => (option 'a);

  let getOrRaise: int => (t 'a) => 'a;

  let insertAt: int => 'a => (t 'a) => (t 'a);
  /** [insertAt index value transient] inserts value into [transient] at [index].
   *
   *  WARNING: Not implemented
   *
   *  Complexity: O(log32 N)
   */

  let persist: (t 'a) => (Vector.t 'a);
  /** [persist transient] returns a persisted Vector. Further attempts to access or mutate [transient]
   *  will throw.
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
};

let module CopyOnWriteArray: {
  /** Opaque wrapper around an underlying array instance that provides copy on write semantics */

  type t 'a;
  /** The CopyOnWriteArray type. */

  include Vector.S1 with type t 'a := t 'a;

  let ofUnsafe: (array 'a) => (t 'a);
  /** [unsafe arr] returns a CopyOnWriteArray backed by [arr]. Note, it is the caller's
   *  responsibility to ensure that [arr] is not subsequently mutated.
   *
   *  Complexity: O(1)
   */
};

let module NavigableSet: {
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

let module IntRange: {
  /** Represents a contiguous Set of discrete integers */

  type t;
  /** The IntRange type.*/

  include NavigableSet.S with type a := int and type t := t;

  let create: int => int => t;

  let empty: t;

  let hash: t => int;
};

let module PersistentSet: {
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

let module PersistentNavigableSet: {
  module type S = {
    type a;
    type t;

    include NavigableSet.S with type a := a and type t := t;
    include PersistentSet.S with type a := a and type t := t;

    let removeFirstOrRaise: t => t;

    let removeLastOrRaise: t => t;
  };

  module type S1 = {
    type t 'a;

    include NavigableSet.S1 with type t 'a := t 'a;
    include PersistentSet.S1 with type t 'a := t 'a;

    let removeFirstOrRaise: t 'a => t 'a;

    let removeLastOrRaise: t 'a => t 'a;
  };
};

let module rec HashSet: {
  /** A set implementation that utilizes hashing and comparison
   *  or equality for collision resolution.
   */

  type t 'a;
  /** The HashSet type. */

  include PersistentSet.S1 with type t 'a := t 'a;

  let empty: unit => (t 'a);
  /** The empty HashSet with HashStrategy.structuralCompare hash strategy. */

  let emptyWith: (HashStrategy.t 'a) => (t 'a);
  /** [emptyWith strategy] returns an empty HashSet using [strategy]
   *  for hashing and collision resolution.
   */

  let equals: (t 'a) => (t 'a) => bool;

  let from: (Iterator.t 'a) => (t 'a);
  /** [from iter] returns an HashSet including the values in [iter]. */

  let fromWith: (HashStrategy.t 'a)  => (Iterator.t 'a) => (t 'a);
  /** [from iter] returns an HashSet including the values in [iter]. */

  let hash: (Hash.t (t 'a));
  /** [hash set] hashes [set]. */

  let mutate: (t 'a) => (TransientHashSet.t 'a);
  /** [mutate set] returns a TransientHashSet containing the same elements as [set].
   *
   *  Complexity: O(1)
   */
}

and TransientHashSet: {
  /** A temporarily mutable HashSet. Once persisted, any further operations on a
   *  TransientHashSet instance will throw. Intended for implementing bulk mutation
   *  operations efficiently.
   */

  type t 'a;
  /** The TransientHashSet type. */

  include TransientCollection.S1 with type t 'a := t 'a;

  let add: 'a => (t 'a) => (t 'a);
  /** [add value transient] adds [value] to [transient].
   *
   *  Complexity: O(log32 N)
   */

  let addAll: (Iterator.t 'a) => (t 'a) => (t 'a);
  /** [addAll iter transient] adds all elements in [iter] to [transient].
   *
   *  Complexity: O(N log32 N)
   */

  let contains: 'a => (t 'a) => bool;
  /** [contains value transient] returns true if any element in [transient] is equal to [value].
   *
   *  Complexity: O(log32 N)
   */

  let empty: unit => (t 'a);
  /** [empty ()] return a new empty TransientHashSet with the HashStrategy.structuralCompare hash strategy. */

  let emptyWith: (HashStrategy.t 'a) => (t 'a);
  /** [emptyWith strategy]  return a new empty TransientHashSet with the [strategy] hash strategy */

  let persist: (t 'a) => (HashSet.t 'a);
  /** [persist transient] returns a persisted HashSet. Further attempts to access or mutate [transient]
   *  will throw.
   */

  let remove: 'a => (t 'a) => (t 'a);
  /** [remove value transient] removes [value] from [transient].
   *
   *  Complexity: O(log32 N)
   */
};

let module SortedSet: {
  /** AVL tree based Set implementation. */
  module type S = {
    type a;
    type t;
    /** The SortedSet type */

    include Comparable.S with type t := t;
    include PersistentNavigableSet.S with type a := a and type t := t;

    let empty: t;
    /** The empty SortedSet using structural comparison */

    let from: (Iterator.t a) => t;
    /** [from iter] returns a SortedSet including the values in [iter] using structural comparison. */

    let hash: (Hash.t t);
    /** [hash set] hashes [set], hashing elements using structural hashing. */

    let hashWith: (Hash.t a) => (Hash.t t);
    /** [hashWith hash set] hashes [set], hashing elements using [hash]. */
  };

  let module Make: (Comparable: Comparable.S) => S with type a = Comparable.t;
};

let module rec IntSet: {
  /** A set implementation optimized for storing sparse ints. */

  type t;
  /** The IntSet type. */

  include PersistentSet.S with type a := int and type t := t;

  let empty: t;
  /** The empty IntSet. */

  let from: (Iterator.t int) => t;
  /** [from iter] returns an IntSet including the values in [iter]. */

  let hash: (Hash.t t);
  /** [hash set] hashes [set], hashing elements using structural hashing. */

  let mutate: t => TransientIntSet.t;
  /** [mutate set] returns a TransientIntSet containing the same elements as [set].
   *
   *  Complexity: O(1)
   */
}

and TransientIntSet: {
  /** A temporarily mutable IntSet. Once persisted, any further operations on a
   *  TransientIntSet instance will throw. Intended for implementing bulk mutation
   *  operations efficiently.
   */

  type t;
  /** The TransientIntSet type. */

  include TransientCollection.S with type a = int and type t := t;

  let add: int => t => t;
  /** [add value transient] adds [value] to [transient].
   *
   *  Complexity: O(log32 N)
   */

  let addAll: (Iterator.t int) => t => t;
  /** [addAll iter transient] adds all elements in [iter] to [transient].
   *
   *  Complexity: O(N log32 N)
   */

  let contains: int => t => bool;
  /** [contains value transient] returns true if any element in [transient] is equal to [value].
   *
   *  Complexity: O(log32 N)
   */

  let empty: unit => t;
  /** [empty ()] return a new empty TransientIntSet. */

  let persist: t => IntSet.t;
  /** [persist transient] returns a persisted IntSet. Further attempts to access or mutate [transient]
   *  will throw.
   */

  let remove: int => t => t;
  /** [remove value transient] removes [value] from [transient].
   *
   *  Complexity: O(log32 N)
   */
};

let module NavigableMap: {
  module type S1 = {
    type k;
    type t 'v;

    include Map.S1 with type k := k and type t 'v := t 'v;

    let first: (t 'v) => (option (k, 'v));
    /** [tryFirst map] returns the first key/value pair in [set] or None. */

    let firstOrRaise: (t 'v) => (k, 'v);
    /** [first map] returns the first key/value pair in [set] or throws. */

    let last: (t 'v) => (option (k, 'v));
    /** [tryLast map] returns the last key/value pair in [set] or None. */

    let lastOrRaise: (t 'v) => (k, 'v);
    /** [last map] returns the last key/value pair in [set] or throws. */

    let reduceRight: ('acc => k => 'v => 'acc) => 'acc => (t 'v) => 'acc;
    /** [reduceRight f acc map] applies the accumulator function [f] to each key/value pair in [map]
     *  in reverse order with the specified seed value [acc], returning the final accumulated value.
     */

    let toIteratorRight: (t 'v) => (Iterator.t (k, 'v));

    let toKeyedIteratorRight: (t 'v) => (KeyedIterator.t k 'v);

    let toSequenceRight: (t 'v) => (Sequence.t (k, 'v));
  };
};

let module PersistentMap: {
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

let module TransientMap: {
  module type S1 = {
    type k;

    type t 'v;

    let alter: k => (option 'v => option 'v) => (t 'v) => (t 'v);
    /** [alter key f transient] enables efficient deep updates to an existing
     *  mapping from [key] in [transient]. If [transient] already has a mapping from [key],
     *  [f] will be called with Some, otherwise it will be called with None.
     *  If [f] returns None, alter removes any mapping from [key] in [transient].
     *  If [f] returns Some, alter returns add or updates the mapping
     *  from [key] in [transient].
     */

    let count: (t 'v) => int;
    /** [count map] returns the number of key/value pairs in [map]. */


    let get: 'k => (t 'v) => (option 'v);
    /** [tryGet key transient] returns the value associated with [key] or None
     *
     *  Complexity: O(log32 N), effectively O(1)
     */

    let getOrRaise: k => (t 'v) => 'v;

    let isEmpty: (t 'v) => bool;
    /** [isEmpty map] returns true if [map] contains no key/value pairs. */

    let isNotEmpty: (t 'v) => bool;
    /** [isNotEmpty map] returns true if [map] contains at least one key/value pair. */

    let put: k => 'v => (t 'v) => (t 'v);
    /** [put key value transient] adds the mapping of [key] to [value] to [transient].
     *
     *  Complexity: O(log32 N), effectively O(1)
     */

    let putAll: (KeyedIterator.t k 'v) => (t 'v) => (t 'v);
    /** [putAll iter transient] adds the key/value pairs in [iter] to [transient].
     *  Key value pairs in [iter] replace existing mappings in [transient].
     */

    let remove: k => (t 'v) => (t 'v);
    /** [remove key transient] removes from [transient] any mappings from [key].
     *
     *  Complexity: O(log32 N), effectively O(1)
     */

    let removeAll: (t 'v) => (t 'v);
    /** [removeAll transient] removes all mappings from [transient].
     *
     *  Complexity: O(1)
     */
  };

  module type S2 = {
    type t 'k 'v;

    let alter: 'k => (option 'v => option 'v) => (t 'k 'v) => (t 'k 'v);
    /** [alter key f transient] enables efficient deep updates to an existing
     *  mapping from [key] in [transient]. If [transient] already has a mapping from [key],
     *  [f] will be called with Some, otherwise it will be called with None.
     *  If [f] returns None, alter removes any mapping from [key] in [transient].
     *  If [f] returns Some, alter returns add or updates the mapping
     *  from [key] in [transient].
     */

    let count: (t 'k 'v) => int;
    /** [count map] returns the number of key/value pairs in [map]. */


    let get: 'k => (t 'k 'v) => (option 'v);
    /** [tryGet key transient] returns the value associated with [key] or None
     *
     *  Complexity: O(log32 N), effectively O(1)
     */

    let getOrRaise: 'k => (t 'k 'v) => 'v;

    let isEmpty: (t 'k 'v) => bool;
    /** [isEmpty map] returns true if [map] contains no key/value pairs. */

    let isNotEmpty: (t 'k 'v) => bool;
    /** [isNotEmpty map] returns true if [map] contains at least one key/value pair. */

    let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);
    /** [put key value transient] adds the mapping of [key] to [value] to [transient].
     *
     *  Complexity: O(log32 N), effectively O(1)
     */

    let putAll: (KeyedIterator.t 'k 'v) => (t 'k 'v) => (t 'k 'v);
    /** [putAll iter transient] adds the key/value pairs in [iter] to [transient].
     *  Key value pairs in [iter] replace existing mappings in [transient].
     */

    let remove: 'k => (t 'k 'v) => (t 'k 'v);
    /** [remove key transient] removes from [transient] any mappings from [key].
     *
     *  Complexity: O(log32 N), effectively O(1)
     */

    let removeAll: (t 'k 'v) => (t 'k 'v);
    /** [removeAll transient] removes all mappings from [transient].
     *
     *  Complexity: O(1)
     */
  };
};

let module rec HashMap: {
  /** A hashed Map. */

  type t 'k 'v;
  /** The HashMap type. */

  include PersistentMap.S2 with type t 'k 'v := t 'k 'v;

  let contains: 'k => 'v => (t 'k 'v) => bool;
  /** [contains key value map] returns true if [map] contains the [key] [value] pair,
   *  using structural equality to equate [value].
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let containsWith: (Equality.t 'v) => 'k => 'v => (t 'k 'v) => bool;
  /** [containsWith equals key value map] returns true if [map] contains the [key] [value] pair,
   *  using [equals] to equate [value].
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let empty: unit => (t 'k 'v);
  /** The empty HashMap using the structuralCompare HashStrategy. */

  let emptyWith: (HashStrategy.t 'k) => (t 'k 'v);
  /** [emptyWith strategy] returns an empty HashMap using the HashStrategy [strategy]. */

  let equals: (t 'k 'v) => (t 'k 'v) => bool;
  /** [equals this that] equates [this] and [that]. Structural equality is used to equate values. */

  let equalsWith: (Equality.t 'v) => (t 'k 'v) => (t 'k 'v) => bool;
  /** [equalsWith equals this that] equates [this] and [that].
   *  [equals] is used to equate values.
   */

  let from: (KeyedIterator.t 'k 'v) => (t 'k 'v);
  /** [from iter] returns a HashMap including the key/value pairs in [seq]
   *  using the structuralCompare HashStrategy.
   */

  let fromWith: (HashStrategy.t 'k) => (KeyedIterator.t 'k 'v) => (t 'k 'v);
  /** [fromWith strategy iter] returns a HashMap including the key/value pairs in [seq]
   *  using the provided HashStrategy [strategy].
   */

  let hash: (Hash.t (t 'k 'v));
  /** [hash map] hashes [map], hashing values using structural hashing. */

  let hashWith: (Hash.t 'v) => (Hash.t (t 'k 'v));
  /** [hashWith hash map] hashes [map], hashing values using [hash]. */

  let mutate: (t 'k 'v) => (TransientHashMap.t 'k 'v);
  /** [mutate map] returns a TransientHashMap containing the same key/values pairs as [map].
   *
   *  Complexity: O(1)
   */

  let toSet: (t 'k 'v) => (Set.t ('k, 'v));
  /** [toSet map] returns a Set view of key/value pairs in [map], using structural equality
   *  to equate values.
   */

  let toSetWith: (Equality.t 'v) => (t 'k 'v) => (Set.t ('k, 'v));
  /** [toSetWith equals map] returns a Set view of key/value pairs in [map],
   *  using [equals] to equate values.
   */
}

and TransientHashMap: {
  /** A temporarily mutable HashMap. Once persisted, any further operations on a
   *  TransientHashSet instance will throw. Intended for implementing bulk mutation
   *  operations efficiently.
   */

  type t 'k 'v;
  /** The TransientHashMap type. */

  include TransientMap.S2 with type t 'k 'v := t 'k 'v;

  let empty: unit => (t 'k 'v);
  /** [empty ()] returns a new empty TransientHashMap. */

  let emptyWith: (HashStrategy.t 'k) => (t 'k 'v);
  /** [emptyWith strategy] returns an empty TransientHashMap using the provided
   *  key HashStrategy.
   */

  let persist: (t 'k 'v) => (HashMap.t 'k 'v);
  /** [persist transient] returns a persisted HashMap. Further attempts to access or mutate [transient]
   *  will throw.
   */
};

let module rec IntMap: {
  /** A Map optimized for integer keys. */

  type t 'v;
  /** The IntMap type. */

  include PersistentMap.S1 with type k = int and type t 'v := t 'v;

  let contains: int => 'v => (t 'v) => bool;
  /** [contains key value map] returns true if [map] contains the [key] [value] pair,
   *  using structural equality to equate [value].
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let containsWith: (Equality.t 'v) => int => 'v => (t 'v) => bool;
  /** [containsWith equals key value map] returns true if [map] contains the [key] [value] pair,
   *  using [equals] to equate [value].
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let empty: (t 'v);

  let equals: (t 'v) => (t 'v) => bool;
  /** [equals this that] equates [this] and [that]. Structural equality is used to equate values. */

  let equalsWith: (Equality.t 'v) => (t 'v) => (t 'v) => bool;
  /** [equalsWith equals this that] equates [this] and [that].
   *  [equals] is used to equate values.
   */

  let from: (KeyedIterator.t int 'v) => (t 'v);
  /** [from iter] returns an IntMap including the key/value pairs in [iter]. */

  let hash: (Hash.t (t 'v));
  /** [hash map] hashes [map], hashing values using structural hashing. */

  let hashWith: (Hash.t 'v) => (Hash.t (t 'v));
  /** [hashWith hash map] hashes [map], hashing values using [hash]. */

  let mutate: (t 'v) => (TransientIntMap.t 'v);
  /** [mutate map] returns a TransientIntMap containing the same key/values pairs as [map].
   *
   *  Complexity: O(1)
   */

  let toSet: (t 'v) => (Set.t (int, 'v));
  /** [toSet map] returns a Set view of key/value pairs in [map], using structural equality
   *  to equate values.
   */

  let toSetWith: (Equality.t 'v) => (t 'v) => (Set.t (int, 'v));
  /** [toSetWith equals map] returns a Set view of key/value pairs in [map],
   *  using [equals] to equate values.
   */
}

and TransientIntMap: {
  type t 'v;

  include TransientMap.S1 with type k = int and type t 'v := t 'v;

  let alter: int => ((option 'v) => (option 'v)) => (t 'v) => (t 'v);
  /** [alter key f transient] enables efficient deep updates to an existing
   *  mapping from [key] in [transient]. If [transient] already has a mapping from [key],
   *  [f] will be called with Some, otherwise it will be called with None.
   *  If [f] returns None, alter removes any mapping from [key] in [transient].
   *  If [f] returns Some, alter returns add or updates the mapping
   *  from [key] in [transient].
   */

  let count: (t 'v) => int;
  /** [count map] returns the number of key/value pairs in [map]. */

  let empty: unit => (t 'v);
  /** [empty ()] returns a new empty TransientIntMap. */


  let get: int => (t 'v) => (option 'v);
  /** [tryGet key transient] returns the value associated with [key] or None
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let getOrRaise: int => (t 'v) => 'v;

  let isEmpty: (t 'v) => bool;
  /** [isEmpty map] returns true if [map] contains no key/value pairs. */

  let isNotEmpty: (t 'v) => bool;
  /** [isNotEmpty map] returns true if [map] contains at least one key/value pair. */

  let persist: (t 'v) => (IntMap.t 'v);
  /** [persist transient] returns a persisted HashBiMap. Further attempts to access or mutate [transient]
   *  will throw.
   */

  let put: int => 'v => (t 'v) => (t 'v);
  /** [put key value transient] adds the mapping of [key] to [value] to [transient].
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let putAll: (KeyedIterator.t int 'v) => (t 'v) => (t 'v);
  /** [putAll iter transient] adds the key/value pairs in [iter] to [transient].
   *  Key value pairs in [iter] replace existing mappings in [transient].
   */

  let remove: int => (t 'v) => (t 'v);
  /** [remove key transient] removes from [transient] any mappings from [key].
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let removeAll: (t 'v) => (t 'v);
  /** [removeAll transient] removes all mappings from [transient].
   *
   *  Complexity: O(1)
   */
};

let module SortedMap: {
  /** AVL tree based Map. */
  module type S = {
    type k;

    type t +'v;
    /** The SortedMap type. */

    include PersistentNavigableMap.S1 with type k := k and type t 'v := t 'v;

    let compare: (Comparator.t (t 'v));
    /** A comparator that compares two SortedMap instances, comparing values using structural
     *  comparison.
     */

    let compareWith: (Comparator.t 'v) => (Comparator.t (t 'v));
    /** [compareWith comparator this that] returns a comparator that compares two SortedMap instances,
     *  comparing values with [comparator].
     */

    let contains: k => 'v => (t 'v) => bool;
    /** [contains key value map] returns true if [map] contains the [key] [value] pair,
     *  using structural equality to equate [value].
     *
     *  Complexity: O(log N)
     */

    let containsWith: (Equality.t 'v) => k => 'v => (t 'v) => bool;
    /** [containsWith equals key value map] returns true if [map] contains the [key] [value] pair,
     *  using [equals] to equate [value].
     *
     *  Complexity: O(log N)
     */

    let empty: (t 'v);
    /** The empty SortedMap using the structural comparator. */

    let equals: (t 'v) => (t 'v) => bool;
    /** [equals this that] equates [this] and [that]. Structural equality is used to equate values. */

    let equalsWith: (Equality.t 'v) => (t 'v) => (t 'v) => bool;
    /** [equalsWith equals this that] equates [this] and [that].
     *  [equals] is used to equate values.
     */

    let from: (KeyedIterator.t k 'v) => (t 'v);
    /** [from iter] returns a SortedMap including the key/value pairs in [iter]
     *  using the structural comparison.
     */

    let hash: (Hash.t (t 'v));
    /** [hash map] hashes [map], hashing keys and values using structural hashing. */

    let hashWith: (Hash.t k) => (Hash.t 'v) => (Hash.t (t 'v));
    /** [hashWith keyHash valueHash map] hashes [map], hashing keys and
     *  values using [keyHash] and [valueHash] respectively.
     */

    let toSet: (t 'v) => (Set.t (k, 'v));
    /** [toSet map] returns a Set view of key/value pairs in [map], using structural equality
     *  to equate values.
     */

    let toSetWith: (Equality.t 'v) => (t 'v) => (Set.t (k, 'v));
    /** [toSetWith equals map] returns a Set view of key/value pairs in [map],
     *  using [equals] to equate values.
     */
  };

  let module Make: (Comparable: Comparable.S) => S with type k = Comparable.t;
};
