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

  /* Will be available in Ocaml 4.03t
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

  /* FIXME: Should the underlying discriminated union be exposed publicly? */
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

let module Seq: {
  /** Functional sequence iterators. */

  type t 'a;
  /** The Seq type. */

  let buffer: int => int => (t 'a) => (t (list 'a));
  /** [buffer count skip seq] returns a Seq that collects elements from [seq]
   *  into buffer lists of size [count], skipping [skip] number of elements in between
   *  creation of new buffers. The returned buffers are guaranteed to be of size [count],
   *  and elements are dropped if [seq] completes before filling the last buffer.
   */

  let compare: (Comparator.t (t 'a));
  /** A comparator that compares two Seq instances using structural comparison to compare elements. */

  let compareWith: (Comparator.t 'a) => (Comparator.t (t 'a));
  /** [compareWith comparator] returns a Comparator that compares two Seq instances
   *  using [comparator] to compare elements.
   */

  let concat: (list (t 'a)) => (t 'a);
  /** [concat seqs] returns a Seq that concatenates all Seq's in the list [seqs]. */

  let concatAll: (t (t 'a)) => (t 'a);
  /** [concatAll seq] returns a Seq that is the concatenation of all the Seqs produced by [seq]. */

  let concatMap: ('a => (t 'b)) => (t 'a) => (t 'b);
  /** An alias for [flatMap] */

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

  let count: (t 'a) => int;
  /** [count seq] returns the number of elements in the seq.
   *
   *  Complexity: O(N)
   */

  let defer: (unit => t 'a) => (t 'a);
  /** [defer f] returns a Seq that invokes the function [f] whenever the Seq is iterated. */

  let distinctUntilChanged: (t 'a) => (t 'a);
  /** [distinctUntilChanged seq] returns a Seq that contains only
   *  distinct contiguous elements from [seq] using structural equality.
   */

  let distinctUntilChangedWith: (Equality.t 'a) => (t 'a) => (t 'a);
  /** [distinctUntilChangedWith equals seq] returns a Seq that contains only
   *  distinct contiguous elements from [seq] using [equals] to equate elements.
   */

  let doOnNext: ('a => unit) => (t 'a) => (t 'a);
  /** [doOnNext f seq] returns a Seq that invokes the function [f] on every element produced by [seq]. */

  let empty: (t 'a);
  /** The empty Seq. */

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

  let every: ('a => bool) => (t 'a) => bool;
  /** [every f seq] returns true if the predicate [f] returns true for every
   *  element in [seq], otherwise false. If [seq] is empty, returns true.
   *
   *  Complexity: O(N)
   */

  let filter: ('a => bool) => (t 'a) => (t 'a);
  /** [filter f seq] returns a seq that only includes elements that satisfy the predicate [f].
   *
   *  Complexity: O(N)
   */

  let find: ('a => bool) => (t 'a) => 'a;
  /** [find f seq] returns the first value for which the predicate [f] returns true.
   *  If no value is found, an exception is thrown.
   *
   *  Complexity: O(N)
   */

  let first: (t 'a) => 'a;
  /** [first seq] returns the first element in [seq] or throws.
   *
   *  Complexity: O(1)
   */

  let flatMap: ('a => t 'b) => (t 'a) => (t 'b);
  /** [flatmap f seq] is the equivalent of [seq |> map f |> concatAll] */

  let flatten: (t (t 'a)) => (t 'a);
  /** An alias for [concatAll] */

  let forEach: ('a => unit) => (t 'a) => unit;
  /** [forEach f seq] iterates through [seq], invoking [f] for each element until [seq] completes.
   *
   *  Complexity: O(N)
   */

   let get: int => (t 'a) => 'a;
   /** [get n seq] returns the [n]th element in [seq] or throws.
    *
    *  Complexity: O(N)
    */

  let hash: (Hash.t (t 'a));
   /** [hash seq] hashes [seq], hashing elements using structural hashing.
    *
    *  Complexity: O(N)
    */

  let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
  /** [hashWith hash seq] hashes [seq], hashing elements using [hash].
   *
   *  Complexity: O(N)
   */

  let inRange: int => (option int) => int => (t int);
  /** [inRange start count step] returns an integer Seq starting with [start]
   *  with [count] elements, with an interval of [step] between elements in the Seq.
   *  If count is None, the Seq is infinite.
   */

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

  let last: (t 'a) => 'a;
  /** [last seq] returns the last element in [seq] or throws.
   *
   *  Complexity: O(N)
   */

  let map: ('a => 'b) => (t 'a) => (t 'b);
  /** [map f seq] returns a Seq whose elements are the result of applying [f] to each element in [seq]. */

  let none: ('a => bool) => (t 'a) => bool;
  /** [none f seq] returns true if the predicate [f] returns false for every
   *  elements in [seq], otherwise true. If [seq] is empty, returns true.
   *
   *  Complexity: O(N)
   */

  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  /** [reduce f acc seq] applies the accumulator function [f] to each element in [seq]
   *  with the specified seed value [acc], returning the final accumulated value.
   *
   *  Complexity: O(N)
   */

  let repeat: 'a => (option int) => (t 'a);
  /** [repeat value count] returns a Seq that repeats [value] [count] times.
   *  If [count] is None, the Seq is infinite.
   */

  let return: 'a => (t 'a);
  /** [return value] returns a single element Seq containing [value]. */

  let scan: ('acc => 'a => 'acc) => 'acc => (t 'a) => (t 'acc);
  /** [scan f acc seq] returns a Seq of accumulated values resulting from the
   *  application of the accumulator function [f] to each element in [seq] with the
   *  specified seed value [acc].
   */

  let skip: int => (t 'a) => (t 'a);
  /** [skip count seq] returns a Seq that skips the first [count] elements in [seq]. */

  let skipWhile: ('a => bool) => (t 'a) => (t 'a);
  /** [skipWhile f seq] returns a Seq that applies the predicate [f] to each element in [seq],
   *  skipping elements until [f] first returns false.
   */

  let some: ('a => bool) => (t 'a) => bool;
  /** [some f seq] returns true if the predicate [f] returns true for any element in [seq], otherwise false.
   *  If [seq] is empty, returns false.
   *
   *  Complexity: O(N)
   */

  let startWith: 'a => (t 'a) => (t 'a);
  /** [startWith value seq] returns a seq whose first elements is [value]. */

  let take: int => (t 'a) => (t 'a);
  /** [take count seq] returns a Seq that includes the first [count] elements in [seq]. */

  let takeWhile: ('a => bool) => (t 'a) => (t 'a);
  /** [takeWhile f seq] returns a Seq that applies the predicate [f] to each element in [seq],
   *  taking elements until [f] first returns false.  */

  let tryFind: ('a => bool) => (t 'a) => (option 'a);
  /** [tryFind f seq] returns the first value for which the predicate [f] returns true or None.
   *
   *  Complexity: O(N)
   */

  let tryFirst: (t 'a) => (option 'a);
  /** [tryFirst seq] returns first element in [seq] or None.
   *
   *  Complexity: O(1)
   */

  let tryGet: int => (t 'a) => (option 'a);
  /** [tryGet n seq] returns the [n]th element in [seq] or none.
   *
   *  Complexity: O(N)
   */

  let tryLast: (t 'a) => (option 'a);
  /** [tryLast seq] returns the last element in [seq] or None.
   *
   *  Complexity: O(N)
   */

  let zip: (list (t 'a)) => (t (list 'a));
  /** [zip seqs] merges a list of n Seqs into a Seq of lists with n values.
   *  Elements are produce until any Seq in [seq] completes.
   */

  let zip2: (t 'a) => (t 'b) => (t ('a, 'b));
  /** [zip2 first second] merges two Seqs into a Seq of tuples.
   *  Elements are produce until either first or second complete.
   */

  let zip3: (t 'a) => (t 'b) => (t 'c) => (t ('a, 'b, 'c));
  /** [zip3 first second third] merges two Seqs into a Seq of triples.
   *  Elements are produce until either first, second, or third complete.
   */

  let zipLongest: (list (t 'a)) => (t (list (option 'a)));
  /** [zip seqs] merges a list of n Seqs into a Seq of lists with n values.
   *  Elements are produce until all Seqs in [seq] complete.
   */

  let zipLongest2: (t 'a) => (t 'b) => (t (option 'a, option 'b));
  /** [zip2 first second] merges two Seqs into a Seq of tuples.
   *  Elements are produce until both first and second complete.
   */

  let zipLongest3: (t 'a) => (t 'b) => (t 'c) => (t (option 'a, option 'b, option 'c));
  /** [zip3 first second third] merges two Seq into a Seq of triples.
   *  Elements are produce until first, second, and third all complete.
   */
};

let module rec Collection: {
  /** A read only view of an underlying set of unique values. The intent of this type is to enable
   *  interop between alternative concrete implementations such as [SortedSet] and [HashSet].
   *  The complexity of functions in this module is dependent upon the underlying concrete implementation.
   */

  type t 'a;
  /** The Collection type. */

  let contains: 'a => (t 'a) => bool;
  /** [contains value collection] returns true if [collection] contains the element [value]. */

  let count: (t 'a) => int;
  /** [count collection] returns the number of elements in [collection]. */

  let empty: (t 'a);
  /** The empty Collection. */

  let equals: (t 'a) => (t 'a) => bool;
  /** [equals this that] equates [this] and [that] ensuring that the Collections have the same count
   *  and contains the same elements.
   */

  let every: ('a => bool) => (t 'a) => bool;
  /** [every f collection] returns true if the predicate [f] returns true for every
   *  element in [collection], otherwise false. If [collection] is empty, returns true.
   */

  let find: ('a => bool) => (t 'a) => 'a;
  /** [find f collection] returns the first value for which the predicate [f] returns true.
   *  If no value is found, an exception is thrown.
   */

  let forEach: ('a => unit) => (t 'a) => unit;
  /** [forEach f collection] iterates through [collection], invoking [f] for each element in the Collection. */

  let hash: (Hash.t (t 'a));
  /** [hash collection] hashes [collection], hashing elements using structural hashing. */

  let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
  /** [hashWith hash collection] hashes [collection], hashing elements using [hash]. */

  let inRange: int => int => int => (t int);
  /** [inRange start count step] returns an integer Collection starting with [start]
   *  with [count] elements, with an interval of [step] between elements in the Collection.
   */

  let intersect: (t 'a) => (t 'a) => (Seq.t 'a);
  /** [intersect this that] returns a Seq of unique elements
   *  which occur in both [this] and [that].
   */

  let isEmpty: (t 'a) => bool;
  /** [isEmpty collection] returns true if [collection] contains no elements. */

  let isNotEmpty: (t 'a) => bool;
  /** [isNotEmpty collection] returns true if [collection] contains at least one element. */

  let none: ('a => bool) => (t 'a) => bool;
  /** [none f collection] returns true if the predicate [f] returns false for
   *  every elements in [collection], otherwise true. If [collection] is empty, returns true.
   */

  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  /** [reduce f acc collection] applies the accumulator function [f] to each element in [collection]
   *  with the specified seed value [acc], returning the final accumulated value.
   */

  let some: ('a => bool) => (t 'a) => bool;
  /** [some f collection] returns true if the predicate [f] returns true for
   *  any element in [collection], otherwise false. If [collection] is empty, returns false.
   */

  let subtract: (t 'a) => (t 'a) => (Seq.t 'a);
  /** [subtract this that] returns a Seq of unique element
   *  which occur in [this] but not in [that].
   */

  let toKeyed: (t 'a) => (Keyed.t 'a 'a);
  /** [toKeyed collection] returns a Keyed collection of values in [collection]. */

  let toSeq: (t 'a) => (Seq.t 'a);
  /** [toSeq collection] returns a Seq of the values in [collection]. */

  let tryFind: ('a => bool) => (t 'a) => (option 'a);
  /** [find f collection] returns the first value for which the predicate [f] returns true or None. */

  let union: (t 'a) => (t 'a) => (Seq.t 'a);
  /** [union this that] returns a Seq of unique elements which occur in either [this] or [that]. */
}

and Keyed: {
  /** A read only view of an underlying set of key value pairs. The intent of this type is to enable
   *  interop between alternative concrete implementations such as [SortedMap] and [HashMap].
   *  The complexity of functions in this module is dependent upon the underlying concrete implementation.
   */

  type t 'k 'v;
  /** The keyed type. */

  let contains: 'k => 'v => (t 'k 'v) => bool;
  /** [contains key value keyed] returns true if [keyed] contains the [key] [value] pair,
   *  using structural equality to equate [value].
   */

  let containsWith: (Equality.t 'v) => 'k => 'v => (t 'k 'v) => bool;
  /** [containsWith equals key value keyed] returns true if [keyed] contains the [key] [value] pair,
   *  using [equals] to equate [value].
   */

  let containsKey: 'k => (t 'k 'v) => bool;
  /** [containsKey key keyed] returns true if [keyed] contains an entry with the key [key]. */

  let count: (t 'k 'v) => int;
  /** [count keyed] returns the number of key value pairs in the Keyed collection. */

  let empty: (t 'k 'v);
  /** The empty Keyed collection. */

  let equals: (t 'k 'v) => (t 'k 'v) => bool;
  /** [equals this that] equates [this] and [that] ensuring that the Keyed collections have the same count
   *  and contains the same key value pairs. Structural equality is used to equate values.
   */

  let equalsWith: (Equality.t 'v) => (t 'k 'v) => (t 'k 'v) => bool;
  /** [equalsWith equals this that] equates [this] and [that] ensuring that the Keyed collections
   *  have the same count, and contains the same key value pairs. [equals] is used to equate values.
   */

  let every: ('k => 'v => bool) => (t 'k 'v) => bool;
  /** [every f keyed] returns true if the predicate [f] returns true for every
   *  key value pair in [keyed], otherwise false. If [keyed] is empty, returns true.
   */

  let find: ('k => 'v => bool) => (t 'k 'v) => ('k, 'v);
  /** [find f keyed] returns the first key value pair for which the predicate [f] returns true.
   *  If no value is found, an exception is thrown.
   */

  let forEach: ('k => 'v => unit) => (t 'k 'v) => unit;
  /** [forEach f keyed] iterates through [keyed], invoking [f] for each key value pair. */

  let get: 'k => (t 'k 'v) => 'v;
  /** [get key keyed] returns the value associated with [key] or throws */

  let hash: (Hash.t (t 'k 'v));
  /** [hash keyed] hashes [keyed], hashing keys and values using structural hashing. */

  let hashWith: (Hash.t 'k) => (Hash.t 'v) => (Hash.t (t 'k 'v));
  /** [hashWith keyHash valueHash keyed] hashes [keyed], hashing keys using [keyHash]
   *  and values using [valueHash].
   */

  let isEmpty: t 'k 'v => bool;
  /** [isEmpty keyed] returns true if [keyed] contains no key value pairs. */

  let isNotEmpty: t 'k 'v => bool;
  /** [isNotEmpty keyed] returns true if [keyed] contains at least one key value pair. */

  let keys: (t 'k 'v) => (Collection.t 'k);
  /** [keys keyed] returns a Collection view of keys in the [keyed]. */

  let map: ('k => 'a => 'b) => (t 'k 'a) => (t 'k 'b);
  /** [map f keyed] returns a Keyed collection whose values are the result of
   *  applying [f] each key value pair in [keyed] lazily. Note: The results of
   *  applying [f] are not memoized, therefore [f] must be pure.
   */

  let none: ('k => 'v => bool) => (t 'k 'v) => bool;
  /** [none f keyed] returns true if the predicate [f] returns false for
   *  every key value pair in [keyed], otherwise true. If [keyed] is empty, returns true.
   */

  let reduce: ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  /** [reduce f acc keyed] applies the accumulator function [f] to each key value pair in [keyed]
   *  with the specified seed value [acc], returning the final accumulated value.
   */

  let some: ('k => 'v => bool) => (t 'k 'v) => bool;
  /** [some f keyed] returns true if the predicate [f] returns true for
   *  any key value pair in [keyed], otherwise false. If [keyed] is empty, returns false.
   */

  let toCollection: (t 'k 'v) => (Collection.t ('k, 'v));
  /** [toCollection keyed] returns a Collection view of key value pairs in [keyed], using structural equality
   *  to equate values.
   */

  let toCollectionWith: (Equality.t 'v) => (t 'k 'v) => (Collection.t ('k, 'v));
  /** [toCollectionWith equals keyed] returns a Collection view of key value pairs in [keyed],
   *  using [equals] to equate values.
   */

  let toSeq: (t 'k 'v) => (Seq.t ('k, 'v));
  /** [toSeq keyed] returns a Seq of the key value pairs in [keyed]. */

  let tryFind: ('k => 'v => bool) => (t 'k 'v) => (option ('k, 'v));
  /** [find f keyed] returns the first key value pair for which the predicate [f] returns true or None. */

  let tryGet: 'k => (t 'k 'v) => (option 'v);
  /** [tryGet key keyed] returns the value associated with [key] or None */

  let values: (t 'k 'v) => (Seq.t 'v);
  /** [values keyed] returns a Seq of non-unique values in the Keyed collection. */
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

let module rec BiMap: {
  /** A hashed Keyed collection preserving the uniqueness of keys to values, and values to keys. */

  type t 'k 'v;
  /** The BiMap type. */

  let contains: 'k => 'v => (t 'k 'v) => bool;
  /** [contains key value bimap] returns true if [bimap] contains the [key] [value] pair.
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let containsKey: 'k => (t 'k 'v) => bool;
  /** [containsKey key bimap] returns true if [bimap] contains an entry with the key [key].
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let count: (t 'k 'v) => int;
  /** [count bimap] returns the number of key value pairs in [bimap].
   *
   *  Complexity: O(1)
   */

  let empty: (t 'k 'v);
  /** The empty BiMap. */

  let emptyWith: (HashStrategy.t 'k) => (HashStrategy.t 'v) => (t 'k 'v);
  /** [emptyWith keyStrategy valueStrategy] returns an empty BiMap using the provided
   *  key and value [HashStrategy]'s.
   */

  let equals: (t 'k 'v) => (t 'k 'v) => bool;
  /** [equals this that] equates [this] and [that] ensuring that the bimaps have the same count
   *  and contains the same key value pairs.
   */

  let every: ('k => 'v => bool) => (t 'k 'v) => bool;
  /** [every f bimap] returns true if the predicate [f] returns true for every
   *  key value pair in [bimap], otherwise false. If [bimap] is empty, returns true.
   */

  let find: ('k => 'v => bool) => (t 'k 'v) => ('k, 'v);
  /** [find f bimap] returns the first key value pair for which the predicate [f] returns true.
   *  If no value is found, an exception is thrown.
   */

  let fromSeq: (Seq.t ('k, 'v)) => (t 'k 'v);
  /** [fromSeq seq] returns a BiMap including the key value pairs in [seq]. If
   *  [seq] includes duplicate keys or values, the last key value pair with the duplicate
   *  key or value is added to the BiMap.
   */

  let fromSeqWith: (HashStrategy.t 'k) => (HashStrategy.t 'v) => (Seq.t ('k, 'v)) => (t 'k 'v);
  /** [fromSeqWith keyStrategy valueStrategy seq] returns a BiMap including the key value
   *  pairs in [seq] using the provided key and value [HashStrategy]'s. If [seq] includes duplicate
   *  keys or values, the last key value pair with the duplicate key or value is added to the BiMap.
   */

  let forEach: ('k => 'v => unit) => (t 'k 'v) => unit;
  /** [forEach f bimap] iterates through [bimap], invoking [f] for each key value pair. */

  let get: 'k => (t 'k 'v) => 'v;
  /** [get key bimap] returns the value associated with [key] or throws
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let hash: (Hash.t (t 'k 'v));
  /** [hash bimap] hashes [bimap] */

  let inverse: (t 'k 'v) => (t 'v 'k);
  /** [inverse bimap] returns a BiMap of value key pairs in [bimap]
   *
   *  Complexity: O(1)
   */

  let isEmpty: (t 'k 'v) => bool;
  /** [isEmpty bimap] returns true if [bimap] contains no key value pairs. */

  let isNotEmpty: (t 'k 'v) => bool;
  /** [isNotEmpty bimap] returns true if [bimap] contains at least one key value pair. */

  let keys: (t 'k 'v) => (Collection.t 'k);
  /** [keys bimap] returns a Collection view of keys in [bimap]. */

  let mutate: (t 'k 'v) => (TransientBiMap.t 'k 'v);
  /** [mutate bimap] returns a TransientBiMap containing the same key values pairs as [bimap].
   *
   *  Complexity: O(1)
   */

  let none: ('k => 'v => bool) => (t 'k 'v) => bool;
  /** [none f bimap] returns true if the predicate [f] returns false for
   *  every key value pair in [bimap], otherwise true. If [bimap] is empty, returns true.
   */

  let put: 'k => 'v => (t 'k 'v) => t 'k 'v;
  /** [put key value bimap] returns a new BiMap containing ([key], [value]). If [bimap] contains and existing
   *  mapping of [key] to a different value or, [value] to a different key, those mappings are removed.
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let putAll: (Seq.t ('k, 'v)) => (t 'k 'v) => (t 'k 'v);
  /** [putAll seq bimap] returns a new BiMap including the key value pairs in [seq].
   *  Key value pairs in seq replace existing mappings in [bimap].
   */

  let reduce: ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  /** [reduce f acc bimap] applies the accumulator function [f] to each key value pair in [bimap]
   *  with the specified seed value [acc], returning the final accumulated value.
   */

  let remove: 'k => (t 'k 'v) => (t 'k 'v);
  /** [remove key bimap] returns a new BiMap without any mapping from [key].
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let removeAll: (t 'k 'v) => (t 'k 'v);
  /** [removeAll bimap] returns an empty BiMap with the same key and value HashStrategy's as [bimap].
   *
   *  Complexity: O(1)
   */

  let removeValue: 'v => (t 'k 'v) => (t 'k 'v);
  /** [removeValue value bimap] returns a new BiMap without any mapping from [value]. */

  let some: ('k => 'v => bool) => (t 'k 'v) => bool;
  /** [some f bimap] returns true if the predicate [f] returns true for
   *  any key value pair in [bimap], otherwise false. If [bimap] is empty, returns false.
   */

  let toCollection: (t 'k 'v) => (Collection.t ('k, 'v));
  /** [toCollection bimap] returns a Collection view of the key value pairs in [bimap]. */

  let toKeyed: (t 'k 'v) => (Keyed.t 'k 'v);
  /** [toKeyed bimap] returns a Keyed collection view of [bimap]. */

  let toSeq: (t 'k 'v) => (Seq.t ('k, 'v));
  /** [toSeq bimap] returns a Seq of the key value pairs in [bimap]. */

  let tryFind: ('k => 'v => bool) => (t 'k 'v) => (option ('k, 'v));
  /** [tryFind f bimap] returns the first key value pair for which the predicate [f] returns true.
   *  If no value is found, an exception is thrown.
   */

  let tryGet: 'k => (t 'k 'v) => (option 'v);
  /** [tryGet key bimap] returns the value associated with [key] or None
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let tryPut: 'k => 'v => (t 'k 'v) => (t 'k 'v);
  /** [tryPut key value bimap] returns a new BiMap containing ([key], [value]). If [bimap] contains and existing
   *  mapping of [key] to a different value or, [value] to a different key, [bimap] is returned unchanged.
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let values: (t 'k 'v) => (Collection.t 'v);
  /** [values bimap] returns a Collection of unique values in the BiMap. */
}

and TransientBiMap: {
  /** A temporarily mutable hashed Keyed collection preserving the uniqueness of keys to values,
   *  and values to keys. Once persisted, any further operations on a TransientBiMap instance
   *  will throw. Intended for implementing bulk mutation operations efficiently.
   */

  type t 'k 'v;
  /** The TransientBiMap type. */

  let count: (t 'k 'v) => int;
  /** [count transient] returns the number of key value pairs in [transient].
   *
   *  Complexity: O(1)
   */

  let empty: unit => (t 'k 'v);
  /** [empty ()] returns a new empty TransientBiMap. */

  let emptyWith: (HashStrategy.t 'k) => (HashStrategy.t 'v) => (t 'k 'v);
  /** [emptyWith keyStrategy valueStrategy] returns an empty TransientBiMap using the provided
   *  key and value [HashStrategy]'s.
   */

  let isEmpty: t 'k 'v => bool;
  /** [isEmpty transient] returns true if [transient] contains no key value pairs. */

  let isNotEmpty: t 'k 'v => bool;
  /** [isNotEmpty transient] returns true if [transient] contains at least one key value pair. */

  let persist: (t 'k 'v) => (BiMap.t 'k 'v);
  /** [persist transient] returns a persisted BiMap. Further attempts to access or mutate [transient]
   *  will throw.
   */

  let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);
  /** [put key value transient] adds the mapping of [key] to [value] to [transient]. If [transient]
   *  contains and existing mapping of [key] to a different value or, [value] to a different key,
   *  those mappings are removed.
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let putAll: (Seq.t ('k, 'v)) => (t 'k 'v) => (t 'k 'v);
  /** [putAll seq transient] adds the key value pairs in [seq] to [transient].
   *  Key value pairs in seq replace existing mappings in [transient].
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

  let removeValue: 'v => (t 'k 'v) => (t 'k 'v);
  /** [removeValue value transient] removes from [transient] any mappings to [value]. */

  let tryGet: 'k => (t 'k 'v) => (option 'v);
  /** [tryGet key transient] returns the value associated with [key] or None
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let tryPut: 'k => 'v => (t 'k 'v) => (t 'k 'v);
  /** [tryPut key value bimap] attempts to add a mapping from [key] to [value]. If [transient]
   *  contains an existing mapping of [key] to a different value or, [value] to a different key,
   *  [transient] is unchanged.
   *
   *  Complexity: O(log32 N), effectively O(1)
   */
};

let module CopyOnWriteArray: {
  /** Opaque wrapper around an underlying array instance that provides copy on write semantics */

  type t 'a;
  /** The CopyOnWriteArray type. */

  let addFirst: 'a => (t 'a) => (t 'a);
  /** [addFirst value cow] returns a new CopyOnWriteArray with [value] added at index [0]
   *  and all other values shifted right.
   *
   *  Complexity: O(N)
   */

  let addFirstAll: (Seq.t 'a) => (t 'a) => (t 'a);
  /** [addFirstAll seq cow] returns a new CopyOnWriteArray with the values in [seq] prepended.
   *
   * Complexity: O(really expensive). don't use this, its for API equivalence to Vector.
   */

  let addLast: 'a => (t 'a) => (t 'a);
  /** [addLast value cow] returns a new CopyOnWriteArray with [value] added at index [count cow].
   *
   *  Complexity: O(N)
   */

  let addLastAll: (Seq.t 'a) => (t 'a) => (t 'a);
  /** [addLastAll seq cow] returns a new CopyOnWriteArray with the values in [seq] appended.
   *
   * Complexity: O(really expensive). don't use this, its for API equivalence to Vector.
   */

  let compare: (Comparator.t (t 'a));
  /** A comparator that compares two CopyOnWriteArrays
   *  using structural comparison to compare elements.
   */

  let compareWith: (Comparator.t 'a) => (Comparator.t (t 'a));
  /** [compareWith comparator] returns a Comparator that compares two CopyOnWriteArrays
   *  using [comparator] to compare elements.
   */

  let concat: (list (t 'a)) => (t 'a);
  /** [concat cows] returns a new CopyOnWriteArray by concatenating the CopyOnWriteArrays in [cows].
   *
   * Complexity: O(N * m)
   */

  let contains: 'a => (t 'a) => bool;
  /** [contains value cow] returns true if any element in [cow] is equal to [value]
   *  using structural equality, otherwise false.
   *
   *  Complexity: O(N)
   */

  let containsWith: (Equality.t 'a) => 'a => (t 'a) => bool;
  /** [containsWith equals cow] returns true if any element in [cow] is equal to [a]
   *  using the equality function [equals], otherwise false.
   *
   *  Complexity: O(N)
   */

  let count: (t 'a) => int;
  /** [count cow] returns the number of elements in [cow]. */

  let empty: (t 'a);
  /** The empty CopyOnWriteArray. */

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

  let every: ('a => bool) => (t 'a) => bool;
  /** [every f cow] returns true if the predicate [f] returns true for every
   *  element in [cow], otherwise false. If [cow] is empty, returns true.
   */

  let everyWithIndex: (int => 'a => bool) => (t 'a) => bool;
  /** [everyWithIndex f cow] returns true if the predicate [f] returns true for every
   *  index/element pair in [cow], otherwise false. If [cow] is empty, returns true.
   */

  let find: ('a => bool) => (t 'a) => 'a;
  /** [find f cow] returns the first value for which the predicate [f] returns true.
   *  If no value is found, an exception is thrown.
   */

  let findWithIndex: (int => 'a => bool) => (t 'a) => 'a;
  /** [findWithIndex f cow] returns the first index/element pair in [cow] for
   *  which the predicate [f] returns true. If no value is found, an exception is thrown.
   */

  let first: (t 'a) => 'a;
  /** [first cow] returns the first element in [cow] or throws. */

  let forEach: ('a => unit) => (t 'a) => unit;
  /** [forEach f cow] iterates through [cow], invoking [f] for each element. */

  let forEachReverse: ('a => unit) => (t 'a) => unit;
  /** [forEachReverse f cow] iterates through [cow] in reverser order,
   *  invoking [f] for each element.
   */

  let forEachWithIndex: (int => 'a => unit) => (t 'a) => unit;
  /** [forEachWithIndex f cow] iterates through [cow], invoking [f] on each index/element pair. */

  let forEachReverseWithIndex: (int => 'a => unit) => (t 'a) => unit;
  /** [forEachWithIndex f cow] iterates through [cow] in reverser order,
   *  invoking [f] on each index/element pair.
   */

  let fromSeq: (Seq.t 'a) => (t 'a);
  /** [fromSeq seq] returns a new CopyOnWriteArray containing the values in [seq].
   *
   * Complexity: O(really expensive). don't use this, its for API equivalence to Vector.
   */

  let fromSeqReversed: (Seq.t 'a) => (t 'a);
  /** [fromSeq seq] returns a new CopyOnWriteArray containing the values in [seq]
   *  in reverse order.
   *
   * Complexity: O(really expensive). don't use this, its for API equivalence to Vector.
   */

  let get: int => (t 'a) => 'a;
  /** [get index cow] returns the element at [index]. Throws if index is out of bounds. */

  let hash: (Hash.t (t 'a));
  /** [hash cow] hashes [cow], hashing elements using structural hashing. */

  let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
  /** [hashWith hash cow] hashes [cow], hashing elements using [hash]. */

  let indexOf: ('a => bool) => (t 'a) => int;
  /** [find f cow] returns the index of the first element for which the predicate [f] returns true.
   *  If no value is found, an exception is thrown.
   */

  let indexOfWithIndex: (int => 'a => bool) => (t 'a) => int;
  /** [indexOfWithIndex f cow] returns the index of the first index/element pair in [cow] for
   *  which the predicate [f] returns true. If no pair is found, an exception is thrown.
   */

  let init: int => (int => 'a) => (t 'a);
  /** [init n f] returns a new CopyOnWriteArray of length [n],
   *  with element number [i] initialized to the result of [f i].
   *  Throws if [n] is less than 0 or greater than max array length.
   */

  let insertAt: int => 'a => (t 'a) => (t 'a);
  /** [insertAt index value cow] inserts value into [cow] at the index [index].
   *
   *  Complexity: O(N)
   */

  let isEmpty: (t 'a) => bool;
  /** [isEmpty cow] returns true if [cow] contains no elements. */

  let isNotEmpty: (t 'a) => bool;
  /** [isNotEmpty cow] returns true if [cow] contains at least one element. */

  let last: (t 'a) => 'a;
  /** [last cow] returns the last element in [cow] or throws. */

  let map: ('a => 'b) => (t 'a) => (t 'b);
  /** [map f cow] returns a new CopyOnWriteArray applying the function [f] to each element in [cow]. */

  let mapWithIndex: (int => 'a => 'b) => (t 'a) => (t 'b);
  /** [map f cow] returns a new CopyOnWriteArray applying the
   *  function [f] to each index/element pair in [cow].
   */

  let mapReverse: ('a => 'b) => (t 'a) => (t 'b);
  /** [mapReverse f cow] returns a new CopyOnWriteArray applying the
   *  function [f] to each element in [cow], reversing the result.
   */

  let mapReverseWithIndex: (int => 'a => 'b) => (t 'a) => (t 'b);
  /** [mapReverseWithIndex f cow] returns a new CopyOnWriteArray applying the
   *  function [f] to each index/element pair in [cow], reversing the result.
   */

  let none: ('a => bool) => (t 'a) => bool;
  /** [none f cow] returns true if the predicate [f] returns false for every
   *  elements in [cow], otherwise true. If [cow] is empty, returns true.
   */

  let noneWithIndex: (int => 'a => bool) => (t 'a) => bool;
  /** [noneWithIndex f cow] returns true if the predicate [f] returns false for every
   *  index/element pair in [cow], otherwise true. If [cow] is empty, returns true.
   */

  let ofUnsafe: (array 'a) => (t 'a);
  /** [unsafe arr] returns a CopyOnWriteArray backed by [arr]. Note, it is the caller's
   *  responsibility to ensure that [arr] is not subsequently mutated.
   *
   *  Complexity: O(1)
   */

  let range: int => (option int) => (t 'a) => (t 'a);
  /** [range startIndex count cow] returns a new CopyOnWriteArray that is a range of
   *  elements in [cow] starting at [startIndex] and [count] elements. If [count] is None,
   *  all elements in [cow] after [startIndex] are returned.
   *
   *  Complexity: O(N)
   */

  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  /** [reduce f acc cow] applies the accumulator function [f] to each element in [cow]
   *  with the specified seed value [acc], returning the final accumulated value.
   */

  let reduceWithIndex: ('acc => int => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  /** [reduceWithIndex f acc cow] applies the accumulator function [f] to each
   *  index/element pair in [cow] with the specified seed value [acc], returning
   *  the final accumulated value.
   */

  let reduceRight: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  /** [reduceRight f acc cow] applies the accumulator function [f] to each element in [cow]
   *  in reverse order with the specified seed value [acc], returning the final accumulated value.
   */

  let reduceRightWithIndex: ('acc => int => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  /** [reduceRightWithIndex f acc cow] applies the accumulator function [f] to each
   *  index/element pair in [cow] in reverse order with the specified seed value [acc],
   *  returning the final accumulated value.
   */

  let removeAt: int => (t 'a) => (t 'a);
  /** [removeAt index cow] returns a new CopyOnWriteArray with the element at [index] removed.
   *
   *  Complexity: O(N)
   */

  let removeAll: (t 'a) => (t 'a);
  /** [removeAll cow] returns the empty CopyOnWriteArray.
   *
   *  Complexity: O(1)
   */

  let removeFirst: (t 'a) => (t 'a);
  /** [removeFirst cow] returns a new CopyOnWriteArray without the first element.
   *
   *  Complexity: O(N)
   */

  let removeLast: (t 'a) => (t 'a);
  /** [removeLast cow] returns a new CopyOnWriteArray without the last element.
   *
   *  Complexity: O(N)
   */

  let return: 'a => (t 'a);
  /** [return value] returns a new CopyOnWriteArray containing a single element, [value]. */

  let reverse: (t 'a) => (t 'a);
  /** [reverse cow] returns a new CopyOnWriteArray with [cow]'s elements reversed.
   *
   *  Complexity: O(N)
   */

  let skip: int => (t 'a) => (t 'a);
  /** [skip count cow] returns a new CopyOnWriteArray that removes the first [count] elements in [cow]. */

  let some: ('a => bool) => (t 'a) => bool;
  /** [some f cow] returns true if the predicate [f] returns true for any element in [cow], otherwise false.
   *  If [cow] is empty, returns false.
   */

  let someWithIndex: (int => 'a => bool) => (t 'a) => bool;
  /** [some f cow] returns true if the predicate [f] returns true for any index/element pair
   *  in [cow], otherwise false. If [cow] is empty, returns false.
   */

  let take: int => (t 'a) => (t 'a);
  /** [take count cow] returns a new CopyOnWriteArray that includes the first [count] elements in [cow]. */

  let toKeyed: (t 'a) => (Keyed.t int 'a);
  /** [toKeyed cow] returns a Keyed view of [cow] */

  let toSeq: (t 'a) => (Seq.t 'a);
  /** [toSeq cow] returns a Seq of the elements in [cow] in order. */

  let toSeqReversed: (t 'a) => (Seq.t 'a);
  /** [toSeqReversed cow] returns a Seq of the elements in [cow] in reverse order. */

  let tryFind: ('a => bool) => (t 'a) => (option 'a);
  /** [tryFind f cow] returns the first value for which the predicate [f] returns true or None. */

  let tryFindWithIndex: (int => 'a => bool) => (t 'a) => (option 'a);
  /** [tryFindWithIndex f cow] returns the first value for which the predicate [f] returns true or None. */

  let tryFirst: (t 'a) => option 'a;
  /** [tryFirst cow] returns first element in [cow] or None. */

  let tryGet: int => (t 'a) => (option 'a);
  /** [tryGet index cow] returns the element at [index] or None if [index] is out of bounds. */

  let tryIndexOf: ('a => bool) => (t 'a) => (option int);
  /** [tryIndexOf f cow] returns the index of the first element for which the predicate [f] returns true.
   *  If no value is found, returns None.
   */

  let tryIndexOfWithIndex: (int => 'a => bool) => (t 'a) => (option int);
  /** [indexOfWithIndex f cow] returns the index of the first index/element pair in [cow] for
   *  which the predicate [f] returns true. If no value is found, returns None.
   */

  let tryLast: (t 'a) => option 'a;
  /** [tryLast cow] returns the last element in [cow] or None. */

  let update: int => 'a => (t 'a) => (t 'a);
  /** [update index value cow] returns a new CopyOnWriteArray with [value]
   *  replacing the element at [index].
   *
   *  Complexity: O(N)
   */

  let updateAll: (int => 'a => 'a) => (t 'a) => (t 'a);
  /** [updateAll f cow] returns a new CopyOnWriteArray updating each element
   *  in [cow] with result of applying the function [f] to each index/element pair.
   *
   *  Complexity: O(N)
   */

  let updateWith: int => ('a => 'a) => (t 'a) => (t 'a);
  /** [updateWith index f cow] returns a new CopyOnWriteArray updating the element
   *  at [index] with the result of applying the function [f] to the element.
   *
   *  Complexity: O(N)
   */
};

let module rec Deque: {
  /** A double-ended queue with efficient appends [addLast], prepends [addFirst]
   *  and removals from either end of the queue [removeFirst] [removeLast].
   */

  type t 'a;
  /** The Deque type. */

  let addFirst: 'a => (t 'a) => (t 'a);
  /** [addFirst value deque] returns a new Deque with [value] prepended.
   *
   *  Complexity: O(1)
   */

  let addFirstAll: (Seq.t 'a) => (t 'a) => (t 'a);
  /** [addFirstAll seq deque] returns a new Deque with the values in [seq] prepended.
   *
   *  Complexity: O(N) the number of elements in [seq].
   */

  let addLast: 'a => (t 'a) => (t 'a);
  /** [addLast value deque] returns a new Deque with [value] appended.
   *
   *  Complexity: O(1)
   */

  let addLastAll: (Seq.t 'a) => (t 'a) => (t 'a);
  /** [addLastAll seq deque] returns a new Deque with the values in [seq] appended.
   *
   *  Complexity: O(N) the number of elements in [seq].
   */

  let compare: (Comparator.t (t 'a));
  /** A comparator that compares two Deques
   *  using structural comparison to compare elements.
   */

  let compareWith: (Comparator.t 'a) => (Comparator.t (t 'a));
  /** [compareWith comparator] returns a Comparator that compares two Deques
   *  using [comparator] to compare elements.
   */

  let contains: 'a => (t 'a) => bool;
  /** [contains value deque] returns true if any element in [deque] is equal to [value]
   *  using structural equality, otherwise false.
   *
   *  Complexity: O(N)
   */

  let containsWith: (Equality.t 'a) => 'a => (t 'a) => bool;
  /** [containsWith equals deque] returns true if any element in [deque] is equal to [a]
   *  using the equality function [equals], otherwise false.
   *
   *  Complexity: O(N)
   */

  let count: (t 'a) => int;
  /** [count deque] returns the number of elements in [deque]. */

  let empty: (t 'a);
  /** The empty Deque. */

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

  let every: ('a => bool) => (t 'a) => bool;
  /** [every f deque] returns true if the predicate [f] returns true for every
   *  element in [deque], otherwise false. If [deque] is empty, returns true.
   */

  let find: ('a => bool) => (t 'a) => 'a;
  /** [find f deque] returns the first value for which the predicate [f] returns true.
   *  If no value is found, an exception is thrown.
   */

  let first: (t 'a) => 'a;
  /** [first deque] returns the first element in [deque] or throws. */

  let forEach: ('a => unit) => (t 'a) => unit;
  /** [forEach f deque] iterates through [deque], invoking [f] for each element. */

  let forEachReverse: ('a => unit) => (t 'a) => unit;
  /** [forEachReverse f deque] iterates through [deque] in reverser order,
   *  invoking [f] for each element.
   */

  let fromSeq: (Seq.t 'a) => (t 'a);
  /** [fromSeq seq] returns a new Deque containing the values in [seq].
   *
   * Complexity: O(N) the number of elements in [seq].
   */

  let fromSeqReversed: (Seq.t 'a) => (t 'a);
  /** [fromSeq seq] returns a new Deque containing the values in [seq]
   *  in reverse order.
   *
   * Complexity: O(N) the number of elements in [seq].
   */

  let hash: (Hash.t (t 'a));
  /** [hash deque] hashes [deque], hashing elements using structural hashing. */

  let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
  /** [hashWith hash deque] hashes [deque], hashing elements using [hash]. */

  let isEmpty: (t 'a) => bool;
  /** [isEmpty deque] returns true if [deque] contains no elements. */

  let isNotEmpty: (t 'a) => bool;
  /** [isNotEmpty deque] returns true if [deque] contains at least one element. */

  let last: (t 'a) => 'a;
  /** [last deque] returns the last element in [deque] or throws. */

  let map: ('a => 'b) => (t 'a) => (t 'b);
  /** [map f deque] returns a new Deque applying the function [f] to each element in [deque]. */

  let mapReverse: ('a => 'b) => (t 'a) => (t 'b);
  /** [mapReverse f deque] returns a new Deque applying the
   *  function [f] to each element in [deque], reversing the result.
   */

  let mutate: (t 'a) => (TransientDeque.t 'a);
  /** [mutate deque] returns a TransientDeque containing the same elements as [deque].
   *
   *  Complexity: O(1)
   */

  let none: ('a => bool) => (t 'a) => bool;
  /** [none f deque] returns true if the predicate [f] returns false for every
   *  elements in [deque], otherwise true. If [deque] is empty, returns true.
   */

  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  /** [reduce f acc deque] applies the accumulator function [f] to each element in [deque]
   *  with the specified seed value [acc], returning the final accumulated value.
   */

  let reduceRight: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  /** [reduceRight f acc deque] applies the accumulator function [f] to each element in [deque]
   *  in reverse order with the specified seed value [acc], returning the final accumulated value.
   */

  let removeAll: (t 'a) => (t 'a);
  /** [removeAll cow] returns the empty Deque.
   *
   *  Complexity: O(1)
   */

  let removeFirst: (t 'a) => (t 'a);
  /** [removeFirst deque] returns a new Deque without the first element.
   *
   *  Complexity: O(1)
   */

  let removeLast: (t 'a) => (t 'a);
  /** [removeLast deque] returns a new Deque without the last element.
   *
   *  Complexity: O(1)
   */

  let return: 'a => (t 'a);
  /** [return value] returns a new Deque containing a single element, [value]. */

  let reverse: (t 'a) => (t 'a);
  /** [reverse deque] returns a new Deque with [deque]'s elements reversed.
   *
   *  Complexity: O(1)
   */

  let some: ('a => bool) => (t 'a) => bool;
  /** [some f deque] returns true if the predicate [f] returns true for any element in [deque], otherwise false.
   *  If [deque] is empty, returns false.
   */

  let toSeq: (t 'a) => (Seq.t 'a);
  /** [toSeq deque] returns a Seq of the elements in [deque] in order. */

  let toSeqReversed: (t 'a) => (Seq.t 'a);
  /** [toSeqReversed deque] returns a Seq of the elements in [deque] in reverse order. */

  let tryFind: ('a => bool) => (t 'a) => (option 'a);
  /** [tryFind f deque] returns the first value for which the predicate [f] returns true or None. */

  let tryFirst: (t 'a) => option 'a;
  /** [tryFirst deque] returns first element in [deque] or None. */

  let tryLast: (t 'a) => option 'a;
  /** [tryLast deque] returns the last element in [deque] or None. */
}

and TransientDeque: {
  /** A temporarily mutable double-ended queue. Once persisted, any further operations on a
   *  TransientDeque instance will throw. Intended for implementing bulk mutation operations efficiently.
   */

  type t 'a;
  /** The TransientDeque type. */

  let addFirst: 'a => (t 'a) => (t 'a);
  /** [addFirst value transient] prepends [value] to [transient].
   *
   *  Complexity: O(1)
   */

  let addLast: 'a => (t 'a) => (t 'a);
  /** [addLast value transient] appends [value] to [transient].
   *
   *  Complexity: O(1)
   */

  let count: (t 'a) => int;
  /** [count transient] returns the number of elements in [transient]. */

  let empty: unit => (t 'a);
  /** [empty ()] returns a new empty TransientDeque. */

  let first: (t 'a) => 'a;
  /** [first transient] returns the first element in [transient] or throws. */

  let isEmpty: (t 'a) => bool;
  /** [isEmpty transient] returns true if [transient] contains no elements. */

  let isNotEmpty: (t 'a) => bool;
  /** [isNotEmpty transient] returns true if [transient] contains at least one element. */

  let last: (t 'a) => 'a;
  /** [last transient] returns the last element in [transient] or throws. */

  let persist: (t 'a) => (Deque.t 'a);
  /** [persist transient] returns a persisted Deque. Further attempts to access or mutate [transient]
   *  will throw.
   */

  let removeAll: (t 'a) => (t 'a);
  /** [removeAll transient] removes all elements from [transient].
   *
   *  Complexity: O(1)
   */

  let removeFirst: (t 'a) => (t 'a);
  /** [removeFirst transient] removes the first element from [transient].
   *
   *  Complexity: O(1)
   */

  let removeLast: (t 'a) => (t 'a);
  /** [removeLast transient] removes the last element from [transient].
   *
   *  Complexity: O(1)
   */

  let reverse: (t 'a) => (t 'a);
  /** [reverse transient] reverse [transient]'s elements.
   *
   *  Complexity: O(1)
   */

  let tryFirst: (t 'a) => option 'a;
  /** [tryFirst transient] returns first element in [transient] or None. */

  let tryLast: (t 'a) => option 'a;
  /** [tryLast transient] returns the last element in [transient] or None. */
};

let module rec HashMap: {
  type t 'k 'v;

  let alter: 'k => (option 'v => option 'v) => (t 'k 'v) => (t 'k 'v);
  let contains: 'k => 'v => (t 'k 'v) => bool;
  let containsWith: (Equality.t 'v) => 'k => 'v => (t 'k 'v) => bool;
  let containsKey: 'k => (t 'k 'v) => bool;
  let count: (t 'k 'v) => int;
  let empty: (t 'k 'v);
  let emptyWith: (HashStrategy.t 'k) => (t 'k 'v);
  let equals: (t 'k 'v) => (t 'k 'v) => bool;
  let equalsWith: (Equality.t 'v) => (t 'k 'v) => (t 'k 'v) => bool;
  let every: ('k => 'v => bool) => (t 'k 'v) => bool;
  let find: ('k => 'v => bool) => (t 'k 'v) => ('k, 'v);
  let forEach: ('k => 'v => unit) => (t 'k 'v) => unit;
  let fromKeyed: (Keyed.t 'k 'v) => (t 'k 'v);
  let fromKeyedWith: (HashStrategy.t 'k) => (Keyed.t 'k 'v) => (t 'k 'v);
  let fromSeq: (Seq.t ('k, 'v)) => (t 'k 'v);
  let fromSeqWith: (HashStrategy.t 'k) => (Seq.t ('k, 'v)) => (t 'k 'v);
  let get: 'k => (t 'k 'v) => 'v;
  let hash: (Hash.t (t 'k 'v));
  let hashWith: (Hash.t 'v) => (Hash.t (t 'k 'v));
  let keys: (t 'k 'v) => (Collection.t 'k);
  let isEmpty: t 'k 'v => bool;
  let isNotEmpty: t 'k 'v => bool;
  let map: ('k => 'a => 'b) => (t 'k 'a) => (t 'k 'b);
  let merge: ('k => (option 'vAcc) => (option 'v) => (option 'vAcc)) => (Keyed.t 'k 'v) => (t 'k 'vAcc)  => (t 'k 'vAcc);
  let mutate: (t 'k 'v) => (TransientHashMap.t 'k 'v);
  let none: ('k => 'v => bool) => (t 'k 'v) => bool;
  let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);
  let putAll: (Seq.t ('k, 'v)) => (t 'k 'v) => (t 'k 'v);
  let reduce: ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let remove: 'k => (t 'k 'v) => (t 'k 'v);
  let removeAll: (t 'k 'v) => (t 'k 'v);
  let some: ('k => 'v => bool) => (t 'k 'v) => bool;
  let toCollection: (t 'k 'v) => (Collection.t ('k, 'v));
  let toCollectionWith: (Equality.t 'v) => (t 'k 'v) => (Collection.t ('k, 'v));
  let toKeyed: (t 'k 'v) => (Keyed.t 'k 'v);
  let toSeq: (t 'k 'v) => (Seq.t ('k, 'v));
  let tryFind: ('k => 'v => bool) => (t 'k 'v) => (option ('k, 'v));
  let tryGet: 'k => (t 'k 'v) => (option 'v);
  let values: (t 'k 'v) => (Seq.t 'v);
}

and TransientHashMap: {
  type t 'k 'v;

  let alter: 'k => (option 'v => option 'v) => (t 'k 'v) => (t 'k 'v);
  let count: (t 'k 'v) => int;
  let empty: unit => (t 'k 'v);
  let emptyWith: (HashStrategy.t 'k) => (t 'k 'v);
  let isEmpty: (t 'k 'v) => bool;
  let isNotEmpty: (t 'k 'v) => bool;
  let persist: (t 'k 'v) => (HashMap.t 'k 'v);
  let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);
  let putAll: (Seq.t ('k, 'v)) => (t 'k 'v) => (t 'k 'v);
  let remove: 'k => (t 'k 'v) => (t 'k 'v);
  let removeAll: (t 'k 'v) => (t 'k 'v);
  let tryGet: 'k => (t 'k 'v) => (option 'v);
};

let module rec HashMultiset: {
  type t 'a;

  let add: 'a => (t 'a) => (t 'a);
  let addAll: (Seq.t 'a) => (t 'a) => (t 'a);
  let contains: 'a => (t 'a) => bool;
  let count: (t 'a) => int;
  let empty: (t 'a);
  let emptyWith: (HashStrategy.t 'a) => (t 'a);
  let equals: (t 'a) => (t 'a) => bool;
  let every: ('a => int => bool) => (t 'a) => bool;
  let find: ('a => int => bool) => (t 'a) => ('a, int);
  let forEach: ('a => int => unit) => (t 'a) => unit;
  let fromSeq: (Seq.t 'a) => (t 'a);
  let fromSeqWith: (HashStrategy.t 'a) => (Seq.t 'a) => (t 'a);
  let get: 'a => (t 'a) => int;
  let hash: (Hash.t (t 'a));
  let isEmpty: (t 'a) => bool;
  let isNotEmpty: (t 'a) => bool;
  let mutate: (t 'a) => (TransientHashMultiset.t 'a);
  let none: ('a => int => bool) => (t 'a) => bool;
  let reduce: ('acc => 'a => int => 'acc) => 'acc => (t 'a) => 'acc;
  let remove: 'a => (t 'a) => (t 'a);
  let removeAll: (t 'a) => (t 'a);
  let set: 'a => int => (t 'a) => (t 'a);
  let some: ('a => int => bool) => (t 'a) => bool;
  let toKeyed: (t 'a) => (Keyed.t 'a int);
  let toSeq: (t 'a) => (Seq.t 'a);
  let tryFind: ('a => int => bool) => (t 'a) => (option ('a, int));
  let values: (t 'a) => (Collection.t 'a);
}

and TransientHashMultiset: {
  type t 'a;

  let add: 'a => (t 'a) => (t 'a);
  let addAll: (Seq.t 'a) => (t 'a) => (t 'a);
  let contains: 'a => (t 'a) => bool;
  let count: (t 'a) => int;
  let empty: unit => (t 'a);
  let emptyWith: (HashStrategy.t 'a) => (t 'a);
  let get: 'a => (t 'a) => int;
  let isEmpty: (t 'a) => bool;
  let isNotEmpty: (t 'a) => bool;
  let persist: (t 'a) => (HashMultiset.t 'a);
  let remove: 'a => (t 'a) => (t 'a);
  let removeAll: (t 'a) => (t 'a);
  let set: 'a => int => (t 'a) => (t 'a);
};

let module rec HashSet: {
  type t 'a;

  let add: 'a => (t 'a) => (t 'a);
  let addAll: (Seq.t 'a) => (t 'a) => (t 'a);
  let contains: 'a => (t 'a) => bool;
  let count: (t 'a) => int;
  let empty: (t 'a);
  let emptyWith: (HashStrategy.t 'a) => (t 'a);
  let equals: (t 'a) => (t 'a) => bool;
  let every: ('a => bool) => (t 'a) => bool;
  let find: ('a => bool) => (t 'a) => 'a;
  let forEach: ('a => unit) => (t 'a) => unit;
  let fromSeq: (Seq.t 'a) => (t 'a);
  let fromSeqWith: (HashStrategy.t 'a)  => (Seq.t 'a) => (t 'a);
  let hash: (Hash.t (t 'a));
  let intersect: (t 'a) => (t 'a) => (t 'a);
  let isEmpty: (t 'a) => bool;
  let isNotEmpty: (t 'a) => bool;
  let mutate: (t 'a) => (TransientHashSet.t 'a);
  let none: ('a => bool) => (t 'a) => bool;
  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let remove: 'a => (t 'a) => (t 'a);
  let removeAll: (t 'a) => (t 'a);
  let some: ('a => bool) => (t 'a) => bool;
  let subtract: (t 'a) => (t 'a) => (t 'a);
  let toCollection: (t 'a) => (Collection.t 'a);
  let toKeyed: (t 'a) => (Keyed.t 'a 'a);
  let toSeq: (t 'a) => (Seq.t 'a);
  let tryFind: ('a => bool) => (t 'a) => (option 'a);
  let union: (t 'a) => (t 'a) => (t 'a);
}

and TransientHashSet: {
  type t 'a;

  let add: 'a => (t 'a) => (t 'a);
  let addAll: (Seq.t 'a) => (t 'a) => (t 'a);
  let contains: 'a => (t 'a) => bool;
  let count: (t 'a) => int;
  let empty: unit => (t 'a);
  let emptyWith: (HashStrategy.t 'a) => (t 'a);
  let isEmpty: (t 'a) => bool;
  let isNotEmpty: (t 'a) => bool;
  let persist: (t 'a) => (HashSet.t 'a);
  let remove: 'a => (t 'a) => (t 'a);
  let removeAll: (t 'a) => (t 'a);
};

let module HashSetMultimap: {
  type t 'k 'v;

  let contains: 'k => 'v => (t 'k 'v) => bool;
  let containsKey: 'k => (t 'k 'v) => bool;
  let count: (t 'k 'v) => int;
  let empty: (t 'k 'v);
  let emptyWith: (HashStrategy.t 'k) => (HashStrategy.t 'v)  => (t 'k 'v);
  let equals: (t 'k 'v) => (t 'k 'v) => bool;
  let every: ('k => 'v => bool) => (t 'k 'v) => bool;
  let find: ('k => 'v => bool) => (t 'k 'v) => ('k, 'v);
  let forEach: ('k => 'v => unit) => (t 'k 'v) => unit;
  let get: 'k => (t 'k 'v) => (HashSet.t 'v);
  let hash: (Hash.t (t 'k 'v));
  let isEmpty: (t 'k 'v) => bool;
  let isNotEmpty: (t 'k 'v) => bool;
  let keys: (t 'k 'v) => (Collection.t 'k);
  let none: ('k => 'v => bool) => (t 'k 'v) => bool;
  let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);
  let putAllValues: 'k => (Seq.t 'v) => (t 'k 'v) => (t 'k 'v);
  let reduce: ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let remove: 'k => (t 'k 'v) => (t 'k 'v);
  let removeAll: (t 'k 'v) => (t 'k 'v);
  let some: ('k => 'v => bool) => (t 'k 'v) => bool;
  let toCollection: (t 'k 'v) => (Collection.t ('k, 'v));
  let toSeq: (t 'k 'v) => (Seq.t ('k, 'v));
  let tryFind: ('k => 'v => bool) => (t 'k 'v) => (option ('k, 'v));
  let values: (t 'k 'v) => (Seq.t 'v);
};

let module rec IntMap: {
  type t 'a;

  let alter: int => ((option 'a) => (option 'a)) => (t 'a) => (t 'a);
  let contains: int => 'a => (t 'a) => bool;
  let containsWith: (Equality.t 'a) => int => 'a => (t 'a) => bool;
  let containsKey: int => (t 'a) => bool;
  let count: (t 'a) => int;
  let empty: (t 'a);
  let equals: (t 'a) => (t 'a) => bool;
  let equalsWith: (Equality.t 'a) => (t 'a) => (t 'a) => bool;
  let every: (int => 'a => bool) => (t 'a) => bool;
  let find: (int => 'a => bool) => (t 'a) => (int, 'a);
  let forEach: (int => 'a => unit) => (t 'a) => unit;
  let fromKeyed: (Keyed.t int 'a) => (t 'a);
  let fromSeq: (Seq.t (int, 'a)) => (t 'a);
  let get: int => (t 'a) => 'a;
  let hash: (Hash.t (t 'a));
  let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
  let isEmpty: (t 'a) => bool;
  let isNotEmpty: (t 'a) => bool;
  let keys: (t 'a) => (Collection.t int);
  let map: (int => 'a => 'b) => (t 'a) => (t 'b);
  let merge: (int => (option 'vAcc) => (option 'v) => (option 'vAcc)) => (Keyed.t int 'v) => (t 'vAcc)  => (t 'vAcc);
  let mutate: (t 'a) => (TransientIntMap.t 'a);
  let none: (int => 'a => bool) => (t 'a) => bool;
  let put: int => 'a => (t 'a) => (t 'a);
  let putAll: (Seq.t (int, 'a)) => (t 'a) => (t 'a);
  let reduce: ('acc => int => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let remove: int => (t 'a) => (t 'a);
  let removeAll: (t 'a) => (t 'a);
  let some: (int => 'a => bool) => (t 'a) => bool;
  let toCollection: (t 'a) => (Collection.t (int, 'a));
  let toCollectionWith: (Equality.t 'a) => (t 'a) => (Collection.t (int, 'a));
  let toKeyed: (t 'a) => (Keyed.t int 'a);
  let toSeq: (t 'a) => (Seq.t ((int, 'a)));
  let tryFind: (int => 'a => bool) => (t 'a) => (option (int, 'a));
  let tryGet: int => (t 'a) => (option 'a);
  let values: (t 'a) => (Seq.t 'a);
}

and TransientIntMap: {
  type t 'a;

  let alter: int => ((option 'a) => (option 'a)) => (t 'a) => (t 'a);
  let count: (t 'a) => int;
  let empty: unit => (t 'a);
  let isEmpty: (t 'a) => bool;
  let isNotEmpty: (t 'a) => bool;
  let persist: (t 'a) => (IntMap.t 'a);
  let put: int => 'a => (t 'a) => (t 'a);
  let putAll: (Seq.t (int, 'a)) => (t 'a) => (t 'a);
  let remove: int => (t 'a) => (t 'a);
  let removeAll: (t 'a) => (t 'a);
  let tryGet: int => (t 'a) => (option 'a);
};

let module rec IntSet: {
  type t;

  let add: int => t => t;
  let addAll: (Seq.t int) => t => t;
  let contains: int => t => bool;
  let count: t => int;
  let empty: t;
  let equals: t => t => bool;
  let every: (int => bool) => t => bool;
  let find: (int => bool) => t => int;
  let forEach: (int => unit) => t => unit;
  let fromSeq: (Seq.t int) => t;
  let hash: (Hash.t t);
  let intersect: t => t => t;
  let isEmpty: t => bool;
  let isNotEmpty: t => bool;
  let mutate: t => TransientIntSet.t;
  let none: (int => bool) => t => bool;
  let reduce: ('acc => int => 'acc) => 'acc => t => 'acc;
  let remove: int => t => t;
  let removeAll: t => t;
  let some: (int => bool) => t => bool;
  let subtract: t => t => t;
  let toCollection: t => (Collection.t int);
  let toKeyed: t => (Keyed.t int int);
  let toSeq: t => (Seq.t int);
  let tryFind: (int => bool) => t => (option int);
  let union: t => t => t;
}

and TransientIntSet: {
  type t;

  let add: int => t => t;
  let addAll: (Seq.t int) => t => t;
  let contains: int => t => bool;
  let count: t => int;
  let empty: unit => t;
  let isEmpty: t => bool;
  let isNotEmpty: t => bool;
  let persist: t => IntSet.t;
  let remove: int => t => t;
  let removeAll: t => t;
};

let module List: {
  /** OCaml singly-linked list */

  type t 'a = list 'a;
  /** The List type. */

  let addFirst: 'a => (t 'a) => (t 'a);
  /** [addFirst value list] returns a new List with [value] prepended.
   *
   *  Complexity: O(1)
   */

  let addFirstAll: (Seq.t 'a) => (t 'a) => (t 'a);
  /** [addFirstAll seq list] returns a new List with the values in [seq] prepended.
   *
   *  Complexity: O(N) the number of elements in [seq].
   */

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

  let count: (t 'a) => int;
  /** [count list] returns the number of elements in [list].
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

  let every: ('a => bool) => (t 'a) => bool;
  /** [every f list] returns true if the predicate [f] returns true for every
   *  element in [list], otherwise false. If [list] is empty, returns true.
   */

  let find: ('a => bool) => (t 'a) => 'a;
  /** [find f list] returns the first value for which the predicate [f] returns true.
   *  If no value is found, an exception is thrown.
   */

  let first: (t 'a) => 'a;
  /** [first list] returns the first element in [list] or throws. */

  let forEach: ('a => unit) => (t 'a) => unit;
  /** [forEach f list] iterates through [list], invoking [f] for each element. */

  let fromSeqReversed: (Seq.t 'a) => (t 'a);
  /** [fromSeq seq] returns a new List containing the values in [seq]
   *  in reverse order.
   *
   * Complexity: O(N) the number of elements in [seq].
   */

  let hash: (Hash.t (t 'a));
  /** [hash list] hashes [list], hashing elements using structural hashing. */

  let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
  /** [hashWith hash list] hashes [list], hashing elements using [hash]. */

  let isEmpty: (t 'a) => bool;
  /** [isEmpty list] returns true if [list] contains no elements. */

  let isNotEmpty: (t 'a) => bool;
  /** [isNotEmpty list] returns true if [list] contains at least one element. */

  let mapReverse: ('a => 'b) => (t 'a) => (t 'b);
  /** [mapReverse f list] returns a new List applying the
   *  function [f] to each element in [list], reversing the result.
   */

  let none: ('a => bool) => (t 'a) => bool;
  /** [none f list] returns true if the predicate [f] returns false for every
   *  elements in [list], otherwise true. If [list] is empty, returns true.
   */

  let reduce: ('acc => 'a => 'acc ) => 'acc => (t 'a) => 'acc;
  /** [reduce f acc list] applies the accumulator function [f] to each element in [list]
   *  with the specified seed value [acc], returning the final accumulated value.
   */

  let removeAll: (t 'a) => (t 'a);
  /** [removeAll list] returns the empty List.
   *
   *  Complexity: O(1)
   */

  let removeFirst: (t 'a) => (t 'a);
  /** [removeFirst list] returns a new List without the first element.
   *
   *  Complexity: O(1)
   */

  let return: 'a => (t 'a);
  /** [return value] returns a new List containing a single element, [value]. */

  let reverse: (t 'a) => (t 'a);
  /** [reverse list] returns a new List with [list]'s elements reversed.
   *
   *  Complexity: O(N)
   */

  let some: ('a => bool) => (t 'a) => bool;
  /** [some f list] returns true if the predicate [f] returns true for any element in [list], otherwise false.
   *  If [list] is empty, returns false.
   */

  let toSeq: (t 'a) => (Seq.t 'a);
  /** [toSeq list] returns a Seq of the elements in [list] in order. */

  let tryFind: ('a => bool) => (t 'a) => (option 'a);
  /** [tryFind f list] returns the first value for which the predicate [f] returns true or None. */

  let tryFirst: (t 'a) => (option 'a);
  /** [tryFirst list] returns first element in [list] or None. */
};

let module Option: {
  /** OCaml option type. Can be considered a collection of zero or one elements.
   *  All operations have a complexity of O(1).
   */

  type t 'a = option 'a;
  /** The Option type. */

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

  let count: (t 'a) => int;
  /** [count option] returns the number of elements in [option]. */

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

  let every: ('a => bool) => (t 'a) => bool;
  /** [every f option] returns true if the predicate [f] returns true for every
   *  element in [option], otherwise false. If [option] is empty, returns true.
   */

  let filter: ('a => bool) => (t 'a) => (t 'a);
  /** [filter f option] returns an option that only includes elements that satisfy the predicate [f]. */


  let find: ('a => bool) => (t 'a) => 'a;
  /** [find f option] returns the first value for which the predicate [f] returns true.
   *  If no value is found, an exception is thrown.
   */

  let first: (t 'a) => 'a;
  /** [first option] returns the first element in [option] or throws. */

  let flatMap: ('a => t 'b) => (t 'a) => (t 'b);
  /** [flatMap f option] returns the result of applying f if [option] is Some, or None. */

  let flatten: (t (t 'a)) => (t 'a);
  /** [flatten option] unboxes a boxed Option */

  let forEach: ('a => unit) => (t 'a) => unit;
  /** [forEach f option] iterates through [option], invoking [f] for each element. */

  let hash: (Hash.t (t 'a));
  /** [hash option] hashes [option], hashing elements using structural hashing. */

  let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
  /** [hashWith hash option] hashes [option], hashing elements using [hash]. */

  let isEmpty: (t 'a) => bool;
  /** [isEmpty option] returns true if [option] contains no elements. */

  let isNotEmpty: (t 'a) => bool;
  /** [isNotEmpty option] returns true if [option] contains at least one element. */

  let last: (t 'a) => 'a;
  /** [last option] returns the last element in [option] or throws. */

  let map: ('a => 'b) => (t 'a) => (t 'b);
  /** [map f option] returns the result of applying f if [option] is Some,
   *  boxing the result in an Option, or None.
   */

  let none: ('a => bool) => (t 'a) => bool;
  /** [none f option] returns true if the predicate [f] returns false for every
   *  elements in [option], otherwise true. If [option] is empty, returns true.
   */

  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  /** [reduce f acc option] applies the accumulator function [f] to each element in [option]
   *  with the specified seed value [acc], returning the final accumulated value.
   */

  let return: 'a => (t 'a);
  /** [return value] returns [Some value]. */

  let some: ('a => bool) => (t 'a) => bool;
  /** [some f option] returns true if the predicate [f] returns true for any element in [option], otherwise false.
   *  If [option] is empty, returns false.
   */

  let toCollection: (option 'a) => (Collection.t 'a);
  /* [toCollection option] returns a Collection view of the option using structural equality equate values. */

  let toCollectionWith: (Equality.t 'a) => (option 'a) => (Collection.t 'a);
  /* [toCollectionWith equals option] returns a Collection view of the option using [equals] equate values. */

  let toSeq: (t 'a) => (Seq.t 'a);
  /* [toSeq option] returns a Seq of the values in [option]. */

  let tryFind: ('a => bool) => (t 'a) => (option 'a);
  /** [find f option] returns the first value for which the predicate [f] returns true or None. */

  let tryFirst: (t 'a) => (option 'a);
  /** [tryFirst option] returns first element in [option] or None. */

  let tryLast: (t 'a) => (option 'a);
  /** [tryLast option] returns the last element in [option] or None. */
};

let module SortedMap: {
  type t 'k 'v;

  let alter: 'k => (option 'v => option 'v) => (t 'k 'v) => (t 'k 'v);
  let compare: (Comparator.t (t 'k 'v));
  let compareWith: (Comparator.t 'v) => (Comparator.t (t 'k 'v));
  let contains: 'k => 'v => (t 'k 'v) => bool;
  let containsWith: (Equality.t 'v) => 'k => 'v => (t 'k 'v) => bool;
  let containsKey: 'k => (t 'k 'v) => bool;
  let count: (t 'k 'v) => int;
  let empty: (t 'k 'v);
  let emptyWith: (Comparator.t 'k) => (t 'k 'v);
  let equals: (t 'k 'v) => (t 'k 'v) => bool;
  let equalsWith: (Equality.t 'v) => (t 'k 'v) => (t 'k 'v) => bool;
  let every: ('k => 'v => bool) => (t 'k 'v) => bool;
  let find: ('k => 'v => bool) => (t 'k 'v) => ('k, 'v);
  let first: (t 'k 'v) => ('k, 'v);
  let forEach: ('k => 'v => unit) => (t 'k 'v) => unit;
  let fromKeyed: (Keyed.t 'k 'v) => (t 'k 'v);
  let fromKeyedWith: (Comparator.t 'k) => (Keyed.t 'k 'v) => (t 'k 'v);
  let fromSeq: (Seq.t ('k, 'v)) => (t 'k 'v);
  let fromSeqWith: (Comparator.t 'k) => (Seq.t ('k, 'v)) => (t 'k 'v);
  let get: 'k => (t 'k 'v) => 'v;
  let hash: (Hash.t (t 'k 'v));
  let hashWith: (Hash.t 'k) => (Hash.t 'v) => (Hash.t (t 'k 'v));
  let isEmpty: t 'k 'v => bool;
  let isNotEmpty: t 'k 'v => bool;
  let keys: (t 'k 'v) => (Collection.t 'k);
  let last: (t 'k 'v) => ('k, 'v);
  let map: ('k => 'a => 'b) => (t 'k 'a) => (t 'k 'b);
  let merge: ('k => (option 'vAcc) => (option 'v) => (option 'vAcc)) => (Keyed.t 'k 'v) => (t 'k 'vAcc)  => (t 'k 'vAcc);
  let none: ('k => 'v => bool) => (t 'k 'v) => bool;
  let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);
  let putAll: (Seq.t ('k, 'v)) => (t 'k 'v) => (t 'k 'v);
  let reduce: ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceRight: ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let remove: 'k => (t 'k 'v) => (t 'k 'v);
  let removeAll: (t 'k 'v) => (t 'k 'v);
  let removeFirst: (t 'k 'v) => (t 'k 'v);
  let removeLast: (t 'k 'v) => (t 'k 'v);
  let some: ('k => 'v => bool) => (t 'k 'v) => bool;
  let toCollection: (t 'k 'v) => (Collection.t ('k, 'v));
  let toCollectionWith: (Equality.t 'v) => (t 'k 'v) => (Collection.t ('k, 'v));
  let toKeyed: (t 'k 'v) => (Keyed.t 'k 'v);
  let toSeq: (t 'k 'v) => (Seq.t ('k, 'v));
  let tryFind: ('k => 'v => bool) => (t 'k 'v) => (option ('k, 'v));
  let tryFirst: (t 'k 'v) => (option ('k, 'v));
  let tryLast: (t 'k 'v) => (option ('k, 'v));
  let tryGet: 'k => (t 'k 'v) => (option 'v);
  let values: (t 'k 'v) => (Seq.t 'v);
};

let module SortedSet: {
  type t 'a;

  let add: 'a => (t 'a) => (t 'a);
  let addAll: (Seq.t 'a) => (t 'a) => (t 'a);
  let compare: (Comparator.t (t 'a));
  let contains: 'a => (t 'a) => bool;
  let count: (t 'a) => int;
  let empty: (t 'a);
  let emptyWith: (Comparator.t 'a) => (t 'a);
  let equals: (t 'a) => (t 'a) => bool;
  let every: ('a => bool) => (t 'a) => bool;
  let find: ('a => bool) => (t 'a) => 'a;
  let first: (t 'a) => 'a;
  let forEach: ('a => unit) => (t 'a) => unit;
  let fromSeq: (Seq.t 'a) => (t 'a);
  let fromSeqWith: (Comparator.t 'a)  => (Seq.t 'a) => (t 'a);
  let hash: (Hash.t (t 'a));
  let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
  let intersect: (t 'a) => (t 'a) => (t 'a);
  let isEmpty: (t 'a) => bool;
  let isNotEmpty: (t 'a) => bool;
  let last: (t 'a) => 'a;
  let none: ('a => bool) => (t 'a) => bool;
  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let reduceRight: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let remove: 'a => (t 'a) => (t 'a);
  let removeAll: (t 'a) => (t 'a);
  let removeFirst: (t 'a) => (t 'a);
  let removeLast: (t 'a) => (t 'a);
  let search: ('a => Ordering.t) => (t 'a) => 'a;
  let some: ('a => bool) => (t 'a) => bool;
  let subtract: (t 'a) => (t 'a) => (t 'a);
  let toCollection: (t 'a) => (Collection.t 'a);
  let toKeyed: (t 'a) => (Keyed.t 'a 'a);
  let toSeq: (t 'a) => (Seq.t 'a);
  let tryFind: ('a => bool) => (t 'a) => (option 'a);
  let tryFirst: (t 'a) => (option 'a);
  let tryLast: (t 'a) => (option 'a);
  let trySearch: ('a => Ordering.t) => (t 'a) => (option 'a);
  let union: (t 'a) => (t 'a) => (t 'a);
};

let module Stack: {
  /** A singly-linked stack with an O(1) count operation. */
  type t 'a;

  let addFirst: 'a => (t 'a) => (t 'a);
  /** [addFirst value stack] returns a new Stack with [value] prepended.
   *
   *  Complexity: O(1)
   */

  let addFirstAll: (Seq.t 'a) => (t 'a) => (t 'a);
  /** [addFirstAll seq stack] returns a new Stack with the values in [seq] prepended.
   *
   *  Complexity: O(N) the number of elements in [seq].
   */

  let compare: (Comparator.t (t 'a));
  /** A comparator that compares two Stacks
   *  using structural comparison to compare elements.
   */

  let compareWith: (Comparator.t 'a) => (Comparator.t (t 'a));
  /** [compareWith comparator] returns a Comparator that compares two Stacks
   *  using [comparator] to compare elements.
   */

  let contains: 'a => (t 'a) => bool;
  /** [contains value stack] returns true if any element in [stack] is equal to [value]
   *  using structural equality, otherwise false.
   *
   *  Complexity: O(N)
   */

  let containsWith: (Equality.t 'a) => 'a => (t 'a) => bool;
  /** [containsWith equals stack] returns true if any element in [stack] is equal to [a]
   *  using the equality function [equals], otherwise false.
   *
   *  Complexity: O(N)
   */

  let count: (t 'a) => int;
  /** [count stack] returns the number of elements in [stack].
   *
   *  Complexity: O(1)
   */

  let empty: (t 'a);
  /** The empty Stack. */

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

  let every: ('a => bool) => (t 'a) => bool;
  /** [every f stack] returns true if the predicate [f] returns true for every
   *  element in [stack], otherwise false. If [stack] is empty, returns true.
   */

  let find: ('a => bool) => (t 'a) => 'a;
  /** [find f stack] returns the first value for which the predicate [f] returns true.
   *  If no value is found, an exception is thrown.
   */

  let first: (t 'a) => 'a;
  /** [first stack] returns the first element in [stack] or throws. */

  let forEach: ('a => unit) => (t 'a) => unit;
  /** [forEach f stack] iterates through [stack], invoking [f] for each element. */

  let fromList: (list 'a) => (t 'a);
  /** [fromList list] returns a Stack backed by [list].
   *
   *  Complexity: O(N)
   */

  let fromSeqReversed: (Seq.t 'a) => (t 'a);
  /** [fromSeq seq] returns a new Stack containing the values in [seq]
   *  in reverse order.
   *
   * Complexity: O(N) the number of elements in [seq].
   */

  let hash: (Hash.t (t 'a));
  /** [hash stack] hashes [stack], hashing elements using structural hashing. */

  let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
  /** [hashWith hash stack] hashes [stack], hashing elements using [hash]. */

  let isEmpty: (t 'a) => bool;
  /** [isEmpty stack] returns true if [stack] contains no elements. */

  let isNotEmpty: (t 'a) => bool;
  /** [isNotEmpty stack] returns true if [stack] contains at least one element. */

  let mapReverse: ('a => 'b) => (t 'a) => (t 'b);
  /** [mapReverse f stack] returns a new Stack applying the
   *  function [f] to each element in [stack], reversing the result.
   */

  let none: ('a => bool) => (t 'a) => bool;
  /** [none f stack] returns true if the predicate [f] returns false for every
   *  elements in [stack], otherwise true. If [stack] is empty, returns true.
   */

  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  /** [reduce f acc stack] applies the accumulator function [f] to each element in [stack]
   *  with the specified seed value [acc], returning the final accumulated value.
   */

  let removeAll: (t 'a) => (t 'a);
  /** [removeAll stack] returns the empty Stack.
   *
   *  Complexity: O(1)
   */

  let removeFirst: (t 'a) => (t 'a);
  /** [removeFirst stack] returns a new Stack without the first element.
   *
   *  Complexity: O(1)
   */

  let return: 'a => (t 'a);
  /** [return value] returns a new Stack containing a single element, [value]. */

  let reverse: (t 'a) => (t 'a);
  /** [reverse stack] returns a new Stack with [stack]'s elements reversed.
   *
   *  Complexity: O(N)
   */

  let some: ('a => bool) => (t 'a) => bool;
  /** [some f stack] returns true if the predicate [f] returns true for any element in [stack], otherwise false.
   *  If [stack] is empty, returns false.
   */

  let toList: (t 'a) => (list 'a);
  /** [toList stack] returns the underlying List backing the stack */

  let toSeq: (t 'a) => (Seq.t 'a);
  /** [toSeq stack] returns a Seq of the elements in [stack] in order. */

  let tryFind: ('a => bool) => (t 'a) => (option 'a);
  /** [tryFind f stack] returns the first value for which the predicate [f] returns true or None. */

  let tryFirst: (t 'a) => (option 'a);
  /** [tryFirst stack] returns first element in [stack] or None. */
};

let module StackMultimap: {
  type t 'k 'v;

  let add: 'k => 'v => (t 'k 'v) => (t 'k 'v);
  let addAllValues: 'k => (Seq.t 'v) => (t 'k 'v) => (t 'k 'v);
  let contains: 'k => 'v => (t 'k 'v) => bool;
  let containsWith: (Equality.t 'v) => 'k => 'v => (t 'k 'v) => bool;
  let containsKey: 'k => (t 'k 'v) => bool;
  let count: (t 'k 'v) => int;
  let empty: (t 'k 'v);
  let emptyWith: (HashStrategy.t 'k) => (t 'k 'v);
  let equals: (t 'k 'v) => (t 'k 'v) => bool;
  let equalsWith: (Equality.t 'v) => (t 'k 'v) => (t 'k 'v) => bool;
  let every: ('k => 'v => bool) => (t 'k 'v) => bool;
  let find: ('k => 'v => bool) => (t 'k 'v) => ('k, 'v);
  let forEach: ('k => 'v => unit) => (t 'k 'v) => unit;
  let get: 'k => (t 'k 'v) => (Stack.t 'v);
  let hash: (Hash.t (t 'k 'v));
  let hashWith: (Hash.t 'v) => (Hash.t (t 'k 'v));
  let isEmpty: t 'k 'v => bool;
  let isNotEmpty: t 'k 'v => bool;
  let keys: (t 'k 'v) => (Collection.t 'k);
  let none: ('k => 'v => bool) => (t 'k 'v) => bool;
  let reduce: ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let remove: 'k => (t 'k 'v) => (t 'k 'v);
  let removeAll: (t 'k 'v) => (t 'k 'v);
  let some: ('k => 'v => bool) => (t 'k 'v) => bool;
  let toSeq: (t 'k 'v) => (Seq.t ('k, 'v));
  let tryFind: ('k => 'v => bool) => (t 'k 'v) => (option ('k, 'v));
  let values: (t 'k 'v) => (Seq.t 'v);
};

let module Table: {
  type t 'row 'column 'value;

  let columns: (t 'row 'column 'value) => (Seq.t 'column);
  let contains: 'row => 'column => 'value => (t 'row 'column 'value) => bool;
  let containsWith: (Equality.t 'value) => 'row => 'column => 'value => (t 'row 'column 'value) => bool;
  let containsRow: 'row => (t 'row 'column 'value) => bool;
  let containsRowAndColumn: 'row => 'column => (t 'row 'column 'value) => bool;
  let count: (t 'row 'column 'value) => int;
  let empty: (t 'row 'column 'value);
  let emptyWith: (HashStrategy.t 'row) => (HashStrategy.t 'column) => (t 'row 'column 'value);
  let equals: (t 'row 'column 'value) => (t 'row 'column 'value)  => bool;
  let equalsWith: (Equality.t 'value) => (t 'row 'column 'value) =>  (t 'row 'column 'value)  => bool;
  let every: ('row => 'column => 'value => bool) => (t 'row 'column 'value) => bool;
  let find: ('row => 'column => 'value => bool) => (t 'row 'column 'value) => ('row, 'column, 'value);
  let forEach: ('row => 'column => 'value  => unit) => (t 'row 'column 'value) => unit;
  let get: 'row => 'column => (t 'row 'column 'value) => 'value;
  let hash: (Hash.t (t 'row 'column 'value));
  let hashWith: (Hash.t 'value) => (Hash.t (t 'row 'column 'value));
  let isEmpty: t 'row 'column 'value => bool;
  let isNotEmpty: t 'row 'column 'value => bool;
  let map: ('row => 'column => 'a => 'b) => (t 'row 'column 'a) => (t 'row 'column 'b);
  let none: ('row => 'column => 'value => bool) => (t 'row 'column 'value) => bool;
  let put: 'row => 'column => 'value => (t 'row 'column 'value) => (t 'row 'column 'value);
  let reduce: ('acc => 'row => 'column => 'value => 'acc) => 'acc => (t 'row 'column 'value) => 'acc;
  let remove: 'row => 'column => (t 'row 'column 'value) => (t 'row 'column 'value);
  let removeAll: (t 'row 'column 'value) => (t 'row 'column 'value);
  let removeRow: 'row => (t 'row 'column 'value) => (t 'row 'column 'value);
  let rows: (t 'row 'column 'value) => (Collection.t 'row);
  let some: ('row => 'column => 'value=> bool) => (t 'row 'column 'value) => bool;
  let toSeq: (t 'row 'column 'value) => (Seq.t ('row, 'column, 'value));
  let tryFind: ('row => 'column => 'value => bool) => (t 'row 'column 'value) => (option ('row, 'column, 'value));
  let tryGet: 'row => 'column => (t 'row 'column 'value) => (option 'value);
  let values: (t 'row 'column 'value) => (Seq.t 'value);
};

let module rec Vector: {
  /** Indexed type that supports efficient operations for
   * prepend, appends, indexing, conctentation, and splits.
   */

  type t 'a;
  /** The vector type */

  let addFirst: 'a => (t 'a) => (t 'a);
  /** [addFirst value vec] returns a new Vector with [value] added at index [0]
   *  and all other values shifted right.
   *
   *  Complexity: O(1)
   */

  let addFirstAll: (Seq.t 'a) => (t 'a) => (t 'a);
  /** [addFirstAll seq vec] returns a new Vector with the values in [seq] prepended.
   *
   *  Complexity: O(N) the number of elements in [seq].
   */

  let addLast: 'a => (t 'a) => (t 'a);
  /** [addLast value vec] returns a new CopyOnWriteArray with [value] added at index [count vec].
   *
   *  Complexity: O(1)
   */

  let addLastAll: (Seq.t 'a) => (t 'a) => (t 'a);
  /** [addLastAll seq vec] returns a new Vector with the values in [seq] appended.
   *
   *  Complexity: O(N) the number of elements in [seq].
   */

  let compare: (Comparator.t (t 'a));
  /** A comparator that compares two Vectors
   *  using structural comparison to compare elements.
   */

  let compareWith: (Comparator.t 'a) => (Comparator.t (t 'a));
  /** [compareWith comparator] returns a Comparator that compares two Vectors
   *  using [comparator] to compare elements.
   */

  let concat: (list (t 'a)) => (t 'a);
  /** [concat vecs] returns a new Vector by concatenating the Vectors in [vec].
   *
   *  WARNING: Not implemented
   *
   *  Complexity: O(log32 N * m)
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

  let count: (t 'a) => int;
  /** [count vec] returns the number of elements in [vec]. */

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

  let every: ('a => bool) => (t 'a) => bool;
  /** [every f vec] returns true if the predicate [f] returns true for every
   *  element in [vec], otherwise false. If [vec] is empty, returns true.
   */

  let everyWithIndex: (int => 'a => bool) => (t 'a) => bool;
  /** [everyWithIndex f vec] returns true if the predicate [f] returns true for every
   *  index/element pair in [vec], otherwise false. If [vec] is empty, returns true.
   */

  let find: ('a => bool) => (t 'a) => 'a;
  /** [find f vec] returns the first value for which the predicate [f] returns true.
   *  If no value is found, an exception is thrown.
   */

  let findWithIndex: (int => 'a => bool) => (t 'a) => 'a;
  /** [findWithIndex f vec] returns the first index/element pair in [vec] for
   *  which the predicate [f] returns true. If no value is found, an exception is thrown.
   */

  let first: (t 'a) => 'a;
  /** [first vec] returns the first element in [vec] or throws. */

  let forEach: ('a => unit) => (t 'a) => unit;
  /** [forEach f vec] iterates through [vec], invoking [f] for each element. */

  let forEachReverse: ('a => unit) => (t 'a) => unit;
  /** [forEachReverse f vec] iterates through [vec] in reverser order,
   *  invoking [f] for each element.
   */

  let forEachWithIndex: (int => 'a => unit) => (t 'a) => unit;
  /** [forEachWithIndex f vec] iterates through [vec], invoking [f] on each index/element pair. */

  let forEachReverseWithIndex: (int => 'a => unit) => (t 'a) => unit;
  /** [forEachWithIndex f vec] iterates through [vec] in reverser order,
   *  invoking [f] on each index/element pair.
   */

  let fromSeq: (Seq.t 'a) => (t 'a);
  /** [fromSeq seq] returns a new Vector containing the values in [seq].
   *
   * Complexity: O(N) the number of elements in [seq].
   */

  let fromSeqReversed: (Seq.t 'a) => (t 'a);
  /** [fromSeq seq] returns a new Vector containing the values in [seq]
   *  in reverse order.
   *
   * Complexity: O(N) the number of elements in [seq].
   */

  let get: int => (t 'a) => 'a;
  /** [get index vec] returns the element at [index]. Throws if index is out of bounds. */

  let hash: (Hash.t (t 'a));
  /** [hash vec] hashes [vec], hashing elements using structural hashing. */

  let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
  /** [hashWith hash vec] hashes [vec], hashing elements using [hash]. */

  let indexOf: ('a => bool) => (t 'a) => int;
  /** [find f vec] returns the index of the first element for which the predicate [f] returns true.
   *  If no value is found, an exception is thrown.
   */

  let indexOfWithIndex: (int => 'a => bool) => (t 'a) => int;
  /** [indexOfWithIndex f vec] returns the index of the first index/element pair in [vec] for
   *  which the predicate [f] returns true. If no pair is found, an exception is thrown.
   */

  let init: int => (int => 'a) => (t 'a);
  /** [init n f] returns a new Vector of length [n],
   *  with element number [i] initialized to the result of [f i].
   */

  let insertAt: int => 'a => (t 'a) => (t 'a);
  /** [insertAt index value vec] returns a new Vector with [value] inserted at [index].
   *
   *  WARNING: Not implemented
   *
   *  Complexity: O(log32 N)
   */

  let isEmpty: (t 'a) => bool;
  /** [isEmpty vec] returns true if [vec] contains no elements. */

  let isNotEmpty: (t 'a) => bool;
  /** [isNotEmpty vec] returns true if [vec] contains at least one element. */

  let last: (t 'a) => 'a;
  /** [last vec] returns the last element in [vec] or throws. */

  let map: ('a => 'b) => (t 'a) => (t 'b);
  /** [map f vec] returns a new Vector applying the function [f] to each element in [vec]. */

  let mapWithIndex: (int => 'a => 'b) => (t 'a) => (t 'b);
  /** [map f vec] returns a new Vector applying the
   *  function [f] to each index/element pair in [vec].
   */

  let mapReverse: ('a => 'b) => (t 'a) => (t 'b);
  /** [mapReverse f vec] returns a new Vector applying the
   *  function [f] to each element in [vec], reversing the result.
   */

  let mapReverseWithIndex: (int => 'a => 'b) => (t 'a) => (t 'b);
  /** [mapReverseWithIndex f vec] returns a new Vector applying the
   *  function [f] to each index/element pair in [vec], reversing the result.
   */

  let mutate: (t 'a) => (TransientVector.t 'a);
  /** [mutate vec] returns a TransientVector containing the same elements as [vec].
    *
    *  Complexity: O(1)
    */

  let none: ('a => bool) => (t 'a) => bool;
  /** [none f vec] returns true if the predicate [f] returns false for every
   *  elements in [vec], otherwise true. If [vec] is empty, returns true.
   */

  let noneWithIndex: (int => 'a => bool) => (t 'a) => bool;
  /** [noneWithIndex f vec] returns true if the predicate [f] returns false for every
   *  index/element pair in [vec], otherwise true. If [vec] is empty, returns true.
   */

  let range: int => (option int) => (t 'a) => (t 'a);
  /** [range startIndex count vec] returns a new Vector that is a range of
   *  elements in [vec] starting at [startIndex] and [count] elements. If [count] is None,
   *  all elements in [vec] after [startIndex] are returned.
   *
   *  Complexity: O(log32 N)
   */

  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  /** [reduce f acc vec] applies the accumulator function [f] to each element in [vec]
   *  with the specified seed value [acc], returning the final accumulated value.
   */

  let reduceWithIndex: ('acc => int => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  /** [reduceWithIndex f acc vec] applies the accumulator function [f] to each
   *  index/element pair in [vec] with the specified seed value [acc], returning
   *  the final accumulated value.
   */

  let reduceRight: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  /** [reduceRight f acc vec] applies the accumulator function [f] to each element in [vec]
   *  in reverse order with the specified seed value [acc], returning the final accumulated value.
   */

  let reduceRightWithIndex: ('acc => int => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  /** [reduceRightWithIndex f acc vec] applies the accumulator function [f] to each
   *  index/element pair in [vec] in reverse order with the specified seed value [acc],
   *  returning the final accumulated value.
   */

  let removeAt: int => (t 'a) => (t 'a);
  /** [removeAt index vec] returns a new Vector with the element at [index] removed.
   *
   *  WARNING: Not implemented
   *
   *  Complexity: O(log32 N)
   */

  let removeAll: (t 'a) => (t 'a);
  /** [removeAll vec] returns the empty Vector.
   *
   *  Complexity: O(1)
   */

  let removeFirst: (t 'a) => (t 'a);
  /** [removeFirst vec] returns a new Vector without the first element.
   *
   *  Complexity: O(1)
   */

  let removeLast: (t 'a) => (t 'a);
  /** [removeLast vec] returns a new CopyOnWriteArray without the last element.
   *
   *  Complexity: O(1)
   */

  let return: 'a => (t 'a);
  /** [return value] returns a new Vector containing a single element, [value]. */

  let reverse: (t 'a) => (t 'a);
  /** [reverse vec] returns a new Vector with [vec]'s elements reversed.
   *
   *  Complexity: O(N)
   */

  let skip: int => (t 'a) => (t 'a);
  /** [skip count vec] returns a new Vector that removes the first [count] elements in [vec]. */

  let some: ('a => bool) => (t 'a) => bool;
  /** [some f vec] returns true if the predicate [f] returns true for any element in [vec], otherwise false.
   *  If [vec] is empty, returns false.
   */

  let someWithIndex: (int => 'a => bool) => (t 'a) => bool;
  /** [some f vec] returns true if the predicate [f] returns true for any index/element pair
   *  in [vec], otherwise false. If [vec] is empty, returns false.
   */

  let take: int => (t 'a) => (t 'a);
  /** [take count vec] returns a new Vector that includes the first [count] elements in [vec]. */

  let toKeyed: (t 'a) => (Keyed.t int 'a);
  /** [toKeyed vec] returns a Keyed view of [vec] */

  let toSeq: (t 'a) => (Seq.t 'a);
  /** [toSeq vec] returns a Seq of the elements in [vec] in order. */

  let toSeqReversed: (t 'a) => (Seq.t 'a);
  /** [toSeqReversed vec] returns a Seq of the elements in [vec] in reverse order. */

  let tryFind: ('a => bool) => (t 'a) => (option 'a);
  /** [tryFind f vec] returns the first value for which the predicate [f] returns true or None. */

  let tryFindWithIndex: (int => 'a => bool) => (t 'a) => (option 'a);
  /** [tryFindWithIndex f vec] returns the first value for which the predicate [f] returns true or None. */

  let tryFirst: (t 'a) => option 'a;
  /** [tryFirst vec] returns first element in [vec] or None. */

  let tryGet: int => (t 'a) => (option 'a);
  /** [tryGet index vec] returns the element at [index] or None if [index] is out of bounds. */

  let tryIndexOf: ('a => bool) => (t 'a) => (option int);
  /** [tryIndexOf f vec] returns the index of the first element for which the predicate [f] returns true.
   *  If no value is found, returns None.
   */

  let tryIndexOfWithIndex: (int => 'a => bool) => (t 'a) => (option int);
  /** [indexOfWithIndex f vec] returns the index of the first index/element pair in [vec] for
   *  which the predicate [f] returns true. If no value is found, returns None.
   */

  let tryLast: (t 'a) => option 'a;
  /** [tryLast vec] returns the last element in [vec] or None.
   *
   *  Complexity: O(1)
   */

  let update: int => 'a => (t 'a) => (t 'a);
  /** [update index value cow] returns a new Vector with [value]
   *  replacing the element at [index].
   *
   *  Complexity: O(log32 N)
   */

  let updateAll: (int => 'a => 'a) => (t 'a) => (t 'a);
  /** [updateAll f vec] returns a new Vector updating each element
   *  in [vec] with result of applying the function [f] to each index/element pair.
   *
   *  Complexity: O(N)
   */

  let updateWith: int => ('a => 'a) => (t 'a) => (t 'a);
  /** [updateWith index f cow] returns a new Vector updating the element
   *  at [index] with the result of applying the function [f] to the element.
   *
   *  Complexity: O(log32 N)
   */
}

and TransientVector: {
  /** A temporarily mutable Vector. Once persisted, any further operations on a
   *  TransientVector instance will throw. Intended for implementing bulk mutation operations efficiently.
   */
  type t 'a;
  /** The TransientVector type. */

  let addFirst: 'a => (t 'a) => (t 'a);
  /** [addFirst value transient] prepends [value] to [transient].
   *
   *  Complexity: O(1)
   */

  let addLast: 'a => (t 'a) => (t 'a);
  /** [addLast value transient] appends [value] to [transient].
   *
   *  Complexity: O(1)
   */

  let count: (t 'a) => int;
  /** [count transient] returns the number of elements in [transient]. */

  let empty: unit => (t 'a);
  /** [empty ()] returns a new empty TransientVector. */

  let first: (t 'a) => 'a;
  /** [first transient] returns the first element in [transient] or throws. */

  let get: int => (t 'a) => 'a;
  /** [get index transient] returns the element at [index]. Throws if index is out of bounds. */

  let insertAt: int => 'a => (t 'a) => (t 'a);
  /** [insertAt index value transient] inserts value into [transient] at [index].
   *
   *  WARNING: Not implemented
   *
   *  Complexity: O(log32 N)
   */

  let isEmpty: (t 'a) => bool;
  /** [isEmpty transient] returns true if [transient] contains no elements. */

  let isNotEmpty: (t 'a) => bool;
  /** [isNotEmpty transient] returns true if [transient] contains at least one element. */

  let last: (t 'a) => 'a;
  /** [last transient] returns the last element in [transient] or throws. */

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

  let removeAll: (t 'a) => (t 'a);
  /** [removeAll transient] removes all elements from [transient].
   *
   *  Complexity: O(1)
   */

  let removeFirst: (t 'a) => (t 'a);
  /** [removeFirst transient] removes the first element from [transient].
   *
   *  Complexity: O(1)
   */

  let removeLast: (t 'a) => (t 'a);
  /** [removeLast transient] removes the last element from [transient].
   *
   *  Complexity: O(1)
   */

  let reverse: (t 'a) => (t 'a);
  /** [reverse transient] reverse [transient]'s elements.
   *
   *  Complexity: O(N)
   */

  let tryFirst: (t 'a) => option 'a;
  /** [tryFirst transient] returns first element in [transient] or None. */

  let tryGet: int => (t 'a) => (option 'a);
  /** [tryGet index transient] returns the element at [index] or None if [index] is out of bounds. */

  let tryLast: (t 'a) => (option 'a);
  /** [tryLast transient] returns the last element in [transient] or None.
   *
   *  Complexity: O(1)
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
