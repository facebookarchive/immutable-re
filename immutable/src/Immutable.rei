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

let module rec Set: {
  /** A read only view of an underlying set of unique values. The intent of this type is to enable
   *  interop between alternative concrete implementations such as [SortedSet] and [HashSet].
   *  The complexity of functions in this module is dependent upon the underlying concrete implementation.
   */

  type t 'a;
  /** The Set type. */

  let contains: 'a => (t 'a) => bool;
  /** [contains value set] returns true if [set] contains the element [value]. */

  let count: (t 'a) => int;
  /** [count set] returns the number of elements in [set]. */

  let empty: (t 'a);
  /** The empty Set. */

  let equals: (t 'a) => (t 'a) => bool;
  /** [equals this that] equates [this] and [that] ensuring that the Sets have the same count
   *  and contains the same elements.
   */

  let every: ('a => bool) => (t 'a) => bool;
  /** [every f set] returns true if the predicate [f] returns true for every
   *  element in [set], otherwise false. If [set] is empty, returns true.
   */

  let find: ('a => bool) => (t 'a) => 'a;
  /** [find f set] returns the first value for which the predicate [f] returns true.
   *  If no value is found, an exception is thrown.
   */

  let forEach: ('a => unit) => (t 'a) => unit;
  /** [forEach f set] iterates through [set], invoking [f] for each element in the Set. */

  let hash: (Hash.t (t 'a));
  /** [hash set] hashes [set], hashing elements using structural hashing. */

  let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
  /** [hashWith hash set] hashes [set], hashing elements using [hash]. */

  let intersect: (t 'a) => (t 'a) => (Seq.t 'a);
  /** [intersect this that] returns a Seq of unique elements
   *  which occur in both [this] and [that].
   */

  let isEmpty: (t 'a) => bool;
  /** [isEmpty set] returns true if [set] contains no elements. */

  let isNotEmpty: (t 'a) => bool;
  /** [isNotEmpty set] returns true if [set] contains at least one element. */

  let none: ('a => bool) => (t 'a) => bool;
  /** [none f set] returns true if the predicate [f] returns false for
   *  every elements in [set], otherwise true. If [set] is empty, returns true.
   */

  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  /** [reduce f acc set] applies the accumulator function [f] to each element in [set]
   *  with the specified seed value [acc], returning the final accumulated value.
   */

  let some: ('a => bool) => (t 'a) => bool;
  /** [some f set] returns true if the predicate [f] returns true for
   *  any element in [set], otherwise false. If [set] is empty, returns false.
   */

  let subtract: (t 'a) => (t 'a) => (Seq.t 'a);
  /** [subtract this that] returns a Seq of unique element
   *  which occur in [this] but not in [that].
   */

  let toMap: (t 'a) => (Map.t 'a 'a);
  /** [toMap set] returns a Map view of [set] as a mapping of values to themselves. */

  let toSeq: (t 'a) => (Seq.t 'a);
  /** [toSeq set] returns a Seq of the values in [set]. */

  let tryFind: ('a => bool) => (t 'a) => (option 'a);
  /** [find f set] returns the first value for which the predicate [f] returns true or None. */

  let union: (t 'a) => (t 'a) => (Seq.t 'a);
  /** [union this that] returns a Seq of unique elements which occur in either [this] or [that]. */
}

and Map: {
  /** A read only view of an underlying set of key/value pairs. The intent of this type is to enable
   *  interop between alternative concrete implementations such as [SortedMap] and [HashMap].
   *  The complexity of functions in this module is dependent upon the underlying concrete implementation.
   */

  type t 'k 'v;
  /** The map type. */

  let contains: 'k => 'v => (t 'k 'v) => bool;
  /** [contains key value map] returns true if [map] contains the [key] [value] pair,
   *  using structural equality to equate [value].
   */

  let containsWith: (Equality.t 'v) => 'k => 'v => (t 'k 'v) => bool;
  /** [containsWith equals key value map] returns true if [map] contains the [key] [value] pair,
   *  using [equals] to equate [value].
   */

  let containsKey: 'k => (t 'k 'v) => bool;
  /** [containsKey key map] returns true if [map] contains an entry with the key [key]. */

  let count: (t 'k 'v) => int;
  /** [count map] returns the number of key/value pairs in [map]. */

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

  let every: ('k => 'v => bool) => (t 'k 'v) => bool;
  /** [every f map] returns true if the predicate [f] returns true for every
   *  key/value pair in [map], otherwise false. If [map] is empty, returns true.
   */

  let find: ('k => 'v => bool) => (t 'k 'v) => ('k, 'v);
  /** [find f map] returns the first key/value pair for which the predicate [f] returns true.
   *  If no value is found, an exception is thrown.
   */

  let forEach: ('k => 'v => unit) => (t 'k 'v) => unit;
  /** [forEach f map] iterates through [map], invoking [f] for each key/value pair. */

  let get: 'k => (t 'k 'v) => 'v;
  /** [get key map] returns the value associated with [key] or throws */

  let hash: (Hash.t (t 'k 'v));
  /** [hash map] hashes [map], hashing keys and values using structural hashing. */

  let hashWith: (Hash.t 'k) => (Hash.t 'v) => (Hash.t (t 'k 'v));
  /** [hashWith keyHash valueHash map] hashes [map], hashing keys using [keyHash]
   *  and values using [valueHash].
   */

  let isEmpty: t 'k 'v => bool;
  /** [isEmpty map] returns true if [map] contains no key/value pairs. */

  let isNotEmpty: t 'k 'v => bool;
  /** [isNotEmpty map] returns true if [map] contains at least one key/value pair. */

  let keys: (t 'k 'v) => (Set.t 'k);
  /** [keys map] returns a Set view of keys in [map]. */

  let map: ('k => 'a => 'b) => (t 'k 'a) => (t 'k 'b);
  /** [map f map] returns a Map whose values are the result of
   *  applying [f] each key/value pair in [map] lazily. Note: The results of
   *  applying [f] are not memoized, therefore [f] must be pure.
   */

  let none: ('k => 'v => bool) => (t 'k 'v) => bool;
  /** [none f map] returns true if the predicate [f] returns false for
   *  every key/value pair in [map], otherwise true. If [map] is empty, returns true.
   */

  let reduce: ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  /** [reduce f acc map] applies the accumulator function [f] to each key/value pair in [map]
   *  with the specified seed value [acc], returning the final accumulated value.
   */

  let some: ('k => 'v => bool) => (t 'k 'v) => bool;
  /** [some f map] returns true if the predicate [f] returns true for
   *  any key/value pair in [map], otherwise false. If [map] is empty, returns false.
   */

  let toSet: (t 'k 'v) => (Set.t ('k, 'v));
  /** [toSet map] returns a Set view of key/value pairs in [map], using structural equality
   *  to equate values.
   */

  let toSetWith: (Equality.t 'v) => (t 'k 'v) => (Set.t ('k, 'v));
  /** [toSetWith equals map] returns a Set view of key/value pairs in [map],
   *  using [equals] to equate values.
   */

  let toSeq: (t 'k 'v) => (Seq.t ('k, 'v));
  /** [toSeq map] returns a Seq of the key/value pairs in [map]. */

  let tryFind: ('k => 'v => bool) => (t 'k 'v) => (option ('k, 'v));
  /** [find f map] returns the first key/value pair for which the predicate [f] returns true or None. */

  let tryGet: 'k => (t 'k 'v) => (option 'v);
  /** [tryGet key map] returns the value associated with [key] or None */

  let values: (t 'k 'v) => (Seq.t 'v);
  /** [values map] returns a Seq of non-unique values in [map]. */
};

let module ContiguousIntSet: {
  /** Represents a contiguous Set of discrete integers */

  type t;
  /** The ContiguousIntSet type.*/

  let create: int => int => t;

  let contains: int => t => bool;

  let count: t => int;

  let empty: t;

  let every: (int => bool) => t => bool;

  let find: (int => bool) => t => int;

  let first: t => int;

  let forEach: (int => unit) => t => unit;

  let hash: t => int;

  let isEmpty: t => bool;

  let isNotEmpty: t => bool;

  let last: t => int;

  let reduce: ('acc => int => 'acc) => 'acc => t => 'acc;

  let reduceRight: ('acc => int => 'acc) => 'acc => t => 'acc;

  let some: (int => bool) => t => bool;

  let toMap: t => (Map.t int int);

  let toSeq: t => (Seq.t int);

  let toSeqReversed: t => (Seq.t int);

  let toSet: t => (Set.t int);

  let tryFind: (int => bool) => t => (option int);

  let tryFirst: t => (option int);

  let tryLast: t => (option int);
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

  let toMap: (t 'a) => (Map.t int 'a);
  /** [toMap cow] returns a Map view of [cow] */

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
  /** A temporarily mutable Deque. Once persisted, any further operations on a
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
/*
let module rec HashBiMap: {
  /** A hashed Map preserving the uniqueness of keys to values, and values to keys. */

  type t 'k 'v;
  /** The HashBiMap type. */

  let contains: 'k => 'v => (t 'k 'v) => bool;
  /** [contains key/value bimap] returns true if [bimap] contains the [key] [value] pair.
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let containsKey: 'k => (t 'k 'v) => bool;
  /** [containsKey key bimap] returns true if [bimap] contains an entry with the key [key].
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let count: (t 'k 'v) => int;
  /** [count bimap] returns the number of key/value pairs in [bimap].
   *
   *  Complexity: O(1)
   */

  let empty: (t 'k 'v);
  /** The empty HashBiMap using the structuralCompare HashStrategy. */

  let emptyWith: (HashStrategy.t 'k) => (HashStrategy.t 'v) => (t 'k 'v);
  /** [emptyWith keyStrategy valueStrategy] returns an empty HashBiMap using the provided
   *  key and value [HashStrategy]'s.
   */

  let equals: (t 'k 'v) => (t 'k 'v) => bool;
  /** [equals this that] equates [this] and [that] ensuring that the bimaps have the same count
   *  and contains the same key/value pairs.
   */

  let every: ('k => 'v => bool) => (t 'k 'v) => bool;
  /** [every f bimap] returns true if the predicate [f] returns true for every
   *  key/value pair in [bimap], otherwise false. If [bimap] is empty, returns true.
   */

  let find: ('k => 'v => bool) => (t 'k 'v) => ('k, 'v);
  /** [find f bimap] returns the first key/value pair for which the predicate [f] returns true.
   *  If no value is found, an exception is thrown.
   */

  let fromSeq: (Seq.t ('k, 'v)) => (t 'k 'v);
  /** [fromSeq seq] returns a HashBiMap including the key/value pairs in [seq]. If
   *  [seq] includes duplicate keys or values, the last key/value pair with the duplicate
   *  key or value is added to the HashBiMap.
   */

  let fromSeqWith: (HashStrategy.t 'k) => (HashStrategy.t 'v) => (Seq.t ('k, 'v)) => (t 'k 'v);
  /** [fromSeqWith keyStrategy valueStrategy seq] returns a HashBiMap including the key/value
   *  pairs in [seq] using the provided key and value [HashStrategy]'s. If [seq] includes duplicate
   *  keys or values, the last key/value pair with the duplicate key or value is added to the HashBiMap.
   */

  let forEach: ('k => 'v => unit) => (t 'k 'v) => unit;
  /** [forEach f bimap] iterates through [bimap], invoking [f] for each key/value pair. */

  let get: 'k => (t 'k 'v) => 'v;
  /** [get key bimap] returns the value associated with [key] or throws
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let hash: (Hash.t (t 'k 'v));
  /** [hash bimap] hashes [bimap] */

  let inverse: (t 'k 'v) => (t 'v 'k);
  /** [inverse bimap] returns a HashBiMap of value key pairs in [bimap]
   *
   *  Complexity: O(1)
   */

  let isEmpty: (t 'k 'v) => bool;
  /** [isEmpty bimap] returns true if [bimap] contains no key/value pairs. */

  let isNotEmpty: (t 'k 'v) => bool;
  /** [isNotEmpty bimap] returns true if [bimap] contains at least one key/value pair. */

  let keys: (t 'k 'v) => (Set.t 'k);
  /** [keys bimap] returns a Set view of keys in [bimap]. */

  let mutate: (t 'k 'v) => (TransientHashBiMap.t 'k 'v);
  /** [mutate bimap] returns a TransientHashBiMap containing the same key/values pairs as [bimap].
   *
   *  Complexity: O(1)
   */

  let none: ('k => 'v => bool) => (t 'k 'v) => bool;
  /** [none f bimap] returns true if the predicate [f] returns false for
   *  every key/value pair in [bimap], otherwise true. If [bimap] is empty, returns true.
   */

  let put: 'k => 'v => (t 'k 'v) => t 'k 'v;
  /** [put key value bimap] returns a new HashBiMap containing ([key], [value]). If [bimap] contains and existing
   *  mapping of [key] to a different value or, [value] to a different key, those mappings are removed.
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let putAll: (Seq.t ('k, 'v)) => (t 'k 'v) => (t 'k 'v);
  /** [putAll seq bimap] returns a new HashBiMap including the key/value pairs in [seq].
   *  Key value pairs in seq replace existing mappings in [bimap].
   */

  let reduce: ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  /** [reduce f acc bimap] applies the accumulator function [f] to each key/value pair in [bimap]
   *  with the specified seed value [acc], returning the final accumulated value.
   */

  let remove: 'k => (t 'k 'v) => (t 'k 'v);
  /** [remove key bimap] returns a new HashBiMap without any mapping from [key].
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let removeAll: (t 'k 'v) => (t 'k 'v);
  /** [removeAll bimap] returns an empty HashBiMap with the same key and value HashStrategy's as [bimap].
   *
   *  Complexity: O(1)
   */

  let removeValue: 'v => (t 'k 'v) => (t 'k 'v);
  /** [removeValue value bimap] returns a new HashBiMap without any mapping from [value]. */

  let some: ('k => 'v => bool) => (t 'k 'v) => bool;
  /** [some f bimap] returns true if the predicate [f] returns true for
   *  any key/value pair in [bimap], otherwise false. If [bimap] is empty, returns false.
   */

  let toSet: (t 'k 'v) => (Set.t ('k, 'v));
  /** [toSet bimap] returns a Set view of the key/value pairs in [bimap]. */

  let toMap: (t 'k 'v) => (Map.t 'k 'v);
  /** [toMap bimap] returns a Map view of [bimap]. */

  let toSeq: (t 'k 'v) => (Seq.t ('k, 'v));
  /** [toSeq bimap] returns a Seq of the key/value pairs in [bimap]. */

  let tryFind: ('k => 'v => bool) => (t 'k 'v) => (option ('k, 'v));
  /** [tryFind f bimap] returns the first key/value pair for which the predicate [f] returns true.
   *  If no value is found, an exception is thrown.
   */

  let tryGet: 'k => (t 'k 'v) => (option 'v);
  /** [tryGet key bimap] returns the value associated with [key] or None
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let tryPut: 'k => 'v => (t 'k 'v) => (t 'k 'v);
  /** [tryPut key value bimap] returns a new HashBiMap containing ([key], [value]). If [bimap] contains and existing
   *  mapping of [key] to a different value or, [value] to a different key, [bimap] is returned unchanged.
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let values: (t 'k 'v) => (Set.t 'v);
  /** [values bimap] returns a Set of unique values in the HashBiMap. */
}

and TransientHashBiMap: {
  /** A temporarily mutable HashBiMap. Once persisted, any further operations on a TransientHashBiMap instance
   *  will throw. Intended for implementing bulk mutation operations efficiently.
   */

  type t 'k 'v;
  /** The TransientHashBiMap type. */

  let count: (t 'k 'v) => int;
  /** [count transient] returns the number of key/value pairs in [transient].
   *
   *  Complexity: O(1)
   */

  let empty: unit => (t 'k 'v);
  /** [empty ()] returns a new empty TransientHashBiMap. */

  let emptyWith: (HashStrategy.t 'k) => (HashStrategy.t 'v) => (t 'k 'v);
  /** [emptyWith keyStrategy valueStrategy] returns an empty TransientHashBiMap using the provided
   *  key and value [HashStrategy]'s.
   */

  let isEmpty: t 'k 'v => bool;
  /** [isEmpty transient] returns true if [transient] contains no key/value pairs. */

  let isNotEmpty: t 'k 'v => bool;
  /** [isNotEmpty transient] returns true if [transient] contains at least one key/value pair. */

  let persist: (t 'k 'v) => (HashBiMap.t 'k 'v);
  /** [persist transient] returns a persisted HashBiMap. Further attempts to access or mutate [transient]
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
  /** [putAll seq transient] adds the key/value pairs in [seq] to [transient].
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
*/
let module rec HashMap: {
  /** A hashed Map. */

  type t 'k 'v;
  /** The HashMap type. */

  let alter: 'k => (option 'v => option 'v) => (t 'k 'v) => (t 'k 'v);
  /** [alter key f map] enables efficient deep updates to an existing
   *  mapping from [key] in [map]. If [map] already has a mapping from [key],
   *  [f] will be called with Some, otherwise it will be called with None.
   *  If [f] returns None, alter returns a new HashMap without a mapping from [key].
   *  If [f] returns Some, alter returns a new HashMap with an updated
   *  mapping from [key].
   */

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

  let containsKey: 'k => (t 'k 'v) => bool;
  /** [containsKey key map] returns true if [map] contains a mapping from [key]. */

  let count: (t 'k 'v) => int;
  /** [count map] returns the number of key/value pairs in [map]. */

  let empty: (t 'k 'v);
  /** The empty HashMap using the structuralCompare HashStrategy. */

  let emptyWith: (HashStrategy.t 'k) => (t 'k 'v);
  /** [emptyWith strategy] returns an empty HashMap using the HashStrategy [strategy]. */

  let equals: (t 'k 'v) => (t 'k 'v) => bool;
  /** [equals this that] equates [this] and [that]. Structural equality is used to equate values. */

  let equalsWith: (Equality.t 'v) => (t 'k 'v) => (t 'k 'v) => bool;
  /** [equalsWith equals this that] equates [this] and [that].
   *  [equals] is used to equate values.
   */

  let every: ('k => 'v => bool) => (t 'k 'v) => bool;
  /** [every f map] returns true if the predicate [f] returns true for every
   *  key/value pair in [map], otherwise false. If [map] is empty, returns true.
   */

  let find: ('k => 'v => bool) => (t 'k 'v) => ('k, 'v);
  /** [find f map] returns the first key/value pair for which the predicate [f] returns true.
   *  If no value is found, an exception is thrown.
   */

  let forEach: ('k => 'v => unit) => (t 'k 'v) => unit;
  /** [forEach f map] iterates through [map], invoking [f] for each key/value pair. */

  let fromMap: (Map.t 'k 'v) => (t 'k 'v);
  /** [fromMap map] returns a HashMap including the key/value pairs in [map]
   *  using the structuralCompare HashStrategy.
   */

  let fromMapWith: (HashStrategy.t 'k) => (Map.t 'k 'v) => (t 'k 'v);
  /** [fromSeqWith strategy map] returns a HashMap including the key/value pairs in [map]
   *  using the provided HashStrategy [strategy].
   */

  let fromSeq: (Seq.t ('k, 'v)) => (t 'k 'v);
  /** [fromSeq seq] returns a HashMap including the key/value pairs in [seq]
   *  using the structuralCompare HashStrategy.
   */

  let fromSeqWith: (HashStrategy.t 'k) => (Seq.t ('k, 'v)) => (t 'k 'v);
  /** [fromSeqWith strategy seq] returns a HashMap including the key/value pairs in [seq]
   *  using the provided HashStrategy [strategy].
   */

  let get: 'k => (t 'k 'v) => 'v;
  /** [get key map] returns the value associated with [key] or throws */

  let hash: (Hash.t (t 'k 'v));
  /** [hash map] hashes [map], hashing values using structural hashing. */

  let hashWith: (Hash.t 'v) => (Hash.t (t 'k 'v));
  /** [hashWith hash map] hashes [map], hashing values using [hash]. */

  let isEmpty: t 'k 'v => bool;
  /** [isEmpty map] returns true if [map] contains no key/value pairs. */

  let isNotEmpty: t 'k 'v => bool;
  /** [isNotEmpty map] returns true if [map] contains at least one key/value pair. */

  let keys: (t 'k 'v) => (Set.t 'k);
  /** [keys map] returns a Set view of keys in [map]. */

  let map: ('k => 'a => 'b) => (t 'k 'a) => (t 'k 'b);
  /** [map f map] returns a new HashMap whose values are the result of
   *  applying [f] each key/value pair in [map].
   */

  let merge: ('k => (option 'vAcc) => (option 'v) => (option 'vAcc)) => (t 'k 'v) => (t 'k 'vAcc) => (t 'k 'vAcc);
  /** [merge f next acc] returns a new HashMap that is the result of applying [f] to
   *  the union of keys in [next] and [acc]. For each key, [f] is called with the mapped
   *  values in [next] and [acc] or None. If [f] returns Some the value is added to the returned Map.
   */

  let mutate: (t 'k 'v) => (TransientHashMap.t 'k 'v);
  /** [mutate map] returns a TransientHashMap containing the same key/values pairs as [map].
   *
   *  Complexity: O(1)
   */

  let none: ('k => 'v => bool) => (t 'k 'v) => bool;
  /** [none f map] returns true if the predicate [f] returns false for
   *  every key/value pair in [map], otherwise true. If [map] is empty, returns true.
   */

  let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);
  /** [put key value map] returns a new HashMap containing a mapping from [key] to [value].
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let putAll: (Seq.t ('k, 'v)) => (t 'k 'v) => (t 'k 'v);
  /** [putAll key values map] returns a new HashMap containing the key/value pairs in [values].
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let reduce: ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  /** [reduce f acc map] applies the accumulator function [f] to each key/value pair in [map]
   *  with the specified seed value [acc], returning the final accumulated value.
   */

  let remove: 'k => (t 'k 'v) => (t 'k 'v);
  /** [remove key map] returns a new HashMap without any mapping from [key].
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let removeAll: (t 'k 'v) => (t 'k 'v);
  /** [removeAll map] returns an empty HashMap with the same key HashStrategy as [map].
   *
   *  Complexity: O(1)
   */

  let some: ('k => 'v => bool) => (t 'k 'v) => bool;
  /** [some f map] returns true if the predicate [f] returns true for
   *  any key/value pair in [map], otherwise false. If [map] is empty, returns false.
   */

  let toSet: (t 'k 'v) => (Set.t ('k, 'v));
  /** [toSet map] returns a Set view of key/value pairs in [map], using structural equality
   *  to equate values.
   */

  let toSetWith: (Equality.t 'v) => (t 'k 'v) => (Set.t ('k, 'v));
  /** [toSetWith equals map] returns a Set view of key/value pairs in [map],
   *  using [equals] to equate values.
   */

  let toMap: (t 'k 'v) => (Map.t 'k 'v);
  /** [toMap map] returns a Map view of [map]. */

  let toSeq: (t 'k 'v) => (Seq.t ('k, 'v));
  /** [toSeq map] returns a Seq of the key/value pairs in [map]. */

  let tryFind: ('k => 'v => bool) => (t 'k 'v) => (option ('k, 'v));
  /** [find f map] returns the first key/value pair for which the predicate [f] returns true or None. */

  let tryGet: 'k => (t 'k 'v) => (option 'v);
  /** [tryGet key map] returns the value associated with [key] or None */

  let values: (t 'k 'v) => (Seq.t 'v);
  /** [values map] returns a Seq of non-unique values in [map]. */
}

and TransientHashMap: {
  /** A temporarily mutable HashMap. Once persisted, any further operations on a
   *  TransientHashSet instance will throw. Intended for implementing bulk mutation
   *  operations efficiently.
   */

  type t 'k 'v;
  /** The TransientHashMap type. */

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

  let empty: unit => (t 'k 'v);
  /** [empty ()] returns a new empty TransientHashMap. */

  let emptyWith: (HashStrategy.t 'k) => (t 'k 'v);
  /** [emptyWith strategy] returns an empty TransientHashMap using the provided
 *  key HashStrategy.
 */

  let isEmpty: (t 'k 'v) => bool;
  /** [isEmpty map] returns true if [map] contains no key/value pairs. */

  let isNotEmpty: (t 'k 'v) => bool;
  /** [isNotEmpty map] returns true if [map] contains at least one key/value pair. */

  let persist: (t 'k 'v) => (HashMap.t 'k 'v);
  /** [persist transient] returns a persisted HashMap. Further attempts to access or mutate [transient]
   *  will throw.
   */

  let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);
  /** [put key value transient] adds the mapping of [key] to [value] to [transient].
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let putAll: (Seq.t ('k, 'v)) => (t 'k 'v) => (t 'k 'v);
  /** [putAll seq transient] adds the key/value pairs in [seq] to [transient].
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

  let tryGet: 'k => (t 'k 'v) => (option 'v);
  /** [tryGet key transient] returns the value associated with [key] or None
   *
   *  Complexity: O(log32 N), effectively O(1)
   */
};
/*
let module rec HashMultiset: {
  /** A Set implementation that tracks the number of times an element has been
   *  added to the Set. Also known as a Bag.
   */

  type t 'a;
  /* The HashMultiset type. */

  let add: 'a => (t 'a) => (t 'a);
  /** [add value set] returns a new HashMultiset with [value].
   *
   *  Complexity: O(log32 N)
   */

  let addAll: (Seq.t 'a) => (t 'a) => (t 'a);
  /** [addAll values set] returns a new HashMultiset with all elements in [values] added.
   *
   *  Complexity: O(N log32 N)
   */

  let contains: 'a => (t 'a) => bool;
  /** [contains value set] returns true if any element in [set] is equal to [value].
   *
   *  Complexity: O(log32 N)
   */

  let count: (t 'a) => int;
  /** [count set] returns the number of elements in [set].
   *
   *  Complexity: O(1)
   */

  let empty: (t 'a);
  /** The empty HashMultiset using [strategy] for hashing and collision resolution. */

  let emptyWith: (HashStrategy.t 'a) => (t 'a);
  /** [emptyWith strategy] returns an empty HashMultiset using [strategy]
   *  for hashing and collision resolution.
   */

  let equals: (t 'a) => (t 'a) => bool;
  /** [equals this that] compares [this] and [that] for equality. */

  let every: ('a => int => bool) => (t 'a) => bool;
  /** [every f set] returns true if the predicate [f] returns true for every
   *  value/count pair in [set], otherwise false. If [set] is empty, returns true.
   */

  let find: ('a => int => bool) => (t 'a) => ('a, int);
  /** [find f set] returns the first value/count pair  for which the predicate [f] returns true.
   *  If no pair is found, an exception is thrown.
   */

  let forEach: ('a => int => unit) => (t 'a) => unit;
  /** [forEach f set] iterates through [set], invoking [f] for each value/count pair. */

  let fromSeq: (Seq.t 'a) => (t 'a);
  /** [fromSeq seq] returns an HashMultiset including the values in [seq]. */

  let fromSeqWith: (HashStrategy.t 'a) => (Seq.t 'a) => (t 'a);
  /** [fromSeq seq] returns an HashMultiset including the values in [seq]. */

  let get: 'a => (t 'a) => int;
  /** [get value set] returns the number of times [value] has been added to [set] or 0. */

  let hash: (Hash.t (t 'a));
  /** [hash set] hashes [set]. */

  let isEmpty: (t 'a) => bool;
  /** [isEmpty set] returns true if [set] contains no elements. */

  let isNotEmpty: (t 'a) => bool;
  /** [isNotEmpty set] returns true if [set] contains at least one element. */

  let mutate: (t 'a) => (TransientHashMultiset.t 'a);
  /** [mutate set] returns a TransientHashMultiset containing the same elements as [set].
   *
   *  Complexity: O(1)
   */

  let none: ('a => int => bool) => (t 'a) => bool;
  /** [none f set] returns true if the predicate [f] returns false for every
   *  value/count pair in [set], otherwise true. If [set] is empty, returns true.
   */

  let reduce: ('acc => 'a => int => 'acc) => 'acc => (t 'a) => 'acc;
  /** [reduce f acc set] applies the accumulator function [f] to each value/count pair in [set]
   *  with the specified seed value [acc], returning the final accumulated value.
   */

  let remove: 'a => (t 'a) => (t 'a);
  /** [remove value set] returns a new HashMultiset without any instances of [value].
   *
   *  Complexity: O(log32 N)
   */

  let removeAll: (t 'a) => (t 'a);
  /** [removeAll set] returns the empty HashMultiset using the same HashStrategy as [set].
   *
   *  Complexity: O(1)
   */

  let set: 'a => int => (t 'a) => (t 'a);
  /** [set value count set] return a new HashMultiset containing [count]
   *  instances of [value].
   *
   *  Complexity: O(log32 N)
   */

  let some: ('a => int => bool) => (t 'a) => bool;
  /** [some f set] returns true if the predicate [f] returns true for any
   *  value/count pair in [set], otherwise false.  If [set] is empty, returns false.
   */

  let toMap: (t 'a) => (Map.t 'a int);
  /** [toMap set] returns a Map view of [set] mapping values to their count */

  let toSeq: (t 'a) => (Seq.t 'a);
  /** [toSeq set] returns a Seq of the values in [set]. If the HashMultiset contains more than
   *  one instance of a value, it will occur in the Seq multiple times.
   */

  let tryFind: ('a => int => bool) => (t 'a) => (option ('a, int));
  /** [tryFind f set] returns the first value/count pair for which
   *  the predicate [f] returns true or None.
   */

  let values: (t 'a) => (Set.t 'a);
  /** [values set] returns a Set of values in [set] */
}

and TransientHashMultiset: {
  /** A temporarily mutable HashMultiset. Once persisted, any further operations on a
   *  TransientHashMultiset instance will throw. Intended for implementing bulk mutation
   *  operations efficiently.
   */

  type t 'a;
  /** The TransientHashMultiset type. */

  let add: 'a => (t 'a) => (t 'a);
  /** [add value transient] adds an instance of [value] to [transient].
   *
   *  Complexity: O(log32 N)
   */

  let addAll: (Seq.t 'a) => (t 'a) => (t 'a);
  /** [addAll values transient] adds all elements in [values] to [transient].
   *
   *  Complexity: O(N log32 N)
   */

  let contains: 'a => (t 'a) => bool;
  /** [contains value transient] returns true if any element in [transient] is equal to [value].
   *
   *  Complexity: O(log32 N)
   */

  let count: (t 'a) => int;
  /** [count transient] returns the number of elements in [transient].
   *
   *  Complexity: O(1)
   */

  let empty: unit => (t 'a);
  /** [empty ()] returns a new empty TransientHashMultiset using
   *  HashStrategy.structuralCompare for hashing and collision resolution.
   */

  let emptyWith: (HashStrategy.t 'a) => (t 'a);
  /** [empty ()] returns a new empty TransientHashMultiset using [strategy]
   *  for hashing and collision resolution.
   */

  let get: 'a => (t 'a) => int;
  /** [get value transient] returns the number of times [value] has been added to [transient] or 0. */

  let isEmpty: (t 'a) => bool;
  /** [isEmpty transient] returns true if [transient] contains no elements. */

  let isNotEmpty: (t 'a) => bool;
  /** [isNotEmpty transient] returns true if [transient] contains at least one element. */

  let persist: (t 'a) => (HashMultiset.t 'a);
  /** [persist transient] returns a persisted HashMultiset. Further attempts to access
   *  or mutate [transient] will throw.
   */

  let remove: 'a => (t 'a) => (t 'a);
  /** [remove value transient] removes all instances of [value] from [transient].
   *
   *  Complexity: O(log32 N)
   */

  let removeAll: (t 'a) => (t 'a);
  /** [removeAll transient] removes all elements from [transient].
   *
   *  Complexity: O(1)
   */

  let set: 'a => int => (t 'a) => (t 'a);
  /** [set value count transient] updates [transient] with [count] instances of [value].
   *  Complexity: O(log32 N)
   */
};
*/
let module rec HashSet: {
  /** A set implementation that utilizes hashing and comparison
   *  or equality for collision resolution.
   */

  type t 'a;
  /** The HashSet type. */

  let add: 'a => (t 'a) => (t 'a);
  /** [add value set] returns a new HashSet with [value], if [set] does not already contain [value].
   *
   *  Complexity: O(log32 N)
   */

  let addAll: (Seq.t 'a) => (t 'a) => (t 'a);
  /** [addAll values set] returns a new HashSet with all elements in [values] added.
   *
   *  Complexity: O(N log32 N)
   */

  let contains: 'a => (t 'a) => bool;
  /** [contains value set] returns true if any element in [set] is equal to [value].
   *
   *  Complexity: O(log32 N)
   */

  let count: (t 'a) => int;
  /** [count set] returns the number of elements in [set].
   *
   *  Complexity: O(1)
   */

  let empty: (t 'a);
  /** The empty HashSet with HashStrategy.structuralCompare hash strategy. */

  let emptyWith: (HashStrategy.t 'a) => (t 'a);
  /** [emptyWith strategy] returns an empty HashSet using [strategy]
   *  for hashing and collision resolution.
   */

  let equals: (t 'a) => (t 'a) => bool;
  /** [equals this that] compares [this] and [that] for equality. */

  let every: ('a => bool) => (t 'a) => bool;
  /** [every f set] returns true if the predicate [f] returns true for every
   *  element in [set], otherwise false. If [set] is empty, returns true.
   */

  let find: ('a => bool) => (t 'a) => 'a;
  /** [find f set] returns the first value for which the predicate [f] returns true.
   *  If no value is found, an exception is thrown.
   */

  let forEach: ('a => unit) => (t 'a) => unit;
  /** [forEach f set] iterates through [set], invoking [f] for each element. */

  let fromSeq: (Seq.t 'a) => (t 'a);
  /** [fromSeq seq] returns an HashSet including the values in [seq]. */

  let fromSeqWith: (HashStrategy.t 'a)  => (Seq.t 'a) => (t 'a);
  /** [fromSeq seq] returns an HashSet including the values in [seq]. */

  let hash: (Hash.t (t 'a));
  /** [hash set] hashes [set]. */

  let intersect: (t 'a) => (t 'a) => (t 'a);
  /** [intersect this that] returns a new HashSet containing only elements that appear in
   *  both [this] and [that].
   *
   *  Complexity: Currently O(N), ideally O(log32 N) if [this] and [that] use the same HashStrategy.
   */

  let isEmpty: (t 'a) => bool;
  /** [isEmpty set] returns true if [set] contains no elements. */

  let isNotEmpty: (t 'a) => bool;
  /** [isNotEmpty set] returns true if [set] contains at least one element. */

  let mutate: (t 'a) => (TransientHashSet.t 'a);
  /** [mutate set] returns a TransientHashSet containing the same elements as [set].
   *
   *  Complexity: O(1)
   */

  let none: ('a => bool) => (t 'a) => bool;
  /** [none f set] returns true if the predicate [f] returns false for every
   *  elements in [set], otherwise true. If [set] is empty, returns true.
   */

  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  /** [reduce f acc set] applies the accumulator function [f] to each element in [set]
   *  with the specified seed value [acc], returning the final accumulated value.
   */

  let remove: 'a => (t 'a) => (t 'a);
  /** [remove value set] returns a new HashSet without [value].
   *
   *  Complexity: O(log32 N)
   */

  let removeAll: (t 'a) => (t 'a);
  /** [removeAll set] returns the empty HashSet using the same HashStrategy as [set].
   *
   *  Complexity: O(1)
   */

  let some: ('a => bool) => (t 'a) => bool;
  /** [some f set] returns true if the predicate [f] returns true for any element in [set], otherwise false.
   *  If [set] is empty, returns false.
   */

  let subtract: (t 'a) => (t 'a) => (t 'a);
  /** [substract this that] returns a new HashSet with the elements of [that] removed from [this].
   *
   *  Complexity: O(N) currently, ideally O(log32 N) if [this] and [that] use the same HashStrategy.
   */

  let toSet: (t 'a) => (Set.t 'a);
  /** [toSet set] returns a Set view of [set] */

  let toMap: (t 'a) => (Map.t 'a 'a);
  /** [toMap set] returns a Map view of [set] as mapping of values to themselves. */

  let toSeq: (t 'a) => (Seq.t 'a);
  /** [toSeq set] returns a Seq of the values in [set]. */

  let tryFind: ('a => bool) => (t 'a) => (option 'a);
  /** [tryFind f set] returns the first value for which the predicate [f] returns true or None. */

  let union: (t 'a) => (t 'a) => (t 'a);
  /** [union this that] returns a new HashSet containing all the elements in
   *  both [this] and [that].
   *
   *  Complexity: currently O(N), ideally O(log32 N) if [this] and [that] use the same HashStrategy.
   */
}

and TransientHashSet: {
  /** A temporarily mutable HashSet. Once persisted, any further operations on a
   *  TransientHashSet instance will throw. Intended for implementing bulk mutation
   *  operations efficiently.
   */

  type t 'a;
  /** The TransientHashSet type. */

  let add: 'a => (t 'a) => (t 'a);
  /** [add value transient] adds [value] to [transient].
   *
   *  Complexity: O(log32 N)
   */

  let addAll: (Seq.t 'a) => (t 'a) => (t 'a);
  /** [addAll values transient] adds all elements in [values] to [transient].
   *
   *  Complexity: O(N log32 N)
   */

  let contains: 'a => (t 'a) => bool;
  /** [contains value transient] returns true if any element in [transient] is equal to [value].
   *
   *  Complexity: O(log32 N)
   */

  let count: (t 'a) => int;
  /** [count transient] returns the number of elements in [transient].
   *
   *  Complexity: O(1)
   */

  let empty: unit => (t 'a);
  /** [empty ()] return a new empty TransientHashSet with the HashStrategy.structuralCompare hash strategy. */

  let emptyWith: (HashStrategy.t 'a) => (t 'a);
  /** [emptyWith strategy]  return a new empty TransientHashSet with the [strategy] hash strategy */

  let isEmpty: (t 'a) => bool;
  /** [isEmpty transient] returns true if [transient] contains no elements. */

  let isNotEmpty: (t 'a) => bool;
  /** [isNotEmpty transient] returns true if [transient] contains at least one element. */

  let persist: (t 'a) => (HashSet.t 'a);
  /** [persist transient] returns a persisted HashSet. Further attempts to access or mutate [transient]
   *  will throw.
   */

  let remove: 'a => (t 'a) => (t 'a);
  /** [remove value transient] removes [value] from [transient].
   *
   *  Complexity: O(log32 N)
   */

  let removeAll: (t 'a) => (t 'a);
  /** [removeAll transient] removes all values from [transient].
   *
   *  Complexity: O(1)
   */
};
/*
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
  let keys: (t 'k 'v) => (Set.t 'k);
  let none: ('k => 'v => bool) => (t 'k 'v) => bool;
  let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);
  let putAllValues: 'k => (Seq.t 'v) => (t 'k 'v) => (t 'k 'v);
  let reduce: ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let remove: 'k => (t 'k 'v) => (t 'k 'v);
  let removeAll: (t 'k 'v) => (t 'k 'v);
  let some: ('k => 'v => bool) => (t 'k 'v) => bool;
  let toSet: (t 'k 'v) => (Set.t ('k, 'v));
  let toSeq: (t 'k 'v) => (Seq.t ('k, 'v));
  let tryFind: ('k => 'v => bool) => (t 'k 'v) => (option ('k, 'v));
  let values: (t 'k 'v) => (Seq.t 'v);
};
*/
let module rec IntMap: {
  /** A Map optimized for integer keys. */

  type t 'a;
  /** The IntMap type. */

  let alter: int => ((option 'a) => (option 'a)) => (t 'a) => (t 'a);
  /** [alter key f map] enables efficient deep updates to an existing
   *  mapping from [key] in [map]. If [map] already has a mapping from [key],
   *  [f] will be called with Some, otherwise it will be called with None.
   *  If [f] returns None, alter returns a new IntMap without a mapping from [key].
   *  If [f] returns Some, alter returns a new IntMap with an updated
   *  mapping from [key].
   */

  let contains: int => 'a => (t 'a) => bool;
  /** [contains key value map] returns true if [map] contains the [key] [value] pair,
   *  using structural equality to equate [value].
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let containsWith: (Equality.t 'a) => int => 'a => (t 'a) => bool;
  /** [containsWith equals key value map] returns true if [map] contains the [key] [value] pair,
   *  using [equals] to equate [value].
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let containsKey: int => (t 'a) => bool;
  /** [containsKey key map] returns true if [map] contains a mapping from [key]. */

  let count: (t 'a) => int;
  /** [count map] returns the number of key/value pairs in [map]. */

  let empty: (t 'a);
  /** The empty IntMap. */

  let equals: (t 'a) => (t 'a) => bool;
  /** [equals this that] equates [this] and [that]. Structural equality is used to equate values. */

  let equalsWith: (Equality.t 'a) => (t 'a) => (t 'a) => bool;
  /** [equalsWith equals this that] equates [this] and [that].
   *  [equals] is used to equate values.
   */

  let every: (int => 'a => bool) => (t 'a) => bool;
  /** [every f map] returns true if the predicate [f] returns true for every
   *  key/value pair in [map], otherwise false. If [map] is empty, returns true.
   */

  let find: (int => 'a => bool) => (t 'a) => (int, 'a);
  /** [find f map] returns the first key/value pair for which the predicate [f] returns true.
   *  If no value is found, an exception is thrown.
   */

  let forEach: (int => 'a => unit) => (t 'a) => unit;
  /** [forEach f map] iterates through [map], invoking [f] for each key/value pair. */

  let fromMap: (Map.t int 'a) => (t 'a);
  /** [fromMap map] returns an IntMap including the key/value pairs in [map]. */

  let fromSeq: (Seq.t (int, 'a)) => (t 'a);
  /** [fromSeq seq] returns an IntMap including the key/value pairs in [seq]. */

  let get: int => (t 'a) => 'a;
  /** [get key map] returns the value associated with [key] or throws */

  let hash: (Hash.t (t 'a));
  /** [hash map] hashes [map], hashing values using structural hashing. */

  let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
  /** [hashWith hash map] hashes [map], hashing values using [hash]. */

  let isEmpty: (t 'a) => bool;
  /** [isEmpty map] returns true if [map] contains no key/value pairs. */

  let isNotEmpty: (t 'a) => bool;
  /** [isNotEmpty map] returns true if [map] contains at least one key/value pair. */

  let keys: (t 'a) => (Set.t int);
  /** [keys map] returns a Set view of keys in [map]. */

  let map: (int => 'a => 'b) => (t 'a) => (t 'b);
  /** [map f map] returns a new IntMap whose values are the result of
   *  applying [f] each key/value pair in [map].
   */

  let merge: (int => (option 'vAcc) => (option 'v) => (option 'vAcc)) => (t 'v) => (t 'vAcc) => (t 'vAcc);
  /** [merge f next acc] returns a new IntMap that is the result of applying [f] to
   *  the union of keys in [next] and [acc]. For each key, [f] is called with the mapped
   *  values in [next] and [acc] or None. If [f] returns Some the value is added to the returned Map.
   */

  let mutate: (t 'a) => (TransientIntMap.t 'a);
  /** [mutate map] returns a TransientIntMap containing the same key/values pairs as [map].
   *
   *  Complexity: O(1)
   */

  let none: (int => 'a => bool) => (t 'a) => bool;
  /** [none f map] returns true if the predicate [f] returns false for
   *  every key/value pair in [map], otherwise true. If [map] is empty, returns true.
   */

  let put: int => 'a => (t 'a) => (t 'a);
  /** [put key value map] returns a new IntMap containing a mapping from [key] to [value].
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let putAll: (Seq.t (int, 'a)) => (t 'a) => (t 'a);
  /** [putAll key values map] returns a new IntMap containing the key/value pairs in [values].
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let reduce: ('acc => int => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  /** [reduce f acc map] applies the accumulator function [f] to each key/value pair in [map]
   *  with the specified seed value [acc], returning the final accumulated value.
   */

  let remove: int => (t 'a) => (t 'a);
  /** [remove key map] returns a new IntMap without any mapping from [key].
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let removeAll: (t 'a) => (t 'a);
  /** [removeAll map] returns an empty IntMap with the same key HashStrategy as [map].
   *
   *  Complexity: O(1)
   */

  let some: (int => 'a => bool) => (t 'a) => bool;
  /** [some f map] returns true if the predicate [f] returns true for
   *  any key/value pair in [map], otherwise false. If [map] is empty, returns false.
   */

  let toSet: (t 'a) => (Set.t (int, 'a));
  /** [toSet map] returns a Set view of key/value pairs in [map], using structural equality
   *  to equate values.
   */

  let toSetWith: (Equality.t 'a) => (t 'a) => (Set.t (int, 'a));
  /** [toSetWith equals map] returns a Set view of key/value pairs in [map],
   *  using [equals] to equate values.
   */

  let toMap: (t 'a) => (Map.t int 'a);
  /** [toMap map] returns a Map view of [map]. */

  let toSeq: (t 'a) => (Seq.t ((int, 'a)));
  /** [toSeq map] returns a Seq of the key/value pairs in [map]. */

  let tryFind: (int => 'a => bool) => (t 'a) => (option (int, 'a));
  /** [toSeq map] returns a Seq of the key/value pairs in [map]. */

  let tryGet: int => (t 'a) => (option 'a);
  /** [tryGet key map] returns the value associated with [key] or None */

  let values: (t 'a) => (Seq.t 'a);
  /** [values map] returns a Seq of non-unique values in [map]. */
}

and TransientIntMap: {
  type t 'a;

  let alter: int => ((option 'a) => (option 'a)) => (t 'a) => (t 'a);
  /** [alter key f transient] enables efficient deep updates to an existing
   *  mapping from [key] in [transient]. If [transient] already has a mapping from [key],
   *  [f] will be called with Some, otherwise it will be called with None.
   *  If [f] returns None, alter removes any mapping from [key] in [transient].
   *  If [f] returns Some, alter returns add or updates the mapping
   *  from [key] in [transient].
   */

  let count: (t 'a) => int;
  /** [count map] returns the number of key/value pairs in [map]. */

  let empty: unit => (t 'a);
  /** [empty ()] returns a new empty TransientIntMap. */

  let isEmpty: (t 'a) => bool;
  /** [isEmpty map] returns true if [map] contains no key/value pairs. */

  let isNotEmpty: (t 'a) => bool;
  /** [isNotEmpty map] returns true if [map] contains at least one key/value pair. */

  let persist: (t 'a) => (IntMap.t 'a);
  /** [persist transient] returns a persisted HashBiMap. Further attempts to access or mutate [transient]
   *  will throw.
   */

  let put: int => 'a => (t 'a) => (t 'a);
  /** [put key value transient] adds the mapping of [key] to [value] to [transient].
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let putAll: (Seq.t (int, 'a)) => (t 'a) => (t 'a);
  /** [putAll seq transient] adds the key/value pairs in [seq] to [transient].
   *  Key value pairs in seq replace existing mappings in [transient].
   */

  let remove: int => (t 'a) => (t 'a);
  /** [remove key transient] removes from [transient] any mappings from [key].
   *
   *  Complexity: O(log32 N), effectively O(1)
   */

  let removeAll: (t 'a) => (t 'a);
  /** [removeAll transient] removes all mappings from [transient].
   *
   *  Complexity: O(1)
   */

  let tryGet: int => (t 'a) => (option 'a);
  /** [tryGet key transient] returns the value associated with [key] or None
   *
   *  Complexity: O(log32 N), effectively O(1)
   */
};

let module rec IntSet: {
  /** A set implementation optimized for storing sparse ints. */

  type t;
  /** The IntSet type. */

  let add: int => t => t;
  /** [add value set] returns a new IntSet with [value], if [set] does not already contain [value].
   *
   *  Complexity: O(log32 N)
   */

  let addAll: (Seq.t int) => t => t;
  /** [addAll values set] returns a new IntSet with all elements in [values] added.
   *
   *  Complexity: O(N log32 N)
   */

  let contains: int => t => bool;
  /** [contains value set] returns true if any element in [set] is equal to [value].
   *
   *  Complexity: O(log32 N)
   */

  let count: t => int;
  /** [count set] returns the number of elements in [set].
   *
   *  Complexity: O(1)
   */

  let empty: t;
  /** The empty IntSet. */

  let equals: t => t => bool;
  /** [equals this that] compares [this] and [that] for equality. */

  let every: (int => bool) => t => bool;
  /** [every f set] returns true if the predicate [f] returns true for every
   *  element in [set], otherwise false. If [set] is empty, returns true.
   */

  let find: (int => bool) => t => int;
  /** [find f set] returns the first value for which the predicate [f] returns true.
   *  If no value is found, an exception is thrown.
   */

  let forEach: (int => unit) => t => unit;
  /** [forEach f set] iterates through [set], invoking [f] for each element until [seq] completes. */

  let fromSeq: (Seq.t int) => t;
  /** [fromSeq seq] returns an IntSet including the values in [seq]. */

  let hash: (Hash.t t);
  /** [hash set] hashes [set], hashing elements using structural hashing. */

  let intersect: t => t => t;
  /** [intersect this that] returns a new IntSet containing only elements that appear in
   *  both [this] and [that].
   *
   *  Complexity: Currently O(N), ideally O(log32 N).
   */

  let isEmpty: t => bool;
  /** [isEmpty set] returns true if [set] contains no elements. */

  let isNotEmpty: t => bool;
  /** [isNotEmpty set] returns true if [set] contains at least one element. */

  let mutate: t => TransientIntSet.t;
  /** [mutate set] returns a TransientIntSet containing the same elements as [set].
   *
   *  Complexity: O(1)
   */

  let none: (int => bool) => t => bool;
  /** [none f set] returns true if the predicate [f] returns false for every
   *  elements in [set], otherwise true. If [set] is empty, returns true.
   */

  let reduce: ('acc => int => 'acc) => 'acc => t => 'acc;
  /** [reduce f acc set] applies the accumulator function [f] to each element in [set]
   *  with the specified seed value [acc], returning the final accumulated value.
   */

  let remove: int => t => t;
  /** [remove value set] returns a new IntSet without [value].
   *
   *  Complexity: O(log32 N)
   */

  let removeAll: t => t;
  /** [removeAll set] returns the empty IntSet.
   *
   *  Complexity: O(1)
   */

  let some: (int => bool) => t => bool;
  /** [some f set] returns true if the predicate [f] returns true for any element in [set], otherwise false.
   *  If [set] is empty, returns false.
   */

  let subtract: t => t => t;
  /** [substract this that] returns a new IntSet with the elements of [that] removed from [this].
   *
   *  Complexity: O(N) currently, ideally O(log32 N).
   */

  let toSet: t => (Set.t int);
  /** [toSet set] returns a Set view of [set] */

  let toMap: t => (Map.t int int);
  /** [toMap set] returns a Map view of [set] as mapping of values to themselves. */

  let toSeq: t => (Seq.t int);
  /** [toSeq set] returns a Seq of the values in [set]. */

  let tryFind: (int => bool) => t => (option int);
  /** [tryFind f set] returns the first value for which the predicate [f] returns true or None. */

  let union: t => t => t;
  /** [union this that] returns a new IntSet containing all the elements in
   *  both [this] and [that].
   *
   *  Complexity: currently O(N), ideally O(log32 N).
   */
}

and TransientIntSet: {
  /** A temporarily mutable IntSet. Once persisted, any further operations on a
   *  TransientIntSet instance will throw. Intended for implementing bulk mutation
   *  operations efficiently.
   */

  type t;
  /** The TransientIntSet type. */

  let add: int => t => t;
  /** [add value transient] adds [value] to [transient].
   *
   *  Complexity: O(log32 N)
   */

  let addAll: (Seq.t int) => t => t;
  /** [addAll values transient] adds all elements in [values] to [transient].
   *
   *  Complexity: O(N log32 N)
   */

  let contains: int => t => bool;
  /** [contains value transient] returns true if any element in [transient] is equal to [value].
   *
   *  Complexity: O(log32 N)
   */

  let count: t => int;
  /** [count transient] returns the number of elements in [transient].
   *
   *  Complexity: O(1)
   */

  let empty: unit => t;
  /** [empty ()] return a new empty TransientIntSet. */

  let isEmpty: t => bool;
  /** [isEmpty transient] returns true if [transient] contains no elements. */

  let isNotEmpty: t => bool;
  /** [isNotEmpty transient] returns true if [transient] contains at least one element. */

  let persist: t => IntSet.t;
  /** [persist transient] returns a persisted IntSet. Further attempts to access or mutate [transient]
   *  will throw.
   */

  let remove: int => t => t;
  /** [remove value transient] removes [value] from [transient].
   *
   *  Complexity: O(log32 N)
   */

  let removeAll: t => t;
  /** [removeAll transient] removes all values from [transient].
   *
   *  Complexity: O(1)
   */
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
  /** OCaml option type. Can be considered a set of zero or one elements.
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

  let toSet: (option 'a) => (Set.t 'a);
  /* [toSet option] returns a Set view of the option using structural equality equate values. */

  let toSetWith: (Equality.t 'a) => (option 'a) => (Set.t 'a);
  /* [toSetWith equals option] returns a Set view of the option using [equals] equate values. */

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
  /** AVL tree based Map. */

  type t 'k 'v;
  /** The SortedMap type. */

  let alter: 'k => (option 'v => option 'v) => (t 'k 'v) => (t 'k 'v);
  /** [alter key f map] enables efficient deep updates to an existing
   *  mapping from [key] in [map]. If [map] already has a mapping from [key],
   *  [f] will be called with Some, otherwise it will be called with None.
   *  If [f] returns None, alter returns a new SortedMap without a mapping from [key].
   *  If [f] returns Some, alter returns a new SortedMap with an updated
   *  mapping from [key].
   */

  let compare: (Comparator.t (t 'k 'v));
  /** A comparator that compares two SortedMap instances, comparing values using structural
   *  comparison. [compare this that] will throw if [this] and [that] do not use the
   *  same key comparison function.
   */

  let compareWith: (Comparator.t 'v) => (Comparator.t (t 'k 'v));
  /** [compareWith comparator this that] returns a comparator that compares two SortedMap instances,
   *  comparing values with [comparator]. Throws if [this] and [that] do not use the
   *  same key comparison function.
   */

  let contains: 'k => 'v => (t 'k 'v) => bool;
  /** [contains key value map] returns true if [map] contains the [key] [value] pair,
   *  using structural equality to equate [value].
   *
   *  Complexity: O(log N)
   */

  let containsWith: (Equality.t 'v) => 'k => 'v => (t 'k 'v) => bool;
  /** [containsWith equals key value map] returns true if [map] contains the [key] [value] pair,
   *  using [equals] to equate [value].
   *
   *  Complexity: O(log N)
   */

  let containsKey: 'k => (t 'k 'v) => bool;
  /** [containsKey key map] returns true if [map] contains a mapping from [key].
   *
   *  Complexity: O(log N)
   */

  let count: (t 'k 'v) => int;
  /** [count map] returns the number of key/value pairs in [map]. */

  let empty: (t 'k 'v);
  /** The empty SortedMap using the structural comparator. */

  let emptyWith: (Comparator.t 'k) => (t 'k 'v);
  /** [emptyWith comparator] returns an empty SortedMap using the provided comparator. */

  let equals: (t 'k 'v) => (t 'k 'v) => bool;
  /** [equals this that] equates [this] and [that]. Structural equality is used to equate values. */

  let equalsWith: (Equality.t 'v) => (t 'k 'v) => (t 'k 'v) => bool;
  /** [equalsWith equals this that] equates [this] and [that].
   *  [equals] is used to equate values.
   */

  let every: ('k => 'v => bool) => (t 'k 'v) => bool;
  /** [every f map] returns true if the predicate [f] returns true for every
   *  key/value pair in [map], otherwise false. If [map] is empty, returns true.
   */

  let find: ('k => 'v => bool) => (t 'k 'v) => ('k, 'v);
  /** [find f map] returns the first key/value pair for which the predicate [f] returns true.
   *  If no value is found, an exception is thrown.
   */

  let first: (t 'k 'v) => ('k, 'v);
  /** [first map] returns the first key/value pair in [set] or throws. */

  let forEach: ('k => 'v => unit) => (t 'k 'v) => unit;
  /** [forEach f map] iterates through [map], invoking [f] for each key/value pair. */

  let fromMap: (Map.t 'k 'v) => (t 'k 'v);
  /** [fromMap map] returns a SortedMap including the key/value pairs in [map]
   *  using the structural comparison.
   */

  let fromMapWith: (Comparator.t 'k) => (Map.t 'k 'v) => (t 'k 'v);
  /** [fromSeqWith comparator map] returns a SortedMap including the key/value pairs in [map]
   *  using the provided Comparator.
   */

  let fromSeq: (Seq.t ('k, 'v)) => (t 'k 'v);
  /** [fromSeq seq] returns a SortedMap including the key/value pairs in [seq]
   *  using the structural comparison.
   */

  let fromSeqWith: (Comparator.t 'k) => (Seq.t ('k, 'v)) => (t 'k 'v);
  /** [fromSeqWith comparator seq] returns a SortedMap including the key/value pairs in [seq]
   *  using the provided Comparator.
   */

  let get: 'k => (t 'k 'v) => 'v;
  /** [get key map] returns the value associated with [key] or throws */

  let hash: (Hash.t (t 'k 'v));
  /** [hash map] hashes [map], hashing keys and values using structural hashing. */

  let hashWith: (Hash.t 'k) => (Hash.t 'v) => (Hash.t (t 'k 'v));
  /** [hashWith keyHash valueHash map] hashes [map], hashing keys and
   *  values using [keyHash] and [valueHash] respectively.
   */

  let isEmpty: t 'k 'v => bool;
  /** [isEmpty map] returns true if [map] contains no key/value pairs. */

  let isNotEmpty: t 'k 'v => bool;
  /** [isNotEmpty map] returns true if [map] contains at least one key/value pair. */

  let keys: (t 'k 'v) => (Set.t 'k);
  /** [keys map] returns a Set view of keys in [map]. */

  let last: (t 'k 'v) => ('k, 'v);
  /** [last map] returns the last key/value pair in [set] or throws. */

  let map: ('k => 'a => 'b) => (t 'k 'a) => (t 'k 'b);
  /** [map f map] returns a new SortedMap whose values are the result of
   *  applying [f] each key/value pair in [map].
   */

  let merge: ('k => (option 'vAcc) => (option 'v) => (option 'vAcc)) => (t 'k 'v) => (t 'k 'vAcc)  => (t 'k 'vAcc);
  /** [merge f next acc] returns a new SortedMap that is the result of applying [f] to
   *  the union of keys in [next] and [acc]. For each key, [f] is called with the mapped
   *  values in [next] and [acc] or None. If [f] returns Some the value is added to the returned Map.
   */

  let none: ('k => 'v => bool) => (t 'k 'v) => bool;
  /** [none f map] returns true if the predicate [f] returns false for
   *  every key/value pair in [map], otherwise true. If [map] is empty, returns true.
   */

  let put: 'k => 'v => (t 'k 'v) => (t 'k 'v);
  /** [put key value map] returns a new SortedMap containing a mapping from [key] to [value].
   *
   *  Complexity: O(log N)
   */

  let putAll: (Seq.t ('k, 'v)) => (t 'k 'v) => (t 'k 'v);
  /** [putAll key values map] returns a new SortedMap containing the key/value pairs in [values].
   *
   *  Complexity: O(log N)
   */

  let reduce: ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  /** [reduce f acc map] applies the accumulator function [f] to each key/value pair in [map]
   *  with the specified seed value [acc], returning the final accumulated value.
   */

  let reduceRight: ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  /** [reduceRight f acc map] applies the accumulator function [f] to each key/value pair in [map]
   *  in reverse order with the specified seed value [acc], returning the final accumulated value.
   */

  let remove: 'k => (t 'k 'v) => (t 'k 'v);
  /** [remove key map] returns a new SortedMap without any mapping from [key].
   *
   *  Complexity: O(log N)
   */

  let removeAll: (t 'k 'v) => (t 'k 'v);
  /** [removeAll map] returns an empty SortedMap with the same key Comparator as [map].
   *
   *  Complexity: O(1)
   */

  let removeFirst: (t 'k 'v) => (t 'k 'v);
  /** [removeFirst map] returns a new SortedMap without the first element.
   *
   *  Complexity: O(log N)
   */

  let removeLast: (t 'k 'v) => (t 'k 'v);
  /** [removeLast map] returns a new SortedMap without the last element.
   *
   *  Complexity: O(log N)
   */

  let some: ('k => 'v => bool) => (t 'k 'v) => bool;
  /** [some f map] returns true if the predicate [f] returns true for
   *  any key/value pair in [map], otherwise false. If [map] is empty, returns false.
   */

  let toSet: (t 'k 'v) => (Set.t ('k, 'v));
  /** [toSet map] returns a Set view of key/value pairs in [map], using structural equality
   *  to equate values.
   */

  let toSetWith: (Equality.t 'v) => (t 'k 'v) => (Set.t ('k, 'v));
  /** [toSetWith equals map] returns a Set view of key/value pairs in [map],
   *  using [equals] to equate values.
   */

  let toMap: (t 'k 'v) => (Map.t 'k 'v);
  /** [toMap map] returns a Map view of [map]. */

  let toSeq: (t 'k 'v) => (Seq.t ('k, 'v));
  /** [toSeq map] returns a Seq of the key/value pairs in [map]. */

  let tryFind: ('k => 'v => bool) => (t 'k 'v) => (option ('k, 'v));
  /** [find f map] returns the first key/value pair for which the predicate [f] returns true or None. */

  let tryFirst: (t 'k 'v) => (option ('k, 'v));
  /** [tryFirst map] returns the first key/value pair in [set] or None. */

  let tryLast: (t 'k 'v) => (option ('k, 'v));
  /** [tryLast map] returns the last key/value pair in [set] or None. */

  let tryGet: 'k => (t 'k 'v) => (option 'v);
  /** [tryGet key map] returns the value associated with [key] or None */

  let values: (t 'k 'v) => (Seq.t 'v);
  /** [values map] returns a Seq of non-unique values in [map]. */
};

let module SortedSet: {
  /** AVL tree based Set implementation. */

  type t 'a;
  /** The SortedSet type */

  let add: 'a => (t 'a) => (t 'a);
  /** [add value set] returns a new SortedSet with [value], if [set] does not already contain [value].
   *
   *  Complexity: O(log N)
   */

  let addAll: (Seq.t 'a) => (t 'a) => (t 'a);
  /** [addAll values set] returns a new SortedSet with all elements in [values] added.
   *
   *  Complexity: O(N log N)
   */

  let compare: (Comparator.t (t 'a));
  /** A comparator that compares two SortedSet instances.
   *  [compare this that] will throw if [this] and [that] do not use the
   *  same comparison function.
   */

  let contains: 'a => (t 'a) => bool;
  /** [contains value set] returns true if any element in [set] is equal to [value].
   *
   *  Complexity: O(log N)
   */

  let count: (t 'a) => int;
  /** [count set] returns the number of elements in the set.
   *
   *  Complexity: O(1)
   */

  let empty: (t 'a);
  /** The empty SortedSet using structural comparison */

  let emptyWith: (Comparator.t 'a) => (t 'a);
  /** [empty comparator] returns an empty SortedSet using the provided [comparator]. */

  let equals: (t 'a) => (t 'a) => bool;
  /** [equals this that] compares [this] and [that] for equality. Two SortedSet are only
   *  equal if they use the same the comparison function and contain the same values.
   */

  let every: ('a => bool) => (t 'a) => bool;
  /** [every f set] returns true if the predicate [f] returns true for every
   *  element in [set], otherwise false. If [set] is empty, returns true.
   */

  let find: ('a => bool) => (t 'a) => 'a;
  /** [find f set] returns the first value for which the predicate [f] returns true.
   *  If no value is found, an exception is thrown.
   */

  let first: (t 'a) => 'a;
  /** [first set] returns the first element in [set] or throws.
   *
   * Complexity: O(log n)
   */

  let forEach: ('a => unit) => (t 'a) => unit;
  /** [forEach f set] iterates through [set], invoking [f] for each element until [seq] completes. */

  let fromSeq: (Seq.t 'a) => (t 'a);
  /** [fromSeq seq] returns a SortedSet including the values in [seq] using structural comparison. */

  let fromSeqWith: (Comparator.t 'a)  => (Seq.t 'a) => (t 'a);
  /** [fromSeq comparator seq] returns a SortedSet including the values in [seq]
   *  the provided [comparator] comparison function.
   */

  let hash: (Hash.t (t 'a));
  /** [hash set] hashes [set], hashing elements using structural hashing. */

  let hashWith: (Hash.t 'a) => (Hash.t (t 'a));
  /** [hashWith hash set] hashes [set], hashing elements using [hash]. */

  let intersect: (t 'a) => (t 'a) => (t 'a);
  /** [intersect this that] returns a new SortedSet containing only elements that appear in
   *  both [this] and [that]. The returned SortedSet uses [this]'s comparator function.
   *
   *  Complexity: O(N) if [this] and [that] use different comparators. O(log N) if they use
   *  the same comparator (NOT IMPLEMENTED).
   */

  let isEmpty: (t 'a) => bool;
  /** [isEmpty set] returns true if [set] contains no elements. */

  let isNotEmpty: (t 'a) => bool;
  /** [isNotEmpty set] returns true if [set] contains at least one element. */

  let last: (t 'a) => 'a;
  /** [last set] returns the last element in [set] or throws.
   *
   * Complexity: O(log n)
   */

  let none: ('a => bool) => (t 'a) => bool;
  /** [none f set] returns true if the predicate [f] returns false for every
   *  elements in [set], otherwise true. If [set] is empty, returns true.
   */

  let reduce: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  /** [reduce f acc set] applies the accumulator function [f] to each element in [set]
   *  with the specified seed value [acc], returning the final accumulated value.
   */

  let reduceRight: ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  /** [reduceRight f acc set] applies the accumulator function [f] to each element in [set]
   *  in reverse order with the specified seed value [acc], returning the final accumulated value.
   */

  let remove: 'a => (t 'a) => (t 'a);
  /** [remove value set] returns a new SortedSet without [value].
   *
   *  Complexity: O(log N)
   */

  let removeAll: (t 'a) => (t 'a);
  /** [removeAll set] returns the empty SortedSet with the same comparator as [set].
   *
   *  Complexity: O(1)
   */

  let removeFirst: (t 'a) => (t 'a);
  /** [removeFirst set] returns a new SortedSet without the first element.
   *
   *  Complexity: O(log N)
   */

  let removeLast: (t 'a) => (t 'a);
  /** [removeLast set] returns a new SortedSet without the last element.
   *
   *  Complexity: O(log N)
   */

  let some: ('a => bool) => (t 'a) => bool;
  /** [some f set] returns true if the predicate [f] returns true for any element in [set], otherwise false.
   *  If [set] is empty, returns false.
   */

  let subtract: (t 'a) => (t 'a) => (t 'a);
  /** [substract this that] returns a new SortedSet with the elements of [that] removed from [this].
   *  The returned SortedSet uses [this]'s comparator function.
   *
   *  Complexity: O(N) if [this] and [that] use different comparators. O(log N) if they use
   *  the same comparator (NOT IMPLEMENTED).
   */

  let toSet: (t 'a) => (Set.t 'a);
  /** [toSet set] returns a Set view of [set] */

  let toMap: (t 'a) => (Map.t 'a 'a);
  /** [toMap set] returns a Map view of [set] as mapping of values to themselves. */

  let toSeq: (t 'a) => (Seq.t 'a);
  /** [toSeq set] returns a Seq of the values in [set]. */

  let tryFind: ('a => bool) => (t 'a) => (option 'a);
  /** [tryFind f set] returns the first value for which the predicate [f] returns true or None. */

  let tryFirst: (t 'a) => (option 'a);
  /** [tryFirst seq] returns first element in [seq] or None.
   *
   * Complexity: O(log n)
   */

  let tryLast: (t 'a) => (option 'a);
  /** [tryLast seq] returns the last element in [seq] or None.
   *
   *  Complexity: O(1)
   */

  let union: (t 'a) => (t 'a) => (t 'a);
  /** [union this that] returns a new SortedSet containing all the elements in
   *  both [this] and [that]. The returned SortedSet uses [this]'s comparator function.
   *
   *  Complexity: O(N) if [this] and [that] use different comparators. O(log N) if they use
   *  the same comparator (NOT IMPLEMENTED).
   */
};

let module Stack: {
  /** A singly-linked stack with an O(1) count operation. */

  type t 'a;
  /** The Stack type. */

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
/*
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
  let keys: (t 'k 'v) => (Set.t 'k);
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
  let rows: (t 'row 'column 'value) => (Set.t 'row);
  let some: ('row => 'column => 'value=> bool) => (t 'row 'column 'value) => bool;
  let toSeq: (t 'row 'column 'value) => (Seq.t ('row, 'column, 'value));
  let tryFind: ('row => 'column => 'value => bool) => (t 'row 'column 'value) => (option ('row, 'column, 'value));
  let tryGet: 'row => 'column => (t 'row 'column 'value) => (option 'value);
  let values: (t 'row 'column 'value) => (Seq.t 'value);
};
*/
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

  let toMap: (t 'a) => (Map.t int 'a);
  /** [toMap vec] returns a Map view of [vec] */

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
