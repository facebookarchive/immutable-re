/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open BiMap;
open Deque;
open Equality;
open HashMap;
open HashMultiset;
open HashSet;
open IntMap;
open Vector;

let isEmpty (count: 'collection => 'int) (collection: 'collection): bool =>
  (count collection) == 0;

let isNotEmpty (count: 'collection => 'int) (collection: 'collection): bool =>
  (count collection) != 0;

let module Hash = {
  type t 'a = Hash.hash 'a;

  let random = Hash.random;
};

let module Equality = {
  type t 'a = Equality.equality 'a;

  /* Will be available in Ocaml 4.03
  let bytes = Equality.bytes;
  let char = Equality.char;
  let int32 = Equality.int32;
  let int64 = Equality.int64;
  let nativeInt = Equality.nativeInt;
  let string = Equality.string;
  */

  let reference = Equality.reference;
  let structural = Equality.structural;
};

let module Ordering = {
  type t = Ordering.ordering;

  let equal = Ordering.equal;
  let greaterThan = Ordering.greaterThan;
  let lessThan = Ordering.lessThan;
};

let module Comparator = {
  type t 'a = Comparator.comparator 'a;

  let bytes = Comparator.bytes;
  let char = Comparator.char;
  let int32 = Comparator.int32;
  let int64 = Comparator.int64;
  let nativeInt = Comparator.nativeInt;
  let string = Comparator.string;
  let structural = Comparator.structural;
};

let module SeqInternal = Seq;
let module Seq = {
  type t 'a = Seq.seq 'a;

  let buffer = Seq.buffer;
  let concat = Seq.concat;
  let concatAll = Seq.concatAll;
  let concatMap = Seq.concatMap;
  let count = Seq.count;
  let defer = Seq.defer;
  let distinctUntilChanged = Seq.distinctUntilChanged;
  let distinctUntilChangedWith = Seq.distinctUntilChangedWith;
  let doOnNext = Seq.doOnNext;
  let empty = Seq.empty;
  let equals = Seq.equals;
  let equalsWith = Seq.equalsWith;
  let every = Seq.every;
  let filter = Seq.filter;
  let find = Seq.find;
  let first = Seq.first;
  let flatMap = Seq.flatMap;
  let flatten = Seq.flatten;
  let forEach = Seq.forEach;
  let hash = Seq.hash;
  let inRange = Seq.inRange;
  let isEmpty = Seq.isEmpty;
  let isNotEmpty = Seq.isNotEmpty;
  let last = Seq.last;
  let map = Seq.map;
  let none = Seq.none;
  let reduce = Seq.reduce;
  let repeat = Seq.repeat;
  let return = Seq.return;
  let scan = Seq.scan;
  let skip = Seq.skip;
  let skipWhile = Seq.skipWhile;
  let some = Seq.some;
  let startWith = Seq.startWith;
  let take = Seq.take;
  let takeWhile = Seq.takeWhile;
  let tryFind = Seq.tryFind;
  let tryFirst = Seq.tryFirst;
  let tryGet = Seq.tryGet;
  let tryLast = Seq.tryLast;
  let zip = Seq.zip;
  let zip2 = Seq.zip2;
  let zip3 = Seq.zip3;
  let zipLongest = Seq.zipLongest;
  let zipLongest2 = Seq.zipLongest2;
  let zipLongest3 = Seq.zipLongest3;
};

let module Collection = {
  type t 'a = Collection.collection 'a;

  let contains = Collection.contains;
  let count = Collection.count;
  let empty = Collection.empty;
  let equals = Collection.equals;
  let hash = Collection.hash;
  let intersect = Collection.intersect;
  let isEmpty collection => isEmpty count collection;
  let isNotEmpty collection => isNotEmpty count collection;
  let subtractFrom = Collection.subtractFrom;
  let toSeq = Collection.toSeq;
  let union = Collection.union;
};

let module Keyed = {
  type t 'k 'v = Keyed.keyed 'k 'v;

  let contains = Keyed.contains;
  let containsKey = Keyed.containsKey;
  let count = Keyed.count;
  let empty = Keyed.empty;
  let equals = Keyed.equals;
  let equalsWith = Keyed.equalsWith;
  let hash = Keyed.hash;
  let isEmpty keyed => isEmpty count keyed;
  let isNotEmpty keyed => isNotEmpty count keyed;
  let keys = Keyed.keys;
  let map = Keyed.map;
  let mapWithKey = Keyed.mapWithKey;
  let ofCollection = Keyed.ofCollection;
  let toCollection = Keyed.toCollection;
  let toSeq = Keyed.toSeq;
  let tryGet = Keyed.tryGet;
};

let module Indexed = {
  type t 'a = Indexed.indexed 'a;

  let count = Indexed.count;
  let empty = Indexed.empty;
  let equals = Indexed.equals;
  let equalsWith = Indexed.equalsWith;
  let hash = Indexed.hash;
  let isEmpty indexed => isEmpty count indexed;
  let isNotEmpty indexed => isNotEmpty count indexed;
  let map = Indexed.map;
  let mapWithIndex = Indexed.mapWithIndex;
  let range = Indexed.range;
  let reverse = Indexed.reverse;
  let toKeyed = Indexed.toKeyed;
  let toSeq = Indexed.toSeq;
  let toSeqReversed = Indexed.toSeqReversed;
  let tryFirst = Indexed.tryFirst;
  let tryGet = Indexed.tryGet;
  let tryLast = Indexed.tryLast;
};

let module HashStrategy = {
  type t 'a = HashStrategy.hashStrategy 'a;

  let createWithComparator = HashStrategy.createWithComparator;
  let createWithEquality = HashStrategy.createWithEquality;
  let identity = HashStrategy.identity;
  let structuralCompare = HashStrategy.structuralCompare;
  let structuralEquality = HashStrategy.structuralEquality;
};

let module BiMap = {
  type t 'k 'v = BiMap.biMap 'k 'v;

  let count = BiMap.count;
  let empty = BiMap.empty;
  let emptyWith = BiMap.emptyWith;
  let isEmpty bimap => isEmpty count bimap;
  let isNotEmpty bimap => isNotEmpty count bimap;
  let fromSeq = BiMap.fromSeq;
  let fromSeqWith = BiMap.fromSeqWith;
  let inverse = BiMap.inverse;
  let mutate = BiMap.mutate;
  let put = BiMap.put;
  let putAll = BiMap.putAll;
  let reduce = BiMap.reduce;
  let reduceWithKey = BiMap.reduceWithKey;
  let remove = BiMap.remove;
  let removeAll = BiMap.removeAll;
  let toKeyed = BiMap.toKeyed;
  let toSeq = BiMap.toSeq;
  let tryGet = BiMap.tryGet;
  let tryPut = BiMap.tryPut;
};

let module TransientBiMap = {
  type t 'k 'v = transientBiMap 'k 'v;

  let count = TransientBiMap.count;
  let isEmpty transient => isEmpty count transient;
  let isNotEmpty transient => isNotEmpty count transient;
  let persist = TransientBiMap.persist;
  let put = TransientBiMap.put;
  let putAll = TransientBiMap.putAll;
  let remove = TransientBiMap.remove;
  let removeAll = TransientBiMap.removeAll;
  let tryGet = TransientBiMap.tryGet;
  let tryPut = TransientBiMap.tryPut;
};

let module CopyOnWriteArray = {
  type t 'a = CopyOnWriteArray.copyOnWriteArray 'a;

  let add = CopyOnWriteArray.add;
  let addFirst = CopyOnWriteArray.addFirst;
  let addLast = CopyOnWriteArray.addLast;
  let concat = CopyOnWriteArray.concat;
  let count = CopyOnWriteArray.count;
  let empty = CopyOnWriteArray.empty;
  let every = CopyOnWriteArray.every;
  let find = CopyOnWriteArray.find;
  let first = CopyOnWriteArray.first;
  let fromSeq = CopyOnWriteArray.fromSeq;
  let get = CopyOnWriteArray.get;
  let init = CopyOnWriteArray.init;
  let insertAt = CopyOnWriteArray.insertAt;
  let isEmpty = CopyOnWriteArray.isEmpty;
  let isNotEmpty = CopyOnWriteArray.isNotEmpty;
  let last = CopyOnWriteArray.last;
  let map = CopyOnWriteArray.map;
  let mapReverse = CopyOnWriteArray.mapReverse;
  let none = CopyOnWriteArray.none;
  let ofUnsafe = CopyOnWriteArray.ofUnsafe;
  let range = CopyOnWriteArray.range;
  let reduce = CopyOnWriteArray.reduce;
  let reduceRight = CopyOnWriteArray.reduceRight;
  let removeAt = CopyOnWriteArray.removeAt;
  let removeAll = CopyOnWriteArray.removeAll;
  let removeFirst = CopyOnWriteArray.removeFirst;
  let removeLast = CopyOnWriteArray.removeLast;
  let reverse = CopyOnWriteArray.reverse;
  let skip = CopyOnWriteArray.skip;
  let some = CopyOnWriteArray.some;
  let take = CopyOnWriteArray.take;
  let toIndexed = CopyOnWriteArray.toIndexed;
  let toSeq = CopyOnWriteArray.toSeq;
  let toSeqReversed = CopyOnWriteArray.toSeqReversed;
  let tryFind = CopyOnWriteArray.tryFind;
  let tryFirst = CopyOnWriteArray.tryFirst;
  let tryGet = CopyOnWriteArray.tryGet;
  let tryLast = CopyOnWriteArray.tryLast;
  let update = CopyOnWriteArray.update;
};

let module Deque = {
  type t 'a = Deque.deque 'a;

  let add = Deque.add;
  let addFirst = Deque.addFirst;
  let addLast = Deque.addLast;
  let count = Deque.count;
  let empty = Deque.empty;
  let every = Deque.every;
  let find = Deque.find;
  let first = Deque.first;
  let isEmpty deque => isEmpty count deque;
  let isNotEmpty deque => isNotEmpty count deque;
  let last = Deque.last;
  let map = Deque.map;
  let mapReverse = Deque.mapReverse;
  let mutate = Deque.mutate;
  let none = Deque.none;
  let reduce = Deque.reduce;
  let reduceRight = Deque.reduceRight;
  let removeAll = Deque.removeAll;
  let removeFirst = Deque.removeFirst;
  let removeLast = Deque.removeLast;
  let reverse = Deque.reverse;
  let some = Deque.some;
  let toSeq = Deque.toSeq;
  let toSeqReversed = Deque.toSeqReversed;
  let tryFind = Deque.tryFind;
  let tryFirst = Deque.tryFirst;
  let tryLast = Deque.tryLast;
};

let module TransientDeque = {
  type t 'a = transientDeque 'a;

  let add = TransientDeque.add;
  let addFirst = TransientDeque.addFirst;
  let addLast = TransientDeque.addLast;
  let count = TransientDeque.count;
  let empty = TransientDeque.empty;
  let first = TransientDeque.first;
  let isEmpty transient => isEmpty count transient;
  let isNotEmpty transient => isNotEmpty count transient;
  let last = TransientDeque.last;
  let persist = TransientDeque.persist;
  let removeAll = TransientDeque.removeAll;
  let removeFirst = TransientDeque.removeFirst;
  let removeLast = TransientDeque.removeLast;
  let reverse = TransientDeque.reverse;
  let tryFirst = TransientDeque.tryFirst;
  let tryLast = TransientDeque.tryLast;
};

let module HashMap = {
  type t 'k 'v  = HashMap.hashMap 'k 'v;

  let count = HashMap.count;
  let empty = HashMap.empty;
  let emptyWith = HashMap.emptyWith;
  let fromKeyed = HashMap.fromKeyed;
  let fromKeyedWith = HashMap.fromKeyedWith;
  let fromSeq = HashMap.fromSeq;
  let fromSeqWith = HashMap.fromSeqWith;
  let isEmpty map => isEmpty count map;
  let isNotEmpty map => isNotEmpty count map;
  let map = HashMap.map;
  let mapWithKey = HashMap.mapWithKey;
  let merge = HashMap.merge;
  let mutate = HashMap.mutate;
  let put = HashMap.put;
  let putAll = HashMap.putAll;
  let reduce = HashMap.reduce;
  let reduceWithKey = HashMap.reduceWithKey;
  let remove = HashMap.remove;
  let removeAll = HashMap.removeAll;
  let toKeyed = HashMap.toKeyed;
  let toSeq = HashMap.toSeq;
  let tryGet = HashMap.tryGet;
};

let module TransientHashMap = {
  type t 'k 'v = transientHashMap 'k 'v;

  let count = TransientHashMap.count;
  let isEmpty transient => isEmpty count transient;
  let isNotEmpty transient => isNotEmpty count transient;
  let persist = TransientHashMap.persist;
  let put = TransientHashMap.put;
  let putAll = TransientHashMap.putAll;
  let remove = TransientHashMap.remove;
  let removeAll = TransientHashMap.removeAll;
  let tryGet = TransientHashMap.tryGet;
};

let module HashMultiset = {
  type t 'a = HashMultiset.hashMultiset 'a;

  let add = HashMultiset.add;
  let addAll = HashMultiset.addAll;
  let contains = HashMultiset.contains;
  let count = HashMultiset.count;
  let empty = HashMultiset.empty;
  let emptyWith = HashMultiset.emptyWith;
  let fromSeq = HashMultiset.fromSeq;
  let fromSeqWith = HashMultiset.fromSeqWith;
  let get = HashMultiset.get;
  let isEmpty multiset => isEmpty count multiset;
  let isNotEmpty multiset => isNotEmpty count multiset;
  let mutate = HashMultiset.mutate;
  let reduce = HashMultiset.reduce;
  let remove = HashMultiset.remove;
  let removeAll = HashMultiset.removeAll;
  let set = HashMultiset.set;
  let toKeyed = HashMultiset.toKeyed;
  let toSeq = HashMultiset.toSeq;
};

let module TransientHashMultiset = {
  type t 'a = transientHashMultiset 'a;

  let add = TransientHashMultiset.add;
  let addAll = TransientHashMultiset.addAll;
  let contains = TransientHashMultiset.contains;
  let count = TransientHashMultiset.count;
  let get = TransientHashMultiset.get;
  let isEmpty transient => isEmpty count transient;
  let isNotEmpty transient => isNotEmpty count transient;
  let persist = TransientHashMultiset.persist;
  let remove = TransientHashMultiset.remove;
  let removeAll = TransientHashMultiset.removeAll;
  let set = TransientHashMultiset.set;
};

let module HashSet = {
  type t 'a = HashSet. hashSet 'a;

  let contains = HashSet.contains;
  let count = HashSet.count;
  let empty = HashSet.empty;
  let emptyWith = HashSet.emptyWith;
  let fromSeq = HashSet.fromSeq;
  let fromSeqWith = HashSet.fromSeqWith;
  let isEmpty set => isEmpty count set;
  let isNotEmpty set => isNotEmpty count set;
  let mutate = HashSet.mutate;
  let put = HashSet.put;
  let putAll = HashSet.putAll;
  let reduce = HashSet.reduce;
  let remove = HashSet.remove;
  let removeAll = HashSet.removeAll;
  let toCollection = HashSet.toCollection;
  let toKeyed = HashSet.toKeyed;
  let toSeq = HashSet.toSeq;
};

let module TransientHashSet = {
  type t 'a = transientHashSet 'a;

  let contains = TransientHashSet.contains;
  let count = TransientHashSet.count;
  let isEmpty transient => isEmpty count transient;
  let isNotEmpty transient => isNotEmpty count transient;
  let persist = TransientHashSet.persist;
  let put = TransientHashSet.put;
  let putAll = TransientHashSet.putAll;
  let remove = TransientHashSet.remove;
  let removeAll = TransientHashSet.removeAll;
};

let module HashSetMultimap = {
  type t 'k 'v = HashSetMultimap.hashSetMultimap 'k 'v;

  let count = HashSetMultimap.count;
  let empty = HashSetMultimap.empty;
  let emptyWith = HashSetMultimap.emptyWith;
  let isEmpty multimap => isEmpty count multimap;
  let isNotEmpty multimap => isNotEmpty count multimap;
  let get = HashSetMultimap.get;
  let put = HashSetMultimap.put;
  let reduce = HashSetMultimap.reduce;
  let reduceWithKey = HashSetMultimap.reduceWithKey;
  let remove = HashSetMultimap.remove;
  let removeAll = HashSetMultimap.removeAll;
  let toKeyed = HashSetMultimap.toKeyed;
  let toSeq = HashSetMultimap.toSeq;
};

let module IntMap = {
  type t 'a = IntMap.intMap 'a;

  let count = IntMap.count;
  let empty = IntMap.empty;
  let fromKeyed = IntMap.fromKeyed;
  let fromSeq = IntMap.fromSeq;
  let isEmpty map => isEmpty count map;
  let isNotEmpty map => isNotEmpty count map;
  let map = IntMap.map;
  let mapWithKey = IntMap.mapWithKey;
  let merge = IntMap.merge;
  let mutate = IntMap.mutate;
  let put = IntMap.put;
  let putAll = IntMap.putAll;
  let reduce = IntMap.reduce;
  let reduceWithKey = IntMap.reduceWithKey;
  let remove = IntMap.remove;
  let removeAll = IntMap.removeAll;
  let toKeyed = IntMap.toKeyed;
  let toSeq = IntMap.toSeq;
  let tryGet = IntMap.tryGet;
};

let module TransientIntMap = {
  type t 'a = transientIntMap 'a;

  let count = TransientIntMap.count;
  let isEmpty transient => isEmpty count transient;
  let isNotEmpty transient => isNotEmpty count transient;
  let persist = TransientIntMap.persist;
  let put = TransientIntMap.put;
  let putAll = TransientIntMap.putAll;
  let remove = TransientIntMap.remove;
  let removeAll = TransientIntMap.removeAll;
  let tryGet = TransientIntMap.tryGet;
};

let module List = {
  type t 'a = list 'a;

  let add = ImmList.add;
  let addFirst = ImmList.addFirst;
  let empty = ImmList.empty;
  let every = ImmList.every;
  let find = ImmList.find;
  let first = ImmList.first;
  let fromSeq = SeqInternal.toReversedList;
  let isEmpty = ImmList.isEmpty;
  let isNotEmpty = ImmList.isNotEmpty;
  let mapReverse = ImmList.mapReverse;
  let none = ImmList.none;
  let reduce = ImmList.reduce;
  let removeAll = ImmList.removeAll;
  let removeFirst = ImmList.removeFirst;
  let reverse = ImmList.reverse;
  let some = ImmList.some;
  let toSeq = SeqInternal.ofList;
  let tryFind = ImmList.tryFind;
  let tryFirst = ImmList.tryFirst;
};

let module Option = {
  type t 'a = option 'a;

  let count = Option.count;
  let empty = Option.empty;
  let filter = Option.filter;
  let flatMap = Option.flatMap;
  let flatten = Option.flatten;
  let forEach = Option.forEach;
  let get = Option.get;
  let isEmpty = Option.isEmpty;
  let isNotEmpty = Option.isNotEmpty;
  let map = Option.map;
  let none = Option.none;
  let reduce = Option.reduce;
  let return = Option.return;
  let some = Option.some;
  let toSeq = SeqInternal.ofOption;
};

let module SortedMap = {
  type t 'k 'v = SortedMap.sortedMap 'k 'v;

  let count = SortedMap.count;
  let empty = SortedMap.empty;
  let emptyWith = SortedMap.emptyWith;
  let fromKeyed = SortedMap.fromKeyed;
  let fromKeyedWith = SortedMap.fromKeyedWith;
  let fromSeq = SortedMap.fromSeq;
  let fromSeqWith = SortedMap.fromSeqWith;
  let isEmpty map => isEmpty count map;
  let isNotEmpty map => isNotEmpty count map;
  let map = SortedMap.map;
  let mapWithKey = SortedMap.mapWithKey;
  let merge = SortedMap.merge;
  let put = SortedMap.put;
  let putAll = SortedMap.putAll;
  let reduce = SortedMap.reduce;
  let reduceWithKey = SortedMap.reduceWithKey;
  let reduceRight = SortedMap.reduceRight;
  let reduceRightWithKey = SortedMap.reduceRightWithKey;
  let remove = SortedMap.remove;
  let removeAll = SortedMap.removeAll;
  let toKeyed = SortedMap.toKeyed;
  let toSeq = SortedMap.toSeq;
  let tryGet = SortedMap.tryGet;
};

let module SortedSet = {
  type t 'a = SortedSet.sortedSet 'a;

  let contains = SortedSet.contains;
  let count = SortedSet.count;
  let empty = SortedSet.empty;
  let emptyWith = SortedSet.emptyWith;
  let fromSeq = SortedSet.fromSeq;
  let fromSeqWith = SortedSet.fromSeqWith;
  let isEmpty set => isEmpty count set;
  let isNotEmpty set => isNotEmpty count set;
  let put = SortedSet.put;
  let putAll = SortedSet.putAll;
  let reduce = SortedSet.reduce;
  let reduceRight = SortedSet.reduceRight;
  let remove = SortedSet.remove;
  let removeAll = SortedSet.removeAll;
  let toCollection = SortedSet.toCollection;
  let toKeyed = SortedSet.toKeyed;
  let toSeq = SortedSet.toSeq;
};

let module Stack ={
  type t 'a = Stack.stack 'a;

  let add = Stack.add;
  let addAll = Stack.addAll;
  let addFirst = Stack.addFirst;
  let count = Stack.count;
  let empty = Stack.empty;
  let every = Stack.every;
  let find = Stack.find;
  let first = Stack.first;
  let fromList = Stack.fromList;
  let fromSeq = Stack.fromSeq;
  let isEmpty stack => isEmpty count stack;
  let isNotEmpty stack => isNotEmpty count stack;
  let mapReverse = Stack.mapReverse;
  let none = Stack.none;
  let reduce = Stack.reduce;
  let removeAll = Stack.removeAll;
  let removeFirst = Stack.removeFirst;
  let reverse = Stack.reverse;
  let some = Stack.some;
  let toList = Stack.toList;
  let toSeq = Stack.toSeq;
  let tryFind = Stack.tryFind;
  let tryFirst = Stack.tryFirst;
};

let module StackMultimap = {
  type t 'k 'v = StackMultimap.stackMultimap 'k 'v;

  let add = StackMultimap.add;
  let count = StackMultimap.count;
  let empty = StackMultimap.empty;
  let emptyWith = StackMultimap.emptyWith;
  let get = StackMultimap.get;
  let isEmpty multimap => isEmpty count multimap;
  let isNotEmpty multimap => isNotEmpty count multimap;
  let reduce = StackMultimap.reduce;
  let reduceWithKey = StackMultimap.reduceWithKey;
  let remove = StackMultimap.remove;
  let removeAll = StackMultimap.removeAll;
  let toKeyed = StackMultimap.toKeyed;
  let toSeq = StackMultimap.toSeq;
};

let module Table = {
  type t 'row 'column 'value = Table.table 'row 'column 'value;

  let count = Table.count;
  let empty = Table.empty;
  let emptyWith = Table.emptyWith;
  let isEmpty table => isEmpty count table;
  let isNotEmpty table => isNotEmpty count table;
  let put = Table.put;
  let reduce = Table.reduce;
  let reduceWithRowAndColumn = Table.reduceWithRowAndColumn;
  let remove = Table.remove;
  let removeAll = Table.removeAll;
  let removeRow = Table.removeRow;
  let toKeyed = Table.toKeyed;
  let toSeq = Table.toSeq;
  let tryGet = Table.tryGet;
};

let module Vector = {
  type t 'a = Vector.vector 'a;

  let add = Vector.add;
  let addFirst = Vector.addFirst;
  let addLast = Vector.addLast;
  /*let concat: (list (t 'a)) => (t 'a);*/
  let count = Vector.count;
  let every = Vector.every;
  let get = Vector.get;
  let empty = Vector.empty;
  let find = Vector.find;
  let first = Vector.first;
  let get = Vector.get;
  /*let insertAt: int => 'a => (t 'a) => (t 'a);*/
  let isEmpty = Vector.isEmpty;
  let isNotEmpty = Vector.isNotEmpty;
  let last = Vector.last;
  let map = Vector.map;
  let mapReverse = Vector.mapReverse;
  let mutate = Vector.mutate;
  let none = Vector.none;
  let reduce = Vector.reduce;
  let reduceRight = Vector.reduceRight;
  /*let removeAt: int => (t 'a) => (t 'a);*/
  let removeAll = Vector.removeAll;
  let removeFirst = Vector.removeFirst;
  let removeLast = Vector.removeLast;
  let reverse = Vector.reverse;
  let skip = Vector.skip;
  let some = Vector.some;
  let take = Vector.take;
  let toIndexed = Vector.toIndexed;
  let toSeq = Vector.toSeq;
  let toSeqReversed = Vector.toSeqReversed;
  let tryFind = Vector.tryFind;
  let tryFirst = Vector.tryFirst;
  let tryGet = Vector.tryGet;
  let tryLast = Vector.tryLast;
  let update = Vector.update;
};

let module TransientVector = {
  type t 'a = transientVector 'a;

  let add = TransientVector.add;
  let addFirst = TransientVector.addFirst;
  let addLast = TransientVector.addLast;
  let count = TransientVector.count;
  let empty = TransientVector.empty;
  let first = TransientVector.first;
  let get = TransientVector.get;
  let isEmpty transient => isEmpty count transient;
  let isNotEmpty transient => isNotEmpty count transient;
  let last = TransientVector.last;
  let persist = TransientVector.persist;
  let removeAll = TransientVector.removeAll;
  let removeFirst = TransientVector.removeFirst;
  let removeLast = TransientVector.removeLast;
  let reverse = TransientVector.reverse;
  let tryFirst = TransientVector.tryFirst;
  let tryGet = TransientVector.tryGet;
  let tryLast = TransientVector.tryLast;
  let update = TransientVector.update;
};
