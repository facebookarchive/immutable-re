/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

let module IteratorInternal = Iterator;
let module SequenceInternal = Sequence;
let module TransientDequeInternal = Deque.TransientDeque;
let module TransientHashBiMapInternal = HashBiMap.TransientHashBiMap;
let module TransientHashMapInternal = HashMap.TransientHashMap;
let module TransientHashMultisetInternal = HashMultiset.TransientHashMultiset;
let module TransientHashSetInternal = HashSet.TransientHashSet;
let module TransientIntMapInternal = IntMap.TransientIntMap;
let module TransientIntSetInternal = IntSet.TransientIntSet;
let module TransientVectorInternal = Vector.TransientVector;

let module Hash = {
  type t 'a = Hash.t 'a;

  let random = Hash.random;
  let structural = Hash.structural;
};

let module Equality = {
  type t 'a = Equality.t 'a;

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
  type t = Ordering.t;

  let equal = Ordering.equal;
  let greaterThan = Ordering.greaterThan;
  let lessThan = Ordering.lessThan;
};

let module Comparator = {
  type t 'a = Comparator.t 'a;

  let bytes = Comparator.bytes;
  let char = Comparator.char;
  let int32 = Comparator.int32;
  let int64 = Comparator.int64;
  let nativeInt = Comparator.nativeInt;
  let string = Comparator.string;
  let structural = Comparator.structural;
};

let module Comparable = Comparable;

let module HashStrategy = {
  type t 'a = HashStrategy.t 'a;

  let createWithComparator = HashStrategy.createWithComparator;
  let createWithEquality = HashStrategy.createWithEquality;
  let identity = HashStrategy.identity;
  let structuralCompare = HashStrategy.structuralCompare;
  let structuralEquality = HashStrategy.structuralEquality;
};

let module Iterator = {
  type t 'a = Iterator.t 'a;

  let concat = Iterator.concat;
  let concatAll = Iterator.concatAll;
  let count = Iterator.count;
  let doOnNext = Iterator.doOnNext;
  let empty = Iterator.empty;
  let filter = Iterator.filter;
  let flatMap = Iterator.flatMap;
  let flatten = Iterator.flatten;
  let forEach = Iterator.forEach;
  let hash = Iterator.hash;
  let hashWith = Iterator.hashWith;
  let map = Iterator.map;
  let reduce = Iterator.reduce;
  let return = Iterator.return;
};

let module Sequence = {
  type t 'a = Sequence.t 'a;

  let buffer = Sequence.buffer;
  let compare = Sequence.compare;
  let compareWith = Sequence.compareWith;
  let contains = Sequence.contains;
  let containsWith = Sequence.containsWith;
  let concat = Sequence.concat;
  let concatAll = Sequence.concatAll;
  let concatMap = Sequence.concatMap;
  let count = Sequence.count;
  let defer = Sequence.defer;
  let distinctUntilChanged = Sequence.distinctUntilChanged;
  let distinctUntilChangedWith = Sequence.distinctUntilChangedWith;
  let doOnNext = Sequence.doOnNext;
  let empty = Sequence.empty;
  let equals = Sequence.equals;
  let equalsWith = Sequence.equalsWith;
  let every = Sequence.every;
  let filter = Sequence.filter;
  let find = Sequence.find;
  let first = Sequence.first;
  let flatMap = Sequence.flatMap;
  let flatten = Sequence.flatten;
  let forEach = Sequence.forEach;
  let generate = Sequence.generate;
  let get = Sequence.get;
  let hash = Sequence.hash;
  let hashWith = Sequence.hashWith;
  let isEmpty = Sequence.isEmpty;
  let isNotEmpty = Sequence.isNotEmpty;
  let last = Sequence.last;
  let map = Sequence.map;
  let none = Sequence.none;
  let reduce = Sequence.reduce;
  let repeat = Sequence.repeat;
  let return = Sequence.return;
  let scan = Sequence.scan;
  let skip = Sequence.skip;
  let skipWhile = Sequence.skipWhile;
  let some = Sequence.some;
  let startWith = Sequence.startWith;
  let take = Sequence.take;
  let takeWhile = Sequence.takeWhile;
  let toIterator = Sequence.toIterator;
  let tryFind = Sequence.tryFind;
  let tryFirst = Sequence.tryFirst;
  let tryGet = Sequence.tryGet;
  let tryLast = Sequence.tryLast;
  let zip = Sequence.zip;
  let zip2 = Sequence.zip2;
  let zip3 = Sequence.zip3;
  let zipLongest = Sequence.zipLongest;
  let zipLongest2 = Sequence.zipLongest2;
  let zipLongest3 = Sequence.zipLongest3;
};

let module KeyedIterator = {
  type t 'k 'v = KeyedIterator.t 'k 'v;

  let count = KeyedIterator.count;
  let doOnNext = KeyedIterator.doOnNext;
  let empty = KeyedIterator.empty;
  let filter = KeyedIterator.filter;
  let flatMap = KeyedIterator.flatMap;
  let forEach = KeyedIterator.forEach;
  let hash = KeyedIterator.hash;
  let hashWith = KeyedIterator.hashWith;
  let keys = KeyedIterator.keys;
  let map = KeyedIterator.map;
  let reduce = KeyedIterator.reduce;
  let toIterator = KeyedIterator.toIterator;
  let values = KeyedIterator.values;
};

let module Set = {
  type t 'a = ImmSet.t 'a;

  let contains = ImmSet.contains;
  let count = ImmSet.count;
  let empty = ImmSet.empty;
  let equals = ImmSet.equals;
  let every = ImmSet.every;
  let find = ImmSet.find;
  let forEach = ImmSet.forEach;
  let hash = ImmSet.hash;
  let hashWith = ImmSet.hashWith;
  let intersect = ImmSet.intersect;
  let isEmpty = ImmSet.isEmpty;
  let isNotEmpty = ImmSet.isNotEmpty;
  let none = ImmSet.none;
  let reduce = ImmSet.reduce;
  let some = ImmSet.some;
  let subtract = ImmSet.subtract;
  let toIterator = ImmSet.toIterator;
  let toKeyedIterator = ImmSet.toKeyedIterator;
  let toMap = ImmMap.ofSet;
  let toSequence = ImmSet.toSequence;
  let tryFind = ImmSet.tryFind;
  let union = ImmSet.union;
};

let module Map = {
  type t 'k 'v = ImmMap.t 'k 'v;

  let contains = ImmMap.contains;
  let containsWith = ImmMap.containsWith;
  let containsKey = ImmMap.containsKey;
  let count = ImmMap.count;
  let empty = ImmMap.empty;
  let equals = ImmMap.equals;
  let equalsWith = ImmMap.equalsWith;
  let every = ImmMap.every;
  let find = ImmMap.find;
  let forEach = ImmMap.forEach;
  let get = ImmMap.get;
  let hash = ImmMap.hash;
  let hashWith = ImmMap.hashWith;
  let isEmpty = ImmMap.isEmpty;
  let isNotEmpty = ImmMap.isNotEmpty;
  let keys = ImmMap.keys;
  let map = ImmMap.map;
  let none = ImmMap.none;
  let reduce = ImmMap.reduce;
  let some = ImmMap.some;
  let toIterator = ImmMap.toIterator;
  let toKeyedIterator = ImmMap.toKeyedIterator;
  let toSet = ImmMap.toSet;
  let toSetWith = ImmMap.toSetWith;
  let toSequence = ImmMap.toSequence;
  let tryFind = ImmMap.tryFind;
  let tryGet = ImmMap.tryGet;
  let values = ImmMap.values;
};

let module IntRange = {
  type t = IntRange.t;

  let create = IntRange.create;
  let contains = IntRange.contains;
  let count = IntRange.count;
  let empty = IntRange.empty;
  let every = IntRange.every;
  let find = IntRange.find;
  let first = IntRange.first;
  let forEach = IntRange.forEach;
  let hash = IntRange.hash;
  let isEmpty = IntRange.isEmpty;
  let isNotEmpty = IntRange.isNotEmpty;
  let last = IntRange.last;
  let reduce = IntRange.reduce;
  let reduceRight = IntRange.reduceRight;
  let some = IntRange.some;
  let toIterator = IntRange.toIterator;
  let toIteratorReversed = IntRange.toIteratorReversed;
  let toKeyedIterator = IntRange.toKeyedIterator;
  let toKeyedIteratorReversed = IntRange.toKeyedIteratorReversed;
  let toMap = IntRange.toMap;
  let toSequence = IntRange.toSequence;
  let toSequenceReversed = IntRange.toSequenceReversed;
  let toSet = IntRange.toSet;
  let tryFind = IntRange.tryFind;
  let tryFirst = IntRange.tryFirst;
  let tryLast = IntRange.tryLast;
};

let module CopyOnWriteArray = {
  type t 'a = CopyOnWriteArray.t 'a;

  let addFirst = CopyOnWriteArray.addFirst;
  let addFirstAll = CopyOnWriteArray.addFirstAll;
  let addLast = CopyOnWriteArray.addLast;
  let addLastAll = CopyOnWriteArray.addLastAll;
  let compare = CopyOnWriteArray.compare;
  let compareWith = CopyOnWriteArray.compareWith;
  let concat = CopyOnWriteArray.concat;
  let contains = CopyOnWriteArray.contains;
  let containsWith = CopyOnWriteArray.containsWith;
  let count = CopyOnWriteArray.count;
  let empty = CopyOnWriteArray.empty;
  let equals = CopyOnWriteArray.equals;
  let equalsWith = CopyOnWriteArray.equalsWith;
  let every = CopyOnWriteArray.every;
  let everyWithIndex = CopyOnWriteArray.everyWithIndex;
  let find = CopyOnWriteArray.find;
  let findWithIndex = CopyOnWriteArray.findWithIndex;
  let first = CopyOnWriteArray.first;
  let forEach = CopyOnWriteArray.forEach;
  let forEachReverse = CopyOnWriteArray.forEachReverse;
  let forEachWithIndex = CopyOnWriteArray.forEachWithIndex;
  let forEachReverseWithIndex = CopyOnWriteArray.forEachReverseWithIndex;
  let from = CopyOnWriteArray.from;
  let fromReversed = CopyOnWriteArray.fromReversed;
  let get = CopyOnWriteArray.get;
  let hash = CopyOnWriteArray.hash;
  let hashWith = CopyOnWriteArray.hashWith;
  let indexOf = CopyOnWriteArray.indexOf;
  let indexOfWithIndex = CopyOnWriteArray.indexOfWithIndex;
  let init = CopyOnWriteArray.init;
  let insertAt = CopyOnWriteArray.insertAt;
  let isEmpty = CopyOnWriteArray.isEmpty;
  let isNotEmpty = CopyOnWriteArray.isNotEmpty;
  let last = CopyOnWriteArray.last;
  let map = CopyOnWriteArray.map;
  let mapWithIndex = CopyOnWriteArray.mapWithIndex;
  let mapReverse = CopyOnWriteArray.mapReverse;
  let mapReverseWithIndex = CopyOnWriteArray.mapReverseWithIndex;
  let none = CopyOnWriteArray.none;
  let noneWithIndex = CopyOnWriteArray.noneWithIndex;
  let ofUnsafe = CopyOnWriteArray.ofUnsafe;
  let range = CopyOnWriteArray.range;
  let reduce = CopyOnWriteArray.reduce;
  let reduceWithIndex = CopyOnWriteArray.reduceWithIndex;
  let reduceRight = CopyOnWriteArray.reduceRight;
  let reduceRightWithIndex = CopyOnWriteArray.reduceRightWithIndex;
  let removeAt = CopyOnWriteArray.removeAt;
  let removeAll = CopyOnWriteArray.removeAll;
  let removeFirst = CopyOnWriteArray.removeFirst;
  let removeLast = CopyOnWriteArray.removeLast;
  let return = CopyOnWriteArray.return;
  let reverse = CopyOnWriteArray.reverse;
  let skip = CopyOnWriteArray.skip;
  let some = CopyOnWriteArray.some;
  let someWithIndex = CopyOnWriteArray.someWithIndex;
  let take = CopyOnWriteArray.take;
  let toIterator = CopyOnWriteArray.toIterator;
  let toIteratorReversed = CopyOnWriteArray.toIteratorReversed;
  let toKeyedIterator = CopyOnWriteArray.toKeyedIterator;
  let toKeyedIteratorReversed = CopyOnWriteArray.toKeyedIteratorReversed;
  let toMap = CopyOnWriteArray.toMap;
  let toSequence = CopyOnWriteArray.toSequence;
  let toSequenceReversed = CopyOnWriteArray.toSequenceReversed;
  let tryFind = CopyOnWriteArray.tryFind;
  let tryFindWithIndex = CopyOnWriteArray.tryFindWithIndex;
  let tryFirst = CopyOnWriteArray.tryFirst;
  let tryGet = CopyOnWriteArray.tryGet;
  let tryIndexOf = CopyOnWriteArray.tryIndexOf;
  let tryIndexOfWithIndex = CopyOnWriteArray.tryIndexOfWithIndex;
  let tryLast = CopyOnWriteArray.tryLast;
  let update = CopyOnWriteArray.update;
  let updateAll = CopyOnWriteArray.updateAll;
  let updateWith = CopyOnWriteArray.updateWith;
};

let module Deque = {
  type t 'a = Deque.t 'a;

  let addFirst = Deque.addFirst;
  let addFirstAll = Deque.addFirstAll;
  let addLast = Deque.addLast;
  let addLastAll = Deque.addLastAll;
  let compare = Deque.compare;
  let compareWith = Deque.compareWith;
  let contains = Deque.contains;
  let containsWith = Deque.containsWith;
  let count = Deque.count;
  let empty = Deque.empty;
  let equals = Deque.equals;
  let equalsWith = Deque.equalsWith;
  let every = Deque.every;
  let find = Deque.find;
  let first = Deque.first;
  let forEach = Deque.forEach;
  let forEachReverse = Deque.forEachReverse;
  let from = Deque.from;
  let fromReversed = Deque.fromReversed;
  let hash = Deque.hash;
  let hashWith = Deque.hashWith;
  let isEmpty = Deque.isEmpty;
  let isNotEmpty = Deque.isNotEmpty;
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
  let return = Deque.return;
  let reverse = Deque.reverse;
  let some = Deque.some;
  let toIterator = Deque.toIterator;
  let toIteratorReversed = Deque.toIteratorReversed;
  let toSequence = Deque.toSequence;
  let toSequenceReversed = Deque.toSequenceReversed;
  let tryFind = Deque.tryFind;
  let tryFirst = Deque.tryFirst;
  let tryLast = Deque.tryLast;
};

let module TransientDeque = {
  type t 'a = TransientDequeInternal.t 'a;

  let addFirst = TransientDequeInternal.addFirst;
  let addLast = TransientDequeInternal.addLast;
  let count = TransientDequeInternal.count;
  let empty = TransientDequeInternal.empty;
  let first = TransientDequeInternal.first;
  let isEmpty = TransientDequeInternal.isEmpty;
  let isNotEmpty = TransientDequeInternal.isNotEmpty;
  let last = TransientDequeInternal.last;
  let persist = TransientDequeInternal.persist;
  let removeAll = TransientDequeInternal.removeAll;
  let removeFirst = TransientDequeInternal.removeFirst;
  let removeLast = TransientDequeInternal.removeLast;
  let reverse = TransientDequeInternal.reverse;
  let tryFirst = TransientDequeInternal.tryFirst;
  let tryLast = TransientDequeInternal.tryLast;
};

let module HashBiMap = {
  type t 'k 'v = HashBiMap.t 'k 'v;

  let contains = HashBiMap.contains;
  let containsKey = HashBiMap.containsKey;
  let count = HashBiMap.count;
  let empty = HashBiMap.empty;
  let emptyWith = HashBiMap.emptyWith;
  let equals = HashBiMap.equals;
  let every = HashBiMap.every;
  let find = HashBiMap.find;
  let get = HashBiMap.get;
  let hash = HashBiMap.hash;
  let isEmpty = HashBiMap.isEmpty;
  let isNotEmpty = HashBiMap.isNotEmpty;
  let forEach = HashBiMap.forEach;
  let fromSequence = HashBiMap.fromSequence;
  let fromSequenceWith = HashBiMap.fromSequenceWith;
  let inverse = HashBiMap.inverse;
  let keys = HashBiMap.keys;
  let mutate = HashBiMap.mutate;
  let none = HashBiMap.none;
  let put = HashBiMap.put;
  let putAll = HashBiMap.putAll;
  let reduce = HashBiMap.reduce;
  let remove = HashBiMap.remove;
  let removeAll = HashBiMap.removeAll;
  let removeValue = HashBiMap.removeValue;
  let some = HashBiMap.some;
  let toSet = HashBiMap.toSet;
  let toMap = HashBiMap.toMap;
  let toSequence = HashBiMap.toSequence;
  let tryFind = HashBiMap.tryFind;
  let tryGet = HashBiMap.tryGet;
  let tryPut = HashBiMap.tryPut;
  let values = HashBiMap.values;
};

let module TransientHashBiMap = {
  type t 'k 'v = TransientHashBiMapInternal.t 'k 'v;

  let count = TransientHashBiMapInternal.count;
  let empty = TransientHashBiMapInternal.empty;
  let emptyWith = TransientHashBiMapInternal.emptyWith;
  let isEmpty = TransientHashBiMapInternal.isEmpty;
  let isNotEmpty = TransientHashBiMapInternal.isNotEmpty;
  let persist = TransientHashBiMapInternal.persist;
  let put = TransientHashBiMapInternal.put;
  let putAll = TransientHashBiMapInternal.putAll;
  let remove = TransientHashBiMapInternal.remove;
  let removeAll = TransientHashBiMapInternal.removeAll;
  let removeValue = TransientHashBiMapInternal.removeValue;
  let tryGet = TransientHashBiMapInternal.tryGet;
  let tryPut = TransientHashBiMapInternal.tryPut;
};

let module HashMap = {
  type t 'k 'v  = HashMap.t 'k 'v;

  let alter = HashMap.alter;
  let contains = HashMap.contains;
  let containsKey = HashMap.containsKey;
  let containsWith = HashMap.containsWith;
  let count = HashMap.count;
  let empty = HashMap.empty;
  let emptyWith = HashMap.emptyWith;
  let equals = HashMap.equals;
  let equalsWith = HashMap.equalsWith;
  let every = HashMap.every;
  let find = HashMap.find;
  let forEach = HashMap.forEach;
  let from = HashMap.from;
  let fromWith = HashMap.fromWith;
  let get = HashMap.get;
  let hash = HashMap.hash;
  let hashWith = HashMap.hashWith;
  let isEmpty = HashMap.isEmpty;
  let isNotEmpty = HashMap.isNotEmpty;
  let keys = HashMap.keys;
  let map = HashMap.map;
  let merge = HashMap.merge;
  let mutate = HashMap.mutate;
  let none = HashMap.none;
  let put = HashMap.put;
  let putAll = HashMap.putAll;
  let reduce = HashMap.reduce;
  let remove = HashMap.remove;
  let removeAll = HashMap.removeAll;
  let some = HashMap.some;
  let toIterator = HashMap.toIterator;
  let toKeyedIterator = HashMap.toKeyedIterator;
  let toMap = HashMap.toMap;
  let toSequence = HashMap.toSequence;
  let toSet = HashMap.toSet;
  let toSetWith = HashMap.toSetWith;
  let tryFind = HashMap.tryFind;
  let tryGet = HashMap.tryGet;
  let values = HashMap.values;
};

let module TransientHashMap = {
  type t 'k 'v = TransientHashMapInternal.t 'k 'v;

  let alter = TransientHashMapInternal.alter;
  let count = TransientHashMapInternal.count;
  let empty = TransientHashMapInternal.empty;
  let emptyWith = TransientHashMapInternal.emptyWith;
  let isEmpty = TransientHashMapInternal.isEmpty;
  let isNotEmpty = TransientHashMapInternal.isNotEmpty;
  let persist = TransientHashMapInternal.persist;
  let put = TransientHashMapInternal.put;
  let putAll = TransientHashMapInternal.putAll;
  let remove = TransientHashMapInternal.remove;
  let removeAll = TransientHashMapInternal.removeAll;
  let tryGet = TransientHashMapInternal.tryGet;
};

let module HashMultiset = {
  type t 'a = HashMultiset.t 'a;

  let add = HashMultiset.add;
  let addAll = HashMultiset.addAll;
  let contains = HashMultiset.contains;
  let count = HashMultiset.count;
  let empty = HashMultiset.empty;
  let emptyWith = HashMultiset.emptyWith;
  let equals = HashMultiset.equals;
  let every = HashMultiset.every;
  let find = HashMultiset.find;
  let forEach = HashMultiset.forEach;
  let fromSequence = HashMultiset.fromSequence;
  let fromSequenceWith = HashMultiset.fromSequenceWith;
  let get = HashMultiset.get;
  let hash = HashMultiset.hash;
  let isEmpty = HashMultiset.isEmpty;
  let isNotEmpty = HashMultiset.isNotEmpty;
  let mutate = HashMultiset.mutate;
  let none = HashMultiset.none;
  let reduce = HashMultiset.reduce;
  let remove = HashMultiset.remove;
  let removeAll = HashMultiset.removeAll;
  let set = HashMultiset.set;
  let some = HashMultiset.some;
  let toMap = HashMultiset.toMap;
  let toSequence = HashMultiset.toSequence;
  let tryFind = HashMultiset.tryFind;
  let values = HashMultiset.values;
};

let module TransientHashMultiset = {
  type t 'a = TransientHashMultisetInternal.t 'a;

  let add = TransientHashMultisetInternal.add;
  let addAll = TransientHashMultisetInternal.addAll;
  let contains = TransientHashMultisetInternal.contains;
  let count = TransientHashMultisetInternal.count;
  let empty = TransientHashMultisetInternal.empty;
  let emptyWith = TransientHashMultisetInternal.emptyWith;
  let get = TransientHashMultisetInternal.get;
  let isEmpty = TransientHashMultisetInternal.isEmpty;
  let isNotEmpty = TransientHashMultisetInternal.isNotEmpty;
  let persist = TransientHashMultisetInternal.persist;
  let remove = TransientHashMultisetInternal.remove;
  let removeAll = TransientHashMultisetInternal.removeAll;
  let set = TransientHashMultisetInternal.set;
};

let module HashSet = {
  type t 'a = HashSet.t 'a;

  let add = HashSet.add;
  let addAll = HashSet.addAll;
  let contains = HashSet.contains;
  let count = HashSet.count;
  let empty = HashSet.empty;
  let emptyWith = HashSet.emptyWith;
  let equals = HashSet.equals;
  let every = HashSet.every;
  let find = HashSet.find;
  let forEach = HashSet.forEach;
  let from = HashSet.from;
  let fromWith = HashSet.fromWith;
  let hash = HashSet.hash;
  let intersect = HashSet.intersect;
  let isEmpty = HashSet.isEmpty;
  let isNotEmpty = HashSet.isNotEmpty;
  let mutate = HashSet.mutate;
  let none = HashSet.none;
  let reduce = HashSet.reduce;
  let remove = HashSet.remove;
  let removeAll = HashSet.removeAll;
  let some = HashSet.some;
  let subtract = HashSet.subtract;
  let toIterator = HashSet.toIterator;
  let toKeyedIterator = HashSet.toKeyedIterator;
  let toSet = HashSet.toSet;
  let toMap = HashSet.toMap;
  let toSequence = HashSet.toSequence;
  let tryFind = HashSet.tryFind;
  let union = HashSet.union;
};

let module TransientHashSet = {
  type t 'a = TransientHashSetInternal.t 'a;

  let add = TransientHashSetInternal.add;
  let addAll = TransientHashSetInternal.addAll;
  let contains = TransientHashSetInternal.contains;
  let count = TransientHashSetInternal.count;
  let empty = TransientHashSetInternal.empty;
  let emptyWith = TransientHashSetInternal.emptyWith;
  let isEmpty = TransientHashSetInternal.isEmpty;
  let isNotEmpty = TransientHashSetInternal.isNotEmpty;
  let persist = TransientHashSetInternal.persist;
  let remove = TransientHashSetInternal.remove;
  let removeAll = TransientHashSetInternal.removeAll;
};

let module HashSetMultimap = {
  type t 'k 'v = HashSetMultimap.t 'k 'v;

  let contains = HashSetMultimap.contains;
  let containsKey = HashSetMultimap.containsKey;
  let count = HashSetMultimap.count;
  let empty = HashSetMultimap.empty;
  let emptyWith = HashSetMultimap.emptyWith;
  let equals = HashSetMultimap.equals;
  let every = HashSetMultimap.every;
  let find = HashSetMultimap.find;
  let forEach = HashSetMultimap.forEach;
  let get = HashSetMultimap.get;
  let hash = HashSetMultimap.hash;
  let isEmpty = HashSetMultimap.isEmpty;
  let isNotEmpty = HashSetMultimap.isNotEmpty;
  let keys = HashSetMultimap.keys;
  let none = HashSetMultimap.none;
  let put = HashSetMultimap.put;
  let putAllValues = HashSetMultimap.putAllValues;
  let reduce = HashSetMultimap.reduce;
  let remove = HashSetMultimap.remove;
  let removeAll = HashSetMultimap.removeAll;
  let some = HashSetMultimap.some;
  let toSet = HashSetMultimap.toSet;
  let toSequence = HashSetMultimap.toSequence;
  let tryFind = HashSetMultimap.tryFind;
  let values = HashSetMultimap.values;
};

let module IntMap = {
  type t 'a = IntMap.t 'a;

  let alter = IntMap.alter;
  let contains = IntMap.contains;
  let containsKey = IntMap.containsKey;
  let containsWith = IntMap.containsWith;
  let count = IntMap.count;
  let empty = IntMap.empty;
  let equals = IntMap.equals;
  let equalsWith = IntMap.equalsWith;
  let every = IntMap.every;
  let from = IntMap.from;
  let find = IntMap.find;
  let forEach = IntMap.forEach;
  let get = IntMap.get;
  let hash = IntMap.hash;
  let hashWith = IntMap.hashWith;
  let isEmpty = IntMap.isEmpty;
  let isNotEmpty = IntMap.isNotEmpty;
  let keys = IntMap.keys;
  let map = IntMap.map;
  let merge = IntMap.merge;
  let mutate = IntMap.mutate;
  let none = IntMap.none;
  let put = IntMap.put;
  let putAll = IntMap.putAll;
  let reduce = IntMap.reduce;
  let remove = IntMap.remove;
  let removeAll = IntMap.removeAll;
  let some = IntMap.some;
  let toIterator = IntMap.toIterator;
  let toKeyedIterator = IntMap.toKeyedIterator;
  let toSet = IntMap.toSet;
  let toSetWith = IntMap.toSetWith;
  let toMap = IntMap.toMap;
  let toSequence = IntMap.toSequence;
  let tryFind = IntMap.tryFind;
  let tryGet = IntMap.tryGet;
  let values = IntMap.values;
};

let module TransientIntMap = {
  type t 'a = TransientIntMapInternal.t 'a;

  let alter = TransientIntMapInternal.alter;
  let count = TransientIntMapInternal.count;
  let empty = TransientIntMapInternal.empty;
  let isEmpty = TransientIntMapInternal.isEmpty;
  let isNotEmpty = TransientIntMapInternal.isNotEmpty;
  let persist = TransientIntMapInternal.persist;
  let put = TransientIntMapInternal.put;
  let putAll = TransientIntMapInternal.putAll;
  let remove = TransientIntMapInternal.remove;
  let removeAll = TransientIntMapInternal.removeAll;
  let tryGet = TransientIntMapInternal.tryGet;
};

let module IntSet = {
  type t = IntSet.t;

  let add = IntSet.add;
  let addAll = IntSet.addAll;
  let contains = IntSet.contains;
  let count = IntSet.count;
  let empty = IntSet.empty;
  let equals = IntSet.equals;
  let every = IntSet.every;
  let find = IntSet.find;
  let forEach = IntSet.forEach;
  let from = IntSet.from;
  let hash = IntSet.hash;
  let intersect = IntSet.intersect;
  let isEmpty = IntSet.isEmpty;
  let isNotEmpty = IntSet.isNotEmpty;
  let mutate = IntSet.mutate;
  let none = IntSet.none;
  let reduce = IntSet.reduce;
  let remove = IntSet.remove;
  let removeAll = IntSet.removeAll;
  let some = IntSet.some;
  let subtract = IntSet.subtract;
  let toIterator = IntSet.toIterator;
  let toKeyedIterator = IntSet.toKeyedIterator;
  let toSet = IntSet.toSet;
  let toMap = IntSet.toMap;
  let toSequence = IntSet.toSequence;
  let tryFind = IntSet.tryFind;
  let union = IntSet.union;
};

let module TransientIntSet = {
  type t = TransientIntSetInternal.t;

  let add = TransientIntSetInternal.add;
  let addAll = TransientIntSetInternal.addAll;
  let contains = TransientIntSetInternal.contains;
  let count = TransientIntSetInternal.count;
  let empty = TransientIntSetInternal.empty;
  let isEmpty = TransientIntSetInternal.isEmpty;
  let isNotEmpty = TransientIntSetInternal.isNotEmpty;
  let persist = TransientIntSetInternal.persist;
  let remove = TransientIntSetInternal.remove;
  let removeAll = TransientIntSetInternal.removeAll;
};

let module List = {
  type t 'a = list 'a;

  let addFirst = ImmList.addFirst;
  let addFirstAll = IteratorInternal.listAddFirstAll;
  let compare = ImmList.compare;
  let compareWith = ImmList.compareWith;
  let contains = ImmList.contains;
  let containsWith = ImmList.containsWith;
  let count = ImmList.count;
  let empty = ImmList.empty;
  let equals = ImmList.equals;
  let equalsWith = ImmList.equalsWith;
  let every = ImmList.every;
  let find = ImmList.find;
  let first = ImmList.first;
  let forEach = ImmList.forEach;
  let fromReversed = IteratorInternal.listFromReverse;
  let hash = ImmList.hash;
  let hashWith = ImmList.hashWith;
  let isEmpty = ImmList.isEmpty;
  let isNotEmpty = ImmList.isNotEmpty;
  let mapReverse = ImmList.mapReverse;
  let none = ImmList.none;
  let reduce = ImmList.reduce;
  let removeAll = ImmList.removeAll;
  let removeFirst = ImmList.removeFirst;
  let return = ImmList.return;
  let reverse = ImmList.reverse;
  let some = ImmList.some;
  let toIterator = IteratorInternal.ofList;
  let toSequence = SequenceInternal.ofList;
  let tryFind = ImmList.tryFind;
  let tryFirst = ImmList.tryFirst;
};

let module Option = {
  type t 'a = option 'a;

  let compare = Option.compare;
  let compareWith = Option.compareWith;
  let contains = Option.contains;
  let containsWith = Option.containsWith;
  let count = Option.count;
  let empty = Option.empty;
  let equals = Option.equals;
  let equalsWith = Option.equalsWith;
  let every = Option.every;
  let filter = Option.filter;
  let find = Option.find;
  let first = Option.first;
  let flatMap = Option.flatMap;
  let flatten = Option.flatten;
  let forEach = Option.forEach;
  let hash = Option.hash;
  let hashWith = Option.hashWith;
  let isEmpty = Option.isEmpty;
  let isNotEmpty = Option.isNotEmpty;
  let last = Option.last;
  let map = Option.map;
  let none = Option.none;
  let reduce = Option.reduce;
  let return = Option.return;
  let some = Option.some;
  let toIterator = Option.toIterator;
  let toSet = ImmSet.ofOption;
  let toSetWith = ImmSet.ofOptionWith;
  let toSequence = SequenceInternal.ofOption;
  let tryFind = Option.tryFind;
  let tryFirst = Option.tryFirst;
  let tryLast = Option.tryLast;
};

let module SortedMap = SortedMap;

let module SortedSet = SortedSet;

let module Stack = {
  type t 'a = Stack.t 'a;

  let addFirst = Stack.addFirst;
  let addFirstAll = Stack.addFirstAll;
  let compare = Stack.compare;
  let compareWith = Stack.compareWith;
  let contains = Stack.contains;
  let containsWith = Stack.containsWith;
  let count = Stack.count;
  let empty = Stack.empty;
  let equals = Stack.equals;
  let equalsWith = Stack.equalsWith;
  let every = Stack.every;
  let find = Stack.find;
  let first = Stack.first;
  let forEach = Stack.forEach;
  let fromList = Stack.fromList;
  let fromReversed = Stack.fromReversed;
  let hash = Stack.hash;
  let hashWith = Stack.hashWith;
  let isEmpty = Stack.isEmpty;
  let isNotEmpty = Stack.isNotEmpty;
  let mapReverse = Stack.mapReverse;
  let none = Stack.none;
  let reduce = Stack.reduce;
  let removeAll = Stack.removeAll;
  let removeFirst = Stack.removeFirst;
  let return = Stack.return;
  let reverse = Stack.reverse;
  let some = Stack.some;
  let toIterator = Stack.toIterator;
  let toList = Stack.toList;
  let toSequence = Stack.toSequence;
  let tryFind = Stack.tryFind;
  let tryFirst = Stack.tryFirst;
};

let module StackMultimap = {
  type t 'k 'v = StackMultimap.t 'k 'v;

  let add = StackMultimap.add;
  let addAllValues = StackMultimap.addAllValues;
  let contains = StackMultimap.contains;
  let containsWith = StackMultimap.containsWith;
  let containsKey = StackMultimap.containsKey;
  let count = StackMultimap.count;
  let empty = StackMultimap.empty;
  let emptyWith = StackMultimap.emptyWith;
  let equals = StackMultimap.equals;
  let equalsWith = StackMultimap.equalsWith;
  let every = StackMultimap.every;
  let find = StackMultimap.find;
  let forEach = StackMultimap.forEach;
  let get = StackMultimap.get;
  let hash = StackMultimap.hash;
  let hashWith = StackMultimap.hashWith;
  let isEmpty = StackMultimap.isEmpty;
  let isNotEmpty = StackMultimap.isNotEmpty;
  let keys = StackMultimap.keys;
  let none = StackMultimap.none;
  let reduce = StackMultimap.reduce;
  let remove = StackMultimap.remove;
  let removeAll = StackMultimap.removeAll;
  let some = StackMultimap.some;
  let toSequence = StackMultimap.toSequence;
  let tryFind = StackMultimap.tryFind;
  let values = StackMultimap.values;
};

let module Table = {
  type t 'row 'column 'value = Table.t 'row 'column 'value;

  let contains = Table.contains;
  let containsRow = Table.containsRow;
  let containsRowAndColumn = Table.containsRowAndColumn;
  let containsWith = Table.containsWith;
  let columns = Table.columns;
  let count = Table.count;
  let empty = Table.empty;
  let emptyWith = Table.emptyWith;
  let equals = Table.equals;
  let equalsWith = Table.equalsWith;
  let every = Table.every;
  let find = Table.find;
  let forEach = Table.forEach;
  let get = Table.get;
  let hash = Table.hash;
  let hashWith = Table.hashWith;
  let isEmpty = Table.isEmpty;
  let isNotEmpty = Table.isNotEmpty;
  let none = Table.none;
  let map = Table.map;
  let put = Table.put;
  let reduce = Table.reduce;
  let remove = Table.remove;
  let removeAll = Table.removeAll;
  let removeRow = Table.removeRow;
  let rows = Table.rows;
  let some = Table.some;
  let toSequence = Table.toSequence;
  let tryFind = Table.tryFind;
  let tryGet = Table.tryGet;
  let values = Table.values;
};

let module Vector = {
  type t 'a = Vector.t 'a;

  let addFirst = Vector.addFirst;
  let addFirstAll = Vector.addFirstAll;
  let addLast = Vector.addLast;
  let addLastAll = Vector.addLastAll;
  let compare = Vector.compare;
  let compareWith = Vector.compareWith;
  let concat = Vector.concat;
  let contains = Vector.contains;
  let containsWith = Vector.containsWith;
  let count = Vector.count;
  let every = Vector.every;
  let everyWithIndex = Vector.everyWithIndex;
  let empty = Vector.empty;
  let equals = Vector.equals;
  let equalsWith = Vector.equalsWith;
  let find = Vector.find;
  let findWithIndex = Vector.findWithIndex;
  let first = Vector.first;
  let forEach = Vector.forEach;
  let forEachReverse = Vector.forEachReverse;
  let forEachWithIndex = Vector.forEachWithIndex;
  let forEachReverseWithIndex = Vector.forEachReverseWithIndex;
  let from = Vector.from;
  let fromReversed = Vector.fromReversed;
  let get = Vector.get;
  let hash = Vector.hash;
  let hashWith = Vector.hashWith;
  let init = Vector.init;
  let indexOf = Vector.indexOf;
  let indexOfWithIndex = Vector.indexOfWithIndex;
  let insertAt = Vector.insertAt;
  let isEmpty = Vector.isEmpty;
  let isNotEmpty = Vector.isNotEmpty;
  let last = Vector.last;
  let map = Vector.map;
  let mapWithIndex = Vector.mapWithIndex;
  let mapReverse = Vector.mapReverse;
  let mapReverseWithIndex = Vector.mapReverseWithIndex;
  let mutate = Vector.mutate;
  let none = Vector.none;
  let noneWithIndex = Vector.noneWithIndex;
  let range = Vector.range;
  let reduce = Vector.reduce;
  let reduceWithIndex = Vector.reduceWithIndex;
  let reduceRight = Vector.reduceRight;
  let reduceRightWithIndex = Vector.reduceRightWithIndex;
  let removeAt = Vector.removeAt;
  let removeAll = Vector.removeAll;
  let removeFirst = Vector.removeFirst;
  let removeLast = Vector.removeLast;
  let return = Vector.return;
  let reverse = Vector.reverse;
  let skip = Vector.skip;
  let some = Vector.some;
  let someWithIndex = Vector.someWithIndex;
  let take = Vector.take;
  let toIterator = Vector.toIterator;
  let toIteratorReversed = Vector.toIteratorReversed;
  let toKeyedIterator = Vector.toKeyedIterator;
  let toKeyedIteratorReversed = Vector.toKeyedIteratorReversed;
  let toMap = Vector.toMap;
  let toSequence = Vector.toSequence;
  let toSequenceReversed = Vector.toSequenceReversed;
  let tryFind = Vector.tryFind;
  let tryFindWithIndex = Vector.tryFindWithIndex;
  let tryFirst = Vector.tryFirst;
  let tryGet = Vector.tryGet;
  let tryIndexOf = Vector.tryIndexOf;
  let tryIndexOfWithIndex = Vector.tryIndexOfWithIndex;
  let tryLast = Vector.tryLast;
  let update = Vector.update;
  let updateAll = Vector.updateAll;
  let updateWith = Vector.updateWith;
};

let module TransientVector = {
  type t 'a = TransientVectorInternal.t 'a;

  let addFirst = TransientVectorInternal.addFirst;
  let addLast = TransientVectorInternal.addLast;
  let count = TransientVectorInternal.count;
  let empty = TransientVectorInternal.empty;
  let first = TransientVectorInternal.first;
  let get = TransientVectorInternal.get;
  let insertAt = TransientVectorInternal.insertAt;
  let isEmpty = TransientVectorInternal.isEmpty;
  let isNotEmpty = TransientVectorInternal.isNotEmpty;
  let last = TransientVectorInternal.last;
  let persist = TransientVectorInternal.persist;
  let removeAll = TransientVectorInternal.removeAll;
  let removeAt = TransientVectorInternal.removeAt;
  let removeFirst = TransientVectorInternal.removeFirst;
  let removeLast = TransientVectorInternal.removeLast;
  let reverse = TransientVectorInternal.reverse;
  let tryFirst = TransientVectorInternal.tryFirst;
  let tryGet = TransientVectorInternal.tryGet;
  let tryLast = TransientVectorInternal.tryLast;
  let update = TransientVectorInternal.update;
  let updateAll = TransientVectorInternal.updateAll;
  let updateWith = TransientVectorInternal.updateWith;
};
