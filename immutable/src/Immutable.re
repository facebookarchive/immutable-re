/*open BiMap;*/
open Deque;
open Equality;
/*open HashMap;
open HashMultiset;*/
open HashSet;
/*open IntMap;*/
open Vector;

let isEmpty (count: 'collection => 'int) (collection: 'collection): bool =>
  (count collection) == 0;

let isNotEmpty (count: 'collection => 'int) (collection: 'collection): bool =>
  (count collection) != 0;

let module Hash = {
  type t 'a = Hash.hash 'a;

  let random = Hash.random;
  let structural = Hash.structural;
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
  let compare = Seq.compare;
  let compareWith = Seq.compareWith;
  let contains = Seq.contains;
  let containsWith = Seq.containsWith;
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
  let hashWith = Seq.hashWith;
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

let module CollectionInternal = Collection;
let module Collection = {
  type t 'a = Collection.collection 'a;

  let contains = Collection.contains;
  let count = Collection.count;
  let empty = Collection.empty;
  let equals = Collection.equals;
  let every = Collection.every;
  let find = Collection.find;
  let forEach = Collection.forEach;
  let hash = Collection.hash;
  let hashWith = Collection.hashWith;
  let inRange = Collection.inRange;
  let intersect = Collection.intersect;
  let isEmpty = Collection.isEmpty;
  let isNotEmpty = Collection.isNotEmpty;
  let none = Collection.none;
  let reduce = Collection.reduce;
  let some = Collection.some;
  let subtract = Collection.subtract;
  let toKeyed = Keyed.ofCollection;
  let toSeq = Collection.toSeq;
  let tryFind = Collection.tryFind;
  let union = Collection.union;
};

let module Keyed = {
  type t 'k 'v = Keyed.keyed 'k 'v;

  let contains = Keyed.contains;
  let containsWith = Keyed.containsWith;
  let containsKey = Keyed.containsKey;
  let count = Keyed.count;
  let empty = Keyed.empty;
  let equals = Keyed.equals;
  let equalsWith = Keyed.equalsWith;
  let every = Keyed.every;
  let find = Keyed.find;
  let forEach = Keyed.forEach;
  let get = Keyed.get;
  let hash = Keyed.hash;
  let hashWith = Keyed.hashWith;
  let isEmpty keyed => isEmpty count keyed;
  let isNotEmpty keyed => isNotEmpty count keyed;
  let keys = Keyed.keys;
  let map = Keyed.map;
  let none = Keyed.none;
  let reduce = Keyed.reduce;
  let some = Keyed.some;
  let toCollection = Keyed.toCollection;
  let toCollectionWith = Keyed.toCollectionWith;
  let toSeq = Keyed.toSeq;
  let tryFind = Keyed.tryFind;
  let tryGet = Keyed.tryGet;
  let values = Keyed.values;
};

let module HashStrategy = {
  type t 'a = HashStrategy.hashStrategy 'a;

  let createWithComparator = HashStrategy.createWithComparator;
  let createWithEquality = HashStrategy.createWithEquality;
  let identity = HashStrategy.identity;
  let structuralCompare = HashStrategy.structuralCompare;
  let structuralEquality = HashStrategy.structuralEquality;
};
/*
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
*/
let module CopyOnWriteArray = {
  type t 'a = CopyOnWriteArray.copyOnWriteArray 'a;

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
  let fromSeq = CopyOnWriteArray.fromSeq;
  let fromSeqReversed = CopyOnWriteArray.fromSeqReversed;
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
  let toKeyed = CopyOnWriteArray.toKeyed;
  let toSeq = CopyOnWriteArray.toSeq;
  let toSeqReversed = CopyOnWriteArray.toSeqReversed;
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
  type t 'a = Deque.deque 'a;

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
  let fromSeq = Deque.fromSeq;
  let fromSeqReversed = Deque.fromSeqReversed;
  let hash = Deque.hash;
  let hashWith = Deque.hashWith;
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
  let return = Deque.return;
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
/*
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
*/
let module HashSet = {
  type t 'a = HashSet.hashSet 'a;

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
  let fromSeq = HashSet.fromSeq;
  let fromSeqWith = HashSet.fromSeqWith;
  let hash = HashSet.hash;
  let intersect = HashSet.intersect;
  let isEmpty set => isEmpty count set;
  let isNotEmpty set => isNotEmpty count set;
  let mutate = HashSet.mutate;
  let none = HashSet.none;
  let reduce = HashSet.reduce;
  let remove = HashSet.remove;
  let removeAll = HashSet.removeAll;
  let some = HashSet.some;
  let subtract = HashSet.subtract;
  let toCollection = HashSet.toCollection;
  let toKeyed = HashSet.toKeyed;
  let toSeq = HashSet.toSeq;
  let tryFind = HashSet.tryFind;
  let union = HashSet.union;
};

let module TransientHashSet = {
  type t 'a = transientHashSet 'a;

  let add = TransientHashSet.add;
  let addAll = TransientHashSet.addAll;
  let contains = TransientHashSet.contains;
  let count = TransientHashSet.count;
  let isEmpty transient => isEmpty count transient;
  let isNotEmpty transient => isNotEmpty count transient;
  let persist = TransientHashSet.persist;
  let remove = TransientHashSet.remove;
  let removeAll = TransientHashSet.removeAll;
};
/*
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
};*/

let module List = {
  type t 'a = list 'a;

  let addFirst = ImmList.addFirst;
  let addFirstAll = SeqInternal.listAddFirstAll;
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
  let fromSeqReversed = SeqInternal.listFromSeqReverse;
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
  let toSeq = SeqInternal.ofList;
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
  let flatMap = Option.flatMap;
  let flatten = Option.flatten;
  let forEach = Option.forEach;
  let get = Option.get;
  let hash = Option.hash;
  let hashWith = Option.hashWith;
  let isEmpty = Option.isEmpty;
  let isNotEmpty = Option.isNotEmpty;
  let map = Option.map;
  let none = Option.none;
  let reduce = Option.reduce;
  let return = Option.return;
  let some = Option.some;
  let toCollection = CollectionInternal.ofOption;
  let toCollectionWith  = CollectionInternal.ofOptionWith;
  let toSeq = SeqInternal.ofOption;
  let tryFind = Option.tryFind;
};
/*
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
*/
let module SortedSet = {
  type t 'a = SortedSet.sortedSet 'a;

  let add = SortedSet.add;
  let addAll = SortedSet.addAll;
  let compare = SortedSet.compare;
  let contains = SortedSet.contains;
  let count = SortedSet.count;
  let empty = SortedSet.empty;
  let emptyWith = SortedSet.emptyWith;
  let equals = SortedSet.equals;
  let every = SortedSet.every;
  let find = SortedSet.find;
  let first = SortedSet.first;
  let forEach = SortedSet.forEach;
  let fromSeq = SortedSet.fromSeq;
  let fromSeqWith = SortedSet.fromSeqWith;
  let hash = SortedSet.hash;
  let hashWith = SortedSet.hashWith;
  let intersect = SortedSet.intersect;
  let isEmpty = SortedSet.isEmpty;
  let isNotEmpty = SortedSet.isNotEmpty;
  let last = SortedSet.last;
  let none = SortedSet.none;
  let reduce = SortedSet.reduce;
  let reduceRight = SortedSet.reduceRight;
  let remove = SortedSet.remove;
  let removeFirst = SortedSet.removeFirst;
  let removeLast = SortedSet.removeLast;
  let removeAll = SortedSet.removeAll;
  let search = SortedSet.search;
  let some = SortedSet.some;
  let subtract = SortedSet.subtract;
  let toCollection = SortedSet.toCollection;
  let toKeyed = SortedSet.toKeyed;
  let toSeq = SortedSet.toSeq;
  let tryFind = SortedSet.tryFind;
  let tryFirst = SortedSet.tryFirst;
  let tryLast= SortedSet.tryLast;
  let trySearch = SortedSet.trySearch;
  let union = SortedSet.union;
};

let module Stack ={
  type t 'a = Stack.stack 'a;

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
  let fromSeqReversed = Stack.fromSeqReversed;
  let hash = Stack.hash;
  let hashWith = Stack.hashWith;
  let isEmpty stack => isEmpty count stack;
  let isNotEmpty stack => isNotEmpty count stack;
  let mapReverse = Stack.mapReverse;
  let none = Stack.none;
  let reduce = Stack.reduce;
  let removeAll = Stack.removeAll;
  let removeFirst = Stack.removeFirst;
  let return = Stack.return;
  let reverse = Stack.reverse;
  let some = Stack.some;
  let toList = Stack.toList;
  let toSeq = Stack.toSeq;
  let tryFind = Stack.tryFind;
  let tryFirst = Stack.tryFirst;
};
/*
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
*/
let module Vector = {
  type t 'a = Vector.vector 'a;

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
  let fromSeq = Vector.fromSeq;
  let fromSeqReversed = Vector.fromSeqReversed;
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
  let toKeyed = Vector.toKeyed;
  let toSeq = Vector.toSeq;
  let toSeqReversed = Vector.toSeqReversed;
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
  type t 'a = transientVector 'a;

  let addFirst = TransientVector.addFirst;
  let addLast = TransientVector.addLast;
  let count = TransientVector.count;
  let empty = TransientVector.empty;
  let first = TransientVector.first;
  let get = TransientVector.get;
  let insertAt = TransientVector.insertAt;
  let isEmpty transient => isEmpty count transient;
  let isNotEmpty transient => isNotEmpty count transient;
  let last = TransientVector.last;
  let persist = TransientVector.persist;
  let removeAll = TransientVector.removeAll;
  let removeAt = TransientVector.removeAt;
  let removeFirst = TransientVector.removeFirst;
  let removeLast = TransientVector.removeLast;
  let reverse = TransientVector.reverse;
  let tryFirst = TransientVector.tryFirst;
  let tryGet = TransientVector.tryGet;
  let tryLast = TransientVector.tryLast;
  let update = TransientVector.update;
  let updateAll = TransientVector.updateAll;
  let updateWith = TransientVector.updateWith;
};
