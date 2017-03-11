open Deque;
open Equality;
open HashBiMap;
open HashMap;
open HashMultiset;
open HashSet;
open IntMap;
open IntSet;
open Vector;

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
  let get = Seq.get;
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

let module Set = {
  type t 'a = ImmSet.set 'a;

  let contains = ImmSet.contains;
  let count = ImmSet.count;
  let empty = ImmSet.empty;
  let equals = ImmSet.equals;
  let every = ImmSet.every;
  let find = ImmSet.find;
  let forEach = ImmSet.forEach;
  let hash = ImmSet.hash;
  let hashWith = ImmSet.hashWith;
  let inRange = ImmSet.inRange;
  let intersect = ImmSet.intersect;
  let isEmpty = ImmSet.isEmpty;
  let isNotEmpty = ImmSet.isNotEmpty;
  let none = ImmSet.none;
  let reduce = ImmSet.reduce;
  let some = ImmSet.some;
  let subtract = ImmSet.subtract;
  let toMap = ImmMap.ofSet;
  let toSeq = ImmSet.toSeq;
  let tryFind = ImmSet.tryFind;
  let union = ImmSet.union;
};

let module Map = {
  type t 'k 'v = ImmMap.map 'k 'v;

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
  let toSet = ImmMap.toSet;
  let toSetWith = ImmMap.toSetWith;
  let toSeq = ImmMap.toSeq;
  let tryFind = ImmMap.tryFind;
  let tryGet = ImmMap.tryGet;
  let values = ImmMap.values;
};

let module HashStrategy = {
  type t 'a = HashStrategy.hashStrategy 'a;

  let createWithComparator = HashStrategy.createWithComparator;
  let createWithEquality = HashStrategy.createWithEquality;
  let identity = HashStrategy.identity;
  let structuralCompare = HashStrategy.structuralCompare;
  let structuralEquality = HashStrategy.structuralEquality;
};

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
  let toMap = CopyOnWriteArray.toMap;
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
  let isEmpty = TransientDeque.isEmpty;
  let isNotEmpty = TransientDeque.isNotEmpty;
  let last = TransientDeque.last;
  let persist = TransientDeque.persist;
  let removeAll = TransientDeque.removeAll;
  let removeFirst = TransientDeque.removeFirst;
  let removeLast = TransientDeque.removeLast;
  let reverse = TransientDeque.reverse;
  let tryFirst = TransientDeque.tryFirst;
  let tryLast = TransientDeque.tryLast;
};


let module HashBiMap = {
  type t 'k 'v = hashBiMap 'k 'v;

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
  let fromSeq = HashBiMap.fromSeq;
  let fromSeqWith = HashBiMap.fromSeqWith;
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
  let toSeq = HashBiMap.toSeq;
  let tryFind = HashBiMap.tryFind;
  let tryGet = HashBiMap.tryGet;
  let tryPut = HashBiMap.tryPut;
  let values = HashBiMap.values;
};

let module TransientHashBiMap = {
  type t 'k 'v = transientHashBiMap 'k 'v;

  let count = TransientHashBiMap.count;
  let empty = TransientHashBiMap.empty;
  let emptyWith = TransientHashBiMap.emptyWith;
  let isEmpty = TransientHashBiMap.isEmpty;
  let isNotEmpty = TransientHashBiMap.isNotEmpty;
  let persist = TransientHashBiMap.persist;
  let put = TransientHashBiMap.put;
  let putAll = TransientHashBiMap.putAll;
  let remove = TransientHashBiMap.remove;
  let removeAll = TransientHashBiMap.removeAll;
  let removeValue = TransientHashBiMap.removeValue;
  let tryGet = TransientHashBiMap.tryGet;
  let tryPut = TransientHashBiMap.tryPut;
};

let module HashMap = {
  type t 'k 'v  = hashMap 'k 'v;

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
  let fromMap = HashMap.fromMap;
  let fromMapWith = HashMap.fromMapWith;
  let fromSeq = HashMap.fromSeq;
  let fromSeqWith = HashMap.fromSeqWith;
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
  let toSet = HashMap.toSet;
  let toSetWith = HashMap.toSetWith;
  let toMap = HashMap.toMap;
  let toSeq = HashMap.toSeq;
  let tryFind = HashMap.tryFind;
  let tryGet = HashMap.tryGet;
  let values = HashMap.values;
};

let module TransientHashMap = {
  type t 'k 'v = transientHashMap 'k 'v;

  let alter = TransientHashMap.alter;
  let count = TransientHashMap.count;
  let empty = TransientHashMap.empty;
  let emptyWith = TransientHashMap.emptyWith;
  let isEmpty = TransientHashMap.isEmpty;
  let isNotEmpty = TransientHashMap.isNotEmpty;
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
  let equals = HashMultiset.equals;
  let every = HashMultiset.every;
  let find = HashMultiset.find;
  let forEach = HashMultiset.forEach;
  let fromSeq = HashMultiset.fromSeq;
  let fromSeqWith = HashMultiset.fromSeqWith;
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
  let toSeq = HashMultiset.toSeq;
  let tryFind = HashMultiset.tryFind;
  let values = HashMultiset.values;
};

let module TransientHashMultiset = {
  type t 'a = transientHashMultiset 'a;

  let add = TransientHashMultiset.add;
  let addAll = TransientHashMultiset.addAll;
  let contains = TransientHashMultiset.contains;
  let count = TransientHashMultiset.count;
  let empty = TransientHashMultiset.empty;
  let emptyWith = TransientHashMultiset.emptyWith;
  let get = TransientHashMultiset.get;
  let isEmpty = TransientHashMultiset.isEmpty;
  let isNotEmpty = TransientHashMultiset.isNotEmpty;
  let persist = TransientHashMultiset.persist;
  let remove = TransientHashMultiset.remove;
  let removeAll = TransientHashMultiset.removeAll;
  let set = TransientHashMultiset.set;
};

let module HashSet = {
  type t 'a = hashSet 'a;

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
  let isEmpty = HashSet.isEmpty;
  let isNotEmpty = HashSet.isNotEmpty;
  let mutate = HashSet.mutate;
  let none = HashSet.none;
  let reduce = HashSet.reduce;
  let remove = HashSet.remove;
  let removeAll = HashSet.removeAll;
  let some = HashSet.some;
  let subtract = HashSet.subtract;
  let toSet = HashSet.toSet;
  let toMap = HashSet.toMap;
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
  let empty = TransientHashSet.empty;
  let emptyWith = TransientHashSet.emptyWith;
  let isEmpty = TransientHashSet.isEmpty;
  let isNotEmpty = TransientHashSet.isNotEmpty;
  let persist = TransientHashSet.persist;
  let remove = TransientHashSet.remove;
  let removeAll = TransientHashSet.removeAll;
};

let module HashSetMultimap = {
  type t 'k 'v = HashSetMultimap.hashSetMultimap 'k 'v;

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
  let toSeq = HashSetMultimap.toSeq;
  let tryFind = HashSetMultimap.tryFind;
  let values = HashSetMultimap.values;
};

let module IntMap = {
  type t 'a = IntMap.intMap 'a;

  let alter = IntMap.alter;
  let contains = IntMap.contains;
  let containsKey = IntMap.containsKey;
  let containsWith = IntMap.containsWith;
  let count = IntMap.count;
  let empty = IntMap.empty;
  let equals = IntMap.equals;
  let equalsWith = IntMap.equalsWith;
  let every = IntMap.every;
  let fromMap = IntMap.fromMap;
  let fromSeq = IntMap.fromSeq;
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
  let toSet = IntMap.toSet;
  let toSetWith = IntMap.toSetWith;
  let toMap = IntMap.toMap;
  let toSeq = IntMap.toSeq;
  let tryFind = IntMap.tryFind;
  let tryGet = IntMap.tryGet;
  let values = IntMap.values;
};

let module TransientIntMap = {
  type t 'a = transientIntMap 'a;

  let alter = TransientIntMap.alter;
  let count = TransientIntMap.count;
  let empty = TransientIntMap.empty;
  let isEmpty = TransientIntMap.isEmpty;
  let isNotEmpty = TransientIntMap.isNotEmpty;
  let persist = TransientIntMap.persist;
  let put = TransientIntMap.put;
  let putAll = TransientIntMap.putAll;
  let remove = TransientIntMap.remove;
  let removeAll = TransientIntMap.removeAll;
  let tryGet = TransientIntMap.tryGet;
};

let module IntSet = {
  type t = intSet;

  let add = IntSet.add;
  let addAll = IntSet.addAll;
  let contains = IntSet.contains;
  let count = IntSet.count;
  let empty = IntSet.empty;
  let equals = IntSet.equals;
  let every = IntSet.every;
  let find = IntSet.find;
  let forEach = IntSet.forEach;
  let fromSeq = IntSet.fromSeq;
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
  let toSet = IntSet.toSet;
  let toMap = IntSet.toMap;
  let toSeq = IntSet.toSeq;
  let tryFind = IntSet.tryFind;
  let union = IntSet.union;
};

let module TransientIntSet = {
  type t = transientIntSet;

  let add = TransientIntSet.add;
  let addAll = TransientIntSet.addAll;
  let contains = TransientIntSet.contains;
  let count = TransientIntSet.count;
  let empty = TransientIntSet.empty;
  let isEmpty = TransientIntSet.isEmpty;
  let isNotEmpty = TransientIntSet.isNotEmpty;
  let persist = TransientIntSet.persist;
  let remove = TransientIntSet.remove;
  let removeAll = TransientIntSet.removeAll;
};

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
  let toSet = ImmSet.ofOption;
  let toSetWith  = ImmSet.ofOptionWith;
  let toSeq = SeqInternal.ofOption;
  let tryFind = Option.tryFind;
  let tryFirst = Option.tryFirst;
  let tryLast = Option.tryLast;
};

let module SortedMap = {
  type t 'k 'v = SortedMap.sortedMap 'k 'v;

  let alter = SortedMap.alter;
  let compare = SortedMap.compare;
  let compareWith = SortedMap.compareWith;
  let contains = SortedMap.contains;
  let containsWith = SortedMap.containsWith;
  let containsKey = SortedMap.containsKey;
  let count = SortedMap.count;
  let empty = SortedMap.empty;
  let emptyWith = SortedMap.emptyWith;
  let equals = SortedMap.equals;
  let equalsWith = SortedMap.equalsWith;
  let every = SortedMap.every;
  let find = SortedMap.find;
  let first = SortedMap.first;
  let forEach = SortedMap.forEach;
  let fromMap = SortedMap.fromMap;
  let fromMapWith = SortedMap.fromMapWith;
  let fromSeq = SortedMap.fromSeq;
  let fromSeqWith = SortedMap.fromSeqWith;
  let get = SortedMap.get;
  let hash = SortedMap.hash;
  let hashWith = SortedMap.hashWith;
  let isEmpty = SortedMap.isEmpty;
  let isNotEmpty = SortedMap.isNotEmpty;
  let keys = SortedMap.keys;
  let last = SortedMap.last;
  let map = SortedMap.map;
  let merge = SortedMap.merge;
  let none = SortedMap.none;
  let put = SortedMap.put;
  let putAll = SortedMap.putAll;
  let reduce = SortedMap.reduce;
  let reduceRight = SortedMap.reduceRight;
  let remove = SortedMap.remove;
  let removeAll = SortedMap.removeAll;
  let removeFirst = SortedMap.removeFirst;
  let removeLast = SortedMap.removeLast;
  let some = SortedMap.some;
  let toSet = SortedMap.toSet;
  let toSetWith = SortedMap.toSetWith;
  let toMap = SortedMap.toMap;
  let toSeq = SortedMap.toSeq;
  let tryFind = SortedMap.tryFind;
  let tryFirst = SortedMap.tryFirst;
  let tryGet = SortedMap.tryGet;
  let tryLast = SortedMap.tryLast;
  let values = SortedMap.values;
};

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
  let some = SortedSet.some;
  let subtract = SortedSet.subtract;
  let toSet = SortedSet.toSet;
  let toMap = SortedSet.toMap;
  let toSeq = SortedSet.toSeq;
  let tryFind = SortedSet.tryFind;
  let tryFirst = SortedSet.tryFirst;
  let tryLast= SortedSet.tryLast;
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
  let toList = Stack.toList;
  let toSeq = Stack.toSeq;
  let tryFind = Stack.tryFind;
  let tryFirst = Stack.tryFirst;
};

let module StackMultimap = {
  type t 'k 'v = StackMultimap.stackMultimap 'k 'v;

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
  let toSeq = StackMultimap.toSeq;
  let tryFind = StackMultimap.tryFind;
  let values = StackMultimap.values;
};

let module Table = {
  type t 'row 'column 'value = Table.table 'row 'column 'value;

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
  let toSeq = Table.toSeq;
  let tryFind = Table.tryFind;
  let tryGet = Table.tryGet;
  let values = Table.values;
};

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
  let toMap = Vector.toMap;
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
  let isEmpty = TransientVector.isEmpty;
  let isNotEmpty = TransientVector.isNotEmpty;
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
