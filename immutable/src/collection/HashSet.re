open Collection;
open Equality;
open EqualitySet;
open Functions;
open Functions.Operators;
open HashStrategy;
open IntMap;
open Keyed;
open Option;
open Option.Operators;
open Ordering;
open Pair;
open Seq;
open SetImpl;
open SortedSet;
open Transient;

type hashNode 'a =
  | EqualitySet (equalitySet 'a)
  | SortedSet (sortedSet 'a)
  | Entry 'a;

type trieSet 'a = intMap (hashNode 'a);
type hashSet 'a = {
  count: int,
  root: trieSet 'a,
  strategy: hashStrategy 'a,
};

type transientTrieSet 'a = transientIntMap (hashNode 'a);
type transientHashSet 'a = {
  mutable count: int,
  root: transientTrieSet 'a,
  strategy: hashStrategy 'a,
};

let module HashNode = {
  let alter
      (strategy: hashStrategy 'a)
      (predicate: 'a => ordering)
      (f: option 'a => option 'a)
      (maybeHashNode: option (hashNode 'a)): (option (hashNode 'a)) => switch maybeHashNode {
    | Some (EqualitySet equalitySet) =>
        let newEqualitySet = equalitySet |> EqualitySet.alter predicate f;

        equalitySet === newEqualitySet ? maybeHashNode :
        (EqualitySet.count newEqualitySet) == 1 ? newEqualitySet
          |> EqualitySet.toSeq
          |> Seq.tryFirst
          >>| (fun entry => Entry entry) :
        Some (EqualitySet newEqualitySet)
    | Some (SortedSet sortedSet) =>
        let newSortedSet = sortedSet |> SortedSet.alter predicate f;

        sortedSet === newSortedSet ? maybeHashNode :
        (SortedSet.count newSortedSet) == 1 ? newSortedSet
          |> SortedSet.toSeq
          |> Seq.tryFirst
          >>| (fun entry => Entry entry) :
        Some (SortedSet newSortedSet)
    | Some (Entry value) when predicate value === Equal => (f @@ Option.return @@ value) >>= fun newValue =>
        value === newValue ? maybeHashNode : Some (Entry newValue)
    | Some (Entry value) => (f None) >>= fun newValue => switch strategy {
        | Equality _ equality =>
            let newEntrySet = EqualitySet.emptyWith equality
              |> EqualitySet.put value
              |> EqualitySet.put newValue;
            Some (EqualitySet newEntrySet)
        | Comparator _ comparator =>
            let newEntrySet = SortedSet.emptyWith comparator
              |> SortedSet.put value
              |> SortedSet.put newValue;
            Some (SortedSet newEntrySet)
      }
    | None => f None >>| (fun value => Entry value)
  };

  let put
      (strategy: hashStrategy 'a)
      (newValue: 'a)
      (maybeHashNode: option (hashNode 'a)): (option (hashNode 'a)) => switch (maybeHashNode, strategy) {
    | (Some (EqualitySet equalitySet), _) =>
        let newEqualitySet = equalitySet |> EqualitySet.put newValue;
        equalitySet === newEqualitySet ? maybeHashNode : Some (EqualitySet newEqualitySet)
    | (Some (SortedSet sortedSet), _) =>
        let newSortedSet = sortedSet |> SortedSet.put newValue;
        sortedSet === newSortedSet ? maybeHashNode : Some (SortedSet newSortedSet)
    | (Some (Entry value), _) when value === newValue => maybeHashNode
    | (Some (Entry value), _) when (HashStrategy.comparator strategy value newValue) === Equal =>
        Some (Entry newValue)
    | (Some (Entry value), Equality _ equality) =>
        let newEntrySet = EqualitySet.emptyWith equality
          |> EqualitySet.put value
          |> EqualitySet.put newValue;
        Some (EqualitySet newEntrySet)
    | (Some (Entry value), Comparator _ comparator) =>
        let newEntrySet = SortedSet.emptyWith comparator
          |> SortedSet.put value
          |> SortedSet.put newValue;
        Some (SortedSet newEntrySet)
    | (None, _) => Some (Entry newValue)
  };

  let remove
      (strategy: hashStrategy 'a)
      (value: 'a)
      (maybeHashNode: option (hashNode 'a)): (option (hashNode 'a)) => switch maybeHashNode {
    | Some (EqualitySet equalitySet) =>
        let newEqualitySet = equalitySet |> EqualitySet.remove value;

        equalitySet === newEqualitySet ? maybeHashNode :
        (EqualitySet.count newEqualitySet) == 1 ? newEqualitySet
          |> EqualitySet.toSeq
          |> Seq.tryFirst
          >>| (fun entry => Entry entry) :
        Some (EqualitySet newEqualitySet)
    | Some (SortedSet sortedSet) =>
        let newSortedSet = sortedSet |> SortedSet.remove value;

        sortedSet === newSortedSet ? maybeHashNode :
        (SortedSet.count newSortedSet) == 1 ? newSortedSet
          |> SortedSet.toSeq
          |> Seq.tryFirst
          >>| (fun entry => Entry entry) :
        Some (SortedSet newSortedSet)
    | Some (Entry entryValue) when (HashStrategy.comparator strategy entryValue value) === Equal => None
    | _ => maybeHashNode
  };

  let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) (hashNode: hashNode 'a): 'acc => switch hashNode {
    | EqualitySet equalitySet => equalitySet |> EqualitySet.reduce f acc
    | SortedSet sortedSet => sortedSet |> SortedSet.reduce f acc
    | Entry entry => f acc entry
  };

  let toSeq (hashNode: hashNode 'a): (seq 'a) => switch hashNode {
    | EqualitySet equalitySet => equalitySet |> EqualitySet.toSeq
    | SortedSet sortedSet => sortedSet |> SortedSet.toSeq
    | Entry entry => Seq.return entry
  };
};

let module HashSetImpl = {
  module type HashSetBase = {
    type set 'a;
    type root 'a;

    let alterRoot: int => (option (hashNode 'a) => option (hashNode 'a)) => (root 'a) => (root 'a);
    let count: set 'a => int;
    let root: set 'a => (root 'a);
    let strategy: set 'a => hashStrategy 'a;
    let tryGet: int => (root 'a) => option (hashNode 'a);
    let updateWith: (root 'a) => int => (hashStrategy 'a) => (set 'a) => set 'a;
  };

  module type S = {
    type set 'a;
    type root 'a;

    let alter: int => ('a => ordering) => (option 'a => option 'a) => (set 'a) => (set 'a);
    let count: (set 'a) => int;
    let contains: 'a => (set 'a) => bool;
    let find: int => ('a => ordering) => (set 'a) => (option 'a);
    let put: 'a => (set 'a) => (set 'a);
    let putAll: (seq 'a) => (set 'a) => (set 'a);
    let remove: 'a => (set 'a) => (set 'a);
  };

  let module Make: (
    X: HashSetBase
  ) => S with type set 'a = X.set 'a and type root 'a = X.root 'a = fun (X: HashSetBase) => {
    type set 'a = X.set 'a;
    type root 'a = X.root 'a;

    let alterImpl
        (alter: option (hashNode 'a) => option (hashNode 'a))
        (hash: int)
        (set: set 'a): (set 'a) => {
      let count = X.count set;
      let newCount = ref count;
      let strategy = set |> X.strategy;

      let alterHashNode (maybeHashNode: option (hashNode 'a)): option (hashNode 'a) => {
        let newHashNode = alter maybeHashNode;

        /* I'm not proud of this but it works */
        newCount := switch (maybeHashNode, newHashNode) {
          | (None, Some (Entry _))
          | (Some (Entry _), Some (EqualitySet _))
          | (Some (Entry _), Some (SortedSet _)) =>
              count + 1

          | (Some (EqualitySet _), Some (Entry _))
          | (Some (SortedSet _), Some (Entry _))
          | (Some (Entry _), None) =>
              count - 1

          | (Some (EqualitySet oldEqualitySet), Some (EqualitySet newEqualitySet)) =>
              count + (newEqualitySet |> EqualitySet.count) - (oldEqualitySet |> EqualitySet.count)
          | (Some (SortedSet oldSortedSet), Some (SortedSet newSortedSet)) =>
              count + (newSortedSet |> SortedSet.count) - (oldSortedSet |> SortedSet.count)

          | (Some (Entry _), Some (Entry _))
          | (None, None) =>
              count

          | _ => failwith "Invalid state"
        };

        newHashNode
      };

      let root = set |> X.root;
      let newRoot = root |> X.alterRoot hash alterHashNode;
      let newCount = !newCount;

      (root === newRoot) && (count == newCount) ? set : set |> X.updateWith newRoot newCount strategy;
    };

    let alter
        (hash: int)
        (predicate: 'a => ordering)
        (f: option 'a => option 'a)
        (set: set 'a): (set 'a) => {
      let strategy = set |> X.strategy;
      alterImpl (HashNode.alter strategy predicate f) hash set
    };

    let count = X.count;

    let find
        (hash: int)
        (predicate: 'a => ordering)
        (set: set 'a): (option 'a) => set
      |> X.root
      |> X.tryGet hash >>= fun
        | EqualitySet equalitySet => equalitySet |> EqualitySet.find predicate
        | SortedSet sortedSet => sortedSet |> SortedSet.find predicate
        | Entry entry => (predicate entry) === Equal ? Some entry : None;

    let contains (entry: 'a) (set: set 'a): bool => {
      let strategy = X.strategy set;
      let predicate = HashStrategy.comparator strategy entry;
      let hash = strategy |> HashStrategy.hash entry;

      set
      |> X.root
      |> X.tryGet hash >>| (fun
        | EqualitySet equalitySet => equalitySet |> EqualitySet.contains entry
        | SortedSet sortedSet => sortedSet |> SortedSet.contains entry
        | Entry entry => (predicate entry) === Equal ? true : false
      ) |? false;
    };

    let put (entry: 'a) (set: set 'a): (set 'a) => {
      let strategy = set |> X.strategy;
      let hash = strategy |> HashStrategy.hash entry;

      alterImpl (HashNode.put strategy entry) hash set
    };

    let putAll (seq: seq 'a) (set: set 'a): (set 'a) => seq
      |> Seq.reduce (fun acc next => acc |> put next) set;

    let remove (entry: 'a) (set: set 'a): (set 'a) => {
      let strategy = set |> X.strategy;
      let hash = strategy |> HashStrategy.hash entry;

      alterImpl (HashNode.remove strategy entry) hash set
    };
  };
};

let module PersistentHashSetImpl = HashSetImpl.Make {
  type set 'a = hashSet 'a;
  type root 'a = trieSet 'a;

  let alterRoot = IntMap.alter;
  let count ({ count }: hashSet 'a): int => count;
  let create = create;
  let root ({ root }: hashSet 'a): (root 'a) => root;
  let strategy ({ strategy }: hashSet 'a): (hashStrategy 'a) => strategy;
  let tryGet = IntMap.tryGet;
  let updateWith
      (root: trieSet 'a)
      (count: int)
      (strategy: hashStrategy 'a)
      (set: hashSet 'a): hashSet 'a =>
    { root, count, strategy };
};

let module TransientHashSetImpl = HashSetImpl.Make {
  type set 'a = transientHashSet 'a;
  type root 'a = transientTrieSet 'a;

  let alterRoot = TransientIntMap.alter;
  let count ({ count }: transientHashSet 'a): int => count;
  let create = create;
  let root ({ root }: set 'a): (root 'a) => root;
  let strategy ({ strategy }: transientHashSet 'a): (hashStrategy 'a) => strategy;
  let tryGet = TransientIntMap.tryGet;
  let updateWith
      (root: transientTrieSet 'a)
      (count: int)
      (strategy: hashStrategy 'a)
      (set: transientHashSet 'a): transientHashSet 'a => {
    set.count = count;
    set;
  };
};

let module TransientHashSet = {
  let alter = TransientHashSetImpl.alter;
  let count = TransientHashSetImpl.count;
  let contains = TransientHashSetImpl.contains;
  let find = TransientHashSetImpl.find;

  let persist ({ count, root, strategy }: transientHashSet 'a): (hashSet 'a) => {
    count,
    root: root |> TransientIntMap.persist,
    strategy
  };

  let put = TransientHashSetImpl.put;
  let putAll = TransientHashSetImpl.putAll;
  let remove = TransientHashSetImpl.remove;
  let removeAll ({ root } as transient: transientHashSet 'a): (transientHashSet 'a) => {
    root |> TransientIntMap.removeAll |> ignore;
    transient.count = 0;
    transient;
  };
};

let alter = PersistentHashSetImpl.alter;
let count = PersistentHashSetImpl.count;
let contains = PersistentHashSetImpl.contains;

let empty (): hashSet 'a => {
  count: 0,
  root: IntMap.empty,
  strategy: HashStrategy.structuralCompare (),
};

let emptyWith (strategy: hashStrategy 'a): (hashSet 'a) => ({
  count: 0,
  root: IntMap.empty,
  strategy,
});

let find = PersistentHashSetImpl.find;

let mutate ({ count, root, strategy }: hashSet 'a): (transientHashSet 'a) => {
  count,
  root: root |> IntMap.mutate,
  strategy,
};

let putAll (seq: seq 'a) (set: hashSet 'a): (hashSet 'a) => set
  |> mutate
  |> TransientHashSet.putAll seq
  |> TransientHashSet.persist;

let fromSeq (seq: seq 'a): (hashSet 'a) =>
  empty () |> putAll seq;

let fromSeqWith (strategy: hashStrategy 'a) (seq: seq 'a): (hashSet 'a) =>
  emptyWith strategy |> putAll seq;

let put = PersistentHashSetImpl.put;

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) ({ root }: hashSet 'a): 'acc => {
  let reducer acc _ hashNode => hashNode |> HashNode.reduce f acc;
  root |> IntMap.reduceWithKey reducer acc
};

let remove = PersistentHashSetImpl.remove;

let removeAll ({ strategy }: hashSet 'a): (hashSet 'a) => emptyWith strategy;

let toSeq ({ root }: hashSet 'a): (seq 'a) => root |> IntMap.toSeq
  |> Seq.flatMap (fun (_, hashNode) => hashNode |> HashNode.toSeq);
