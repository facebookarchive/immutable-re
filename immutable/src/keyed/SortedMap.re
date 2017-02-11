open Comparator;
open Equality;
open Functions;
open Keyed;
open Option;
open Option.Operators;
open Pair;
open Seq;
open SortedSet;

type sortedMap 'k 'v = {
  keyComparator: comparator 'k,
  set: sortedSet ('k, 'v),
};

let alter
    (key: 'k)
    (f: option 'v => option 'v)
    ({ keyComparator, set } as map: sortedMap 'k 'v): (sortedMap 'k 'v) => {
  let newSet = set |> SortedSet.alter (KeyedEntry.keyComparatorFinder keyComparator key) (KeyedEntry.alter key f);
  set === newSet ? map : ({ keyComparator, set: newSet });
};

let count ({ set }: sortedMap 'k 'v): int =>
  set |> SortedSet.count;

let emptyWith (keyComparator: comparator 'k): sortedMap 'k 'v => ({
  keyComparator,
  set: SortedSet.emptyWith (KeyedEntry.keyComparator keyComparator),
});

let empty (): sortedMap 'k 'v =>
  emptyWith Comparator.structural;

let tryGet (key: 'k) ({ keyComparator, set }: sortedMap 'k 'v): (option 'v) => set
  |> SortedSet.find (KeyedEntry.keyComparatorFinder keyComparator key)
  >>| snd;

let put (key: 'k) (value: 'v) ({ keyComparator, set } as map: sortedMap 'k 'v): (sortedMap 'k 'v) => {
  let newSet = set |> SortedSet.put (key, value);
  set === newSet ? map : ({ keyComparator, set: newSet });
};

let remove (key: 'k) (map: sortedMap 'k 'v): (sortedMap 'k 'v) =>
  map |> alter key alwaysNone;

let removeAll ({ keyComparator, set }: sortedMap 'k 'v): (sortedMap 'k 'v) =>
  emptyWith keyComparator;

let toSeq ({ set }: sortedMap 'k 'v): (seq ('k, 'v)) =>
  set |> SortedSet.toSeq;

let toKeyed (map: sortedMap 'k 'v): (keyed 'k 'v) => Keyed.create
  count::(map |> count)
  seq::(map |> toSeq)
  tryGet::(fun k => map |> tryGet k);

let putAll (seq: seq ('k, 'v)) (map: sortedMap 'k 'v): (sortedMap 'k 'v) => seq
  |> Seq.reduce (fun acc (k, v) => acc |> put k v) map;

let fromSeqWith (comparator: comparator 'k) (seq: seq ('k, 'v)): (sortedMap 'k 'v) =>
  emptyWith comparator |> putAll seq;

let fromSeq (seq: seq ('k, 'v)): (sortedMap 'k 'v) => fromSeqWith (Comparator.structural) seq;

let fromKeyedWith (comparator: comparator 'k) (keyed: keyed 'k 'v): (sortedMap 'k 'v) =>
  keyed |> Keyed.toSeq |> fromSeqWith comparator;

let fromKeyed (keyed: keyed 'k 'v): (sortedMap 'k 'v) => keyed |> Keyed.toSeq |> fromSeq;

let map (f: 'a => 'b) ({ keyComparator } as map: sortedMap 'k 'a): (sortedMap 'k 'b) => {
  let empty = emptyWith keyComparator;
  empty |> putAll (map |> toSeq |> Seq.map (Pair.mapSnd f));
};

let mapWithKey (f: 'k => 'a => 'b) ({ keyComparator } as map: sortedMap 'k 'a): (sortedMap 'k 'b) => {
  let empty = emptyWith keyComparator;
  empty |> putAll (map |> toSeq |> Seq.map @@ Pair.mapSndWithFst @@ f);
};

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) ({ set }: sortedMap 'k 'a): 'acc => {
  let reducer acc (_, value) => f acc value;
  set |> SortedSet.reduce reducer acc
};

let reduceWithKey (f: 'acc => 'k => 'a => 'acc) (acc: 'acc) ({ set }: sortedMap 'k 'a): 'acc => {
  let reducer acc (key, value) => f acc key value;
  set |> SortedSet.reduce reducer acc
};

let reduceRight (f: 'acc => 'a => 'acc) (acc: 'acc) ({ set }: sortedMap 'k 'a): 'acc => {
  let reducer acc (_, value) => f acc value;
  set |> SortedSet.reduceRight reducer acc
};

let reduceRightWithKey (f: 'acc => 'k => 'a => 'acc) (acc: 'acc) ({ set }: sortedMap 'k 'a): 'acc => {
  let reducer acc (key, value) => f acc key value;
  set |> SortedSet.reduceRight reducer acc
};

let merge
    (f: 'k => (option 'vAcc) => (option 'v) => (option 'vAcc))
    (next: keyed 'k 'v)
    (map: sortedMap 'k 'vAcc): (sortedMap 'k 'vAcc) =>
  Collection.union (map |> toKeyed |> Keyed.keys) (next |> Keyed.keys)
    |> Seq.reduce (
        fun acc key => {
          let result = f key (map |> tryGet key) (next |> Keyed.tryGet key);
          switch result {
            | None => acc |> remove key
            | Some value => acc |> put key value
          }
        }
      )
      map;
