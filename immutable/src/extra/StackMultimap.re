open Equality;
open Keyed;
open HashMap;
open HashStrategy;
open Option.Operators;
open Pair;
open Seq;
open Stack;

type stackMultimap 'k 'v = {
  count: int,
  map: hashMap 'k (stack 'v),
};

let add (key: 'k) (value: 'v) ({ count, map }: stackMultimap 'k 'v): (stackMultimap 'k 'v) => ({
  count: count + 1,
  map: map |> HashMap.put key (map |> HashMap.tryGet key |? Stack.empty |> Stack.addFirst value),
});

let count ({ count }: stackMultimap 'k 'v) => count;

let empty (): (stackMultimap 'k 'v) => ({
  count: 0,
  map: HashMap.empty (),
});

let emptyWith (hashStrategy: hashStrategy 'k): (stackMultimap 'k 'v) => ({
  count: 0,
  map: HashMap.emptyWith hashStrategy,
});

let get (key: 'k) ({ map }: stackMultimap 'k 'v): (stack 'v) =>
  map |> HashMap.tryGet key |? Stack.empty;

let reduce (f: 'acc => 'v => 'acc) (acc: 'acc) ({ map }: stackMultimap 'k 'v): 'acc => {
  let rec reducer acc _ values =>
    values |> Stack.reduce (fun acc v => f acc v) acc;

  map |> HashMap.reduceWithKey reducer acc;
};

let reduceWithKey (f: 'acc => 'k => 'v => 'acc) (acc: 'acc) ({ map }: stackMultimap 'k 'v): 'acc => {
  let rec reducer acc key values =>
    values |> Stack.reduce (fun acc v => f acc key v) acc;

  map |> HashMap.reduceWithKey reducer acc;
};

let remove (key: 'k) ({ count, map } as stackMultimap: stackMultimap 'k 'v): (stackMultimap 'k 'v) =>
  map |> HashMap.tryGet key >>| (fun stack => ({
    count: count - (Stack.count stack),
    map: map |> HashMap.remove key,
  })) |? stackMultimap;

let removeAll ({ map }: stackMultimap 'k 'v): (stackMultimap 'k 'v) =>
  { count: 0, map: map |> HashMap.removeAll };

let toKeyed ({ map }: stackMultimap 'k 'v): (keyed 'k (seq 'v)) => Keyed.create
  count::(map |> HashMap.count)
  seq::(map |> HashMap.toSeq |> Seq.map (fun (k, stack) => (k, stack |> Stack.toSeq)))
  tryGet::(fun k => map |> HashMap.tryGet k >>| (fun stack => stack |> Stack.toSeq));

let toSeq ({ map }: stackMultimap 'k 'v): (seq ('k, 'v)) => map
  |> HashMap.toSeq
  |> Seq.flatMap (
    fun (k, stack) =>
      stack |> Stack.toSeq |> Seq.map (Pair.create k)
  );
