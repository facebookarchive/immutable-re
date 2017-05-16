/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type s 'keyedIterable 'k 'v = {
  reduce: 'acc . (while_::('acc => 'k => 'v => bool) => ('acc => 'k => 'v => 'acc) => 'acc => 'keyedIterable => 'acc)
};

type t 'k 'v =
  | Empty
  | Instance 'keyedIterable (s 'keyedIterable 'k 'v): t 'k 'v;

type keyedIterable 'k 'v = t 'k 'v;

module type SGeneric = {
  type t 'k 'v;
  type k 'k;
  type v 'v;

  let every: (k 'k => v 'v => bool) => (t 'k 'v) => bool;
  let find: selector::(k 'k => v 'v => 'c) => (k 'k => v 'v => bool) => (t 'k 'v) => (option 'c);
  let findOrRaise: selector::(k 'k => v 'v => 'c) => (k 'k => v 'v => bool) => (t 'k 'v) => 'c;
  let forEach: while_::(k 'k => v 'v => bool)? => (k 'k => v 'v => unit) => (t 'k 'v) => unit;
  let keys: (t 'k 'v) => (Iterable.t (k 'k));
  let none: (k 'k => v 'v => bool) => (t 'k 'v) => bool;
  let reduce: while_::('acc => k 'k => v 'v => bool)? => ('acc => k 'k => v 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceKeys: while_::('acc => k  'k => bool)? => ('acc => k 'k => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceValues: while_::('acc => v 'v => bool)? => ('acc => v 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let some: (k 'k => v 'v => bool) => (t 'k 'v) => bool;
  let toIterable: (k 'k => v 'v => 'c) => t 'k 'v => Iterable.t 'c;
  let toKeyedIterable: t 'k 'v => keyedIterable (k 'k) (v 'v);
  let values: (t 'k 'v) => Iterable.t (v 'v);
};

module type S1 = {
  type k;
  type t 'v;

  let every: (k => 'v => bool) => (t 'v) => bool;
  let find: selector::(k => 'v => 'c) => (k => 'v => bool) => (t 'v) => (option 'c);
  let findOrRaise: selector::(k => 'v => 'c) => (k => 'v => bool) => (t 'v) => 'c;
  let forEach: while_::(k => 'v => bool)? => (k => 'v => unit) => (t 'v) => unit;
  let keys: (t 'v) => (Iterable.t k);
  let none: (k => 'v => bool) => (t 'v) => bool;
  let reduce: while_::('acc => k => 'v => bool)? => ('acc => k => 'v => 'acc) => 'acc => (t 'v) => 'acc;
  let reduceKeys: while_::('acc => k => bool)? => ('acc => k => 'acc) => 'acc => (t 'v) => 'acc;
  let reduceValues: while_::('acc => 'v => bool)? => ('acc => 'v => 'acc) => 'acc => (t 'v) => 'acc;
  let some: (k => 'v => bool) => (t 'v) => bool;
  let toIterable: (k => 'v => 'c) => t 'v => Iterable.t 'c;
  let toKeyedIterable: t 'v => keyedIterable k 'v;
  let values: (t 'v) => Iterable.t 'v;
};

module type S2 = {
  type t 'k 'v;

  let every: ('k => 'v => bool) => (t 'k 'v) => bool;
  let find: selector::('k => 'v => 'c) => ('k => 'v => bool) => (t 'k 'v) => (option 'c);
  let findOrRaise: selector::('k => 'v => 'c) => ('k => 'v => bool) => (t 'k 'v) => 'c;
  let forEach: while_::('k => 'v => bool)? => ('k => 'v => unit) => (t 'k 'v) => unit;
  let keys: (t 'k 'v) => (Iterable.t 'k);
  let none: ('k => 'v => bool) => (t 'k 'v) => bool;
  let reduce: while_::('acc => 'k => 'v => bool)? => ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceKeys: while_::('acc => 'k => bool)? => ('acc => 'k => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceValues: while_::('acc => 'v => bool)? => ('acc => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let some: ('k => 'v => bool) => (t 'k 'v) => bool;
  let toIterable: ('k => 'v => 'c) => t 'k 'v => Iterable.t 'c;
  let toKeyedIterable: t 'k 'v => keyedIterable 'k 'v;
  let values: (t 'k 'v) => Iterable.t 'v;
};

let everyPredicate acc _ _ => acc;
let findPredicate acc _ _ => Option.isEmpty acc;
let nonePredicate = everyPredicate;
let somePredicate acc _ _ => not acc;

let module MakeGeneric = fun (Base: {
  type t 'k 'v;
  type k 'k;
  type v 'v;

  let isEmpty: (t 'k 'v) => bool;
  let reduce: while_::('acc => k 'k => v 'v => bool) => ('acc => k 'k => v 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceKeys: while_::('acc => k 'k => bool) => ('acc => k 'k => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceValues: while_::('acc => v 'v => bool) => ('acc => v 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
}) => ({
  include Base;

  let every (f: k 'k => v 'v => bool) (iter: t 'k 'v): bool =>
    iter |> reduce while_::everyPredicate (fun _ => f) true;

  let find selector::(selector: k 'k => v 'v => 'c) (f: k 'k => v 'v => bool) (iter: t 'k 'v): (option 'c) =>
    iter |> reduce
      while_::findPredicate
      (fun _ k v => if (f k v) (Some (selector k v)) else None)
      None;

  let findOrRaise selector::(selector: k 'k => v 'v => 'c) (f: k 'k => v 'v => bool) (iter: t 'k 'v): 'c =>
    find ::selector f iter |> Option.firstOrRaise;

  let forEach
      while_::(predicate: k 'k => v 'v => bool)=Functions.alwaysTrue2
      (f: k 'k => v 'v => unit)
      (iter: t 'k 'v) =>
    if (predicate === Functions.alwaysTrue2) {
      iter|> Base.reduce while_::Functions.alwaysTrue3 (fun _ => f) ();
    }
    else iter |> reduce while_::(fun _ => predicate) (fun _ => f) ();

  let none (f: k 'k => v 'v => bool) (iter: t 'k 'v): bool =>
    iter |> reduce
      while_::nonePredicate
      (fun _ k v => f k v |> not)
      true;

  let reduce
      while_::(predicate: 'acc => k 'k => v 'v => bool)=Functions.alwaysTrue3
      (f: 'acc => k 'k => v 'v => 'acc)
      (acc: 'acc)
      (map: t 'k 'v): 'acc =>
    reduce while_::predicate f acc map;

  let reduceKeys
      while_::(predicate: 'acc => k 'k => bool)=Functions.alwaysTrue2
      (f: 'acc => k 'k => 'acc)
      (acc: 'acc)
      (map: t 'k 'v): 'acc =>
    reduceKeys while_::predicate f acc map;

  let reduceValues
      while_::(predicate: 'acc => v 'v => bool)=Functions.alwaysTrue2
      (f: 'acc => v 'v => 'acc)
      (acc: 'acc)
      (map: t 'k 'v): 'acc =>
    reduceValues while_::predicate f acc map;

  let some (f: k 'k => v 'v => bool) (iter: t 'k 'v): bool =>
    iter |> reduce while_::somePredicate (fun _ => f) false;

  let keysIterableBase: Iterable.s (t 'k 'v) (k 'k) = { reduce: Base.reduceKeys };

  let keys (keyedIterable: t 'k _): Iterable.t (k 'k) =>
    if (isEmpty keyedIterable) (Iterable.empty ())
    else Iterable.Instance keyedIterable keysIterableBase;

  let toIterable (selector: k 'k => v 'v => 'c) (keyedIterable: t 'k 'v): (Iterable.t 'c) =>
    if (isEmpty keyedIterable) (Iterable.empty ())
    else Iterable.Instance keyedIterable {
      reduce: fun while_::predicate f acc map =>
      if (predicate === Functions.alwaysTrue2) {
        let f acc k v => f acc (selector k v);
        reduce f acc map;
      }
      else {
        let memoizedPair = MutableOption.empty ();

        let predicate acc k v => {
          let pair = (selector k v);
          memoizedPair |> MutableOption.set pair;
          predicate acc pair;
        };

        let f acc _ _ => f acc (MutableOption.firstOrRaise memoizedPair);
        reduce while_::predicate f acc map;
      }
    };

  let keyedIterableBase: s (t 'k 'v) (k 'k) (v 'v) = { reduce: Base.reduce };

  let toKeyedIterable (keyedIterable: t 'k 'v): (keyedIterable (k 'k) (v 'v)) =>
    if (isEmpty keyedIterable) Empty
    else Instance keyedIterable keyedIterableBase;

  let valuesIterableBase: Iterable.s (t 'k 'v) (v 'v) = { reduce: Base.reduceValues };

  let values (keyedIterable: t 'k _): Iterable.t (v 'v) =>
    if (isEmpty keyedIterable) (Iterable.empty ())
    else Iterable.Instance keyedIterable valuesIterableBase;
}: SGeneric with type t 'k 'v := Base.t 'k 'v and type k 'k := Base.k 'k and type v 'v := Base.v 'v);

let module Make1 = fun (Base: {
  type k;
  type t 'v;

  let isEmpty: (t 'v) => bool;
  let reduce: while_::('acc => k => 'v => bool) => ('acc => k => 'v => 'acc) => 'acc => (t 'v) => 'acc;
  let reduceKeys: while_::('acc => k => bool) => ('acc => k => 'acc) => 'acc => (t 'v) => 'acc;
  let reduceValues: while_::('acc => 'v => bool) => ('acc => 'v => 'acc) => 'acc => (t 'v) => 'acc;
}) => ((MakeGeneric {
  type t 'k 'v  = Base.t 'v;
  type k 'k = Base.k;
  type v 'v = 'v;

  let isEmpty = Base.isEmpty;
  let reduce = Base.reduce;
  let reduceKeys = Base.reduceKeys;
  let reduceValues  = Base.reduceValues;
}): S1 with type t 'v := Base.t 'v and type k := Base.k);

let module Make2 = fun (Base: {
  type t 'k 'v;

  let isEmpty: (t 'k 'v) => bool;
  let reduce: while_::('acc => 'k => 'v => bool) => ('acc => 'k => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceKeys: while_::('acc => 'k => bool) => ('acc => 'k => 'acc) => 'acc => (t 'k 'v) => 'acc;
  let reduceValues: while_::('acc => 'v => bool) => ('acc => 'v => 'acc) => 'acc => (t 'k 'v) => 'acc;
}) => ((MakeGeneric {
  type t 'k 'v  = Base.t 'k 'v;
  type k 'k = 'k;
  type v 'v = 'v;

  let isEmpty = Base.isEmpty;
  let reduce = Base.reduce;
  let reduceKeys = Base.reduceKeys;
  let reduceValues  = Base.reduceValues;
}): S2 with type t 'k 'v := Base.t 'k 'v);

include (Make2 {
  type nonrec t 'k 'v = t 'k 'v;

  let isEmpty (iter: t 'k 'v): bool =>
    iter === Empty;

  include (KeyedReducer.Make2 {
    type nonrec t 'k 'v = t 'k 'v;

    let reduce
        while_::(predicate:'acc => 'k => 'v => bool)
        (f: 'acc => 'k => 'v => 'acc)
        (acc: 'acc)
        (iter: t 'k 'v): 'acc => switch iter {
      | Empty => acc
      | Instance iter { reduce } => iter |> reduce while_::predicate f acc;
    };
  }: KeyedReducer.S2 with type t 'k 'v := t 'k 'v);
}: S2 with type t 'k 'v := t 'k 'v);

let concatImpl: s 'keyedIterable 'k 'v = {
  reduce: fun while_::predicate f acc iters => {
    let shouldContinue = ref true;

    let predicate acc key value => {
      let result = predicate acc key value;
      shouldContinue := result;
      result;
    };

    iters |> ImmList.reduce
      while_::(fun _ _ => !shouldContinue)
      (fun acc next => next |> reduce while_::predicate f acc)
      acc
  }
};

let concat (iters: list (t 'k 'v)): (t 'k 'v) => switch iters {
  | [] => Empty
  | _ => Instance iters concatImpl
};

let deferImpl: s 'keyedIterable 'k 'v = {
  reduce: fun while_::predicate f acc provider =>
    (provider ()) |> reduce while_::predicate f acc
};

let defer (provider: unit => (t 'k 'v)): (t 'k 'v) =>
  Instance provider deferImpl;

let distinctUntilChangedWith
    keyEquals::(keyEquals: Equality.t 'k)
    valueEquals::(valueEquals: Equality.t 'v)
    (iter: t 'k 'v): (t 'k 'v) => switch iter {
  | Empty => Empty
  | Instance iter { reduce } => Instance iter {
      reduce: fun while_::predicate f acc iter => {
        let previousKey = MutableOption.empty ();
        let previousValue = MutableOption.empty ();

        let shouldNotSkip nextKey nextValue =>
          (MutableOption.isEmpty previousKey) ||
          (not (
            previousKey |> MutableOption.firstOrRaise |> keyEquals nextKey  &&
            previousValue |> MutableOption.firstOrRaise |> valueEquals nextValue
          ));

        let predicate acc key value =>
          if (shouldNotSkip key value) (predicate acc key value)
          else true;

        let f acc key value =>
          if (shouldNotSkip key value) {
            previousKey |> MutableOption.set key;
            previousValue |> MutableOption.set value;
            f acc key value;
          }
          else acc;

        iter |> reduce while_::predicate f acc
      }
    };
};

let doOnNext (sideEffect: 'k => 'v => unit) (iter: t 'k 'v): (t 'k 'v) => switch iter {
  | Empty => Empty
  | Instance iter { reduce } => Instance iter {
      reduce: fun while_::predicate f acc iter => iter |> reduce
        while_::predicate
        (fun acc k v => { sideEffect k v; (f acc k v) })
        acc
    }
};

let empty (): t 'k 'v => Empty;

let filter (filter: 'k => 'v => bool) (iter: t 'k 'v): (t 'k 'v) => switch iter {
  | Empty => Empty
  | Instance iter { reduce } => Instance iter {
      reduce: fun while_::predicate f acc iter => {
        let predicate acc key value =>
          if (filter key value) (predicate acc key value)
          else true;

        iter |> reduce
          while_::predicate
          (fun acc k v => if (filter k v) (f acc k v) else acc)
          acc;
      }
    }
};

let flatMap (mapper: 'kA => 'vA => t 'kB 'vB) (iter: t 'kA 'vA): (t 'kB 'vB) => switch iter {
  | Empty => Empty
  | Instance iter { reduce: reduceIter } => Instance iter {
      reduce: fun while_::predicate f acc iter => {
        let shouldContinue = ref true;

        let predicate acc key value => {
          let result = predicate acc key value;
          shouldContinue := result;
          result;
        };

        iter |> reduceIter while_::(fun _ _ _ => !shouldContinue) (
          fun acc k v => (mapper k v) |> reduce while_::predicate f acc
        ) acc
      }
    }
};

let fromEntriesImpl: s (Iterable.t ('k, 'v)) 'k 'v  = {
  reduce: fun while_::predicate f acc iter => {
    let predicate acc (k, v) => predicate acc k v;
    let f acc (k, v) => f acc k v;

    iter |> Iterable.reduce while_::predicate f acc;
  }
};

let fromEntries (iter: Iterable.t ('k, 'v)): (t 'k 'v) => switch iter {
  | Iterable.Empty => Empty
  | Iterable.Instance  _ _ => Instance iter fromEntriesImpl
};

let generateImpl: s ('k => 'v => 'k, 'k => 'v => 'v, 'k, 'v) 'k 'v = {
  reduce: fun while_::predicate f acc (genKey, genValue, initialKey, initialValue) => {
    let rec recurse genKey genValue key value predicate f acc => {
      let nextKey = genKey key value;
      let nextValue = genValue key value;

      if (predicate acc nextKey nextValue) {
        let acc = f acc nextKey nextValue;
        recurse genKey genValue nextKey nextValue predicate f acc;
      }
      else acc;
    };

    if (predicate acc initialKey initialValue) {
      let acc = f acc initialKey initialValue;
      recurse genKey genValue initialKey initialValue predicate f acc;
    } else acc;
  }
};

let generate
    genKey::(genKey: 'k => 'v => 'k)
    genValue::(genValue: 'k => 'v => 'v)
    (initialKey: 'k)
    (initialValue: 'v): (t 'k 'v) =>
  Instance (genKey, genValue, initialKey, initialValue) generateImpl;

let map
    keyMapper::(keyMapper: 'kA => 'vA => 'kB)
    valueMapper::(valueMapper: 'kA => 'vA => 'vB)
    (iter: t 'kA 'vA): (t 'kB 'vB) => switch iter {
  | Empty => Empty
  | Instance iter { reduce }  => Instance iter {
      reduce: fun while_::predicate f acc iter => {
        let memoizedKey = MutableOption.empty ();
        let memoizedValue = MutableOption.empty ();

        let predicate acc key value => {
          let nextKey = keyMapper key value;
          let nextValue = valueMapper key value;

          memoizedKey |> MutableOption.set nextKey;
          memoizedValue |> MutableOption.set nextValue;

          predicate acc nextKey nextValue
        };

        let f acc _ _ =>
          f acc (MutableOption.firstOrRaise memoizedKey) (MutableOption.firstOrRaise memoizedValue);

        iter |> reduce while_::predicate f acc
      }
    }
};

let mapKeys (mapper: 'a => 'v => 'b) (iter: t 'a 'v): (t 'b 'v) => switch iter {
  | Empty => Empty
  | Instance iter { reduce } => Instance iter {
      reduce: fun while_::predicate f acc iter => {
        let memoizedKey = MutableOption.empty ();

        let predicate acc key value => {
          let nextKey = mapper key value;

          memoizedKey |> MutableOption.set nextKey;
          predicate acc nextKey value
        };

        let f acc _ value =>
          f acc (MutableOption.firstOrRaise memoizedKey) value;

        iter |> reduce while_::predicate f acc
      }
    }
};

let mapValues (mapper: 'k => 'a => 'b) (iter: t 'k 'a): (t 'k 'b) => switch iter {
  | Empty => Empty
  | Instance iter { reduce } => Instance iter {
      reduce: fun while_::predicate f acc iter => {
        let memoizedValue = MutableOption.empty ();

        let predicate acc key value => {
          let nextValue = mapper key value;

          memoizedValue |> MutableOption.set nextValue;
          predicate acc key nextValue
        };

        let f acc key _ => f acc key (MutableOption.firstOrRaise memoizedValue);

        iter |> reduce while_::predicate f acc
      }
    }
};

let returnImpl: s ('k, 'v) 'k 'v = {
  reduce: fun while_::predicate f acc (key, value) =>
    if (predicate acc key value) (f acc key value)
    else acc
};

let return (key: 'k) (value: 'v): (t 'k 'v) =>
  Instance (key, value) returnImpl;

let scan
    (reducer: 'acc => 'k => 'v => 'acc)
    (initialValue: 'acc)
    (iter: t 'k 'v): (Iterable.t 'acc) => switch iter {
  | Empty => Iterable.return initialValue
  | Instance iter { reduce } => Iterable.Instance iter {
      reduce: fun while_::predicate f acc iter =>
        if (predicate acc initialValue)  {
          let result = ref (f acc initialValue);
          let memoized = ref initialValue;

          let predicate acc key value => {
            let nextValue = reducer acc key value;
            memoized := nextValue;
            predicate !result nextValue;
          };

          let f _ _ _ => {
            let acc = !memoized;
            result := f !result acc;
            acc
          };

          iter |> reduce while_::predicate f initialValue |> ignore;

          !result
        } else acc
    }
};

let skip (count: int) (iter: t 'k 'v): (t 'k 'v) => if (count === 0) iter else switch iter {
  | Empty => Empty
  | Instance iter { reduce } => Instance iter {
      reduce: fun while_::predicate f acc iter => {
        let count = ref count;

        let predicate acc key value => {
          if (!count > 0) true
          else (predicate acc key value);
        };

        let f acc key value => {
          count := !count - 1;

          if (!count >= 0) acc
          else f acc key value;
        };

        iter |> reduce while_::predicate f acc;
      }
    }
};

let skipWhile (keepSkipping: 'k => 'v => bool) (iter: t 'k 'v): (t 'k 'v) => switch iter {
  | Empty => Empty
  | Instance iter { reduce } => Instance iter {
      reduce: fun while_::predicate f acc iter => {
        let doneSkipping = ref false;

        let predicate acc key value =>
          if (!doneSkipping) (predicate acc key value)
          else true;

        let f acc key value =>
          if (!doneSkipping) (f acc key value)
          else if (keepSkipping key value) acc
          else {
            doneSkipping := true;
            f acc key value;
          };

        iter |> reduce while_::predicate f acc
      }
    }
};

let startWith (key: 'k) (value: 'v) (iter: t 'k 'v): (t 'k 'v) =>
  concat [return key value, iter];

let take (count: int) (iter: t 'k 'v): (t 'k 'v) => if (count === 0) Empty else switch iter {
  | Empty => Empty
  | Instance iter { reduce } => Instance iter {
      reduce: fun while_::predicate f acc iter => {
        let count = ref count;

        let predicate acc key value => {
          if (!count > 0) (predicate acc key value)
          else false;
        };

        let f acc key value => {
          count := !count - 1;
          f acc key value;
        };

        iter |> reduce while_::predicate f acc;
      }
    }
};

let takeWhile (keepTaking: 'k => 'v => bool) (iter: t 'k 'v): (t 'k 'v) => switch iter {
  | Empty => Empty
  | Instance iter { reduce } => Instance iter {
      reduce: fun while_::predicate f acc iter => {
        let predicate acc key value =>
          if (keepTaking key value) (predicate acc key value)
          else false;

        iter |> reduce while_::predicate f acc
      }
    }
};

let toKeyedIterable (iter: t 'k 'v): (t 'k 'v) => iter;
