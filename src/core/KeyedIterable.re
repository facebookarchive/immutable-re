/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

let module KeyedIterator = {
  type t 'k 'v 'keyedIterable  = {
    reduce: 'acc . (while_::('acc => 'k => 'v => bool) => ('acc => 'k => 'v => 'acc) => 'acc => 'keyedIterable => 'acc)
  };
};

type t 'k 'v =
  | Empty
  | KeyedIterable 'keyedIterable (KeyedIterator.t 'k 'v 'keyedIterable): t 'k 'v;

let reduce
    while_::(predicate:'acc => 'k => 'v => bool)=Functions.alwaysTrue3
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    (iter: t 'k 'v): 'acc => switch iter {
  | Empty => acc
  | KeyedIterable iter { reduce } => iter |> reduce while_::predicate f acc;
};

let empty (): t 'k 'v => Empty;

let concatKeyedIterator: KeyedIterator.t 'k 'v 'keyedIterable = {
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
  | _ => KeyedIterable iters concatKeyedIterator
};

let deferIterator: KeyedIterator.t 'k 'v 'keyedIterable = {
  reduce: fun while_::predicate f acc provider =>
    (provider ()) |> reduce while_::predicate f acc
};

let defer (provider: unit => (t 'k 'v)): (t 'k 'v) =>
  KeyedIterable provider deferIterator;

let distinctUntilChangedWith
    keyEquals::(keyEquals: Equality.t 'k)
    valueEquals::(valueEquals: Equality.t 'v)
    (iter: t 'k 'v): (t 'k 'v) => switch iter {
  | Empty => Empty
  | KeyedIterable iter { reduce } => KeyedIterable iter {
      reduce: fun while_::predicate f acc iter => {
        let previousKey = ref [||];
        let previousValue = ref [||];

        let predicate acc key value =>
          if ((Array.length !previousKey) === 0) (
            predicate acc key value
          )
          else if (
            keyEquals (!previousKey).(0) key |> not ||
            valueEquals (!previousValue).(0) value |> not
          ) (
            predicate acc key value
          )
          else true;

        let f acc key value =>
          if ((Array.length !previousKey) === 0) {
            previousKey := [| key |];
            previousValue := [| value |];
            f acc key value;
          }
          else if (
            keyEquals (!previousKey).(0) key |> not ||
            valueEquals (!previousValue).(0) value |> not
          ) {
            (!previousKey).(0) = key;
            (!previousValue).(0) = value;
            f acc key value
          }
          else acc;

        iter |> reduce while_::predicate f acc
      }
    };
};

let doOnNext (sideEffect: 'k => 'v => unit) (iter: t 'k 'v): (t 'k 'v) => switch iter {
  | Empty => Empty
  | KeyedIterable iter { reduce } => KeyedIterable iter {
      reduce: fun while_::predicate f acc iter => iter |> reduce
        while_::predicate
        (fun acc k v => { sideEffect k v; (f acc k v) })
        acc
    }
};

let filter (filter: 'k => 'v => bool) (iter: t 'k 'v): (t 'k 'v) => switch iter {
  | Empty => Empty
  | KeyedIterable iter { reduce } => KeyedIterable iter {
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
  | KeyedIterable iter { reduce: reduceIter } => KeyedIterable iter {
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

let fromEntriesIterator: KeyedIterator.t 'k 'v (Iterable.t ('k, 'v)) = {
  reduce: fun while_::predicate f acc iter => {
    let predicate acc (k, v) => predicate acc k v;
    let f acc (k, v) => f acc k v;

    iter |> Iterable.reduce while_::predicate f acc;
  }
};

let fromEntries (iter: Iterable.t ('k, 'v)): (t 'k 'v) => switch iter {
  | Iterable.Empty => Empty
  | Iterable.Iterable  _ _ => KeyedIterable iter fromEntriesIterator
};

let generateIterator: KeyedIterator.t 'k 'v ('k => 'v => 'k, 'k => 'v => 'v, 'k, 'v) = {
  reduce: fun while_::predicate f acc (genKey, genValue, initialKey, initialValue) => {
    let rec recurse genKey genValue  key value predicate f acc => {
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
  KeyedIterable (genKey, genValue, initialKey, initialValue) generateIterator;

let keysIterator: Iterable.Iterator.t 'k (t 'k _) = {
  reduce: fun while_::predicate f acc iter => iter |> reduce
    while_::(fun acc k _ => predicate acc k)
    (fun acc k _ => f acc k)
    acc
};

let keys (iter: t 'k 'v): (Iterable.t 'k) => switch iter {
  | Empty => Iterable.Empty
  | KeyedIterable _ _ => Iterable.Iterable iter keysIterator
};

let map
    keyMapper::(keyMapper: 'kA => 'vA => 'kB)
    valueMapper::(valueMapper: 'kA => 'vA => 'vB)
    (iter: t 'kA 'vA): (t 'kB 'vB) => switch iter {
  | Empty => Empty
  | KeyedIterable iter { reduce }  => KeyedIterable iter {
      reduce: fun while_::predicate f acc iter => {
        let memoizedKey = ref [||];
        let memoizedValue = ref [||];

        let predicate acc key value => {
          let nextKey = keyMapper key value;
          let nextValue = valueMapper key value;

          if ((Array.length !memoizedKey) === 0) {
            memoizedKey := [| nextKey |];
            memoizedValue := [| nextValue |]
          }
          else {
            (!memoizedKey).(0) = nextKey;
            (!memoizedValue).(0) = nextValue;
          };

          predicate acc nextKey nextValue
        };

        let f acc _ _ => f acc (!memoizedKey).(0) (!memoizedValue).(0);

        iter |> reduce while_::predicate f acc
      }
    }
};

let mapKeys (mapper: 'a => 'v => 'b) (iter: t 'a 'v): (t 'b 'v) => switch iter {
  | Empty => Empty
  | KeyedIterable iter { reduce } => KeyedIterable iter {
      reduce: fun while_::predicate f acc iter => {
        let memoizedKey = ref [||];

        let predicate acc key value => {
          let nextKey = mapper key value;

          if ((Array.length !memoizedKey) === 0) { memoizedKey := [| nextKey |] }
          else { (!memoizedKey).(0) = nextKey; };

          predicate acc nextKey value
        };

        let f acc _ value => f acc (!memoizedKey).(0) value;

        iter |> reduce while_::predicate f acc
      }
    }
};

let mapValues (mapper: 'k => 'a => 'b) (iter: t 'k 'a): (t 'k 'b) => switch iter {
  | Empty => Empty
  | KeyedIterable iter { reduce } => KeyedIterable iter {
      reduce: fun while_::predicate f acc iter => {
        let memoizedValue = ref [||];

        let predicate acc key value => {
          let nextValue = mapper key value;

          if ((Array.length !memoizedValue) === 0) { memoizedValue := [| nextValue |] }
          else { (!memoizedValue).(0) = nextValue; };

          predicate acc key nextValue
        };

        let f acc key _ => f acc key (!memoizedValue).(0);

        iter |> reduce while_::predicate f acc
      }
    }
};

let returnIterator: KeyedIterator.t 'k 'v ('k, 'v)= {
  reduce: fun while_::predicate f acc (key, value) =>
    if (predicate acc key value) (f acc key value)
    else acc
};

let return (key: 'k) (value: 'v): (t 'k 'v) =>
  KeyedIterable (key, value) returnIterator;

let scan
    (reducer: 'acc => 'k => 'v => 'acc)
    (initialValue: 'acc)
    (iter: t 'k 'v): (Iterable.t 'acc) => switch iter {
  | Empty => Iterable.Empty
  | KeyedIterable iter { reduce } => Iterable.Iterable iter {
      reduce: fun while_::predicate f acc iter =>
        if (predicate acc initialValue)  {
          let result = ref (f acc initialValue);
          let memoized = [| initialValue |];

          let predicate acc key value => {
            let nextValue = reducer acc key value;
            memoized.(0) = nextValue;
            predicate !result nextValue;
          };

          let f _ _ _ => {
            let acc = memoized.(0);
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
  | KeyedIterable iter { reduce } => KeyedIterable iter {
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
  | KeyedIterable iter { reduce } => KeyedIterable iter {
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
  | KeyedIterable iter { reduce } => KeyedIterable iter {
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
  | KeyedIterable iter { reduce } => KeyedIterable iter {
      reduce: fun while_::predicate f acc iter => {
        let predicate acc key value =>
          if (keepTaking key value) (predicate acc key value)
          else false;

        iter |> reduce while_::predicate f acc
      }
    }
};

let valuesIterator: Iterable.Iterator.t 'v (t _ 'v) = {
  reduce: fun while_::predicate f acc iter => iter |> reduce
    while_::(fun acc _ v => predicate acc v)
    (fun acc _ v => f acc v)
    acc
};

let values (iter: t 'k 'v): (Iterable.t 'v) => switch iter {
  | Empty => Iterable.Empty
  | KeyedIterable _ _ => Iterable.Iterable iter valuesIterator
};

let toKeyedIterable (iter: t 'k 'v): (t 'k 'v) => iter;

let toIterableIterator: Iterable.Iterator.t ('k, 'v) (t 'k 'v) = {
  reduce: fun while_::predicate f acc iter => iter |> reduce
    while_::(fun acc k v => predicate acc (k, v))
    (fun acc k v => f acc (k, v))
    acc
};

let toIterable (iter: t 'k 'v): (Iterable.t ('k, 'v)) => switch iter {
  | Empty => Iterable.Empty
  | KeyedIterable _ _ => Iterable.Iterable iter toIterableIterator
};

let increment acc _ _ => acc + 1;
let count (reducer: t 'k 'v): int =>
  reducer |> reduce increment 0;

let every (f: 'k => 'v => bool) (iter: t 'k 'v): bool =>
  iter |> reduce
    while_::(fun acc _ _ => acc)
    (fun _ => f)
    true;

let find (f: 'k => 'v => bool) (iter: t 'k 'v): (option ('k, 'v)) =>
  iter |> reduce
    while_::(fun acc _ _ => Option.isEmpty acc)
    (fun _ k v => if (f k v) (Some (k, v)) else None)
    None;

let findOrRaise (f: 'k => 'v => bool) (iter: t 'k 'v): ('k, 'v) =>
  find f iter |> Option.firstOrRaise;

let findKey (f: 'k => 'v => bool) (iter: t 'k 'v): (option 'k) =>
  iter |> reduce
    while_::(fun acc _ _ => Option.isEmpty acc)
    (fun _ k v => if (f k v) (Some k) else None)
    None;

let findKeyOrRaise (f: 'k => 'v => bool) (iter: t 'k 'v): 'k =>
  findKey f iter |> Option.firstOrRaise;

let findValue (f: 'k => 'v => bool) (iter: t 'k 'v): (option 'v) =>
  iter |> reduce
    while_::(fun acc _ _ => Option.isEmpty acc)
    (fun _ k v => if (f k v) (Some v) else None)
    None;

let findValueOrRaise (f: 'k => 'v => bool) (iter: t 'k 'v): 'v =>
  findValue f iter |> Option.firstOrRaise;

let first (iter: t 'k 'v): (option ('k, 'v)) =>
  iter |> reduce
    while_::(fun acc _ _ => Option.isEmpty acc)
    (fun _ k v => Option.return (k, v))
    None;

let firstOrRaise (iter: t 'k 'v): ('k, 'v) =>
  iter |> first |> Option.firstOrRaise;

let forEach
    while_::(predicate: 'k => 'v => bool)=Functions.alwaysTrue2
    (f: 'k => 'v => unit)
    (iter: t 'k 'v) =>
  iter |> reduce while_::(fun _ => predicate) (fun _ => f) ();

let none (f: 'k => 'v => bool) (iter: t 'k 'v): bool =>
  iter |> reduce
    while_::(fun acc _ _ => acc)
    (fun _ k v => f k v |> not)
    true;

let some (f: 'k => 'v => bool) (iter: t 'k 'v): bool =>
  iter |> reduce
    while_::(fun acc _ _ => not acc)
    (fun _ => f)
    false;
