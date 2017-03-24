/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'k 'v = {
  reduce: 'acc . ('acc => 'k => 'v => bool) => ('acc => 'k => 'v => 'acc) => 'acc => 'acc,
};

let reduce
    while_::(predicate:'acc => 'k => 'v => bool)=Functions.alwaysTrue3
    (f: 'acc => 'k => 'v => 'acc)
    (acc: 'acc)
    (iter: t 'k 'v): 'acc =>
  iter.reduce predicate f acc;

let emptyReducer _ _ acc => acc;

let empty (): t 'k 'v => {
  reduce: emptyReducer
};

let concat (iters: list (t 'k 'v)): (t 'k 'v) => switch iters {
  | [] => empty ()
  | _ => {
    reduce: fun predicate f acc => {
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
  }
};

let defer (provider: unit => (t 'k 'v)): (t 'k 'v) => {
  reduce: fun predicate f acc =>
    (provider ()).reduce predicate f acc
};

let distinctUntilChangedWith
    keyEquals::(keyEquals: Equality.t 'k)
    valueEquals::(valueEquals: Equality.t 'v)
    (iter: t 'k 'v): (t 'k 'v) => if (iter.reduce === emptyReducer) iter else {
  reduce: fun predicate f acc => {
    let previousKey = ref [||];
    let previousValue = ref [||];

    let predicate acc key value =>
      if (!previousKey === [||]) (
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
      if (!previousKey === [||]) {
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

let doOnNext (sideEffect: 'k => 'v => unit) (iter: t 'k 'v): (t 'k 'v) =>
  if (iter.reduce === emptyReducer) iter
  else {
    reduce: fun predicate f acc => iter |> reduce
      while_::predicate
      (fun acc k v => { sideEffect k v; (f acc k v) })
      acc
  };

let filter (filter: 'k => 'v => bool) (iter: t 'k 'v): (t 'k 'v) =>
  if (iter.reduce === emptyReducer) iter
  else {
    reduce: fun predicate f acc => {
      let predicate acc key value =>
        if (filter key value) (predicate acc key value)
        else true;

      iter |> reduce
        while_::predicate
        (fun acc k v => if (filter k v) (f acc k v) else acc)
        acc;
    }
  };

let flatMap (mapper: 'kA => 'vA => t 'kB 'vB) (iter: t 'kA 'vA): (t 'kB 'vB) =>
  if (iter.reduce === emptyReducer) (empty ())
  else {
    reduce: fun predicate f acc => {
      let shouldContinue = ref true;

      let predicate acc key value => {
        let result = predicate acc key value;
        shouldContinue := result;
        result;
      };

      iter |> reduce while_::(fun _ _ _ => !shouldContinue) (
        fun acc k v => (mapper k v) |> reduce while_::predicate f acc
      ) acc
    }
  };

let keys (iter: t 'k 'v): (Iterator.t 'k) =>
  if (iter.reduce === emptyReducer) (Iterator.empty ())
  else {
    reduce: fun predicate f acc => iter |> reduce
      while_::(fun acc k _ => predicate acc k)
      (fun acc k _ => f acc k)
      acc
  };

let map
    keyMapper::(keyMapper: 'kA => 'vA => 'kB)
    valueMapper::(valueMapper: 'kA => 'vA => 'vB)
    (iter: t 'kA 'vA): (t 'kB 'vB) => if (iter.reduce === emptyReducer) (empty ()) else {
  reduce: fun predicate f acc => {
    let memoizedKey = ref [||];
    let memoizedValue = ref [||];

    let predicate acc key value => {
      let nextKey = keyMapper key value;
      let nextValue = valueMapper key value;

      if (!memoizedKey === [||]) {
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
};

let mapKeys (mapper: 'a => 'v => 'b) (iter: t 'a 'v): (t 'b 'v) =>
  if (iter.reduce === emptyReducer) (empty ())
  else {
    reduce: fun predicate f acc => {
      let memoizedKey = ref [||];

      let predicate acc key value => {
        let nextKey = mapper key value;

        if (!memoizedKey === [||]) { memoizedKey := [| nextKey |] }
        else { (!memoizedKey).(0) = nextKey; };

        predicate acc nextKey value
      };

      let f acc _ value => f acc (!memoizedKey).(0) value;

      iter |> reduce while_::predicate f acc
    }
  };

let mapValues (mapper: 'k => 'a => 'b) (iter: t 'k 'a): (t 'k 'b) =>
  if (iter.reduce === emptyReducer) (empty ())
  else {
    reduce: fun predicate f acc => {
      let memoizedValue = ref [||];

      let predicate acc key value => {
        let nextValue = mapper key value;

        if (!memoizedValue === [||]) { memoizedValue := [| nextValue |] }
        else { (!memoizedValue).(0) = nextValue; };

        predicate acc key nextValue
      };

      let f acc key _ => f acc key (!memoizedValue).(0);

      iter |> reduce while_::predicate f acc
    }
  };

let repeat (key:'k) (value: 'v): (t 'k 'v) => {
  reduce: fun predicate f acc => {
    let rec recurse key value predicate f acc =>
      if (predicate acc key value) {
        let acc = f acc key value;
        recurse key value predicate f acc;
      }
      else acc;
    recurse key value predicate f acc;
  }
};

let return (key: 'k) (value: 'v): (t 'k 'v) => {
  reduce: fun predicate f acc =>
    if (predicate acc key value) (f acc key value)
    else acc
};

let scan
    (reducer: 'acc => 'k => 'v => 'acc)
    (initialValue: 'acc)
    (iter: t 'k 'v): (Iterator.t 'acc) => if (iter.reduce === emptyReducer) (Iterator.empty ()) else {
  reduce: fun predicate f acc => {
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

    iter.reduce predicate f initialValue |> ignore;

    !result
  }
};

let skip (count: int) (iter: t 'k 'v): (t 'k 'v) =>
  if (iter.reduce === emptyReducer) iter
  else if (count == 0) iter
  else {
    reduce: fun predicate f acc => {
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
  };

let skipWhile (keepSkipping: 'k => 'v => bool) (iter: t 'k 'v): (t 'k 'v) =>
  if (iter.reduce === emptyReducer) iter
  else {
    reduce: fun predicate f acc => {
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
  };

let startWith (key: 'k) (value: 'v) (iter: t 'k 'v): (t 'k 'v) =>
  concat [return key value, iter];

let take (count: int) (iter: t 'k 'v): (t 'k 'v) =>
  if (iter.reduce === emptyReducer) iter
  else if (count === 0) (empty ())
  else {
    reduce: fun predicate f acc => {
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
  };

let takeWhile (keepTaking: 'k => 'v => bool) (iter: t 'k 'v): (t 'k 'v) =>
  if (iter.reduce === emptyReducer) iter
  else {
    reduce: fun predicate f acc => {
      let predicate acc key value =>
        if (keepTaking key value) (predicate acc key value)
        else false;

      iter |> reduce while_::predicate f acc
    }
  };

let values (iter: t 'k 'v): (Iterator.t 'v) =>
  if (iter.reduce === emptyReducer ) (Iterator.empty ())
  else {
    reduce: fun predicate f acc => iter |> reduce
      while_::(fun acc _ v => predicate acc v)
      (fun acc _ v => f acc v)
      acc
  };

let toIterator (iter: t 'k 'v): (Iterator.t ('k, 'v)) =>
  if (iter.reduce === emptyReducer ) (Iterator.empty ())
  else {
    reduce: fun predicate f acc => iter |> reduce
      while_::(fun acc k v => predicate acc (k, v))
      (fun acc k v => f acc (k, v))
      acc
  };

let module KeyedReducer = KeyedReducer.Make2 {
  type nonrec t 'k 'v = t 'k 'v;
  let reduce = reduce;
};
