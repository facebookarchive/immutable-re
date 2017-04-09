/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Functions.Operators;

let module Iterator = {
  type t 'a 'iterable  = {
    reduce: 'acc . (while_::('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => 'iterable => 'acc)
  };
};

type t 'a =
  | Empty
  | Iterable 'iterable (Iterator.t 'a 'iterable): t 'a;

let reduce
    while_::(predicate: 'acc => 'a => bool)=Functions.alwaysTrue2
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (iter: t 'a): 'acc => switch iter {
  | Empty => acc
  | Iterable iter { reduce } =>
      iter |> reduce while_::predicate f acc;
};

let empty (): t 'a => Empty;

let concatIterator: Iterator.t 'a 'iterable = {
  reduce: fun while_::predicate f acc iters => {
    let shouldContinue = ref true;

    let predicate acc next => {
      let result = predicate acc next;
      shouldContinue := result;
      result;
    };

    iters |> ImmList.reduce
      while_::(fun _ _ => !shouldContinue)
      (fun acc next => next |> reduce while_::predicate f acc)
      acc
  }
};

let concat (iters: list (t 'a)): (t 'a) => switch iters {
  | [] => empty ()
  | _ => Iterable iters concatIterator
};

let deferIterator: Iterator.t 'a 'iterable = {
  reduce: fun while_::predicate f acc provider =>
    (provider ()) |> reduce while_::predicate f acc
};

let defer (provider: unit => (t 'a)): (t 'a) =>
  Iterable provider deferIterator;

let distinctUntilChangedWith
    (equals: Equality.t 'a)
    (iterable: t 'a): (t 'a) => switch iterable {
  | Empty => Empty
  | Iterable iter { reduce } => Iterable iter {
      reduce: fun while_::predicate f acc iter => {
        let previous = ref [||];

        let predicate acc next =>
          if (!previous === [||]) (
            predicate acc next
          )
          else if (equals (!previous).(0) next |> not) (
            predicate acc next
          )
          else true;

        let f acc next =>
          if (!previous === [||]) {
            previous := [| next |];
            f acc next
          }
          else if (equals (!previous).(0) next |> not) {
            (!previous).(0) = next;
            f acc next
          }
          else acc;

        iter |> reduce while_::predicate f acc
      }
    }
};

let doOnNext (sideEffect: 'a => unit) (iterable: t 'a): (t 'a) => switch iterable {
  | Empty => Empty
  | Iterable iter { reduce } => Iterable iter {
      reduce: fun while_::predicate f acc iter => iter |> reduce
        while_::predicate (fun acc next => {
          sideEffect next; (f acc next)
        }) acc
    }
};

let filter (filter: 'a => bool) (iterable: t 'a): (t 'a) => switch iterable {
  | Empty => Empty
  | Iterable iter { reduce } => Iterable iter {
      reduce: fun while_::predicate f acc iter => {
        let predicate acc next =>
          if (filter next) (predicate acc next)
          else true;

        iter |> reduce
          while_::predicate (fun acc next =>
            if (filter next) (f acc next) else acc
          ) acc;
      }
    }
};

let flattenIterator: Iterator.t 'a 'iterable = {
  reduce: fun while_::predicate f acc iters => {
    let shouldContinue = ref true;

    let predicate acc next => {
      let result = predicate acc next;
      shouldContinue := result;
      result;
    };

    iters |> reduce while_::(fun _ _ => !shouldContinue) (fun acc next =>
      next |> reduce while_::predicate f acc
    ) acc
  }
};

let flatten (iters: t (t 'a)): (t 'a) => switch iters {
  | Empty => Empty
  | Iterable _ _ => Iterable iters flattenIterator
};

let generateIterator: Iterator.t 'acc ('acc => 'acc, 'acc) = {
  reduce: fun while_::predicate f acc (gen, initialValue) => {
    let rec recurse gen value predicate f acc => {
      let nextValue = gen value;

      if (predicate acc nextValue) {
        let acc = f acc nextValue;
        recurse gen nextValue predicate f acc;
      }
      else acc;
    };

    if (predicate acc initialValue) {
      let acc = f acc initialValue;
      recurse gen initialValue predicate f acc;
    } else acc;
  }
};

let generate (gen: 'acc => 'acc) (initialValue: 'acc): (t 'acc) =>
  Iterable (gen, initialValue) generateIterator;

let listAddFirstAll (iter: t 'a) (list: list 'a): (list 'a) =>
  iter |> reduce (fun acc next => acc |> ImmList.addFirst next) list;

let listFromReverse (iter: t 'a): (list 'a) =>
  [] |> listAddFirstAll iter;

let map
    (mapper: 'a => 'b)
    (iter: t 'a): (t 'b) => switch iter {
  | Empty => Empty
  | Iterable iter { reduce } => Iterable iter {
      reduce: fun while_::predicate f acc iter => {
        let memoize = ref [||];

        let predicate acc next => {
          let next = mapper next;

          if (!memoize === [||]) { memoize := [| next |] }
          else { (!memoize).(0) = next; };

          predicate acc next
        };

        let f acc _ => f acc (!memoize).(0);

        iter |> reduce while_::predicate f acc
      }
    }
};

let flatMap (mapper: 'a => t 'b) (iter: t 'a): (t 'b) =>
  iter |> map mapper |> flatten;

let listIterator: Iterator.t 'a (list 'a) = { reduce: ImmList.reduceImpl };

let ofList (list: list 'a): (t 'a) => switch list {
  | [] => Empty
  | _ => Iterable list listIterator
};

let returnIterator: Iterator.t 'a 'a = {
  reduce: fun while_::predicate f acc value =>
    if (predicate acc value) (f acc value)
    else acc
};

let return (value: 'a): (t 'a) => Iterable value returnIterator;

let scan
    (reducer: 'acc => 'a => 'acc)
    (initialValue: 'acc)
    (iter: t 'a): (t 'acc) => switch iter {
  | Empty => Empty
  | Iterable iter { reduce } => Iterable iter {
      reduce: fun while_::predicate f acc iter =>
        if (predicate acc initialValue) {
          let result = ref (f acc initialValue);
          let memoized = [| initialValue |];

          let predicate acc next => {
            let nextValue = reducer acc next;
            memoized.(0) = nextValue;
            predicate !result nextValue;
          };

          let f _ _ => {
            let acc = memoized.(0);
            result := f !result acc;
            acc
          };

          iter |> reduce while_::predicate f initialValue |> ignore;

          !result
      } else acc
    }
};

let skip (count: int) (iter: t 'a): (t 'a) => if (count === 0) iter else switch iter {
  | Empty => Empty
  | Iterable iter { reduce } => Iterable iter {
      reduce: fun while_::predicate f acc iter => {
        let count = ref count;

        let predicate acc next => {
          if (!count > 0) true
          else (predicate acc next);
        };

        let f acc next => {
          count := !count - 1;

          if (!count >= 0) acc
          else f acc next;
        };

        iter |> reduce while_::predicate f acc;
      }
    }
};

let skipWhile (keepSkipping: 'a => bool) (iter: t 'a): (t 'a) => switch iter {
  | Empty => Empty
  | Iterable iter { reduce } => Iterable iter {
      reduce: fun while_::predicate f acc iter => {
        let doneSkipping = ref false;

        let predicate acc next =>
          if (!doneSkipping) (predicate acc next)
          else true;

        let f acc next =>
          if (!doneSkipping) (f acc next)
          else if (keepSkipping next) acc
          else {
            doneSkipping := true;
            f acc next;
          };

        iter |> reduce while_::predicate f acc
      }
    }
};

let startWith (value: 'a) (iter: t 'a): (t 'a) =>
  concat [return value, iter];

let take (count: int) (iter: t 'a): (t 'a) => if (count === 0) Empty else switch iter {
  | Empty => Empty
  | Iterable iter { reduce } => Iterable iter {
      reduce: fun while_::predicate f acc iter => {
        let count = ref count;

        let predicate acc next => {
          if (!count > 0) (predicate acc next)
          else false;
        };

        let f acc next => {
          count := !count - 1;
          f acc next;
        };

        iter |> reduce while_::predicate f acc;
      }
    }
};

let takeWhile (keepTaking: 'a => bool) (iter: t 'a): (t 'a) => switch iter {
  | Empty => Empty
  | Iterable iter { reduce } => Iterable iter {
      reduce: fun while_::predicate f acc iter => {
        let predicate acc next =>
          if (keepTaking next) (predicate acc next)
          else false;

        iter |> reduce while_::predicate f acc
      }
    }
};

let toIterable (iter: t 'a): (t 'a) => iter;

let buffer
    count::(count: int)
    skip::(skip: int)
    (iter: t 'a): (t (list 'a)) =>
  if (count <= 0 || skip <= 0) (failwith "out of range")
  else iter |> scan (
      fun (lst, counted, skipped) next =>
        if (counted < count && skipped < skip) ([next, ...lst], counted + 1, skipped + 1)
        else if (skipped < skip) (lst, counted, skipped + 1)
        else if (counted < count) ([next, ...lst], counted + 1, skipped)
        else if (skip < count) ([next, ...(ImmList.take (count - skip) lst)], counted, skipped)
        else ([next], 1, 1)
      ) ([], 0, 0)
    |> filter (fun (_, counted, skipped) => counted === count && skipped === skip)
    |> map (fun (lst, _, _) => lst);



let increment acc _ => acc + 1;
let count (iterable: t 'a): int =>
  iterable |> reduce increment 0;

let every (f: 'a => bool) (iterable: t 'a): bool =>
  iterable |> reduce while_::(fun acc _ => acc) (fun _ => f) true;

let find (f: 'a => bool) (iterable: t 'a): (option 'a) =>
  iterable |> reduce while_::(fun acc _ => Option.isEmpty acc) (
    fun _ next => if (f next) (Some next) else None
  ) None;

let findOrRaise (f: 'a => bool) (iterable: t 'a): 'a =>
  find f iterable |> Option.firstOrRaise;

let first (iterable: t 'a): (option 'a) =>
  iterable |> reduce
    while_::(fun acc _ => Option.isEmpty acc)
    (fun _ => Option.return)
    None;

let firstOrRaise (iterable: t 'a): 'a =>
  iterable |> first |> Option.firstOrRaise;

let forEach while_::(predicate: 'a => bool)=Functions.alwaysTrue (f: 'a => unit) (iterable: t 'a) =>
  if (predicate === Functions.alwaysTrue ) {
    iterable |> reduce (fun _ => f) ();
  }
  else iterable |> reduce while_::(fun _ => predicate) (fun _ => f) ();

let none (f: 'a => bool) (iterable: t 'a): bool =>
  iterable |> reduce while_::(fun acc _ => acc) (fun _ => f >> not) true;

let some (f: 'a => bool) (iterable: t 'a): bool =>
  iterable |> reduce while_::(fun acc _ => not acc) (fun _ => f) false;
