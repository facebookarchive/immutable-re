/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open Functions.Operators;

type s 'iterable 'a = {
  reduce: 'acc . while_::('acc => 'a => bool) => ('acc => 'a => 'acc) => 'acc => 'iterable => 'acc
};

type t 'a =
  | Empty
  | Instance 'iterable (s 'iterable 'a): t 'a;

type iterable 'a = t 'a;

module type SGeneric = {
  type elt 'a;
  type t 'a;

  let every: (elt 'a => bool) => t 'a => bool;
  let find: (elt 'a => bool) => t 'a => (option (elt 'a));
  let findOrRaise: (elt 'a => bool) => t 'a => elt 'a;
  let forEach: while_::(elt 'a => bool)? => (elt 'a => unit) => t 'a => unit;
  let none: (elt 'a => bool) => t 'a => bool;
  let reduce: while_::('acc => elt 'a => bool)? => ('acc => elt 'a => 'acc) => 'acc => t 'a => 'acc;
  let some: (elt 'a => bool) => t 'a => bool;
  let toIterable: t 'a => (iterable (elt 'a));
};

module type S = {
  type a;
  type t;

  let every: (a => bool) => t => bool;
  let find: (a => bool) => t => (option a);
  let findOrRaise: (a => bool) => t => a;
  let forEach: while_::(a => bool)? => (a => unit) => t  => unit;
  let none: (a => bool) => t => bool;
  let reduce: while_::('acc => a => bool)? => ('acc => a => 'acc) => 'acc => t => 'acc;
  let some: (a => bool) => t => bool;
  let toIterable: t => (iterable a);
};

module type S1 = {
  type t 'a;

  let every: ('a => bool) => (t 'a) => bool;
  let find: ('a => bool) => (t 'a) => (option 'a);
  let findOrRaise: ('a => bool) => (t 'a) => 'a;
  let forEach: while_::('a => bool)? => ('a => unit) => (t 'a) => unit;
  let none: ('a => bool) => (t 'a) => bool;
  let reduce: while_::('acc => 'a => bool)? => ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let some: ('a => bool) => (t 'a) => bool;
  let toIterable: t 'a => (iterable 'a);
};

module type Base = {
  type elt 'a;
  type t 'a;

  let isEmpty: t 'a => bool;
  let reduce: while_::('acc => elt 'a => bool) => ('acc => elt 'a => 'acc) => 'acc => t 'a => 'acc;
};

let module MakeGeneric = fun (Base: Base) => ({
  include Base;

  let everyPredicate acc _ => acc;
  let findPredicate acc _ => Option.isEmpty acc;
  let nonePredicate = everyPredicate;
  let somePredicate acc _ => not acc;

  let every (f: elt 'a => bool) (iterable: t 'a): bool =>
    iterable |> Base.reduce while_::everyPredicate (fun _ => f) true;

  let find (f: elt 'a => bool) (iterable: t 'a): (option (elt 'a)) =>
    iterable |> Base.reduce while_::findPredicate (
      fun _ next => if (f next) (Some next) else None
    ) None;

  let findOrRaise (f: elt 'a => bool) (iterable: t 'a): elt 'a =>
    find f iterable |> Option.firstOrRaise;

  let forEach while_::(predicate: elt 'a => bool)=Functions.alwaysTrue (f: elt 'a => unit) (iterable: t 'a) =>
    if (predicate === Functions.alwaysTrue) {
      iterable |> Base.reduce while_::Functions.alwaysTrue2 (fun _ => f) ();
    }
    else iterable |> reduce while_::(fun _ => predicate) (fun _ => f) ();

  let none (f: elt 'a => bool) (iterable: t 'a): bool =>
    iterable |> Base.reduce while_::nonePredicate (fun _ => f >> not) true;

  let reduce
      while_::(predicate: 'acc => elt 'a => bool)=Functions.alwaysTrue2
      (f: 'acc => elt 'a => 'acc)
      (acc: 'acc)
      (iterable: t 'a): 'acc =>
    reduce while_::predicate f acc iterable;

  let some (f: elt 'a => bool) (iterable: t 'a): bool =>
    iterable |> reduce while_::somePredicate (fun _ => f) false;

  let iterableBase: s (t 'a) (elt 'a) = { reduce: Base.reduce };

  let toIterable (iterable: t 'a): (iterable (elt 'a)) =>
    if (isEmpty iterable) Empty
    else Instance iterable iterableBase;
}: SGeneric with type t 'a := Base.t 'a and type elt 'a := Base.elt 'a);

let reduce
    while_::(predicate: 'acc => 'a => bool)
    (f: 'acc => 'a => 'acc)
    (acc: 'acc)
    (iter: t 'a): 'acc => switch iter {
  | Empty => acc
  | Instance iter { reduce } =>
      iter |> reduce while_::predicate f acc;
};

let concatImpl: s 'iterable 'a  = {
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
  | [] => Empty
  | _ => Instance iters concatImpl
};

let deferImpl: s 'iterable 'a = {
  reduce: fun while_::predicate f acc provider =>
    (provider ()) |> reduce while_::predicate f acc
};

let defer (provider: unit => (t 'a)): (t 'a) =>
  Instance provider deferImpl;

let distinctUntilChangedWith
    (equals: Equality.t 'a)
    (iterable: t 'a): (t 'a) => switch iterable {
  | Empty => Empty
  | Instance iter { reduce } => Instance iter {
      reduce: fun while_::predicate f acc iter => {
        let previous = MutableOption.empty ();

        let shouldNotSkip next =>
          MutableOption.isEmpty previous ||
          previous |> MutableOption.firstOrRaise |> equals next |> not;

        let predicate acc next =>
          if (shouldNotSkip next) (predicate acc next)
          else true;

        let f acc next =>
          if (shouldNotSkip next) {
            previous |> MutableOption.set next;
            f acc next
          }
          else acc;

        iter |> reduce while_::predicate f acc
      }
    }
};

let doOnNext (sideEffect: 'a => unit) (iterable: t 'a): (t 'a) => switch iterable {
  | Empty => Empty
  | Instance iter { reduce } => Instance iter {
      reduce: fun while_::predicate f acc iter => iter |> reduce
        while_::predicate (fun acc next => {
          sideEffect next; (f acc next)
        }) acc
    }
};

let empty (): t 'a => Empty;

let filter (filter: 'a => bool) (iterable: t 'a): (t 'a) => switch iterable {
  | Empty => Empty
  | Instance iter { reduce } => Instance iter {
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

let flattenImpl: s 'iterable 'a = {
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
  | Instance _ _ => Instance iters flattenImpl
};

let generateImpl: s ('acc => 'acc, 'acc) 'acc  = {
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
  Instance (gen, initialValue) generateImpl;

let isEmpty (iterable: t 'a): bool =>
  iterable === Empty;

let listAddFirstAll (iter: t 'a) (list: list 'a): (list 'a) =>
  iter |> reduce while_::Functions.alwaysTrue2 (fun acc next => acc |> ImmList.addFirst next) list;

let listFromReverse (iter: t 'a): (list 'a) =>
  [] |> listAddFirstAll iter;

let map
    (mapper: 'a => 'b)
    (iter: t 'a): (t 'b) => switch iter {
  | Empty => Empty
  | Instance iter { reduce } => Instance iter {
      reduce: fun while_::predicate f acc iter => {
        let memoize = MutableOption.empty ();

        let predicate acc next => {
          let next = mapper next;
          memoize |> MutableOption.set next;
          predicate acc next
        };

        let f acc _ => f acc (MutableOption.firstOrRaise memoize);

        iter |> reduce while_::predicate f acc
      }
    }
};

let flatMap (mapper: 'a => t 'b) (iter: t 'a): (t 'b) =>
  iter |> map mapper |> flatten;

let listImpl: s (list 'a) 'a = { reduce: ImmList.reduceImpl };

let ofList (list: list 'a): (t 'a) => switch list {
  | [] => Empty
  | _ => Instance list listImpl
};

let returnImpl: s 'a 'a = {
  reduce: fun while_::predicate f acc value =>
    if (predicate acc value) (f acc value)
    else acc
};

let return (value: 'a): (t 'a) => Instance value returnImpl;

let scan
    (reducer: 'acc => 'a => 'acc)
    (initialValue: 'acc)
    (iter: t 'a): (t 'acc) => switch iter {
  | Empty => return initialValue
  | Instance iter { reduce } => Instance iter {
      reduce: fun while_::predicate f acc iter =>
        if (predicate acc initialValue) {
          let result = ref (f acc initialValue);
          let memoized = ref initialValue;

          let predicate acc next => {
            let nextValue = reducer acc next;
            memoized := nextValue;
            predicate !result nextValue;
          };

          let f _ _ => {
            let acc = !memoized;
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
  | Instance iter { reduce } => Instance iter {
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
  | Instance iter { reduce } => Instance iter {
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
  | Instance iter { reduce } => Instance iter {
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
  | Instance iter { reduce } => Instance iter {
      reduce: fun while_::predicate f acc iter => {
        let predicate acc next =>
          if (keepTaking next) (predicate acc next)
          else false;

        iter |> reduce while_::predicate f acc
      }
    }
};
