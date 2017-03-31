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

let skip (count: int) (iter: t 'a): (t 'a) => if (count ===0) Empty else switch iter {
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

type iterable 'a = t 'a;

module type S = {
  type a;
  type t;

  let reduce: while_::('acc => a => bool)? => ('acc => a => 'acc) => 'acc => t => 'acc;
  let toIterable: t => (iterable a);
};

module type S1 = {
  type t 'a;

  let reduce: while_::('acc => 'a => bool)? => ('acc => 'a => 'acc) => 'acc => (t 'a) => 'acc;
  let toIterable: t 'a => (iterable 'a);
};

let module Reducer = {
  module type Iterable = S;
  module type Iterable1 = S1;

  module type S = {
    type a;
    type t;

    let count: t => int;
    let every: (a => bool) => t => bool;
    let find: (a => bool) => t => (option a);
    let findOrRaise: (a => bool) => t => a;
    let first: t => (option a);
    let firstOrRaise: t => a;
    let forEach: while_::(a => bool)? => (a => unit) => t => unit;
    let none: (a => bool) => t => bool;
    let some: (a => bool) => t => bool;
  };

  module type S1 = {
    type t 'a;

    let count: t 'a => int;
    let every: ('a => bool) => (t 'a) => bool;
    let find: ('a => bool) => (t 'a) => (option 'a);
    let findOrRaise: ('a => bool) => (t 'a) => 'a;
    let first: t 'a => (option 'a);
    let firstOrRaise: t 'a => 'a;
    let forEach: while_::('a => bool)? => ('a => unit) => (t 'a) => unit;
    let none: ('a => bool) => (t 'a) => bool;
    let some: ('a => bool) => (t 'a) => bool;
  };

  let module Make = fun (Iterable: Iterable) => {
    type a = Iterable.a;
    type t = Iterable.t;

    let increment acc _ => acc + 1;
    let count (reduceable: t): int =>
      reduceable |> Iterable.reduce increment 0;

    let every (f: a => bool) (reduceable: t): bool =>
      reduceable |> Iterable.reduce while_::(fun acc _ => acc) (fun _ => f) true;

    let find (f: a => bool) (reduceable: t): (option a) =>
      reduceable |> Iterable.reduce while_::(fun acc _ => Option.isEmpty acc) (
        fun _ next => if (f next) (Some next) else None
      ) None;

    let findOrRaise (f: a => bool) (reduceable: t): a =>
      find f reduceable |> Option.firstOrRaise;

    let first (reduceable: t): (option a) =>
      reduceable |> Iterable.reduce
        while_::(fun acc _ => Option.isEmpty acc)
        (fun _ => Option.return)
        None;

    let firstOrRaise (reduceable: t): a =>
      reduceable |> first |> Option.firstOrRaise;

    let forEach while_::(predicate: a => bool)=Functions.alwaysTrue (f: a => unit) (reduceable: t) =>
      if (predicate === Functions.alwaysTrue ) {
        reduceable |> Iterable.reduce (fun _ => f) ();
      }
      else reduceable |> Iterable.reduce while_::(fun _ => predicate) (fun _ => f) ();

    let none (f: a => bool) (reduceable: t): bool =>
      reduceable |> Iterable.reduce while_::(fun acc _ => acc) (fun _ => f >> not) true;

    let some (f: a => bool) (reduceable: t): bool =>
      reduceable |> Iterable.reduce while_::(fun acc _ => not acc) (fun _ => f) false;
  };

  let module Make1 = fun (Iterable: Iterable1) => {
    type t 'a = Iterable.t 'a;

    let increment acc _ => acc + 1;
    let count (reduceable: t 'a): int =>
      reduceable |> Iterable.reduce increment 0;

    let every (f: 'a => bool) (reduceable: t 'a): bool =>
      reduceable |> Iterable.reduce while_::(fun acc _ => acc) (fun _ => f) true;

    let find (f: 'a => bool) (reduceable: t 'a): (option 'a) =>
      reduceable |> Iterable.reduce while_::(fun acc _ => Option.isEmpty acc) (
        fun _ next => if (f next) (Some next) else None
      ) None;

    let findOrRaise (f: 'a => bool) (reduceable: t 'a): 'a =>
      find f reduceable |> Option.firstOrRaise;

    let first (reduceable: t 'a): (option 'a) =>
      reduceable |> Iterable.reduce
        while_::(fun acc _ => Option.isEmpty acc)
        (fun _ => Option.return)
        None;

    let firstOrRaise (reduceable: t 'a): 'a =>
      reduceable |> first |> Option.firstOrRaise;

    let forEach while_::(predicate: 'a => bool)=Functions.alwaysTrue (f: 'a => unit) (reduceable: t 'a) =>
      if (predicate === Functions.alwaysTrue ) {
        reduceable |> Iterable.reduce (fun _ => f) ();
      }
      else reduceable |> Iterable.reduce while_::(fun _ => predicate) (fun _ => f) ();

    let none (f: 'a => bool) (reduceable: t 'a): bool =>
      reduceable |> Iterable.reduce while_::(fun acc _ => acc) (fun _ => f >> not) true;

    let some (f: 'a => bool) (reduceable: t 'a): bool =>
      reduceable |> Iterable.reduce while_::(fun acc _ => not acc) (fun _ => f) false;
  };

  include Make1 ({
    type nonrec t 'a = t 'a;
    let reduce = reduce;
    let toIterable = toIterable;
  });
};

let module ListReducer = Reducer.Make1 ({
  type t 'a = list 'a;
  let reduce = ImmList.reduce;
  let toIterable = ofList;
});
