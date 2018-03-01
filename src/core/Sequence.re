/***
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open Functions.Operators;

open Option.Operators;

type iterator('a) =
  | Next('a, t('a))
  | Completed
and t('a) = unit => iterator('a);

let rec compareWith =
        (comparator: Comparator.t('a), this: t('a), that: t('a))
        : Ordering.t =>
  switch (this(), that()) {
  | (Next(thisValue, thisNext), Next(thatValue, thatNext)) =>
    let cmp = comparator(thisValue, thatValue);
    if (cmp === Ordering.equal) {
      compareWith(comparator, thisNext, thatNext)
    } else {
      cmp
    }
  | (Completed, Completed) => Ordering.equal
  | (Next(_, _), Completed) => Ordering.greaterThan
  | (Completed, Next(_, _)) => Ordering.lessThan
  };

let emptySeq = () => Completed;

let rec equalsWith =
        (equality: Equality.t('a), this: t('a), that: t('a))
        : bool =>
  that === this
  || (
    switch (that(), this()) {
    | (Next(thisValue, thisNext), Next(thatValue, thatNext)) =>
      if (equality(thisValue, thatValue)) {
        equalsWith(equality, thisNext, thatNext)
      } else {
        false
      }
    | (Completed, Completed) => true
    | _ => false
    }
  );

let isEmpty = (seq: t('a)) => seq === emptySeq;

let rec reduce =
        (
          ~while_ as predicate: ('acc, 'a) => bool,
          reducer: ('acc, 'a) => 'acc,
          acc: 'acc,
          seq: t('a)
        )
        : 'acc =>
  switch (seq()) {
  | Next(value, next) when predicate(acc, value) =>
    let acc = reducer(acc, value);
    reduce(~while_=predicate, reducer, acc, next)
  | _ => acc
  };

let empty = () : t('a) => emptySeq;

let yield = (value: 'a, continuation: unit => t('a)): t('a) => () =>
  Next(value, continuation());

let return = (value: 'a) : t('a) => yield(value, empty);

let rec ofList = (list: list('a)) : t('a) =>
  () =>
    switch list {
    | [value] => Next(value, empty())
    | [value, ...tail] => Next(value, ofList(tail))
    | [] => Completed
    };

let flatten = (seq: t(t('a))) : t('a) => {
  let rec continuedWith = (continuation: t(t('a)), iter: iterator('a)) : iterator('a) =>
    switch iter {
    | Next(value, next) => Next(value, next >> continuedWith(continuation))
    | Completed => continuation() |> flattenIter
    }
  and flattenIter = (iter: iterator(t('a))) : iterator('a) =>
    switch iter {
    | Next(value, next) => value() |> continuedWith(next)
    | Completed => Completed
    };
  () => seq() |> flattenIter
};

let concat = (seqs: list(t('a))) : t('a) => seqs |> ofList |> flatten;

let defer = (f: unit => t('a)) : t('a) => () => f((), ());

let rec filter = (f: 'a => bool, seq: t('a)) : t('a) => {
  let rec filterIter = (f: 'a => bool, iter: iterator('a)) : iterator('b) =>
    switch iter {
    | Next(value, next) =>
      if (f(value)) {
        Next(value, filter(f, next))
      } else {
        next() |> filterIter(f)
      }
    | Completed => Completed
    };
  () => seq() |> filterIter(f)
};

let first = (seq: t('a)) : option('a) =>
  switch (seq()) {
  | Next(value, _) => Some(value)
  | Completed => None
  };

let firstOrRaise = (seq: t('a)) : 'a =>
  switch (seq()) {
  | Next(value, _) => value
  | Completed => failwith("empty")
  };

let rec generate = (f: 'acc => 'acc, acc: 'acc) : t('acc) => () => Next(acc, generate(f, f(acc)));

let rec map = (f: 'a => 'b, seq: t('a)) : t('b) =>
  () =>
    switch (seq()) {
    | Next(value, next) => Next(f(value), map(f, next))
    | Completed => Completed
    };

let doOnNext = (f: 'a => unit, seq: t('a)) : t('a) =>
  seq
  |> map(
       (next) => {
         f(next);
         next
       }
     );

let flatMap = (f: 'a => t('b), seq: t('a)) : t('b) => seq |> map(f) |> flatten;

let ofOption = (opt: option('a)) : t('a) =>
  switch opt {
  | Some(value) => return(value)
  | None => empty()
  };

let scan = (reducer: ('acc, 'a) => 'acc, acc: 'acc, seq: t('a)) : t('acc) => {
  let rec recurse = (reducer, acc, seq, ()) =>
    switch (seq()) {
    | Next(value, next) =>
      let acc = reducer(acc, value);
      Next(acc, recurse(reducer, acc, next))
    | Completed => Completed
    };
  () => Next(acc, recurse(reducer, acc, seq))
};

let distinctUntilChangedWith = (equality: Equality.t('a), seq: t('a)) : t('a) =>
  () => {
    let rec iter = (equality: Equality.t('a), prevValue: 'a, next: t('a)) : t('a) =>
      () =>
        switch (next()) {
        | Next(value, next) =>
          if (equality(prevValue, value)) {
            iter(equality, prevValue, next, ())
          } else {
            Next(value, iter(equality, value, next))
          }
        | Completed => Completed
        };
    switch (seq()) {
    | Next(value, next) => Next(value, iter(equality, value, next))
    | Completed => Completed
    }
  };

let rec seek = (count: int, seq: t('a)) : t('a) => {
  Preconditions.failIf("count must be >= 0", count < 0);
  if (count === 0) {
    seq
  } else {
    switch (seq()) {
    | Next(_, next) => seek(count - 1, next)
    | Completed => seq
    }
  }
};

let rec seekWhile = (f: 'a => bool, seq: t('a)) : t('a) =>
  switch (seq()) {
  | Next(value, next) =>
    if (f(value)) {
      seekWhile(f, next)
    } else {
      seq
    }
  | Completed => seq
  };

let skip = (count: int, seq: t('a)) : t('a) => {
  Preconditions.failIf("count must be >= 0", count < 0);
  () => {
    let rec skipIter = (count: int, iter: iterator('a)) : iterator('a) =>
      switch iter {
      | Next(_, next) =>
        if (count > 0) {
          skipIter(count - 1, next())
        } else {
          iter
        }
      | Completed => Completed
      };
    skipIter(count, seq())
  }
};

let skipWhile = (f: 'a => bool, seq: t('a)) : t('a) =>
  () => {
    let rec skipIter = (f, iter: iterator('a)) : iterator('a) =>
      switch iter {
      | Next(value, next) =>
        if (f(value)) {
          skipIter(f, next())
        } else {
          iter
        }
      | Completed => Completed
      };
    skipIter(f, seq())
  };

let startWith = (value: 'a, seq: t('a)) : t('a) => concat([return(value), seq]);

let rec takeWhile = (f: 'a => bool, seq: t('a)) : t('a) =>
  () =>
    switch (seq()) {
    | Next(value, next) =>
      if (f(value)) {
        Next(value, takeWhile(f, next))
      } else {
        Completed
      }
    | Completed => Completed
    };

let rec take = (count: int, seq: t('a)) : t('a) => {
  Preconditions.failIf("count must be >= 0", count < 0);
  () =>
    if (count > 0) {
      switch (seq()) {
      | Next(value, next) => Next(value, take(count - 1, next))
      | Completed => Completed
      }
    } else {
      Completed
    }
};

let rec mapReverseImpl = (f: 'a => 'b, src: list('a), dst: list('b)) : list('b) =>
  switch src {
  | [head, ...tail] => mapReverseImpl(f, tail, [f(head), ...dst])
  | [] => dst
  };

let mapReverse = (f: 'a => 'b, list: list('a)) : list('b) => mapReverseImpl(f, list, []);

let rec zip = (seqs: list(t('a))) : t(list('a)) =>
  () => {
    let iters = seqs |> mapReverse(Functions.call);
    let nextSequence: t(list('a)) =
      () =>
        (
          iters
          |> mapReverse(
               (next) =>
                 switch next {
                 | Next(_, next) => next
                 | Completed => empty()
                 }
             )
          |> zip
        )
          ();
    iters
    |> ImmList.reduce(
         (acc, next) =>
           switch (acc, next) {
           | (Some(Completed), _) => acc
           | (_, Completed) => Some(Completed)
           | (Some(Next(values, _)), Next(value, _)) =>
             Next([value, ...values], nextSequence) |> Option.return
           | (None, Next(value, _)) => Next([value], nextSequence) |> Option.return
           },
         None
       )
    |? Completed
  };

let rec zip2With = (~zipper: ('a, 'b) => 'c, a: t('a), b: t('b)) : t('c) =>
  () =>
    switch (a(), b()) {
    | (Next(aValue, aNext), Next(bValue, bNext)) =>
      Next(zipper(aValue, bValue), zip2With(~zipper, aNext, bNext))
    | _ => Completed
    };

let rec zip3With = (~zipper: ('a, 'b, 'c) => 'd, a: t('a), b: t('b), c: t('c)) : t('d) =>
  () =>
    switch (a(), b(), c()) {
    | (Next(aValue, aNext), Next(bValue, bNext), Next(cValue, cNext)) =>
      Next(zipper(aValue, bValue, cValue), zip3With(~zipper, aNext, bNext, cNext))
    | _ => Completed
    };

let rec reverseImpl = (src: list('a), dst: list('a)) : list('a) =>
  switch src {
  | [head, ...tail] => reverseImpl(tail, [head, ...dst])
  | [] => dst
  };

let reverse = (list: list('a)) : list('a) => reverseImpl(list, []);

let zipLongest = (seqs: list(t('a))) : t(list(option('a))) =>
  seqs
  |> mapReverse((seq) => concat([seq |> map(Option.return), generate(Functions.identity, None)]))
  |> reverse
  |> zip
  |> takeWhile(ImmList.some(Option.isNotEmpty));

let rec zipLongest2With = (~zipper: (option('a), option('b)) => 'c, a: t('a), b: t('b)) : t('c) =>
  () =>
    switch (a(), b()) {
    | (Next(aValue, aNext), Next(bValue, bNext)) =>
      Next(zipper(Some(aValue), Some(bValue)), zipLongest2With(~zipper, aNext, bNext))
    | (Next(aValue, aNext), Completed) =>
      Next(zipper(Some(aValue), None), zipLongest2With(~zipper, aNext, empty()))
    | (Completed, Next(bValue, bNext)) =>
      Next(zipper(None, Some(bValue)), zipLongest2With(~zipper, empty(), bNext))
    | _ => Completed
    };

let rec zipLongest3With =
        (~zipper: (option('a), option('b), option('c)) => 'd, a: t('a), b: t('b), c: t('c))
        : t('d) =>
  () =>
    switch (a(), b(), c()) {
    | (Next(aValue, aNext), Next(bValue, bNext), Next(cValue, cNext)) =>
      Next(
        zipper(Some(aValue), Some(bValue), Some(cValue)),
        zipLongest3With(~zipper, aNext, bNext, cNext)
      )
    | (Next(aValue, aNext), Next(bValue, bNext), Completed) =>
      Next(
        zipper(Some(aValue), Some(bValue), None),
        zipLongest3With(~zipper, aNext, bNext, empty())
      )
    | (Next(aValue, aNext), Completed, Next(cValue, cNext)) =>
      Next(
        zipper(Some(aValue), None, Some(cValue)),
        zipLongest3With(~zipper, aNext, empty(), cNext)
      )
    | (Completed, Next(bValue, bNext), Next(cValue, cNext)) =>
      Next(
        zipper(None, Some(bValue), Some(cValue)),
        zipLongest3With(~zipper, empty(), bNext, cNext)
      )
    | (Completed, Next(bValue, bNext), Completed) =>
      Next(zipper(None, Some(bValue), None), zipLongest3With(~zipper, empty(), bNext, empty()))
    | (Next(aValue, aNext), Completed, Completed) =>
      Next(zipper(Some(aValue), None, None), zipLongest3With(~zipper, aNext, empty(), empty()))
    | (Completed, Completed, Next(cValue, cNext)) =>
      Next(zipper(None, None, Some(cValue)), zipLongest3With(~zipper, empty(), empty(), cNext))
    | _ => Completed
    };
