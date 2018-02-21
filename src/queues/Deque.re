/***
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
type t('a) =
  | Ascending(Vector.t('a))
  | Descending(Vector.t('a));

let count = (deque: t('a)) : int =>
  switch deque {
  | Ascending(vector)
  | Descending(vector) => Vector.count(vector)
  };

let firstOrRaise = (deque: t('a)) : 'a =>
  switch deque {
  | Ascending(vector) => Vector.getOrRaise(0, vector)
  | Descending(vector) => Vector.getOrRaise(count(deque) - 1, vector)
  };

let lastOrRaise = (deque: t('a)) : 'a =>
  switch deque {
  | Ascending(vector) => Vector.getOrRaise(count(deque) - 1, vector)
  | Descending(vector) => Vector.getOrRaise(0, vector)
  };

let reduce =
    (~while_ as predicate: ('acc, 'a) => bool, f: ('acc, 'a) => 'acc, acc: 'acc, deque: t('a))
    : 'acc =>
  switch deque {
  | Ascending(vector) => vector |> Vector.reduce(~while_=predicate, f, acc)
  | Descending(vector) => vector |> Vector.reduceReversed(~while_=predicate, f, acc)
  };

let reduceReversed =
    (~while_ as predicate: ('acc, 'a) => bool, f: ('acc, 'a) => 'acc, acc: 'acc, deque: t('a))
    : 'acc =>
  switch deque {
  | Ascending(vector) => vector |> Vector.reduceReversed(~while_=predicate, f, acc)
  | Descending(vector) => vector |> Vector.reduce(~while_=predicate, f, acc)
  };

let toSequence = (deque: t('a)) : Sequence.t('a) =>
  switch deque {
  | Ascending(vector) => vector |> Vector.toSequence
  | Descending(vector) => vector |> Vector.toSequenceReversed
  };

let toSequenceReversed = (deque: t('a)) : Sequence.t('a) =>
  switch deque {
  | Ascending(vector) => vector |> Vector.toSequenceReversed
  | Descending(vector) => vector |> Vector.toSequence
  };

let addFirst = (value: 'a, deque: t('a)) : t('a) =>
  switch deque {
  | Ascending(vector) => Ascending(vector |> Vector.addFirst(value))
  | Descending(vector) => Descending(vector |> Vector.addLast(value))
  };

let addLast = (value: 'a, deque: t('a)) : t('a) =>
  switch deque {
  | Ascending(vector) => Ascending(vector |> Vector.addLast(value))
  | Descending(vector) => Descending(vector |> Vector.addFirst(value))
  };

let empty = () : t('a) => Ascending(Vector.empty());

let removeFirstOrRaise = (deque: t('a)) : t('a) =>
  switch deque {
  | Ascending(vector) => Ascending(Vector.removeFirstOrRaise(vector))
  | Descending(vector) => Descending(Vector.removeLastOrRaise(vector))
  };

let removeLastOrRaise = (deque: t('a)) : t('a) =>
  switch deque {
  | Ascending(vector) => Ascending(Vector.removeLastOrRaise(vector))
  | Descending(vector) => Descending(Vector.removeFirstOrRaise(vector))
  };

let return = (value: 'a) : t('a) => Ascending(Vector.return(value));

let reverse = (deque: t('a)) : t('a) =>
  switch deque {
  | Ascending(vector) => Descending(vector)
  | Descending(vector) => Ascending(vector)
  };

let removeAll = (_: t('a)) : t('a) => empty();
