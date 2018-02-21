/***
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
type transientDequeImpl('a) =
  | Ascending(TransientVector.t('a))
  | Descending(TransientVector.t('a));

type t('a) = Transient.t(transientDequeImpl('a));

let mutate = (deque: Deque.t('a)) : t('a) =>
  switch deque {
  | Ascending(vector) => Transient.create(Ascending(Vector.mutate(vector)))
  | Descending(vector) => Transient.create(Descending(Vector.mutate(vector)))
  };

let addFirst = (value: 'a, transient: t('a)) : t('a) =>
  switch (Transient.get(transient)) {
  | Ascending(vector) =>
    TransientVector.addFirst(value, vector) |> ignore;
    transient
  | Descending(vector) =>
    TransientVector.addLast(value, vector) |> ignore;
    transient
  };

let addLast = (value: 'a, transient: t('a)) : t('a) =>
  switch (Transient.get(transient)) {
  | Ascending(vector) =>
    TransientVector.addLast(value, vector) |> ignore;
    transient
  | Descending(vector) =>
    TransientVector.addFirst(value, vector) |> ignore;
    transient
  };

let count = (transient: t('a)) : int =>
  switch (Transient.get(transient)) {
  | Ascending(vector)
  | Descending(vector) => TransientVector.count(vector)
  };

let empty = () : t('a) => Deque.empty() |> mutate;

let first = (transient: t('a)) : option('a) =>
  switch (Transient.get(transient)) {
  | Ascending(vector) => vector |> TransientVector.first
  | Descending(vector) => vector |> TransientVector.last
  };

let firstOrRaise = (transient: t('a)) : 'a =>
  switch (Transient.get(transient)) {
  | Ascending(vector) => TransientVector.firstOrRaise(vector)
  | Descending(vector) => TransientVector.lastOrRaise(vector)
  };

let last = (transient: t('a)) : option('a) =>
  switch (Transient.get(transient)) {
  | Ascending(vector) => vector |> TransientVector.last
  | Descending(vector) => vector |> TransientVector.first
  };

let lastOrRaise = (transient: t('a)) : 'a =>
  switch (Transient.get(transient)) {
  | Ascending(vector) => TransientVector.lastOrRaise(vector)
  | Descending(vector) => TransientVector.firstOrRaise(vector)
  };

let persist = (transient: t('a)) : Deque.t('a) =>
  switch (Transient.persist(transient)) {
  | Ascending(vector) => Ascending(TransientVector.persist(vector))
  | Descending(vector) => Descending(TransientVector.persist(vector))
  };

let removeAllImpl = (_: Transient.Owner.t, _: transientDequeImpl('a)) : transientDequeImpl('a) =>
  Ascending(TransientVector.empty());

let removeAll = (transient: t('a)) : t('a) => transient |> Transient.update(removeAllImpl);

let removeFirstOrRaise = (transient: t('a)) : t('a) =>
  switch (Transient.get(transient)) {
  | Ascending(vector) =>
    TransientVector.removeFirstOrRaise(vector) |> ignore;
    transient
  | Descending(vector) =>
    TransientVector.removeLastOrRaise(vector) |> ignore;
    transient
  };

let removeLastOrRaise = (transient: t('a)) : t('a) =>
  switch (Transient.get(transient)) {
  | Ascending(vector) =>
    TransientVector.removeLastOrRaise(vector) |> ignore;
    transient
  | Descending(vector) =>
    TransientVector.removeFirstOrRaise(vector) |> ignore;
    transient
  };

let reverseImpl = (_: Transient.Owner.t, vector: transientDequeImpl('a)) =>
  switch vector {
  | Ascending(vector) => Descending(vector)
  | Descending(vector) => Ascending(vector)
  };

let reverse = (transient: t('a)) : t('a) => transient |> Transient.update(reverseImpl);
