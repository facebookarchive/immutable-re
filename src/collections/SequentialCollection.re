/***
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
type s('collection, 'a) = {
  count: 'collection => int,
  firstOrRaise: 'collection => 'a,
  reduce: 'acc .(~while_: ('acc, 'a) => bool, ('acc, 'a) => 'acc, 'acc, 'collection) => 'acc,
  toSequence: 'collection => Sequence.t('a)
};

type t('a) =
  | Empty
  | Instance('collection, s('collection, 'a)): t('a);

type sequentialCollection('a) = t('a);

module type SGeneric = {
  type elt('a);
  type t('a);
  include Collection.SGeneric with type elt('a) := elt('a) and type t('a) := t('a);
  let first: t('a) => option(elt('a));
  let firstOrRaise: t('a) => elt('a);
  let toSequentialCollection: t('a) => sequentialCollection(elt('a));
};

module type S = {
  type a;
  type t;
  include Collection.S with type a := a and type t := t;
  let first: t => option(a);
  let firstOrRaise: t => a;
  let toSequentialCollection: t => sequentialCollection(a);
};

module type S1 = {
  type t('a);
  include Collection.S1 with type t('a) := t('a);
  let first: t('a) => option('a);
  let firstOrRaise: t('a) => 'a;
  let toSequentialCollection: t('a) => sequentialCollection('a);
};

module type Base = {
  type elt('a);
  type t('a);
  include Collection.Base with type t('a) := t('a) and type elt('a) := elt('a);
  let firstOrRaise: t('a) => elt('a);
};

module MakeGeneric =
       (Base: Base)
       : (SGeneric with type t('a) := Base.t('a) and type elt('a) := Base.elt('a)) => {
  include Base;
  include (
    Collection.MakeGeneric(
      {
        type nonrec elt('a) = elt('a);
        type nonrec t('a) = t('a);
        let count = Base.count;
        let reduce = Base.reduce;
        let toSequence = Base.toSequence;
      }
    ):
      Collection.SGeneric with type t('a) := t('a) and type elt('a) := elt('a)
  );
  let first = (collection: t('a)) : option(elt('a)) =>
    if (isEmpty(collection)) {
      None
    } else {
      Some(firstOrRaise(collection))
    };
  let sequentialCollectionBase: s(t('a), elt('a)) = {
    count,
    firstOrRaise,
    reduce: Base.reduce,
    toSequence
  };
  let toSequentialCollection = (collection: t('a)) : sequentialCollection(elt('a)) =>
    if (isEmpty(collection)) {
      Empty
    } else {
      Instance(collection, sequentialCollectionBase)
    };
};

let count = (collection: t('a)) : int =>
  switch collection {
  | Empty => 0
  | Instance(collection, {count}) => count(collection)
  };

let empty = () : t('a) => Empty;

let firstOrRaise = (collection: t('a)) : 'a =>
  switch collection {
  | Empty => failwith("empty")
  | Instance(collection, {firstOrRaise}) => firstOrRaise(collection)
  };

let reduce =
    (~while_ as predicate: ('acc, 'a) => bool, f: ('acc, 'a) => 'acc, acc: 'acc, collection: t('a))
    : 'acc =>
  switch collection {
  | Empty => acc
  | Instance(collection, {reduce}) => collection |> reduce(~while_=predicate, f, acc)
  };

let toSequence = (collection: t('a)) : Sequence.t('a) =>
  switch collection {
  | Empty => Sequence.empty()
  | Instance(collection, {toSequence}) => toSequence(collection)
  };
