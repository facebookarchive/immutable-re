/***
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
type t('a) =
  | Empty
  | Instance(
              'collection,
              SequentialCollection.s('collection, 'a),
              SequentialCollection.s('collection, 'a)
            ): t('a);

type navigableCollection('a) = t('a);

module type SGeneric = {
  type elt('a);
  type t('a);
  include SequentialCollection.SGeneric with type elt('a) := elt('a) and type t('a) := t('a);
  let last: t('a) => option(elt('a));
  let lastOrRaise: t('a) => elt('a);
  let reduceReversed:
    (~while_: ('acc, elt('a)) => bool=?, ('acc, elt('a)) => 'acc, 'acc, t('a)) => 'acc;
  let toCollectionReversed: t('a) => Collection.t(elt('a));
  let toIterableReversed: t('a) => Iterable.t(elt('a));
  let toNavigableCollection: t('a) => navigableCollection(elt('a));
  let toNavigableCollectionReversed: t('a) => navigableCollection(elt('a));
  let toSequenceReversed: t('a) => Sequence.t(elt('a));
  let toSequentialCollectionReversed: t('a) => SequentialCollection.t(elt('a));
};

module type S = {
  type a;
  type t;
  include SequentialCollection.S with type a := a and type t := t;
  let last: t => option(a);
  let lastOrRaise: t => a;
  let reduceReversed: (~while_: ('acc, a) => bool=?, ('acc, a) => 'acc, 'acc, t) => 'acc;
  let toCollectionReversed: t => Collection.t(a);
  let toIterableReversed: t => Iterable.t(a);
  let toNavigableCollection: t => navigableCollection(a);
  let toNavigableCollectionReversed: t => navigableCollection(a);
  let toSequenceReversed: t => Sequence.t(a);
  let toSequentialCollectionReversed: t => SequentialCollection.t(a);
};

module type S1 = {
  type t('a);
  include SequentialCollection.S1 with type t('a) := t('a);
  let last: t('a) => option('a);
  let lastOrRaise: t('a) => 'a;
  let reduceReversed: (~while_: ('acc, 'a) => bool=?, ('acc, 'a) => 'acc, 'acc, t('a)) => 'acc;
  let toCollectionReversed: t('a) => Collection.t('a);
  let toIterableReversed: t('a) => Iterable.t('a);
  let toNavigableCollection: t('a) => navigableCollection('a);
  let toNavigableCollectionReversed: t('a) => navigableCollection('a);
  let toSequenceReversed: t('a) => Sequence.t('a);
  let toSequentialCollectionReversed: t('a) => SequentialCollection.t('a);
};

module type Base = {
  type elt('a);
  type t('a);
  include SequentialCollection.Base with type t('a) := t('a) and type elt('a) := elt('a);
  let lastOrRaise: t('a) => elt('a);
  let reduceReversed:
    (~while_: ('acc, elt('a)) => bool, ('acc, elt('a)) => 'acc, 'acc, t('a)) => 'acc;
  let toSequenceReversed: t('a) => Sequence.t(elt('a));
};

module MakeGeneric =
       (Base: Base)
       : (SGeneric with type t('a) := Base.t('a) and type elt('a) := Base.elt('a)) => {
  include Base;
  include (
    SequentialCollection.MakeGeneric(Base):
      SequentialCollection.SGeneric with type t('a) := t('a) and type elt('a) := elt('a)
  );
  let last = (collection: t('a)) : option(elt('a)) =>
    if (isEmpty(collection)) {
      None
    } else {
      Some(lastOrRaise(collection))
    };
  module ReversedSequentialCollection =
    SequentialCollection.MakeGeneric(
      {
        type nonrec t('a) = t('a);
        type nonrec elt('a) = elt('a);
        let count = count;
        let firstOrRaise = lastOrRaise;
        let reduce = Base.reduceReversed;
        let toSequence = toSequenceReversed;
      }
    );
  let reduceReversed = ReversedSequentialCollection.reduce;
  let toIterableReversed = ReversedSequentialCollection.toIterable;
  let toSequenceReversed = ReversedSequentialCollection.toSequence;
  let toCollectionReversed = ReversedSequentialCollection.toCollection;
  let toSequentialCollectionReversed = ReversedSequentialCollection.toSequentialCollection;
  let navigableCollectionBase: SequentialCollection.s(t('a), elt('a)) = {
    count,
    firstOrRaise,
    reduce: Base.reduce,
    toSequence
  };
  let navigableCollectionReversedBase: SequentialCollection.s(t('a), elt('a)) = {
    count,
    firstOrRaise: lastOrRaise,
    reduce: Base.reduceReversed,
    toSequence: toSequenceReversed
  };
  let toNavigableCollection = (collection: t('a)) : navigableCollection(elt('a)) =>
    if (isEmpty(collection)) {
      Empty
    } else {
      Instance(collection, navigableCollectionBase, navigableCollectionReversedBase)
    };
  let toNavigableCollectionReversed = (collection: t('a)) : navigableCollection(elt('a)) =>
    if (isEmpty(collection)) {
      Empty
    } else {
      Instance(collection, navigableCollectionReversedBase, navigableCollectionBase)
    };
};

let count = (collection: t('a)) : int =>
  switch collection {
  | Empty => 0
  | Instance(collection, {count}, _) => count(collection)
  };

let empty = () : t('a) => Empty;

let firstOrRaise = (collection: t('a)) : 'a =>
  switch collection {
  | Empty => failwith("empty")
  | Instance(collection, {firstOrRaise}, _) => firstOrRaise(collection)
  };

let lastOrRaise = (collection: t('a)) : 'a =>
  switch collection {
  | Empty => failwith("empty")
  | Instance(collection, _, {firstOrRaise}) => firstOrRaise(collection)
  };

let reduce =
    (~while_ as predicate: ('acc, 'a) => bool, f: ('acc, 'a) => 'acc, acc: 'acc, collection: t('a))
    : 'acc =>
  switch collection {
  | Empty => acc
  | Instance(collection, {reduce}, _) => collection |> reduce(~while_=predicate, f, acc)
  };

let reduceReversed =
    (~while_ as predicate: ('acc, 'a) => bool, f: ('acc, 'a) => 'acc, acc: 'acc, collection: t('a))
    : 'acc =>
  switch collection {
  | Empty => acc
  | Instance(collection, _, {reduce}) => collection |> reduce(~while_=predicate, f, acc)
  };

let toSequence = (collection: t('a)) : Sequence.t('a) =>
  switch collection {
  | Empty => Sequence.empty()
  | Instance(collection, {toSequence}, _) => toSequence(collection)
  };

let toSequenceReversed = (collection: t('a)) : Sequence.t('a) =>
  switch collection {
  | Empty => Sequence.empty()
  | Instance(collection, _, {toSequence}) => toSequence(collection)
  };
