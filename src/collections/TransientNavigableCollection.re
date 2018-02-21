/***
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
module type SGeneric = {
  type t('a);
  type elt('a);
  include
    TransientSequentialCollection.SGeneric with type t('a) := t('a) and type elt('a) := elt('a);
  let addLast: (elt('a), t('a)) => t('a);
  let addLastAll: (Iterable.t(elt('a)), t('a)) => t('a);
  let last: t('a) => option(elt('a));
  let lastOrRaise: t('a) => elt('a);
  let removeLastOrRaise: t('a) => t('a);
};

module type S1 = {
  type t('a);
  include TransientSequentialCollection.S1 with type t('a) := t('a);
  let addLast: ('a, t('a)) => t('a);
  let addLastAll: (Iterable.t('a), t('a)) => t('a);
  let last: t('a) => option('a);
  let lastOrRaise: t('a) => 'a;
  let removeLastOrRaise: t('a) => t('a);
};

module type Base = {
  type elt('a);
  type t('a);
  include TransientSequentialCollection.Base with type t('a) := t('a) and type elt('a) := elt('a);
  let addLast: (elt('a), t('a)) => t('a);
  let firstOrRaise: t('a) => elt('a);
  let lastOrRaise: t('a) => elt('a);
  let removeFirstOrRaise: t('a) => t('a);
  let removeLastOrRaise: t('a) => t('a);
};

module MakeGeneric =
       (Base: Base)
       : (SGeneric with type t('a) := Base.t('a) and type elt('a) := Base.elt('a)) => {
  include Base;
  include (
    TransientSequentialCollection.MakeGeneric(Base):
      TransientSequentialCollection.SGeneric with type t('a) := t('a) and type elt('a) := elt('a)
  );
  let last = (collection: t('a)) : option(elt('a)) =>
    if (isEmpty(collection)) {
      None
    } else {
      Some(lastOrRaise(collection))
    };
  let addLastAll = (iter: Iterable.t(elt('a)), transient: t('a)) : t('a) =>
    iter
    |> Iterable.reduce(
         ~while_=Functions.alwaysTrue2,
         (acc, next) => acc |> addLast(next),
         transient
       );
};
