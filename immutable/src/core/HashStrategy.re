/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

type t 'a =
  | Comparator (Hash.t 'a) (Comparator.t 'a)
  | Equality (Hash.t 'a) (Equality.t 'a);

let createWithComparator hash::(hash: Hash.t 'a) comparator::(comparator: Comparator.t 'a): (t 'a) =>
  Comparator hash comparator;

let createWithEquality hash::(hash: Hash.t 'a) equality::(equality: Equality.t 'a): (t 'a) =>
  Equality hash equality;

let identity: (t 'a) =
  Equality Hash.structural Equality.reference;

let structuralCompare: (t 'a) =
  Comparator Hash.structural Comparator.structural;

let structuralEquality: (t 'a) =
  Equality Hash.structural Equality.structural;

let comparator (strategy: t 'a): (Comparator.t 'a) => switch strategy {
  | Comparator _ comparator => comparator;
  | Equality _ equality => fun x y =>
      /* FIXME: Should this just throw? */
      if (equality x y) Ordering.equal
      else Ordering.greaterThan;
};

let equals (strategy: t 'a): (Equality.t 'a) => switch strategy {
  | Comparator _ comparator => Comparator.toEquality comparator;
  | Equality _ equals => equals
};

let hash (strategy: t 'a): (Hash.t 'a) => switch strategy {
  | Comparator hash _
  | Equality hash _ => hash;
};
