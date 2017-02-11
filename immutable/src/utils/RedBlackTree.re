open Equality;
open Functions;
open Keyed;
open Option.Operators;
open Seq;

/**
 * Almost straight port of: https://www.cs.kent.ac.uk/people/staff/smk/redblack/Untyped.hs
 * Minor changes to allow for abstract tree constructor function.
 * Also see: http://matt.might.net/papers/germane2014deletion.pdf
 * Admittedly, the type definitions in this code are incredibly terse.
 * The intent is to mirror exactly the reference Haskell implementation.
 */
type color = | R | B;

type redBlackTree 'a =
  | E
  | T color (redBlackTree 'a) 'a (redBlackTree 'a);

let blacken (tree: redBlackTree 'a): (redBlackTree 'a) => switch tree {
  | T R a y b => T B a y b
  | _ => tree
};

let validate (tree: redBlackTree 'a): (redBlackTree 'a) => {
  let rec blackHeight (tree: redBlackTree 'a): int => switch tree {
    | E => 1
    | T color left _ right =>
        let leftBlackHeight = blackHeight left;
        let rightBlackHeight = blackHeight right;
        leftBlackHeight == 0 ? 0 :
        rightBlackHeight == 0 ? 0 :
        leftBlackHeight != rightBlackHeight ? 0 :
        leftBlackHeight + switch color { | B => 1 | _ => 0 };
  };

  blackHeight tree != 0 ? tree : failwith "invalid redblack tree";
};

let balance
    (t: color => redBlackTree 'a => 'a => redBlackTree 'a => redBlackTree 'a)
    (a: redBlackTree 'a)
    (x: 'a)
    (b: redBlackTree 'a): (redBlackTree 'a) =>
  switch (a, x, b) {
    | (T R a x b, y, T R c z d)
    | (T R (T R a x b) y c, z, d)
    | (T R a x (T R b y c), z, d)
    | (a, x, T R b y (T R c z d))
    | (a, x, T R (T R b y c) z d) =>
        let left = t B a x b;
        let right = t B c z d;
        t R left y right
    | _ => t B a x b
  };

let sub1 (tree: redBlackTree 'a): (redBlackTree 'a) => switch tree {
  | T B a x b => T R a x b
  | _ => failwith "invariance violation"
};

let balLeft
    (t: color => redBlackTree 'a => 'a => redBlackTree 'a => redBlackTree 'a)
    (left: redBlackTree 'a)
    (value: 'a)
    (right: redBlackTree 'a): (redBlackTree 'a) => switch (left, value, right) {
  | (T R a x b, y, c) =>
      t R (t B a x b) y c
  | (bl, x, T B a y b) =>
      balance t bl x (T R a y b)
  | (bl, x, T R (T B a y b) z c) =>
      t R (t B bl x a) y (balance t b z (sub1 c))
  | _ => failwith "Invalid state"
};

let balRight
    (t: color => redBlackTree 'a => 'a => redBlackTree 'a => redBlackTree 'a)
    (left: redBlackTree 'a)
    (value: 'a)
    (right: redBlackTree 'a): (redBlackTree 'a) => switch (left, value, right) {
  | (a, x, T R b y c) =>
      t R a x (t B b y c)
  | (T B a x b, y, bl) =>
      balance t (T R a x b) y bl
  | (T R a x (T B b y c), z, bl) =>
      t R (balance t (sub1 a) x b) y (t B c z bl)
  | _ => failwith "Invalid state"
};

let rec app
    (t: color => redBlackTree 'a => 'a => redBlackTree 'a => redBlackTree 'a)
    (a: redBlackTree 'a)
    (b: redBlackTree 'a): (redBlackTree 'a) => switch (a, b) {
  | (E, x) => x
  | (x, E) => x
  | (T R a x b, T R c y d) => switch (app t b c) {
      | T R bPrime z cPrime =>
          t R (t R a x bPrime) z (t R cPrime y d)
      | bc => t R a x (t R bc y d)
    }
  | (T B a x b, T B c y d) => switch (app t b c) {
      | T R bPrime z cPrime =>
          t R (t B a x bPrime) z (t B cPrime y d)
      | bc => balLeft t a x (T B bc y d)
    }
  | (a, T R b x c) => t R (app t a b) x c
  | (T R a x b, c) => t R a x (app t b c)
};

let rec reduce (f: 'acc => 'a => 'acc) (acc: 'acc) (tree: redBlackTree 'a): 'acc => switch tree {
  | T _ left value right =>
     let acc = reduce f acc left;
     let acc = f acc value;
     let acc = reduce f acc right;
     acc
  | E => acc
};

let rec reduceRight (f: 'acc => 'a => 'acc) (acc: 'acc) (tree: redBlackTree 'a): 'acc => switch tree {
  | T _ left value right =>
     let acc = reduceRight f acc right;
     let acc = f acc value;
     let acc = reduceRight f acc left;
     acc
  | E => acc
};

let rec toSeq (tree: redBlackTree 'a): (seq 'a) => switch tree {
  | T _ left value right => Seq.concat [
      Seq.defer(fun () => toSeq left),
      Seq.return value,
      Seq.defer(fun () => toSeq right),
    ]
  | E => Seq.empty
};

let rec toSeqReversed (tree: redBlackTree 'a): (seq 'a) => switch tree {
  | T _ left value right => Seq.concat [
      Seq.defer(fun () => toSeqReversed right),
      Seq.return value,
      Seq.defer(fun () => toSeqReversed left),
    ]
  | E => Seq.empty
};
