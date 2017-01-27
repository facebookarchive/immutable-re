/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Comparator;
open Equality;
open Hash;
open Ordering;

type hashStrategy 'a =
  | Comparator (hash 'a) (comparator 'a)
  | Equality (hash 'a) (equality 'a);

let createWithComparator (hash: hash 'a) (comparator: comparator 'a): (hashStrategy 'a) =>
  Comparator hash comparator;

let createWithEquality (hash: hash 'a) (equality: equality 'a): (hashStrategy 'a) =>
  Equality hash equality;

let identity (): hashStrategy 'a =>
  Equality (Hash.random ()) Equality.reference;

let structuralCompare (): hashStrategy 'a =>
  Comparator (Hash.random ()) Comparator.structural;

let structuralEquality (): hashStrategy 'a =>
  Equality (Hash.random ()) Equality.structural;

let comparator (strategy: hashStrategy 'a): comparator 'a => switch strategy {
  | Comparator _ comparator => comparator;
  | Equality _ equality => fun x y => equality x y ? Equal : GreaterThan;
};

let hash (value: 'a) (strategy: hashStrategy 'a): int => switch strategy {
  | Comparator hash _
  | Equality hash _ => hash value;
};
