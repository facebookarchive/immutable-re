/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Hashtbl;
open Sys;

type hash 'a = 'a => int;

let prng = lazy (Random.State.make_self_init ());

let random ():  hash 'a =>  {
  let seed = Random.State.bits (Lazy.force prng);
  Hashtbl.seeded_hash seed
};

let structural = Hashtbl.hash;

let initialValue = 17;

let reducer (hash: hash 'a) (acc: int) (next: 'a) =>
  (31 * acc) + (hash next);
