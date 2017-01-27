/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

open Hashtbl;
open Sys;

type hash 'a = 'a => int;

let random ():  hash 'a =>  {
  let time = Sys.time () *. 1000000.0 |> int_of_float;
  Hashtbl.seeded_hash time
};
