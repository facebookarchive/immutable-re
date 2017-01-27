/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

let alwaysFailWith: string => unit => 'a;
let alwaysFalse: _ => bool;
let alwaysNone: _ => option _;
let alwaysTrue: _ => bool;
let alwaysUnit: _ => unit;
let call: (unit => 'a) => 'a;
let compose: ('a => 'b) => ('b => 'c) => 'a => 'c;
let flip: ('a => 'b => 'c) => 'b => 'a => 'c;
let identity: 'a => 'a;
let isFalse: bool => bool;
let isTrue: bool => bool;
let return: 'a => _ => 'a;

let module Operators: {
  let (>>): ('a => 'b) => ('b => 'c) => 'a => 'c;
};
