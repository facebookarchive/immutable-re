/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

let create: 'a => 'b => ('a, 'b);

let mapSndWithFst: ('a => 'b => 'c) => ('a, 'b) => ('a, 'c);

let mapSnd: ('b => 'c) => ('a, 'b) => ('a, 'c);

let pairify: 'a => ('a, 'a);
