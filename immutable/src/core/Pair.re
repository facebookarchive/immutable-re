/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */
 
let create (a: 'a) (b: 'b): ('a, 'b) => (a, b);

let mapSndWithFst (f: 'k => 'v1 => 'v2) ((k, v): ('k, 'v1)): ('k, 'v2) => (k, f k v);

let mapSnd (f: 'b => 'c) ((a, b): ('a, 'b)): ('a, 'c) => (a , f b);

let pairify (a: 'a): ('a, 'a) => (a, a);
