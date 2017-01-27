/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 */

type equality 'a = 'a => 'a => bool;

/* Will be available in Ocaml 4.03
let bytes = Bytes.equal;
let char = Char.equal;
let float = Float.equal;
let int32 = Int32.equal;
let int64 = Int64.equal;
let nativeInt = Nativeint.equal;
let string = String.equal
*/

let reference (that: 'a) (this: 'a): bool => that === this;
let structural (that: 'a) (this: 'a): bool => that == this;
