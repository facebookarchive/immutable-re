let alwaysFailWith (message: string) () => failwith message;
let alwaysFalse _: bool => false;
let alwaysNone _: option _ => None;
let alwaysTrue _: bool => true;
let alwaysUnit _ => ();
let call (f: unit => 'a): 'a => f ();
let compose (f1: 'a => 'b) (f2: 'b => 'c) (a: 'a): 'c => f2 (f1 a);
let flip (f: 'a => 'b => 'c): ('b => 'a => 'c) => fun b a => f a b;
let identity (a: 'a): 'a => a;
let isFalse = not;
let isTrue (a: bool): bool => a;
let return (a: 'a): (_ => 'a) => fun _ => a;

let module Operators = {
  let (>>) (f1: 'a => 'b) (f2: 'b => 'c): ('a => 'c) => compose f1 f2;
};
