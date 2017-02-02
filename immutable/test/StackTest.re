open Immutable;
open ReUnit.Test;

let test = describe "Stack" (StackTester.test 10 (module {
  type t 'a = Stack.t 'a;

  let addFirst = Stack.addFirst;
  let count = Stack.count;
  let empty = Stack.empty;
  let every = Stack.every;
  let first = Stack.first;
  let isEmpty = Stack.isEmpty;
  let isNotEmpty = Stack.isNotEmpty;
  let mapReverse = Stack.mapReverse;
  let none = Stack.none;
  let reduce = Stack.reduce;
  let removeAll = Stack.removeAll;
  let removeFirst = Stack.removeFirst;
  let reverse = Stack.reverse;
  let some = Stack.some;
  let toSeq = Stack.toSeq;
  let tryFirst = Stack.tryFirst;
}));
