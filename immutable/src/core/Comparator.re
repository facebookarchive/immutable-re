open Ordering;

type comparator 'a = 'a => 'a => ordering;

let make (compare: 'a => 'a => int) (that: 'a) (this: 'a): ordering => {
  let cmp = compare that this;

  cmp > 0 ? GreaterThan :
  cmp < 0 ? LessThan :
  Equal;
};

let bytes = make Bytes.compare;
let char = make Char.compare;
let int32 = make Int32.compare;
let int64 = make Int64.compare;
let nativeInt = make Nativeint.compare;
let string = make String.compare;
let structural (that: 'a) (this: 'a): ordering => make compare that this;
