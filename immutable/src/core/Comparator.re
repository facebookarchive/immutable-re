type t 'a = 'a => 'a => Ordering.t;

let make (compare: 'a => 'a => int) (that: 'a) (this: 'a): Ordering.t => {
  let cmp = compare that this;

  cmp > 0 ? Ordering.greaterThan :
  cmp < 0 ? Ordering.lessThan :
  Equal;
};

let bytes = make Bytes.compare;
let char = make Char.compare;
let int32 = make Int32.compare;
let int64 = make Int64.compare;
let nativeInt = make Nativeint.compare;
let string = make String.compare;
let structural (that: 'a) (this: 'a): Ordering.t => make compare that this;
