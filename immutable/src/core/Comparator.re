type t 'a = 'a => 'a => Ordering.t;

let make (compare: 'a => 'a => int) (that: 'a) (this: 'a): Ordering.t => {
  let cmp = compare that this;

  if (cmp > 0) Ordering.greaterThan
  else if (cmp < 0) Ordering.lessThan
  else Ordering.equal;
};

let bytes = make Bytes.compare;
let char = make Char.compare;
let int32 = make Int32.compare;
let int64 = make Int64.compare;
let nativeInt = make Nativeint.compare;
let string = make String.compare;
let structural (that: 'a) (this: 'a): Ordering.t => make compare that this;
