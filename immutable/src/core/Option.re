let count (opt: option 'a): int => switch opt {
  | Some _ => 1
  | None => 0
};

let empty: (option 'a) = None;

let filter (f: 'a => bool) (opt: option 'a): (option 'a) => switch opt {
  | Some x => f x ? opt : None
  | _ => None
};

let flatMap (f: 'a => option 'b) (opt: option 'a): option 'b => switch opt {
  | Some a => f a
  | _ => None
};

let flatten (opt: option (option 'a)): (option 'a) => switch opt {
  | Some (Some a) => Some a
  | _ => None
};

let forEach (f: 'a => unit) (opt: option 'a): unit => switch opt {
  | Some a => f a
  | _ => ()
};

let get (opt: option 'a): 'a => switch opt {
  | Some x => x
  | None => failwith "option is none"
};

let isEmpty (opt: option _): bool => switch opt {
  | Some _ => false
  | None => true
};

let isNotEmpty (opt: option _): bool => switch opt {
  | Some _ => true
  | None => false
};

let map (f: 'a => 'b) (opt: option 'a): option 'b => switch opt {
  | Some a => Some (f a)
  | _ => None
};

let none (f: 'a => bool) (opt: option 'a): bool => switch opt {
  | Some a => f a
  | _ => true
};

let reduce (f: 'acc => 'a => 'acc) (acc: 'acc) (opt: option 'a): 'acc => switch opt {
  | Some a => f acc a
  | _ => acc
};

let return (a: 'a): (option 'a) => Some a;

let some (f: 'a => bool) (opt: option 'a): bool => switch opt {
  | Some a => f a
  | _ => false
};

let orCompute (compute: unit => 'a) (opt: option 'a): 'a => switch (opt) {
  | Some a => a
  | _ => compute ()
};

let orDefault (defaultValue: 'a) (opt: option 'a): 'a => switch (opt) {
  | Some a => a
  | _ => defaultValue
};

let module Operators = {
  let (>>=) (opt: option 'a) (f: 'a => option 'b): option 'b => flatMap f opt;
  let (>>|) (opt: option 'a) (f:'a => 'b): option 'b => map f opt;
  let (|?) (opt: option 'a) (defaultValue: 'a): 'a => orDefault defaultValue opt;
};
