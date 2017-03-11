type t 'a =
  | Comparator (Hash.t 'a) (Comparator.t 'a)
  | Equality (Hash.t 'a) (Equality.t 'a);

let createWithComparator (hash: Hash.t 'a) (comparator: Comparator.t 'a): (t 'a) =>
  Comparator hash comparator;

let createWithEquality (hash: Hash.t 'a) (equality: Equality.t 'a): (t 'a) =>
  Equality hash equality;

let identity: (t 'a) =
  Equality Hash.structural Equality.reference;

let structuralCompare: (t 'a) =
  Comparator Hash.structural Comparator.structural;

let structuralEquality: (t 'a) =
  Equality Hash.structural Equality.structural;

let comparator (strategy: t 'a): (Comparator.t 'a) => switch strategy {
  | Comparator _ comparator => comparator;
  | Equality _ equality => fun x y => equality x y ? Equal : GreaterThan;
};

let equals (strategy: t 'a): (Equality.t 'a) => switch strategy {
  | Comparator _ comparator => fun x y => (comparator x y) === Equal
  | Equality _ equals => equals
};

let hash (strategy: t 'a): (Hash.t 'a) => switch strategy {
  | Comparator hash _
  | Equality hash _ => hash;
};
