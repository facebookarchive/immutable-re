module type S = {
  type t;

  let equals: Equality.t t;
};

module type S1 = {
  type t 'a;

  let equals: (Equality.t (t 'a));
};
