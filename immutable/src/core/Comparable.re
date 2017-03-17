module type S = {
  type t;

  let compare: Comparator.t t;
};
