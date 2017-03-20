module type S = {
  type t;

  include Equatable.S with type t := t;

  let compare: Comparator.t t;
};
