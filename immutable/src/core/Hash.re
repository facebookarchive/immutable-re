type t 'a = 'a => int;

let prng = lazy (Random.State.make_self_init ());

let random ():  t 'a =>  {
  let seed = Random.State.bits (Lazy.force prng);
  Hashtbl.seeded_hash seed
};

let structural = Hashtbl.hash;

let initialValue = 17;

let reducer (hash: t 'a) (acc: int) (next: 'a): int =>
  (31 * acc) + (hash next);
