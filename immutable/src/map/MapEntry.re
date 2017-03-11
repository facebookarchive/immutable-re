let hash (keyHash: Hash.t 'k) (valueHash: Hash.t 'v) (key: 'k) (value: 'v): int =>
  (keyHash key) lxor (valueHash value);

let hashReducer
    (keyHash: Hash.t 'k)
    (valueHash: Hash.t 'v)
    (acc: int)
    (key: 'k)
    (value: 'v): int =>
  acc + (hash keyHash valueHash key value);
