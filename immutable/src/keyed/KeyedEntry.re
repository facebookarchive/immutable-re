open Hash;

let hash (keyHash: hash 'k) (valueHash: hash 'v) (key: 'k) (value: 'v): int =>
  (keyHash key) lxor (valueHash value);

let hashReducer
    (keyHash: hash 'k)
    (valueHash: hash 'v)
    (acc: int)
    (key: 'k)
    (value: 'v): int =>
  acc + (hash keyHash valueHash key value);
