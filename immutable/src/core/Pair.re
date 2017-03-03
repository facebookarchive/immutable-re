let create (a: 'a) (b: 'b): ('a, 'b) => (a, b);

let mapSnd (f: 'b => 'c) ((a, b): ('a, 'b)): ('a, 'c) => (a , f b);
