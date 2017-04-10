![Immutable Collections For Reason](https://github.com/facebookincubator/immutable-re/raw/master/docs/images/logo-800x215.png?raw=true)
===========================================================================
Pure [Reason](https://facebook.github.io/reason) implementation of persistent immutable data structures.

# Features
Immutable-re provides a complete set of efficient persistent immutable data
structures for [Reason](https://facebook.github.io/reason) and [OCaml](http://www.ocaml.org/),
targeting both OCaml native and byte code compilation modes, as well JavaScript using
 [BuckleScript](https://github.com/bloomberg/bucklescript).

The api includes concrete implementations of vectors, sets, and maps. Many
implementations support [transient mutability](http://clojure.org/reference/transients)
for efficient batch mutations. Additionally Immutable-re provides lazy
functional iterators and sequences, along with type definitions for basic operators
such as equality, comparison, and hashing.

For more details see the [API docs](http://facebookincubator.github.io/immutable-re/api.html).

# Installing

## Installing via NPM (JS Workflow)

```bash
npm install --save immutable-re
```

## Installing `immutable` via OPAM (native workflow)

```bash
# On macOS, install opam via Homebrew:
brew update
brew install opam
# On Linux, see here (you will need opam >= 1.2.2): http://opam.ocaml.org/doc/Install.html

opam init
# Add this to your ~/.bashrc (or ~/.zshrc):
#   eval $(opam config env)

opam update
opam switch 4.02.3
eval $(opam config env)
opam install immutable
```

## Contributing to development

```bash
# On macOS, install opam via Homebrew:
brew update
brew install opam
# On Linux, see here (you will need opam >= 1.2.2): http://opam.ocaml.org/doc/Install.html

opam init
# Add this to your ~/.bashrc (or ~/.zshrc):
#   eval $(opam config env)

opam update
opam switch 4.02.3
eval $(opam config env)
git clone git@github.com:facebookincubator/immutable-re.git
cd immutable-re
opam pin add -y immutable .
```

# Status

Immutable-re is under active development and is not yet production ready. We are
releasing this early alpha to get community input and contributions. If you are
interested in contributing please follow the directions [here](https://github.com/facebookincubator/immutable-re/blob/master/CONTRIBUTING.md).

For JavaScript developers needing production ready immutable collections,
we recommend using the [Reason BuckleScript bindings to Immutable.js](https://github.com/BuckleTypes/bs-immutablejs.git).

License
-------
Immutable-re is [BSD-licensed](https://github.com/facebookincubator/immutable-re/blob/master/LICENSE.txt). We also provide an additional [patent grant](https://github.com/facebookincubator/immutable-re/blob/master/PATENTS.txt).
