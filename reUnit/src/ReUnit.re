/***
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
let identity = (a) => a;

module Equality = {
  let rec listWith = (~equals: ('a, 'a) => bool, this: list('a), that: list('a)) : bool =>
    switch (this, that) {
    | ([thisHead, ...thisTail], [thatHead, ...thatTail]) =>
      if (equals(thisHead, thatHead)) {
        listWith(~equals, thisTail, thatTail)
      } else {
        false
      }
    | ([], []) => true
    | _ => false
    };
  let bool = (this: bool, that: bool) => this === that;
  let int = (this: int, that: int) => this === that;
  let optionWith = (~equals: ('a, 'a) => bool, this: option('a), that: option('a)) : bool =>
    switch (this, that) {
    | (Some(this), Some(that)) => equals(this, that)
    | (None, None) => true
    | _ => false
    };
};

module ToString = {
  let ofList = (toString: 'a => string, list: list('a)) : string => {
    let buffer = Buffer.create(16);
    Buffer.add_string(buffer, "[");
    let rec recurse = (list) =>
      switch list {
      | [head, ...tail] =>
        Buffer.add_string(buffer, toString(head));
        Buffer.add_string(buffer, ", ");
        recurse(tail)
      | [] => ()
      };
    recurse(list);
    Buffer.add_string(buffer, "]");
    buffer |> Buffer.contents
  };
  let ofOption = (toString: 'a => string, opt: option('a)) : string =>
    switch opt {
    | Some(x) => "Some (" ++ toString(x) ++ ")"
    | None => "None"
    };
};

module Test = {
  type t =
    | Describe(string, list(t))
    | It(string, unit => unit);
  let describe = (label: string, tests: list(t)) : t => Describe(label, tests);
  let it = (label: string, f: unit => unit) : t => It(label, f);
  let toList = (test: t) : list((string, unit => unit)) => {
    let rec recurse = (context: string, test: t) : list((string, unit => unit)) =>
      switch test {
      | Describe(label, tests) =>
        let label = context ++ "[" ++ label ++ "]";
        tests |> List.rev_map(recurse(label)) |> List.flatten
      | It(label, f) =>
        let label = context ++ "(" ++ label ++ ")";
        [(label, f)]
      };
    recurse("", test)
  };
};

module Expect = {
  let toBeEqualToWith =
      (~equals: ('a, 'a) => bool, ~toString: 'a => string, expected: 'a, actual: 'a) =>
    if (! equals(expected, actual)) {
      failwith("expected: " ++ toString(expected) ++ " but got: " ++ toString(actual))
    };
  let toBeEqualToFalse = toBeEqualToWith(~equals=Equality.bool, ~toString=string_of_bool, false);
  let toBeEqualToInt = toBeEqualToWith(~equals=Equality.int, ~toString=string_of_int);
  let toBeEqualToListWith = (~equals: ('a, 'a) => 'bool, ~toString: 'a => string) =>
    toBeEqualToWith(~equals=Equality.listWith(~equals), ~toString=ToString.ofList(toString));
  let toBeEqualToListOfInt = (list: list(int)) =>
    toBeEqualToListWith(~equals=Pervasives.(==), ~toString=string_of_int, list);
  let toBeEqualToListOfString = (list: list(string)) =>
    toBeEqualToListWith(~equals=Pervasives.(==), ~toString=(s) => s, list);
  let toBeEqualToNoneWith = (~toString: 'a => string) =>
    toBeEqualToWith(
      ~equals=Equality.optionWith(~equals=Pervasives.(===)),
      ~toString=ToString.ofOption(toString),
      None
    );
  let toBeEqualToNoneOfInt = (expect: option(int)) =>
    toBeEqualToNoneWith(~toString=string_of_int, expect);
  let toBeEqualToNoneOfString = (expect: option('a)) =>
    toBeEqualToNoneWith(~toString=identity, expect);
  let toBeEqualToSomeWith = (~equals: ('a, 'a) => bool, ~toString: 'a => string, value: 'a) =>
    toBeEqualToWith(
      ~equals=Equality.optionWith(~equals),
      ~toString=ToString.ofOption(toString),
      Some(value)
    );
  let toBeEqualToSomeOfInt = (value: int) =>
    toBeEqualToSomeWith(~equals=Equality.int, ~toString=string_of_int, value);
  let toBeEqualToSomeOfString = (value: string) =>
    toBeEqualToSomeWith(~equals=Pervasives.(==), ~toString=identity, value);
  let toBeEqualToString = (value: string) =>
    toBeEqualToWith(~equals=Pervasives.(==), ~toString=identity, value);
  let toBeEqualToTrue = toBeEqualToWith(~equals=Equality.bool, ~toString=string_of_bool, true);
  exception DidNotRaise;
  let shouldRaise = (expr: unit => 'a) =>
    try {
      expr();
      raise(DidNotRaise)
    } {
    | DidNotRaise => failwith("expected exception to be raised")
    | _ => ()
    };
};

let run = (tests: Test.t) : unit => {
  /* In theory we can swap this simple test runner with ounit */
  let execute = ((label, f)) : int => {
    let startTime = Sys.time();
    let result =
      try {
        f();
        None
      } {
      | exn => Some(exn)
      };
    let endTime = Sys.time();
    Printf.printf("%f %s", endTime -. startTime, label);
    switch result {
    | Some(exn) =>
      print_string("Test Failure!!!!\n");
      Printexc.to_string(exn) |> print_string;
      print_newline();
      Printexc.print_backtrace(stdout);
      print_newline();
      0
    | None =>
      print_newline();
      1
    }
  };
  let (total, success) =
    tests
    |> Test.toList
    |> List.rev_map(execute)
    |> List.fold_left(((total, success), result) => (total + 1, success + result), (0, 0));
  Printf.printf("Executed %i tests. %i tests succeeded.", total, success);
  print_newline()
};
