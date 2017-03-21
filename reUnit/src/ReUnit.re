/**
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

let identity a => a;

let module Equality = {
  let rec listWith
      (equals: 'a => 'a => bool)
      (this: list 'a)
      (that: list 'a): bool => switch (this, that) {
    | ([thisHead, ...thisTail], [thatHead, ...thatTail]) =>
      if (equals thisHead thatHead) (listWith equals thisTail thatTail)
      else false
    | ([], []) => true
    | _ => false
  };
};

let module ToString = {
  let ofList (toString: 'a => string) (list: list 'a): string => {
    let buffer = Buffer.create 16;
    Buffer.add_string buffer "[";

    let rec recurse list => switch list {
      | [head, ...tail] =>
        Buffer.add_string buffer (toString head);
        Buffer.add_string buffer ", ";
        recurse tail;
      | [] => ();
    };

    recurse list;
    Buffer.add_string buffer "]";
    buffer |> Buffer.contents;
  };

  let ofOption (toString: 'a => string) (opt: option 'a): string => switch opt {
    | Some x => "Some (" ^ (toString x) ^ ")"
    | None => "None"
  };
};

let module Test = {
  type t =
    | Describe string (list t)
    | It string (unit => unit);

  let describe (label: string) (tests: list t): t =>
    Describe label tests;

  let it (label: string) (f: unit => unit): t =>
    It label f;

  let toList (test: t): (list (string, unit => unit)) => {
    let rec recurse (context: string) (test: t): (list (string, unit => unit)) => switch test {
      | Describe label tests =>
          let label = context ^ "[" ^ label ^ "]";
          tests |> List.rev_map (recurse label) |> List.flatten;
      | It label f =>
          let label = context ^ ", (" ^ label ^ ")";
          [(label, f)]
    };
    recurse "" test
  };
};

let module Expect = {
  type t 'a = | Value 'a | Error exn;

  let expect (value: 'a): (t 'a) => Value value;

  let failwith (message: string): (t 'a) =>
    try (failwith message) { | exn => Error exn };

  let flatMap (f: 'a => t 'b) (expect: t 'a): (t 'b) => switch expect {
    | Value a => f a
    | Error exn => Error exn
  };

  let forEach (f: 'a => unit) (expect: t 'a): unit => switch expect {
    | Value a => (f a)
    | Error _ => ()
  };

  let get (expect: t 'a): 'a => switch expect {
    | Value a => a
    | Error exn => /* reraise */ raise exn
  };

  let map (f: 'a => 'b) (expect: t 'a): (t 'b) => switch expect {
    | Value a => Value (f a)
    | Error exn => Error exn
  };

  let return = expect;

  let toBeEqualToWith
      (equals: 'a => 'a => bool)
      (toString: 'a => string)
      (expected: 'a)
      (expect: t 'a) => expect |> flatMap (fun value =>
    if (not (equals expected value)) (
      failwith ("expected: " ^ (toString expected) ^ " but got: " ^ (toString value))
    )
    else return value
  ) |> get |> ignore;

  let toBeEqualToStructuralEquality (toString: 'a => string) =>
    toBeEqualToWith Pervasives.(==) toString;

  let toBeEqualToFalse = toBeEqualToStructuralEquality string_of_bool false;

  let toBeEqualToInt = toBeEqualToStructuralEquality string_of_int;

  let toBeEqualToList (equals: 'a => 'a => 'bool) (toString: 'a => string) =>
    toBeEqualToWith (Equality.listWith equals) (ToString.ofList toString);

  let toBeEqualToListOfInt (list: list int) =>
    toBeEqualToList Pervasives.(==) string_of_int list;

  let toBeEqualToListOfString (list: list string) =>
    toBeEqualToList Pervasives.(==) (fun s => s) list;

  let toBeEqualToNone (toString: 'a => string) =>
    toBeEqualToStructuralEquality (ToString.ofOption toString) None;

  let toBeEqualToNoneOfInt (expect: t (option int)) =>
    toBeEqualToNone string_of_int expect;

  let toBeEqualToNoneOfString (expect: t (option 'a)) =>
    toBeEqualToNone identity expect;

  let toBeEqualToSome (toString: 'a => string) (value: 'a) =>
    toBeEqualToStructuralEquality (ToString.ofOption toString) (Some value);

  let toBeEqualToSomeOfInt (value: int) => toBeEqualToSome string_of_int value;

  let toBeEqualToSomeOfString (value: string) => toBeEqualToSome identity value;

  let toBeEqualToString = toBeEqualToStructuralEquality identity;

  let toBeEqualToTrue = toBeEqualToStructuralEquality string_of_bool true;

  let defer (expr: unit => 'a): (t 'a) =>
    try (Value (expr ())) { | exn => Error exn };

  let shouldRaise (expr: unit => 'a) => {
    let result = try (Value (expr ())) { | exn => Error exn };
    result |> forEach (fun _ =>
      Pervasives.failwith "expected exception to be raised"
    );
  };
};

let run (tests: Test.t): unit => {
  /* In theory we can swap this simple test runner with ounit */
  let execute (label, f): int => {
    let startTime = Sys.time ();
    let result = try { f (); None } { | exn => Some exn };
    let endTime = Sys.time ();
    Printf.printf "%f, %s" (endTime -. startTime) label;

    switch result {
      | Some exn =>
          print_string "Test Failure!!!!\n";
          Printexc.to_string exn |> print_string;
          print_newline ();
          Printexc.print_backtrace stdout;
          print_newline ();
          0;
      | None =>
          print_newline ();
          1
    }
  };

  let (total, success) = tests
    |> Test.toList
    |> List.rev_map execute
    |> List.fold_left
      (fun (total, success) result => (total + 1, success + result))
      (0, 0);

  Printf.printf "Executed %i tests. %i tests succeeded." total success;
  print_newline ();
};
