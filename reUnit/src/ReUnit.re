open Immutable;

let identity a => a;

let module Test = {
  type t =
    | Describe string (list t)
    | It string (unit => unit);

  let describe (label: string) (tests: list t): t =>
    Describe label tests;

  let it (label: string) (f: unit => unit): t =>
    It label f;

  let toSeq (test: t): (Seq.t (string, unit => unit)) => {
    let rec toSeqImpl (context: string) (test: t): (Seq.t (string, unit => unit)) => switch test {
      | Describe label tests =>
          let label = context ^ "[" ^ label ^ "]";
          tests |> List.toSeq |> Seq.flatMap (toSeqImpl label)
      | It label f =>
          let label = context ^ ", (" ^ label ^ ")";
          Seq.return (label, f)
    };
    toSeqImpl "" test
  };
};

let module Expect = {
  type t 'a = | Value 'a | Error exn;

  let defer (expr: unit => 'a): (t 'a) =>
    try (Value (expr ())) { | exn => Error exn };

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

  /* FIXME: this is so broken */
  let stringOfSeq (toString: 'a => string) (seq: Seq.t 'a): string =>
    "["  ^ (seq |> Seq.reduce
      (fun acc next => acc ^ ", " ^ (toString next))
      ""
    ) ^ "]";

  let stringOfOption (toString: 'a => string) (opt: option 'a): string =>
    opt |> Option.reduce (fun _ => toString) "";

  let toBeEqualToWith
      (equals: Equality.t 'a)
      (toString: 'a => string)
      (expected: 'a)
      (expect: t 'a) => expect |> flatMap (fun value =>
    if (not (equals expected value)) (
      failwith ("expected: " ^ (toString expected) ^ " but got: " ^ (toString value))
    )
    else return value
  ) |> get |> ignore;

  let toBeEqualTo (toString: 'a => string) =>
   toBeEqualToWith Equality.structural toString;

  let toBeEqualToSeqWith
      (equals: Equality.t 'a)
      (toString: 'a => string) =>
    toBeEqualToWith (Seq.equalsWith equals) (stringOfSeq toString);

  let toBeEqualToSeq (toString: 'a => string) =>
    toBeEqualToWith Seq.equals (stringOfSeq toString);

  let toBeEqualToSeqOfInt (seq: Seq.t int) (expect: t (Seq.t int)) =>
    toBeEqualToSeq string_of_int seq expect;

  let toBeEqualToSeqOfString (seq: Seq.t string) (expect: t (Seq.t string)) =>
    toBeEqualToSeq identity seq expect;

  let toBeEqualToEmptySeq (toString: 'a => string) =>
    toBeEqualToSeq toString Seq.empty;

  let toBeEqualToEmptySeqOfString = toBeEqualToEmptySeq identity;

  let toBeEqualToFalse = toBeEqualTo string_of_bool false;

  let toBeEqualToInt = toBeEqualTo string_of_int;

  let toBeEqualToNone (toString: 'a => string) =>
    toBeEqualTo (stringOfOption toString) None;

  let toBeEqualToNoneOfInt (expect: t (option int)) =>
    toBeEqualToNone string_of_int expect;

  let toBeEqualToNoneOfString (expect: t (option 'a)) =>
    toBeEqualToNone identity expect;

  let toBeEqualToSome (toString: 'a => string) (value: 'a) =>
    toBeEqualTo (stringOfOption toString) (Some value);

  let toBeEqualToSomeOfInt (value: int) => toBeEqualToSome string_of_int value;

  let toBeEqualToSomeOfString (value: string) => toBeEqualToSome identity value;

  let toBeEqualToString = toBeEqualTo identity;

  let toBeEqualToTrue = toBeEqualTo string_of_bool true;

  let throws (expect: t 'a) => expect
    |> forEach(fun _ => Pervasives.failwith "expected exception to be thrown");
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
    |> Test.toSeq
    |> Seq.map execute
    |> Seq.reduce
      (fun (total, success) result => (total + 1, success + result))
      (0, 0);

  Printf.printf "Executed %i tests. %i tests succeeded." total success;
  print_newline ();
};
