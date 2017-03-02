open Immutable;
open ReUnit;

ReUnit.run (SetPerfTester.test 5 500000);
print_newline ();

/*
[100000, 100000, 100000, 100000, 100000, 100000] |> List.toSeq |> Seq.forEach(fun i => {
  ReUnit.run (MapPerfTester.test i);
  print_newline ();
});*/

ReUnit.run (VectorPerfTester.test 5 10000000);
print_newline ();
