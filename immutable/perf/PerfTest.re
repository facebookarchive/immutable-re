open Immutable;
open ReUnit;

[100000, 100000, 100000, 100000, 100000, 100000] |> List.toSeq |> Seq.forEach(fun i => {
  ReUnit.run (SetPerfTester.test i);
  print_newline ();
});
/*
[100000, 100000, 100000, 100000, 100000, 100000] |> List.toSeq |> Seq.forEach(fun i => {
  ReUnit.run (MapPerfTester.test i);
  print_newline ();
});*/

[1000000, 1000000, 1000000, 1000000, 1000000, 1000000, 1000000] |> List.toSeq |> Seq.forEach(fun i => {
  ReUnit.run (VectorPerfTester.test i);
  print_newline ();
});
