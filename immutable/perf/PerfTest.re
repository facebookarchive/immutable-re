open Immutable;
open ReUnit;

ReUnit.run (SetPerfTester.test 5 500000);
print_newline ();

ReUnit.run (MapPerfTester.test 5 500000);
print_newline ();

ReUnit.run (VectorPerfTester.test 5 10000000);
print_newline ();
