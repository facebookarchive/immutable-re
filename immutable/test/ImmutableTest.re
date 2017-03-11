open ReUnit.Test;

ReUnit.run (describe "Immutable.re" [
  CopyOnWriteArrayTest.test,
  DequeTest.test,
  FunctionsTest.test,/*
  HashBiMapTest.test,
  HashMapTest.test,
  HashMultisetTest.test,*/
  HashSetTest.test,/*
  HashSetMultimapTest.test,
  IntMapTest.test,*/
  MapTest.test,
  OptionTest.test,/*
  SeqTest.test,*/
  SetTest.test,/*
  SortedMapTest.test,*/
  SortedSetTest.test,
  StackTest.test,/*
  StackMultimapTest.test,
  TableTest.test,*/
  TransientTest.test,
  VectorTest.test,
]);
