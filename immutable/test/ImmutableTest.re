open ReUnit;
open ReUnit.Test;

ReUnit.run (describe "Immutable.re" [
/*  BiMapTest.test,*/
  CollectionTest.test,
  CopyOnWriteArrayTest.test,
  DequeTest.test,
  FunctionsTest.test,/*
  HashMapTest.test,
  HashMultisetTest.test,
  HashSetTest.test,
  HashSetMultimapTest.test,
  IntMapTest.test,*/
  KeyedTest.test,
  OptionTest.test,/*
  SeqTest.test,
  SortedMapTest.test,*/
  SortedSetTest.test,
  StackTest.test,/*
  StackMultimapTest.test,
  TableTest.test,*/
  TransientTest.test,
  VectorTest.test,
]);
