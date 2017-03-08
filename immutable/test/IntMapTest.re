/*open Immutable;
open MapTester;
open ReUnit;
open ReUnit.Test;

let numOfElements = 10000;

let test = describe "IntMap" [
  describe "TransientIntMap" (
    MapTester.test
      (fun () => IntMap.empty |> IntMap.mutate)
      TransientIntMap.put
      TransientIntMap.remove
      TransientIntMap.tryGet
      numOfElements
  ),
  ...(
    MapTester.test
      (fun () => IntMap.empty)
      IntMap.put
      IntMap.remove
      IntMap.tryGet
      numOfElements
  )
];*/
