open Equality;
open Functions;
open Hash;
open Keyed;
open Option.Operators;
open Pair;
open Seq;

type indexed 'a = {
  count: int,
  rseq: (seq 'a),
  seq: (seq 'a),
  tryGet: int => (option 'a),
};

let count (indexed: indexed 'a): int => indexed.count;

let create
    count::(count: int)
    rseq::(rseq: seq 'a)
    seq::(seq: seq 'a)
    tryGet::(tryGet: int => (option 'a)): (indexed 'a) =>
  ({ count, rseq, seq, tryGet });

let empty: indexed 'a = {
  count: 0,
  rseq: Seq.empty,
  seq: Seq.empty,
  tryGet: alwaysNone,
};

let equalsWith (valueEquality: equality 'v) (that: indexed 'a) (this: indexed 'a): bool =>
  (this === that) ||
  ((this.count == that.count) && (Seq.equalsWith valueEquality this.seq that.seq));

let equals (that: indexed 'a) (this: indexed 'a): bool =>
  equalsWith Equality.structural that this;

let hashWith (hash: (hash 'a)) ({ seq }: indexed 'a): int =>
  seq |> Seq.hashWith hash;

let hash ({ seq }: indexed 'a): int => seq |> Seq.hash;

let map (f: 'a => 'b) ({ count, rseq, seq, tryGet }: indexed 'a): (indexed 'b) => create
  count::count
  rseq::(rseq |> Seq.map f)
  seq::(seq |> Seq.map f)
  tryGet::(fun a => a |> tryGet >>| f);

let mapWithIndex (f: int => 'a => 'b) ({ count, rseq, seq, tryGet }:indexed 'a): (indexed 'b) => create
  count::count
  rseq::(rseq |> Seq.mapWithIndex f)
  seq::(seq |> Seq.mapWithIndex f)
  tryGet::(fun a => a |> tryGet >>| (f a));

let range
    (startIndex: int)
    (newCount: option int)
    ({ count, rseq, seq, tryGet } as indexed: indexed 'a): (indexed 'a) => {
  let newCount = newCount |? count - startIndex;
  let endIndex = startIndex + newCount;

  startIndex == 0 && newCount == count ? indexed : {
    count: newCount,
    rseq: rseq |> Seq.skip (count - endIndex) |> Seq.take (endIndex - startIndex),
    seq: seq |> Seq.skip startIndex |> Seq.take newCount,
    tryGet: fun (index: int) => index + startIndex < endIndex
      ? tryGet (index + startIndex)
      : None,
  };
};

let reverse ({ count, rseq, seq, tryGet}: indexed 'a): (indexed 'a) => ({
  count,
  rseq: seq,
  seq: rseq,
  tryGet: fun (index: int) => tryGet (count - index - 1),
});

let toKeyed (indexed: indexed 'a): (keyed int 'a) => Keyed.create
  count::indexed.count
  seq::(indexed.seq |> Seq.mapWithIndex Pair.create)
  tryGet::indexed.tryGet;

let toSeq (indexed: indexed 'a): (seq 'a) => indexed.seq;

let toSeqReversed (indexed: indexed 'a): (seq 'a) => indexed.rseq;

let tryFirst ({ tryGet }: indexed 'a): option 'a => tryGet 0;

let tryGet (index: int) ({ tryGet }: indexed 'a): (option 'a) => tryGet index;

let tryLast ({ count, tryGet }: indexed 'a): option 'a => count > 0
  ? tryGet (count - 1)
  : None;
