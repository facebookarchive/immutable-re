open Collection;
open Equality;
open Keyed;
open HashMap;
open HashStrategy;
open Option.Operators;
open Pair;
open Seq;

type table 'row 'column 'value = {
  count: int,
  map: (hashMap 'row (hashMap 'column 'value)),
  columnStrategy: hashStrategy 'column,
};

let count ({ count }: table 'row 'column 'value): int => count;

let empty (): (table 'row 'column 'value) => ({
  count: 0,
  map: HashMap.empty (),
  columnStrategy: HashStrategy.structuralCompare (),
});

let emptyWith
    (rowStrategy: hashStrategy 'row)
    (columnStrategy: hashStrategy 'column): (table 'row 'column 'value) => ({
  count: 0,
  map: HashMap.emptyWith rowStrategy,
  columnStrategy,
});

let put
    (row: 'row)
    (column: 'column)
    (value: 'value)
    ({ count, map, columnStrategy }: table 'row 'column 'value): (table 'row 'column 'value) => {
  let oldColumns = map |> HashMap.tryGet row |? HashMap.emptyWith columnStrategy;
  let newColumns = oldColumns |> HashMap.put column value;
  {
    count: count + (newColumns|> HashMap.count) - (oldColumns |> HashMap.count),
    map: map |> HashMap.put row newColumns,
    columnStrategy,
  }
};

let reduce
    (f: 'acc => 'value => 'acc)
    (acc: 'acc)
    ({ map }: table 'row 'column 'value): 'acc => {
  let reducer acc columns =>
    columns |> HashMap.reduce f acc;

  map |> HashMap.reduce reducer acc
};

let reduceWithRowAndColumn
    (f: 'acc => 'row => 'column => 'value => 'acc)
    (acc: 'acc)
    ({ map }: table 'row 'column 'value): 'acc => {
  let reducer acc row columns => {
    let columnReducer acc column value => f acc row column value;
    columns |> HashMap.reduceWithKey columnReducer acc;
  };

  map |> HashMap.reduceWithKey reducer acc
};

let remove
    (row: 'row)
    (column: 'column)
    ({ count, map, columnStrategy } as table: table 'row 'column 'value): (table 'row 'column 'value) =>
  (map |> HashMap.tryGet row >>= fun columns =>
  columns |> HashMap.tryGet column >>| fun value =>
  ({
    count: count - 1,
    map: map |> HashMap.put row (columns |> HashMap.remove column),
    columnStrategy,
  })) |? table;

let remove
    (row: 'row)
    (column: 'column)
    ({ count, map, columnStrategy } as table: table 'row 'column 'value): (table 'row 'column 'value) =>
  (map |> HashMap.tryGet row >>| fun oldColumns => {
    let newColumns = oldColumns |> HashMap.remove column;
    {
      count: count + (newColumns |> HashMap.count) - (oldColumns |> HashMap.count),
      map: map |> HashMap.put row newColumns,
      columnStrategy,
    }
  }) |? table;

let removeAll ({ map, columnStrategy }: table 'row 'column 'value): (table 'row 'column 'value) =>
  { count: 0, map: map |> HashMap.removeAll, columnStrategy };

let removeRow
    (row: 'row)
    ({ count, map, columnStrategy } as table: table 'row 'column 'value): (table 'row 'column 'value) =>
  (map |> HashMap.tryGet row >>| fun columns => ({
    count: count - (columns |> HashMap.count),
    map: map |> HashMap.remove row,
    columnStrategy,
  })) |? table;

let toKeyed ({ count, map }: table 'row 'column 'value): (keyed 'row (keyed 'column 'value)) => Keyed.create
  count::(map |> HashMap.count)
  seq::(map |> HashMap.toSeq |> Seq.map (Pair.mapSnd HashMap.toKeyed))
  tryGet::(fun k => map |> HashMap.tryGet k >>| HashMap.toKeyed);

let toSeq ({ map }: table 'row 'column 'value): (seq ('row, 'column, 'value)) =>
  map |> HashMap.toSeq |> Seq.flatMap (
    fun (row, columnToValue) => columnToValue |> HashMap.toSeq |> Seq.map (
      fun (column, value) => (row, column, value)
    )
  );

let tryGet (row: 'row) (column: 'column) ({ map }: table 'row 'column 'value): (option 'value) =>
  map |> (HashMap.tryGet row) >>= (HashMap.tryGet column);
