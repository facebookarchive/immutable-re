open Functions.Operators;
open Option.Operators;

type t 'row 'column 'value = {
  count: int,
  map: (HashMap.t 'row (HashMap.t 'column 'value)),
  columnStrategy: HashStrategy.t 'column,
};

let columns ({ map }: t 'row 'column 'value): (Seq.t 'column) =>
  map |> HashMap.values |> Seq.flatMap (HashMap.keys >> ImmSet.toSeq);

let contains (row: 'row) (column: 'column) (value: 'value) ({ map }: t 'row 'column 'value): bool =>
  map |> HashMap.tryGet row >>| (HashMap.contains column value) |? false;

let containsWith
    (valueEquals: Equality.t 'v)
    (row: 'row)
    (column: 'column)
    (value: 'value)
    ({ map }: t 'row 'column 'value): bool =>
  map |> HashMap.tryGet row >>| (HashMap.containsWith valueEquals column value) |? false;

let containsRow (row: 'row) ({ map }: t 'row 'column 'value): bool =>
  map |> HashMap.containsKey row;

let containsRowAndColumn (row: 'row) (column: 'column) ({ map }: t 'row 'column 'value): bool =>
  map |> HashMap.tryGet row >>| (HashMap.containsKey column) |? false;

let count ({ count }: t 'row 'column 'value): int => count;

let empty: (t 'row 'column 'value) = {
  count: 0,
  map: HashMap.empty,
  columnStrategy: HashStrategy.structuralCompare,
};

let emptyWith
    (rowStrategy: HashStrategy.t 'row)
    (columnStrategy: HashStrategy.t 'column): (t 'row 'column 'value) => {
  count: 0,
  map: HashMap.emptyWith rowStrategy,
  columnStrategy,
};

let equals (this: t 'row 'column 'value) (that: t 'row 'column 'value): bool =>
  HashMap.equalsWith HashMap.equals this.map that.map;

let equalsWith
    (valueEquals: Equality.t 'value)
    (this: t 'row 'column 'value)
    (that: t 'row 'column 'value): bool =>
  HashMap.equalsWith (HashMap.equalsWith valueEquals) this.map that.map;

let every (f: 'row => 'column => 'value => bool) ({ map }: t 'row 'column 'value): bool => {
  let f' row => HashMap.every (fun value column => f row value column);
  map |> HashMap.every f';
};

let forEach (f: 'row => 'column => 'value => unit) ({ map }: t 'row 'column 'value) => {
  let f' row => HashMap.forEach (fun value column => f row value column);
  map |> HashMap.forEach f';
};

let get (row: 'row) (column: 'column) ({ map }: t 'row 'column 'value): 'value =>
  map |> HashMap.get row |> HashMap.get column;

let hash ({ map }: t 'row 'column 'value): int =>
  map |> HashMap.hashWith HashMap.hash;

let hashWith (valueHash: Hash.t 'value) ({ map }: t 'row 'column 'value): int =>
  map |> HashMap.hashWith (HashMap.hashWith valueHash);

let isEmpty ({ map }: t 'row 'column 'value): bool =>
  map |> HashMap.isEmpty;

let isNotEmpty ({ map }: t 'row 'column 'value): bool =>
  map |> HashMap.isNotEmpty;

let keys ({ map }: t 'row 'column 'value): (ImmSet.t 'k) =>
  map |> HashMap.keys;

let map
    (f: 'row => 'column => 'a => 'b)
    ({ columnStrategy, count, map }: t 'row 'column 'a): (t 'row 'column 'b) => {
  let f' row map => map |> HashMap.map (fun col value => f row col value);
  {
    columnStrategy,
    count,
    map: map |> HashMap.map f',
  };
};

let none (f: 'row => 'column => 'value => bool) ({ map }: t 'row 'column 'value): bool => {
  let f' row => HashMap.none (fun value column => f row value column);
  map |> HashMap.every f';
};

let put
    (row: 'row)
    (column: 'column)
    (value: 'value)
    ({ count, map, columnStrategy }: t 'row 'column 'value): (t 'row 'column 'value) => {
  let oldColumns = map |> HashMap.tryGet row |? HashMap.emptyWith columnStrategy;
  let newColumns = oldColumns |> HashMap.put column value;
  {
    count: count + (newColumns|> HashMap.count) - (oldColumns |> HashMap.count),
    map: map |> HashMap.put row newColumns,
    columnStrategy,
  }
};

let reduce
    (f: 'acc => 'row => 'column => 'value => 'acc)
    (acc: 'acc)
    ({ map }: t 'row 'column 'value): 'acc => {
  let reducer acc row columns => {
    let columnReducer acc column value => f acc row column value;
    columns |> HashMap.reduce columnReducer acc;
  };

  map |> HashMap.reduce reducer acc
};

let remove
    (row: 'row)
    (column: 'column)
    ({ count, map, columnStrategy } as table: t 'row 'column 'value): (t 'row 'column 'value) =>
  (map |> HashMap.tryGet row >>= fun columns =>
  columns |> HashMap.tryGet column >>| fun _ =>
  ({
    count: count - 1,
    map: map |> HashMap.put row (columns |> HashMap.remove column),
    columnStrategy,
  })) |? table;

let removeAll ({ map, columnStrategy }: t 'row 'column 'value): (t 'row 'column 'value) =>
  { count: 0, map: map |> HashMap.removeAll, columnStrategy };

let removeRow
    (row: 'row)
    ({ count, map, columnStrategy } as table: t 'row 'column 'value): (t 'row 'column 'value) =>
  (map |> HashMap.tryGet row >>| fun columns => ({
    count: count - (columns |> HashMap.count),
    map: map |> HashMap.remove row,
    columnStrategy,
  })) |? table;

let rows ({ map }: t 'row 'column 'value): (ImmSet.t 'row) =>
  map |> HashMap.keys;

let some (f: 'row => 'column => 'value => bool) ({ map }: t 'row 'column 'value): bool => {
  let f' row => HashMap.some (fun value column => f row value column);
  map |> HashMap.some f';
};

let toSeq ({ map }: t 'row 'column 'value): (Seq.t ('row, 'column, 'value)) =>
  map |> HashMap.toSeq |> Seq.flatMap (
    fun (row, columnToValue) => columnToValue |> HashMap.toSeq |> Seq.map (
      fun (column, value) => (row, column, value)
    )
  );

let tryFind
    (f: 'row => 'column => 'value => bool)
    ({ map }: t 'row 'column 'value): (option ('row, 'column, 'value)) => {
  let result = ref None;

  let f' row map => map |> HashMap.tryFind (
    fun column value => if (f row column value) {
      result := Some (row, column, value);
      true
    } else false
  ) |> Option.isNotEmpty;

  map |> HashMap.tryFind f' |> ignore;
  !result
};

let find
    (f: 'row => 'column => 'value => bool)
    (table: t 'row 'column 'value): ('row, 'column, 'value) =>
  table |> tryFind f |> Option.first;

let tryGet (row: 'row) (column: 'column) ({ map }: t 'row 'column 'value): (option 'value) =>
  map |> (HashMap.tryGet row) >>= (HashMap.tryGet column);

let values ({ map }: t 'row 'column 'value): (Seq.t 'value) =>
  map |> HashMap.values |> Seq.flatMap HashMap.values;
