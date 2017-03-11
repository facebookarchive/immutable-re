let failEmpty () =>
  failwith "empty";

let failIfOutOfRange (count: int) (index: int) =>
  index < 0 ? failwith "Index must be greater than 0" :
  /* FIXME: When reason support string interpolation include the count */
  index >= count ? failwith "Index must be less than count" :
  ();

let noneIfIndexOutOfRange (count:int) (index: int) (f: int => 'a): (option 'a) =>
  index < 0 ? None:
  index >= count ? None :
  f index |> Option.return;

let failIf (msg: string) (condition: bool): unit =>
  condition ? failwith msg : ();
