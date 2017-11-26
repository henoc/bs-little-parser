type 'a t =
  ParseSuccess of 'a * Input.t
  | ParseFailure of string * Input.t

let getResult parseResult =
  match parseResult with
  | ParseSuccess (p, _) -> Some p
  | ParseFailure (_, _) -> None

let getIndex parseResult =
  match parseResult with
  | ParseSuccess (_, i) -> i.index
  | ParseFailure (_, i) -> i.index

let map f parseResult =
  match parseResult with
  | ParseSuccess (p, q)-> ParseSuccess ((f p), q)
  | ParseFailure (x, y) -> ParseFailure (x, y)

