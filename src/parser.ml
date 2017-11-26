
type 'a t = Input.t -> 'a ParseResult.t

let parse (input: Input.t) (parser: 'a t) =
  parser input

let andThen p q = fun input ->
  match p input with
  | ParseResult.ParseSuccess (result1, input2) ->
    (match q input2 with
    | ParseResult.ParseSuccess (result2, input3) ->
        ParseResult.ParseSuccess ( (result1, result2), input3)
    | ParseResult.ParseFailure (message, input) -> ParseResult.ParseFailure (message, input))
  | ParseResult.ParseFailure (message, input) -> ParseResult.ParseFailure (message, input)

let orElse p q = fun input ->
  match p input with
  | ParseResult.ParseSuccess (s, t) -> ParseResult.ParseSuccess (s, t)
  | ParseResult.ParseFailure _ -> q input

let rep p = fun input ->
  let rec loop acc input = match p input with
  | ParseResult.ParseSuccess (r, i) -> loop (r :: acc) i
  | ParseResult.ParseFailure _ -> (List.rev acc, input)
  in
  let (r, i) = loop [] input in
  ParseResult.ParseSuccess (r, i)

let rep1 p = andThen p @@ rep p

let optional p = fun input -> match p input with
| ParseResult.ParseFailure _ -> ParseResult.ParseSuccess (None, input)
| ParseResult.ParseSuccess (r, i) -> ParseResult.ParseSuccess (Some r, i)

let andPred p = fun input -> match p input with
| ParseResult.ParseSuccess (r, _) -> ParseResult.ParseSuccess (r, input)
| others -> others

let notPred p = fun input -> match p input with
| ParseResult.ParseSuccess (_, i) -> ParseResult.ParseFailure ("notPred failure", i)
| ParseResult.ParseFailure _ -> ParseResult.ParseSuccess (None, input)

(** internal use only *)
let skipWhitespace (whitepspace: string) (input: Input.t) =
  let rec listChar str = match str with
    | "" -> []
    | str -> (String.get str 0 ) :: (listChar (String.sub str 1 ( (String.length str)-1) ) )
  in
  let spaceChars = listChar whitepspace in
  let rec contain chr charList = match charList with
  | [] -> false
  | c :: tl -> if chr == c then true else contain chr tl
  in
  let rec loop (input: Input.t) =
    if (String.length input.text <= input.index) then input 
    else if (contain input.text.[input.index] spaceChars) then loop {input with index=input.index+1}
    else input
  in
  loop input

let charParser c =
  fun (rawInput: Input.t) ->
    let input = skipWhitespace rawInput.whitespace rawInput in
    match (String.length input.text <= input.index) with
    | true -> ParseResult.ParseFailure ("no more length", input)
    | false ->
      let firstChar = input.text.[input.index] in
      match firstChar == c with
      | true -> ParseResult.ParseSuccess (c, {input with index=input.index + 1})
      | false -> ParseResult.ParseFailure
      (
        "different char '" ^ (Char.escaped firstChar) ^ "' found, expected: '" ^ (Char.escaped c) ^ "'",
        input
      )

let stringParser s =
  fun (rawInput: Input.t) ->
    let input = skipWhitespace rawInput.whitespace rawInput in
    match String.length input.text - input.index < String.length s with
    | true -> ParseResult.ParseFailure ("no more length", input)
    | false ->
      let substr = String.sub input.text input.index (String.length s) in
      match substr == s with
      | true -> ParseResult.ParseSuccess (s, {input with index=input.index + (String.length s)})
      | false -> ParseResult.ParseFailure
      (
        "remined text doesn't start with " ^ s,
        input
      )
