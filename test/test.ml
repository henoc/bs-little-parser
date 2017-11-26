open Parser

let input = Input.{text = "abcabc  abc"; index = 0; whitespace = " "}

let ab = stringParser "ab"
let abc = stringParser "abc"

let () =
  input
  |> rep abc
  |> Js.log