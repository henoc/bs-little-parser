# bs-little-parser

Little parser combinator for BuckleScript or Reason.

## Usage

```ml
open BsLittleParser.Parser

let input = BsLittleParser.Input.{text = "abcabc  abc"; index = 0; whitespace = " "}

let abc = stringParser "abc"

let () =
  input
  |> rep abc
  |> Js.log
```