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

Type `Input.t` has a whitespace which specify a character sequence should be ignored.

## Parsers

| parser | description |
|:-----|:------|
| str s | expect string |
| chr c | expect char |
| regex r | expect regex |
| p `<*>` q | execute q only if p succeeds |
| p `<|>` q | execute q only if p fails |
| p `<*` q | same with `<*>` but throw the result of q away |
| p `*>` q | same with `<*>` but throw the result of p away |
| p `>>` fnq | apply the result of p to fnq then execute the return parser |
| p `^^` fn | apply the result of p to fn |
| rep p | repeat p |
| rep1 p | repeat p at least once |
| opt p | execute p zero or once |
| andPred p | execute p without consuming the input |
| notPred p | succeeds only if p fails without consuming the input |

## License

MIT
