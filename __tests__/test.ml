open Parser
open Jest

let () =
  let open Expect in
  test "parse" (fun () ->
    let input = Input.{text = "abcdefg"; index = 0; whitespace = " "} in
    let abc = stringParser "abc" in
    let result = abc input in
    match ParseResult.getResult result with
    | Some x ->
      expect x |> toEqual "abc" |> ignore;
      expect (ParseResult.getIndex result) |> toBe 3
    | None -> assert false
  )
