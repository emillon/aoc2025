type op =
  | Add
  | Mul
[@@deriving sexp]

let parse1 =
  let open Parsing_util in
  let open Angstrom in
  let op = enum [ "+", Add; "*", Mul ] in
  let blank = take_while Char.is_whitespace in
  parse_using
    (let+ numbers = many1 (blank *> number)
     and+ ops = many1 (blank *> op)
     and+ _ = blank *> end_of_input in
     numbers, ops)
;;

let f1 s =
  let numbers, ops = parse1 s in
  let lines = List.chunks_of ~length:(List.length ops) numbers in
  let columns = List.transpose_exn lines in
  List.zip_exn columns ops
  |> List.map ~f:(fun (col, op) ->
    match op with
    | Add -> Algo.sum col
    | Mul -> Algo.product col)
  |> Algo.sum
;;

let parse2 s =
  String.split_lines s |> List.map ~f:String.to_list |> List.transpose_exn |> List.rev
;;

let flush_st (cur, res) =
  assert (List.is_empty cur);
  res
;;

let f2 s =
  parse2 s
  |> List.fold ~init:([], 0) ~f:(fun (cur, res) cs ->
    let cd = List.drop_last_exn cs in
    let last = List.last_exn cs in
    let stripped = String.of_list cd |> String.strip in
    if String.is_empty stripped
    then cur, res
    else (
      let n = Int.of_string stripped in
      match last, n with
      | ' ', n -> n :: cur, res
      | '+', n -> [], res + Algo.sum (n :: cur)
      | '*', n -> [], res + Algo.product (n :: cur)
      | _ -> raise_s [%message (last : char) (n : int)]))
  |> flush_st
;;

let sample =
  String.concat_lines
    [ "123 328  51 64 "; " 45 64  387 23 "; "  6 98  215 314"; "*   +   *   +  " ]
;;

let%expect_test _ =
  print_s [%message (f1 sample : int) (f2 sample : int)];
  [%expect {| (("f1 sample" 4277556) ("f2 sample" 3263827)) |}]
;;

let run () = Run.run ~f1 ~f2 Day06_input.data
