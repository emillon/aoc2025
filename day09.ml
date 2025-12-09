let parse =
  let open Parsing_util in
  let open Angstrom in
  parse_lines_using (both number (char ',' *> number))
;;

let area (x, y) =
  let side n = Int.abs n + 1 in
  side x * side y
;;

let f1 s =
  let points = parse s in
  let sizes =
    let open List.Let_syntax in
    let%bind a = points in
    let%bind b = points in
    let%map () = Algo.guard (Vec.compare a b > 0) in
    area (Vec.sub a b)
  in
  List.max_elt ~compare:Int.compare sizes |> Option.value_exn
;;

let f2 _ = 0

let sample =
  String.concat_lines [ "7,1"; "11,1"; "11,7"; "9,7"; "9,5"; "2,5"; "2,3"; "7,3" ]
;;

let%expect_test _ =
  print_s [%message (f1 sample : int) (f2 sample : int)];
  [%expect {| (("f1 sample" 50) ("f2 sample" 0)) |}]
;;

let run () = Run.run ~f1 ~f2 Day09_input.data
