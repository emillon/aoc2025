let parse =
  let open Angstrom in
  let open Parsing_util in
  parse_using
    (let range =
       let+ a = number
       and+ _ = char '-'
       and+ b = number in
       a, b
     in
     let+ ranges = many1 (range <* end_of_line)
     and+ _ = end_of_line
     and+ ingredients = many1 (number <* end_of_line) in
     ranges, ingredients)
;;

let make_diet ranges =
  List.fold ranges ~init:Diet.Int.empty ~f:(fun d (a, b) ->
    let i = Diet.Int.Interval.make a b in
    Diet.Int.add i d)
;;

let f1 s =
  let ranges, ingredients = parse s in
  let d = make_diet ranges in
  List.count ingredients ~f:(fun n -> Diet.Int.mem n d)
;;

let f2 s = parse s |> fst |> make_diet |> Diet.Int.cardinal

let sample =
  String.concat_lines
    [ "3-5"; "10-14"; "16-20"; "12-18"; ""; "1"; "5"; "8"; "11"; "17"; "32" ]
;;

let%expect_test _ =
  print_s [%message (f1 sample : int) (f2 sample : int)];
  [%expect {| (("f1 sample" 3) ("f2 sample" 14)) |}]
;;

let run () = Run.run ~f1 ~f2 Day05_input.data
