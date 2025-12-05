type t =
  { ranges : (int * int) list
  ; ingredients : int list
  }
[@@deriving sexp]

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
     { ranges; ingredients })
;;

let f1 s =
  let t = parse s in
  List.count t.ingredients ~f:(fun n ->
    List.exists t.ranges ~f:(fun (a, b) -> a <= n && n <= b))
;;

let f2 s =
  let t = parse s in
  List.fold t.ranges ~init:Diet.Int.empty ~f:(fun d (a, b) ->
    Diet.Int.add (Diet.Int.Interval.make a b) d)
  |> Diet.Int.cardinal
;;

let sample =
  String.concat_lines
    [ "3-5"; "10-14"; "16-20"; "12-18"; ""; "1"; "5"; "8"; "11"; "17"; "32" ]
;;

let%expect_test _ =
  print_s [%message (parse sample : t) (f1 sample : int) (f2 sample : int)];
  [%expect
    {|
    (("parse sample"
      ((ranges ((3 5) (10 14) (16 20) (12 18))) (ingredients (1 5 8 11 17 32))))
     ("f1 sample" 3) ("f2 sample" 14))
    |}]
;;

let run () = Run.run ~f1 ~f2 Day05_input.data
