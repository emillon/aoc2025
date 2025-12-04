let parse_line s =
  String.to_list s |> List.map ~f:(fun c -> Char.to_string c |> Int.of_string)
;;

let parse s = String.split_lines s |> List.map ~f:parse_line

let rec elts_and_rest = function
  | [] -> []
  | x :: xs -> (x, xs) :: elts_and_rest xs
;;

let all_joltages l =
  let open List.Let_syntax in
  let%bind d1, rest = elts_and_rest l in
  let%map d2 = rest in
  (10 * d1) + d2
;;

let best_joltage l =
  all_joltages l |> List.max_elt ~compare:Int.compare |> Option.value_exn
;;

module K = struct
  module T = struct
    type t =
      { digits : int
      ; list : int list
      }
    [@@deriving compare, sexp_of]
  end

  include T
  include Comparable.Make (T)
end

let best best_rec ~digits l =
  match digits, l with
  | 1, l -> List.max_elt l ~compare:Int.compare
  | _, [] -> None
  | _, x :: xs ->
    let pick =
      let open Option.Let_syntax in
      let%map low = best_rec ~digits:(digits - 1) xs in
      (x * Int.pow 10 (digits - 1)) + low
    in
    let dont_pick = best_rec ~digits xs in
    Option.merge ~f:Int.max pick dont_pick
;;

let best_joltage2 l =
  let cache = ref (Map.empty (module K)) in
  let rec best_cached ~digits l =
    let key = { K.digits; list = l } in
    match Map.find !cache key with
    | Some data -> data
    | None ->
      let data = best best_cached ~digits l in
      Ref.replace cache (Map.add_exn ~key ~data);
      data
  in
  best_cached ~digits:12 l |> Option.value_exn
;;

let sample =
  String.concat_lines
    [ "987654321111111"; "811111111111119"; "234234234234278"; "818181911112111" ]
;;

let%expect_test "best_joltage" =
  sample
  |> parse
  |> List.iter ~f:(fun l -> print_s [%message (l : int list) (best_joltage2 l : int)]);
  [%expect
    {|
    ((l (9 8 7 6 5 4 3 2 1 1 1 1 1 1 1)) ("best_joltage2 l" 987654321111))
    ((l (8 1 1 1 1 1 1 1 1 1 1 1 1 1 9)) ("best_joltage2 l" 811111111119))
    ((l (2 3 4 2 3 4 2 3 4 2 3 4 2 7 8)) ("best_joltage2 l" 434234234278))
    ((l (8 1 8 1 8 1 9 1 1 1 1 2 1 1 1)) ("best_joltage2 l" 888911112111))
    |}]
;;

let f1 s = parse s |> List.map ~f:best_joltage |> Algo.sum
let f2 s = parse s |> List.map ~f:best_joltage2 |> Algo.sum

let%expect_test _ =
  print_s [%message (parse sample : int list list) (f1 sample : int) (f2 sample : int)];
  [%expect
    {|
    (("parse sample"
      ((9 8 7 6 5 4 3 2 1 1 1 1 1 1 1) (8 1 1 1 1 1 1 1 1 1 1 1 1 1 9)
       (2 3 4 2 3 4 2 3 4 2 3 4 2 7 8) (8 1 8 1 8 1 9 1 1 1 1 2 1 1 1)))
     ("f1 sample" 357) ("f2 sample" 3121910778619))
    |}]
;;

let run () = Run.run ~f1 ~f2 Day03_input.data
