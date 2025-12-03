type t = (int * int) list [@@deriving sexp]

let parse s =
  s
  |> String.strip
  |> String.split ~on:','
  |> List.map ~f:(fun part ->
    match String.split part ~on:'-' with
    | [ a; b ] -> Int.of_string a, Int.of_string b
    | l -> raise_s [%message (l : string list)])
;;

let sample =
  "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
;;

let%expect_test _ =
  print_s [%message (parse sample : t)];
  [%expect
    {|
    ("parse sample"
     ((11 22) (95 115) (998 1012) (1188511880 1188511890) (222220 222224)
      (1698522 1698528) (446443 446449) (38593856 38593862) (565653 565659)
      (824824821 824824827) (2121212118 2121212124)))
    |}]
;;

let is_invalid_id n =
  (n < 100 && n % 11 = 0)
  || (n < 10000 && n % 101 = 0 && n / 101 >= 10)
  || (n < 1000000 && n % 1001 = 0 && n / 1001 >= 100)
  || (n < 100000000 && n % 10001 = 0 && n / 10001 >= 1000)
  || (n < 10000000000 && n % 100001 = 0 && n / 100001 >= 10000)
  || (n < 1000000000000 && n % 1000001 = 0 && n / 1000001 >= 100000)
;;

let f1 s =
  parse s
  |> List.concat_map ~f:(fun (min, max) ->
    List.range min max ~stop:`inclusive |> List.filter ~f:is_invalid_id)
  |> Algo.sum
;;

let%expect_test _ =
  print_s [%message (f1 sample : int)];
  [%expect {| ("f1 sample" 1227775554) |}]
;;

let f2 _ = 0
let run () = Run.run ~f1 ~f2 Day02_input.data
