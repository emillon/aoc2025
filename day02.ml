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

let repeats (d, min) n = n % d = 0 && n / d >= min

let is_invalid_id n =
  let mask =
    match () with
    | _ when n <= 99 -> 11, 1
    | _ when n <= 9999 -> 101, 10
    | _ when n <= 999999 -> 1001, 100
    | _ when n <= 99999999 -> 10001, 1000
    | _ when n <= 9999999999 -> 100001, 10000
    | _ -> assert false
  in
  repeats mask n
;;

let f_gen s ~f =
  parse s
  |> List.concat_map ~f:(fun (min, max) ->
    List.range min max ~stop:`inclusive |> List.filter ~f)
  |> Algo.sum
;;

let f1 = f_gen ~f:is_invalid_id

let is_invalid_id2 n =
  let masks =
    match () with
    | _ when n <= 99 -> [ 11, 1 ]
    | _ when n <= 999 -> [ 111, 1 ]
    | _ when n <= 9999 -> [ 1111, 1; 0101, 10 ]
    | _ when n <= 99999 -> [ 11111, 1 ]
    | _ when n <= 999999 -> [ 111111, 1; 010101, 10; 001001, 100 ]
    | _ when n <= 9999999 -> [ 1111111, 1 ]
    | _ when n <= 99999999 -> [ 11111111, 1; 01010101, 10; 00010001, 1000 ]
    | _ when n <= 999999999 -> [ 111111111, 1; 001001001, 100 ]
    | _ when n <= 9999999999 -> [ 1111111111, 1; 0101010101, 10; 0000100001, 10000 ]
    | _ -> assert false
  in
  List.exists masks ~f:(fun mask -> repeats mask n)
;;

let f2 = f_gen ~f:is_invalid_id2

let%expect_test _ =
  print_s [%message (f1 sample : int) (f2 sample : int)];
  [%expect {| (("f1 sample" 1227775554) ("f2 sample" 4174379265)) |}]
;;

let run () = Run.run ~f1 ~f2 Day02_input.data
