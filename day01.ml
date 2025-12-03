type dir =
  | L
  | R
[@@deriving sexp]

type instr =
  { dir : dir
  ; steps : int
  }
[@@deriving sexp]

let parse_line s =
  let dir =
    match s.[0] with
    | 'L' -> L
    | 'R' -> R
    | c -> raise_s [%message "parse_dir" (c : char)]
  in
  let rest = String.subo ~pos:1 s in
  let steps = Int.of_string rest in
  { dir; steps }
;;

let sample =
  String.concat_lines
    [ "L68"; "L30"; "R48"; "L5"; "R60"; "L55"; "L1"; "L99"; "R14"; "L82" ]
;;

let parse s = String.split_lines s |> List.map ~f:parse_line

let f1 s =
  let pos = ref 50 in
  let zeroes = ref 0 in
  parse s
  |> List.iter ~f:(fun { dir; steps } ->
    (match dir with
     | L -> pos := (!pos - steps) % 100
     | R -> pos := (!pos + steps) % 100);
    if !pos = 0 then Int.incr zeroes);
  !zeroes
;;

let f2 s =
  let pos = ref 50 in
  let zeroes = ref 0 in
  parse s
  |> List.iter ~f:(fun { dir; steps } ->
    match dir with
    | L ->
      for _ = 1 to steps do
        pos := (!pos - 1) % 100;
        if !pos = 0 then Int.incr zeroes
      done
    | R ->
      for _ = 1 to steps do
        pos := (!pos + 1) % 100;
        if !pos = 0 then Int.incr zeroes
      done);
  !zeroes
;;

let%expect_test "f2" =
  f2 sample |> [%sexp_of: int] |> print_s;
  [%expect {| 6 |}]
;;

let run () = Run.run ~f1 ~f2 Day01_input.data
