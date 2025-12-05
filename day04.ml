let parse s =
  Vec.parse_2d
    s
    ~init:(Set.empty (module Vec))
    ~f:(fun pos acc c ->
      match c with
      | '@' -> Set.add acc pos
      | '.' -> acc
      | _ -> raise_s [%message (c : char)])
;;

let reachable s =
  Set.filter s ~f:(fun pos ->
    List.count (Vec.neighbours8 pos) ~f:(fun npos -> Set.mem s npos) < 4)
;;

let f1 s = parse s |> reachable |> Set.length

let sample =
  String.concat_lines
    [ "..@@.@@@@."
    ; "@@@.@.@.@@"
    ; "@@@@@.@.@@"
    ; "@.@@@@..@."
    ; "@@.@@@@.@@"
    ; ".@@@@@@@.@"
    ; ".@.@.@.@@@"
    ; "@.@@@.@@@@"
    ; ".@@@@@@@@."
    ; "@.@.@@@.@."
    ]
;;

let reduce1 s = Set.diff s (reachable s)

let rec reduce s =
  let r = reduce1 s in
  if Set.equal r s then r else reduce r
;;

let f2 s =
  let start = parse s in
  let stop = reduce start in
  Set.length start - Set.length stop
;;

let%expect_test _ =
  print_s [%message (f1 sample : int) (f2 sample : int)];
  [%expect {| (("f1 sample" 13) ("f2 sample" 43)) |}]
;;

let run () = Run.run ~f1 ~f2 Day04_input.data
