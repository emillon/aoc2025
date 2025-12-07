let f1 s =
  let lines = String.split_lines s in
  let hd = List.hd_exn lines in
  let start =
    String.findi hd ~f:(fun _ c -> Char.equal c 'S') |> Option.value_exn |> fst
  in
  let splits = ref 0 in
  let _ : Set.M(Int).t =
    List.fold
      lines
      ~init:(Set.singleton (module Int) start)
      ~f:(fun tachyons line ->
        String.foldi line ~init:tachyons ~f:(fun i acc c ->
          match c, Set.mem tachyons i with
          | ('.' | '^'), false -> acc
          | ('.' | 'S'), true -> Set.add acc i
          | '^', true ->
            Int.incr splits;
            let acc = Set.add acc (i - 1) in
            let acc = Set.add acc (i + 1) in
            let acc = Set.remove acc i in
            acc
          | _ -> raise_s [%message (c : char)]))
  in
  !splits
;;

let f2 _ = 0

let sample =
  String.concat_lines
    [ ".......S......."
    ; "..............."
    ; ".......^......."
    ; "..............."
    ; "......^.^......"
    ; "..............."
    ; ".....^.^.^....."
    ; "..............."
    ; "....^.^...^...."
    ; "..............."
    ; "...^.^...^.^..."
    ; "..............."
    ; "..^...^.....^.."
    ; "..............."
    ; ".^.^.^.^.^...^."
    ; "..............."
    ]
;;

let%expect_test _ =
  print_s [%message (f1 sample : int)];
  [%expect {| ("f1 sample" 21) |}]
;;

let run () = Run.run ~f1 ~f2 Day07_input.data
