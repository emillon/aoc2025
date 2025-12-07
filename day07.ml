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

let f2 s =
  let lines = String.split_lines s in
  let hd = List.hd_exn lines in
  let start =
    String.findi hd ~f:(fun _ c -> Char.equal c 'S') |> Option.value_exn |> fst
  in
  List.fold
    lines
    ~init:(Map.singleton (module Int) start 1)
    ~f:(fun tachyons line ->
      String.foldi
        line
        ~init:(Map.empty (module Int))
        ~f:(fun i acc c ->
          let add n = function
            | None -> n
            | Some m -> m + n
          in
          match c, Map.find tachyons i with
          | ('.' | '^'), None -> acc
          | ('.' | 'S'), Some n -> Map.update acc i ~f:(add n)
          | '^', Some n ->
            let acc = Map.update acc (i - 1) ~f:(add n) in
            let acc = Map.update acc (i + 1) ~f:(add n) in
            let acc = Map.remove acc i in
            acc
          | _ -> raise_s [%message (c : char)]))
  |> Map.fold ~init:0 ~f:(fun ~key:_ ~data total -> total + data)
;;

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
  print_s [%message (f1 sample : int) (f2 sample : int)];
  [%expect {| (("f1 sample" 21) ("f2 sample" 40)) |}]
;;

let run () = Run.run ~f1 ~f2 Day07_input.data
