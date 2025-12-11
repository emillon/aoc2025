module G = Graph.Persistent.Digraph.Concrete (String)

let parse s =
  let open Parsing_util in
  let open Angstrom in
  parse_lines_using (both (word <* string ": ") (sep_by1 (char ' ') word)) s
  |> List.fold ~init:G.empty ~f:(fun acc (src, dsts) ->
    List.fold dsts ~init:acc ~f:(fun acc dst -> G.add_edge acc src dst))
;;

let ways g ~start ~stop =
  let dfs dfs_rec n seen =
    if String.equal n stop
    then 1
    else
      G.fold_succ
        (fun n' r -> if Set.mem seen n' then r else r + dfs_rec n' (Set.add seen n'))
        g
        n
        0
  in
  let cache = Hashtbl.create (module String) in
  let rec dfs_cached n seen =
    match Hashtbl.find cache n with
    | Some v -> v
    | None ->
      let v = dfs dfs_cached n seen in
      Hashtbl.set cache ~key:n ~data:v;
      v
  in
  dfs_cached start (Set.singleton (module String) start)
;;

let f1 s = parse s |> ways ~start:"you" ~stop:"out"

let ways2 g ~start ~c1 ~c2 ~stop =
  let sc1 = ways g ~start ~stop:c1 in
  let sc2 = ways g ~start ~stop:c2 in
  let c1s = ways g ~start:c1 ~stop in
  let c2s = ways g ~start:c2 ~stop in
  let c1c2 = ways g ~start:c1 ~stop:c2 in
  let c2c1 = ways g ~start:c2 ~stop:c1 in
  (sc1 * c1c2 * c2s) + (sc2 * c2c1 * c1s)
;;

let f2 s = parse s |> ways2 ~start:"svr" ~c1:"dac" ~c2:"fft" ~stop:"out"

let sample =
  String.concat_lines
    [ "aaa: you hhh"
    ; "you: bbb ccc"
    ; "bbb: ddd eee"
    ; "ccc: ddd eee fff"
    ; "ddd: ggg"
    ; "eee: out"
    ; "fff: out"
    ; "ggg: out"
    ; "hhh: ccc fff iii"
    ; "iii: out"
    ]
;;

let sample2 =
  String.concat_lines
    [ "svr: aaa bbb"
    ; "aaa: fft"
    ; "fft: ccc"
    ; "bbb: tty"
    ; "tty: ccc"
    ; "ccc: ddd eee"
    ; "ddd: hub"
    ; "hub: fff"
    ; "eee: dac"
    ; "dac: fff"
    ; "fff: ggg hhh"
    ; "ggg: out"
    ; "hhh: out"
    ]
;;

let%expect_test _ =
  print_s [%message (f1 sample : int) (f2 sample2 : int)];
  [%expect {| (("f1 sample" 5) ("f2 sample2" 2)) |}]
;;

let run () = Run.run ~f1 ~f2 Day11_input.data
