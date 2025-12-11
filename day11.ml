module G = Graph.Persistent.Digraph.Concrete (String)

let parse s =
  let open Parsing_util in
  let open Angstrom in
  parse_lines_using (both (word <* string ": ") (sep_by1 (char ' ') word)) s
  |> List.fold ~init:G.empty ~f:(fun acc (src, dsts) ->
    List.fold dsts ~init:acc ~f:(fun acc dst -> G.add_edge acc src dst))
;;

let ways g ~start ~stop =
  let q = Queue.create () in
  Queue.enqueue q (start, Set.singleton (module String) start);
  let r = ref 0 in
  while not (Queue.is_empty q) do
    let n, seen = Queue.dequeue_exn q in
    if String.equal n stop
    then Int.incr r
    else
      G.succ g n
      |> List.filter_map ~f:(fun n' ->
        if Set.mem seen n' then None else Some (n', Set.add seen n'))
      |> Queue.enqueue_all q
  done;
  !r
;;

let f1 s = parse s |> ways ~start:"you" ~stop:"out"
let f2 _ = 0

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

let%expect_test _ =
  print_s [%message (f1 sample : int) (f2 sample : int)];
  [%expect {| (("f1 sample" 5) ("f2 sample" 0)) |}]
;;

let run () = Run.run ~f1 ~f2 Day11_input.data
