module Vec3 = struct
  type t = int * int * int [@@deriving compare, equal, hash, sexp]

  let x (x, _, _) = x
  let diff (ax, ay, az) (bx, by, bz) = ax - bx, ay - by, az - bz
  let l2_norm_sq (x, y, z) = (x * x) + (y * y) + (z * z)
  let l2_dist_sq a b = l2_norm_sq (diff a b)
end

let parse =
  let open Parsing_util in
  let open Angstrom in
  parse_lines_using
    (let+ x = number
     and+ _ = char ','
     and+ y = number
     and+ _ = char ','
     and+ z = number in
     x, y, z)
;;

let pairs ~compare l =
  let open List.Let_syntax in
  let%bind x = l in
  let%bind y = l in
  let%map () = Algo.guard (compare x y < 0) in
  x, y
;;

module G = Graph.Persistent.Graph.Concrete (Vec3)
module C = Graph.Components.Make (G)

let sorted_pairs points =
  pairs ~compare:Vec3.compare points
  |> List.sort
       ~compare:(Comparable.lift Int.compare ~f:(fun (a, b) -> Vec3.l2_dist_sq a b))
;;

let f1 ?(n = 1000) s =
  let points = parse s in
  let sorted_pairs = sorted_pairs points in
  let top_pairs = List.take sorted_pairs n in
  let g = List.fold top_pairs ~init:G.empty ~f:(fun g (a, b) -> G.add_edge g a b) in
  let sccs = C.scc_list g in
  let scc_sizes =
    List.map sccs ~f:List.length |> List.sort ~compare:[%compare: int Comparable.reversed]
  in
  List.take scc_sizes 3 |> Algo.product
;;

let is_connected ~npoints g =
  let sccs = C.scc_array g in
  Array.length sccs = 1 && List.length sccs.(0) = npoints
;;

(* TODO: slow - could use an online algo for connectivity using union find *)
let f2 s =
  let points = parse s in
  let sorted_pairs = sorted_pairs points in
  let npoints = List.length points in
  List.fold_until
    sorted_pairs
    ~init:G.empty
    ~f:(fun g (a, b) ->
      let g' = G.add_edge g a b in
      if is_connected ~npoints g' then Stop (Vec3.x a * Vec3.x b) else Continue g')
    ~finish:(fun _ -> assert false)
;;

let sample =
  String.concat_lines
    [ "162,817,812"
    ; "57,618,57"
    ; "906,360,560"
    ; "592,479,940"
    ; "352,342,300"
    ; "466,668,158"
    ; "542,29,236"
    ; "431,825,988"
    ; "739,650,466"
    ; "52,470,668"
    ; "216,146,977"
    ; "819,987,18"
    ; "117,168,530"
    ; "805,96,715"
    ; "346,949,466"
    ; "970,615,88"
    ; "941,993,340"
    ; "862,61,35"
    ; "984,92,344"
    ; "425,690,689"
    ]
;;

let%expect_test _ =
  print_s [%message (f1 ~n:10 sample : int) (f2 sample : int)];
  [%expect {| (("f1 ~n:10 sample" 40) ("f2 sample" 25272)) |}]
;;

let run () = Run.run ~f1 ~f2 Day08_input.data
