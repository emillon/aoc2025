let parse =
  let open Parsing_util in
  let open Angstrom in
  parse_lines_using (both number (char ',' *> number))
;;

let area (x, y) =
  let side n = Int.abs n + 1 in
  side x * side y
;;

let pairs l =
  let open List.Let_syntax in
  let%bind a = l in
  let%bind b = l in
  let%map () = Algo.guard (Vec.compare a b > 0) in
  a, b
;;

let f1 s =
  let points = parse s in
  let sizes =
    let open List.Let_syntax in
    let%map a, b = pairs points in
    area (Vec.sub a b)
  in
  List.max_elt ~compare:Int.compare sizes |> Option.value_exn
;;

let rec inner_segments = function
  | a :: (b :: _ as t) -> (a, b) :: inner_segments t
  | [ _ ] -> []
  | [] -> assert false
;;

let segments l =
  let first = List.hd_exn l in
  let last = List.last_exn l in
  (last, first) :: inner_segments l
;;

type rectangle =
  { xmin : int
  ; xmax : int
  ; ymin : int
  ; ymax : int
  }
[@@deriving sexp]

let rectangle (ax, ay) (bx, by) =
  let xmin = Int.min ax bx in
  let xmax = Int.max ax bx in
  let ymin = Int.min ay by in
  let ymax = Int.max ay by in
  { xmin; xmax; ymin; ymax }
;;

let rectangle_area { xmin; xmax; ymin; ymax } = (xmax - xmin + 1) * (ymax - ymin + 1)

type position =
  | Before
  | Within
  | After
[@@deriving equal, sexp]

let position a (min, max) = if a <= min then Before else if a < max then Within else After

let intersects { xmin; xmax; ymin; ymax } ((ax, ay), (bx, by)) =
  let rx = xmin, xmax in
  let ry = ymin, ymax in
  let u, ru, v1, v2, rv =
    if ax = bx
    then ax, rx, ay, by, ry
    else if ay = by
    then ay, ry, ax, bx, rx
    else assert false
  in
  match position u ru with
  | Before | After -> false
  | Within ->
    let d1 = position v1 rv in
    let d2 = position v2 rv in
    if equal_position d1 d2 then false else true
;;

let f2 s =
  let points = parse s in
  let segments = segments points in
  pairs points
  |> List.filter_map ~f:(fun (a, b) ->
    let rect = rectangle a b in
    if List.exists segments ~f:(intersects rect) then None else Some (rectangle_area rect))
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn
;;

let sample =
  String.concat_lines [ "7,1"; "11,1"; "11,7"; "9,7"; "9,5"; "2,5"; "2,3"; "7,3" ]
;;

let%expect_test _ =
  print_s [%message (f1 sample : int) (f2 sample : int)];
  [%expect {| (("f1 sample" 50) ("f2 sample" 24)) |}]
;;

let run () = Run.run ~f1 ~f2 Day09_input.data
