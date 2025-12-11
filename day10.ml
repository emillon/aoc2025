module Mask = struct
  module T = struct
    type t = int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)

  let set_bit m i = Int.bit_or m (Int.shift_left 1 i)

  let of_bits l =
    assert (List.length l < 32);
    List.foldi l ~init:0 ~f:(fun i acc b -> if b then set_bit acc i else acc)
  ;;

  let of_bit_numbers l = List.fold l ~init:0 ~f:set_bit
  let empty = 0
  let merge = Int.bit_xor
end

type t =
  { lights : Mask.t
  ; buttons : Mask.t list
  ; joltages : int list
  }
[@@deriving sexp]

let parse =
  let open Parsing_util in
  let open Angstrom in
  let light = enum [ ".", false; "#", true ] in
  let button = char '(' *> sep_by1 (char ',') number <* char ')' in
  let joltages = char '{' *> sep_by1 (char ',') number <* char '}' in
  parse_lines_using
    (let+ light_bits = char '[' *> many1 light <* string "] "
     and+ button_bits = sep_by1 (char ' ') button <* char ' '
     and+ joltages in
     { lights = Mask.of_bits light_bits
     ; buttons = List.map ~f:Mask.of_bit_numbers button_bits
     ; joltages
     })
;;

let fewest_presses t =
  let q = Queue.create () in
  let is_winning (m, _) = Mask.equal m t.lights in
  let initial_state = Mask.empty, 0 in
  let visited = ref (Set.empty (module Mask)) in
  let is_visited (m, _) = Set.mem !visited m in
  let mark_visited (m, _) = visited := Set.add !visited m in
  let next (m, i) = List.map t.buttons ~f:(fun button -> Mask.merge m button, i + 1) in
  Queue.enqueue q initial_state;
  let exception Found of int in
  try
    while not (Queue.is_empty q) do
      let state = Queue.dequeue_exn q in
      if is_winning state
      then raise (Found (snd state))
      else if is_visited state
      then ()
      else (
        mark_visited state;
        Queue.enqueue_all q (next state))
    done;
    assert false
  with
  | Found n -> n
;;

let f1 s = parse s |> List.map ~f:fewest_presses |> Algo.sum
let f2 _ = 0

let sample =
  String.concat_lines
    [ "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
    ; "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
    ; "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
    ]
;;

let%expect_test _ =
  print_s [%message (parse sample : t list) (f1 sample : int) (f2 sample : int)];
  [%expect
    {|
    (("parse sample"
      (((lights 6) (buttons (8 10 4 12 5 3)) (joltages (3 5 4 7)))
       ((lights 8) (buttons (29 12 17 7 30)) (joltages (7 5 12 7 2)))
       ((lights 46) (buttons (31 25 55 6)) (joltages (10 11 11 5 10 5)))))
     ("f1 sample" 7) ("f2 sample" 0))
    |}]
;;

let run () = Run.run ~f1 ~f2 Day10_input.data
