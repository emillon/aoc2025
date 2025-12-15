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
  { lights : bool list
  ; buttons : int list list
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
    (let+ lights = char '[' *> many1 light <* string "] "
     and+ buttons = sep_by1 (char ' ') button <* char ' '
     and+ joltages in
     { lights; buttons; joltages })
;;

let fewest_presses t =
  let q = Queue.create () in
  let win_mask = Mask.of_bits t.lights in
  let is_winning (m, _) = Mask.equal m win_mask in
  let initial_state = Mask.empty, 0 in
  let visited = ref (Set.empty (module Mask)) in
  let is_visited (m, _) = Set.mem !visited m in
  let mark_visited (m, _) = visited := Set.add !visited m in
  let button_masks = List.map ~f:Mask.of_bit_numbers t.buttons in
  let next (m, i) = List.map button_masks ~f:(fun button -> Mask.merge m button, i + 1) in
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

let z3_exprs ctx t =
  let module I = Z3.Arithmetic.Integer in
  let module B = Z3.Boolean in
  let module A = Z3.Arithmetic in
  let p =
    Array.init (List.length t.buttons) ~f:(fun i ->
      I.mk_const ctx (Z3.Symbol.mk_int ctx i))
  in
  let ( == ) = B.mk_eq ctx in
  let ( >= ) = A.mk_ge ctx in
  let int n = I.mk_numeral_i ctx n in
  let joltage_sum i =
    List.filter_mapi t.buttons ~f:(fun ibtn l ->
      Option.some_if (List.mem l i ~equal:Int.equal) p.(ibtn))
    |> A.mk_add ctx
  in
  let press_eqns = List.mapi t.joltages ~f:(fun i jolt -> joltage_sum i == int jolt) in
  let pos_eqns = Array.to_list p |> List.map ~f:(fun pi -> pi >= int 0) in
  let eqns = press_eqns @ pos_eqns in
  let all_presses = A.mk_add ctx (Array.to_list p) in
  eqns, all_presses
;;

let fewest_presses2 t =
  let cfg = [ "model", "true"; "proof", "false" ] in
  let ctx = Z3.mk_context cfg in
  let opt = Z3.Optimize.mk_opt ctx in
  let exprs, opt_expr = z3_exprs ctx t in
  Z3.Optimize.add opt exprs;
  let (_ : Z3.Optimize.handle) = Z3.Optimize.minimize opt opt_expr in
  let (_ : Z3.Solver.status) = Z3.Optimize.check opt in
  let model = Z3.Optimize.get_model opt |> Option.value_exn in
  Z3.Model.eval model opt_expr true
  |> Option.value_exn
  |> Z3.Expr.to_string
  |> Int.of_string
;;

let f2 s = parse s |> List.map ~f:fewest_presses2 |> Algo.sum

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
      (((lights (false true true false))
        (buttons ((3) (1 3) (2) (2 3) (0 2) (0 1))) (joltages (3 5 4 7)))
       ((lights (false false false true false))
        (buttons ((0 2 3 4) (2 3) (0 4) (0 1 2) (1 2 3 4)))
        (joltages (7 5 12 7 2)))
       ((lights (false true true true false true))
        (buttons ((0 1 2 3 4) (0 3 4) (0 1 2 4 5) (1 2)))
        (joltages (10 11 11 5 10 5)))))
     ("f1 sample" 7) ("f2 sample" 33))
    |}]
;;

let run () = Run.run ~f1 ~f2 Day10_input.data
