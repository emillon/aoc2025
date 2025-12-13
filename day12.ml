type region =
  { width : int
  ; height : int
  ; nums : int list
  }
[@@deriving sexp]

let parse =
  let open Parsing_util in
  let open Angstrom in
  let shape_line = take_while1 (fun c -> not (Char.is_whitespace c)) in
  let shape =
    number *> char ':' *> end_of_line *> many1 (shape_line <* end_of_line) *> end_of_line
  in
  let region =
    let+ width = number <* char 'x'
    and+ height = number <* string ": "
    and+ nums = sep_by1 (char ' ') number <* end_of_line in
    { width; height; nums }
  in
  parse_using (many1 shape *> many1 region)
;;

let is_valid region =
  let shape_count = Algo.sum region.nums in
  region.width * region.height >= 9 * shape_count
;;

let f1 s = parse s |> List.count ~f:is_valid
let f2 _ = 0
let run () = Run.run ~f1 ~f2 Day12_input.data
