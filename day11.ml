type t = (string * string list) list [@@deriving sexp]

let parse =
  let open Parsing_util in
  let open Angstrom in
  parse_lines_using (both (word <* string ": ") (sep_by1 (char ' ') word))
;;

let f1 _ = 0
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
  print_s [%message (parse sample : t) (f1 sample : int) (f2 sample : int)];
  [%expect
    {|
    (("parse sample"
      ((aaa (you hhh)) (you (bbb ccc)) (bbb (ddd eee)) (ccc (ddd eee fff))
       (ddd (ggg)) (eee (out)) (fff (out)) (ggg (out)) (hhh (ccc fff iii))
       (iii (out))))
     ("f1 sample" 0) ("f2 sample" 0))
    |}]
;;

let run () = Run.run ~f1 ~f2 Day11_input.data
