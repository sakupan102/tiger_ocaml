open Tiger_ocaml
module T = Tree

let a = Temp.newTemp ()
let b = Temp.newTemp ()

let test_exp =
  T.BINOP (T.MUL, T.MEM (T.TEMP a), T.BINOP (T.PLUS, T.CONST 3, T.TEMP b))

let concat_temps (temps : Temp.temp list) : string =
  List.fold_left (fun res temp -> res ^ string_of_int temp) "" temps

let print_instruction (instr : Assem.instr) : unit =
  match instr with
  | OPER oper ->
      let dest_string = concat_temps oper.dst
      and src_string = concat_temps oper.src in
      print_string
        (oper.assem ^ "DEST: " ^ dest_string ^ " SRC: " ^ src_string ^ "\n")
  | LABEL label -> print_string label.assem
  | MOVE move ->
      let dest_string = string_of_int move.dst
      and src_string = string_of_int move.src in
      print_string
        (move.assem ^ "DEST: " ^ dest_string ^ " SRC: " ^ src_string ^ "\n")

let print_instructions (ilist : Assem.instr list) : unit =
  List.iter print_instruction ilist

let () =
  print_string (Printf.sprintf "a: %d, b: %d\n" a b);
  let _ = Codegen.munch_exp test_exp in
  let instruction_list = !Codegen.ilist in
  print_instructions instruction_list
