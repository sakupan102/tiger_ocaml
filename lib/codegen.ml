module A = Assem
module S = Symbol
module T = Tree

let ilist : A.instr list ref = ref []

let op_to_assem : T.relop -> string = function
  | T.EQ -> "je"
  | T.NE -> "jne"
  | T.LT -> "jl"
  | T.LE -> "jle"
  | T.GT -> "jg"
  | T.GE -> "jge"

let binop_to_assem : T.binop -> string = function
  | T.PLUS -> "add"
  | T.MINUS -> "sub"
  | T.MUL -> "immul"
  | T.DIV -> "idiv"
  | T.AND -> "and"
  | T.OR -> "or"
  | T.XOR -> "xor"
  | T.LSHIFT -> "shl"
  | T.RSHIFT -> "shr"
  | T.ARSHIFT -> failwith "Compiler - unhandled munchStm case"

let result (gen : Temp.temp -> unit) : Temp.temp =
  let t = Temp.newTemp () in
  gen t;
  t

let emit x = ilist := x :: !ilist

let rec munch_stm : T.stm -> unit = function
  | T.MOVE (T.MEM e1, e2) ->
      emit
        (A.MOVE
           { assem = "mov [`d0], `s0"; dst = munch_exp e1; src = munch_exp e2 })
  | T.MOVE (e1, T.MEM e2) ->
      emit
        (A.MOVE
           { assem = "mov `d0, [`s0]"; dst = munch_exp e1; src = munch_exp e2 })
  | T.MOVE (e1, e2) ->
      emit
        (A.MOVE
           { assem = "mov `d0, s0"; dst = munch_exp e1; src = munch_exp e2 })
  | T.CJUMP (op, T.TEMP e1, T.TEMP e2, true_label, false_label) ->
      emit
        (A.OPER
           { assem = "cmp `s0, `s1"; dst = []; src = [ e1; e2 ]; jump = None });
      emit
        (A.OPER
           {
             assem = op_to_assem op ^ " " ^ Symbol.name true_label;
             dst = [];
             src = [];
             jump = Some [ true_label; false_label ];
           })
  | T.CJUMP (op, T.TEMP e1, T.MEM e2, true_label, false_label) ->
      emit
        (A.OPER
           {
             assem = "cmp `s0, [`s1]";
             dst = [];
             src = [ e1; munch_exp e2 ];
             jump = None;
           });
      emit
        (A.OPER
           {
             assem = op_to_assem op ^ " " ^ Symbol.name true_label;
             dst = [];
             src = [];
             jump = Some [ true_label; false_label ];
           })
  | T.CJUMP (op, T.MEM e1, T.TEMP e2, true_label, false_label) ->
      emit
        (A.OPER
           {
             assem = "cmp [`s0], `s1";
             dst = [];
             src = [ munch_exp e1; e2 ];
             jump = None;
           });
      emit
        (A.OPER
           {
             assem = op_to_assem op ^ " " ^ Symbol.name true_label;
             dst = [];
             src = [];
             jump = Some [ true_label; false_label ];
           })
  | T.CJUMP (op, T.TEMP e1, T.CONST e2, true_label, false_label) ->
      emit
        (A.OPER
           {
             assem = "cmp s0 " ^ string_of_int e2;
             dst = [];
             src = [ e1 ];
             jump = None;
           });
      emit
        (A.OPER
           {
             assem = op_to_assem op ^ " " ^ Symbol.name true_label;
             dst = [];
             src = [];
             jump = Some [ true_label; false_label ];
           })
  | T.CJUMP (op, e1, e2, true_label, false_label) ->
      emit
        (A.OPER
           {
             assem = "cmp s0, s1";
             dst = [];
             src = [ munch_exp e1; munch_exp e2 ];
             jump = None;
           });
      emit
        (A.OPER
           {
             assem = op_to_assem op ^ " " ^ Symbol.name true_label;
             dst = [];
             src = [];
             jump = Some [ true_label; false_label ];
           })
  | T.JUMP (T.NAME to_label, _) ->
      emit
        (A.OPER
           {
             assem = "jump " ^ Symbol.name to_label;
             dst = [];
             src = [];
             jump = Some [ to_label ];
           })
  | T.LABEL label -> emit (A.LABEL { assem = Symbol.name label; lab = label })
  | T.EXP exp ->
      let e = munch_exp exp in
      emit (A.MOVE { assem = "mov `d0, `s0"; src = e; dst = e })
  | exp ->
      Print_tree.print exp;
      failwith "Compiler - unhandled munchStm case"

and munch_exp : Tree.exp -> Temp.temp = function
  | T.TEMP t -> t
  | T.MEM location ->
      result (fun t ->
          emit
            (A.MOVE
               { assem = "mov `d0, [`s0]"; src = munch_exp location; dst = t }))
  | T.BINOP (op, left_exp, right_exp) ->
      result (fun t ->
          let left_temp = munch_exp left_exp in
          emit (A.MOVE { assem = "mov `d0, `s0"; src = left_temp; dst = t });
          emit
            (A.OPER
               {
                 assem = binop_to_assem op ^ " `d0, `s0";
                 src = [ munch_exp right_exp ];
                 dst = [ t ];
                 jump = None;
               }))
  | T.CONST i ->
      result (fun t ->
          emit
            (A.OPER
               {
                 assem = "mov `d0, " ^ string_of_int i;
                 dst = [ t ];
                 src = [];
                 jump = None;
               }))
  | T.NAME label ->
      result (fun t ->
          emit
            (A.OPER
               {
                 assem = "mov `d0, " ^ Symbol.name label;
                 dst = [ t ];
                 src = [];
                 jump = None;
               }))
  | exp ->
      Print_tree.print (T.EXP exp);
      failwith "Compiler - unhandled munchExp case"
