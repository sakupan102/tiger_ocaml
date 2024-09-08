module type T = sig
  type level
  type access
  type exp

  val unEx : exp -> Tree.exp
  val unNx : exp -> Tree.stm
  val unCx : exp -> Temp.label * Temp.label -> Tree.stm
  val outermost : level
  val new_level : level -> Temp.label -> bool list -> level
  val formals : level -> access list
  val alloc_local : level -> bool -> access
  val simple_var : access * level -> exp
end

module Make = struct
  type level = { prev : level option; frame : Frame.frame; uniq : unit ref }
  type access = level * Frame.access

  exception TranslateError of (Tiger.pos option * string) list

  type exp =
    | Ex of Tree.exp
    | Nx of Tree.stm
    | Cx of (Temp.label * Temp.label -> Tree.stm)

  let unEx = function
    | Ex exp -> exp
    | Cx genstm ->
        let result = Temp.newTemp ()
        and true_label = Temp.new_label ()
        and false_label = Temp.new_label () in
        Tree.ESEQ
          ( Tree.seq
              [
                Tree.MOVE (Tree.TEMP result, Tree.CONST 1);
                genstm (true_label, false_label);
                Tree.LABEL false_label;
                Tree.MOVE (Tree.TEMP result, Tree.CONST 0);
                Tree.LABEL true_label;
              ],
            Tree.TEMP result )
    | Nx exp -> Tree.ESEQ (exp, Tree.CONST 0)

  let unCx = function
    | Cx genstm -> genstm
    | Ex exp ->
        fun (true_label, false_label) ->
          Tree.CJUMP (Tree.EQ, exp, Tree.CONST 0, true_label, false_label)
    | Nx _ ->
        failwith
          "Impossible condition in unCx. Should not happen in well typed \
           programs."

  let unNx = function Nx stm -> stm | Ex exp -> Tree.EXP exp
  let int_exp (num : int) = Ex (Tree.CONST num)
  let nil_exp () = Ex (Tree.CONST 0)

  let string_exp (str : string) =
    let new_label = Temp.new_label () in
    (* TODO: 文字列を配置するFrame.Strigを実装する *)
    Ex (Tree.NAME new_label)

  let arith_exp (left_exp, op, right_exp) =
    match op with
    | Absyn.PlusOp -> Ex (Tree.BINOP (Tree.PLUS, unEx left_exp, unEx right_exp))
    | Absyn.MinusOp ->
        Ex (Tree.BINOP (Tree.MINUS, unEx left_exp, unEx right_exp))
    | Absyn.TimesOp -> Ex (Tree.BINOP (Tree.MUL, unEx left_exp, unEx right_exp))
    | Absyn.DivideOp ->
        Ex (Tree.BINOP (Tree.DIV, unEx left_exp, unEx right_exp))

  let compare_exp (left_exp, op, right_exp) =
    let convert_op (op : Absyn.oper) : Tree.relop =
      match op with
      | Absyn.EqOp -> Tree.EQ
      | Absyn.NeqOp -> Tree.NE
      | Absyn.GeOp -> Tree.GE
      | Absyn.GtOp -> Tree.GT
      | Absyn.LeOp -> Tree.LE
      | Absyn.LtOp -> Tree.LT
    in
    Cx
      (fun (true_label, false_label) ->
        Tree.CJUMP
          (convert_op op, unEx left_exp, unEx right_exp, true_label, false_label))

  let simple_var (((var_level, access), level) : access * level) : exp =
    let rec static_link_path frame_pointer_pos level =
      if var_level.uniq == level.uniq then
        Ex (Frame.exp access frame_pointer_pos)
      else
        match level.prev with
        | None -> raise @@ TranslateError [ (None, "variation not found") ]
        | Some prev_level ->
            let static_link_access = List.hd level.frame.formals in
            let next_frame_pointer =
              Frame.exp static_link_access frame_pointer_pos
            in
            static_link_path next_frame_pointer prev_level
    in
    static_link_path (Tree.TEMP Frame.fp) level

  let subscript_var (var, index) =
    Ex
      (Tree.MEM
         (Tree.BINOP
            ( Tree.PLUS,
              unEx var,
              Tree.BINOP (Tree.MUL, unEx index, Tree.CONST Frame.wordsize) )))

  let outermost : level =
    {
      prev = None;
      frame = Frame.new_frame (Temp.new_label ()) [];
      uniq = ref ();
    }

  let new_level prev_level new_label escapes =
    let formals = true :: escapes in
    let new_frame = Frame.new_frame new_label formals in
    { prev = Some prev_level; frame = new_frame; uniq = ref () }

  let alloc_local level escape =
    let access = Frame.alloc_local level.frame escape in
    (level, access)

  let formals (lev : level) : access list =
    let frmls = lev.frame.formals in
    (* TODO - If some functions don't need the static link update this *)
    (* Need to remove the one we added in newLevel *)
    List.tl (List.map (fun frml -> (lev, frml)) frmls)
end
