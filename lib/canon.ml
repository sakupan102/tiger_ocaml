module T = Tree

let ( % ) x y =
  match (x, y) with
  | T.EXP (T.CONST _), _ -> y
  | _, T.EXP (T.CONST _) -> x
  | _ -> T.SEQ (x, y)

let commute = function
  | T.EXP (T.CONST _), _ -> true
  | _, T.NAME _ -> true
  | _, T.CONST _ -> true
  | _ -> false

let rec reorder = function
  | exp :: rest ->
      let stms, exps = do_exp exp and stms', exps' = reorder rest in
      (T.SEQ (stms, stms'), exps :: exps')
  | [] -> (T.EXP (T.CONST 0), [])

and reorder_exp ((exps : T.exp list), build_fun) : T.stm * T.exp =
  let stm, exps = reorder exps in
  (stm, build_fun exps)

and reorder_stm ((exps : T.exp list), build_fun) : T.stm =
  let stm, exps' = reorder exps in
  T.SEQ (stm, build_fun exps')

and do_stm (stm : T.stm) =
  match stm with
  | T.JUMP (exp, labels) ->
      reorder_stm ([ exp ], fun [ exp ] -> T.JUMP (exp, labels))
  | T.CJUMP (cond, left_exp, right_exp, label1, label2) ->
      reorder_stm
        ( [ left_exp; right_exp ],
          fun [ a; b ] -> T.CJUMP (cond, a, b, label1, label2) )
  | T.SEQ (left_seq, right_seq) ->
      let left_seq' = do_stm left_seq and right_seq' = do_stm right_seq in
      T.SEQ (left_seq', right_seq')
  | T.MOVE (T.TEMP t, T.CALL (func, args)) ->
      reorder_stm
        ( func :: args,
          fun (func :: args) -> T.MOVE (T.TEMP t, T.CALL (func, args)) )
  | T.MOVE (location, value) ->
      reorder_stm ([ location; value ], fun [ a; b ] -> T.MOVE (a, b))
  | T.EXP (T.CALL (fn, args)) ->
      reorder_stm (fn :: args, fun (fn :: args) -> T.EXP (T.CALL (fn, args)))
  | T.EXP exp -> reorder_stm ([ exp ], fun [ a ] -> T.EXP a)
  | _ -> stm

and do_exp (exp : T.exp) =
  match exp with
  | T.BINOP (op, left_exp, right_exp) ->
      reorder_exp ([ right_exp; left_exp ], fun [ a; b ] -> T.BINOP (op, a, b))
  | T.MEM exp -> reorder_exp ([ exp ], fun [ a ] -> T.MEM a)
  | T.ESEQ (stm, exp) ->
      let stm' = do_stm stm and stm'', exp' = do_exp exp in
      (T.SEQ (stm', stm''), exp)
  | T.CALL (fn, args) ->
      reorder_exp (fn :: args, fun (fn :: args) -> T.CALL (fn, args))
  | _ -> (T.EXP (T.CONST 0), exp)

let linearize (stm0 : T.stm) : T.stm list =
  let rec linear = function
    | T.SEQ (stm, stms) -> stm :: linear stms
    | _ -> [ stm0 ]
  in
  linear (do_stm stm0)
