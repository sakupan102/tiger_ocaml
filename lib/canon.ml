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

type block = T.stm list

let basic_blocks stms =
  let finish = Temp.new_label () in
  let rec split_blocks (stms : T.stm list) (blocks : T.stm list list) =
    match stms with
    | [] -> blocks
    | _ ->
        let rec next (current_stms : T.stm list) (current_block : T.stm list) =
          match current_stms with
          | (T.JUMP _ as t) :: rest -> end_block rest (t :: current_block)
          | (T.CJUMP _ as t) :: rest -> end_block rest (t :: current_block)
          | T.LABEL label :: _ as li ->
              end_block li (T.JUMP (T.NAME label, [ label ]) :: current_block)
          | s :: rest -> next rest (s :: current_block)
          | [] ->
              end_block [] (T.JUMP (T.NAME finish, [ finish ]) :: current_block)
        and end_block (current_stms : T.stm list) (current_block : T.stm list) =
          match current_stms with
          | T.LABEL _ :: _ ->
              split_blocks current_stms (List.rev current_block :: blocks)
          | _ ->
              split_blocks
                (T.LABEL (Temp.new_label ()) :: current_stms)
                (List.rev current_block :: blocks)
        in
        next stms []
  in
  (split_blocks stms [], finish)

let enter_block table block =
  match block with
  | T.LABEL label :: _ as block -> Symbol.enter (table, label, block)
  | _ -> table

let rec split_last = function
  | [ last ] -> ([], last)
  | head :: rest ->
      let heads, last = split_last rest in
      (head :: heads, last)

let rec trace
    ( (label_to_block : T.stm list Symbol.table),
      (T.LABEL label :: _ as current_block),
      rest_blocks ) =
  let label_to_block = Symbol.enter (label_to_block, label, []) in
  match split_last current_block with
  | body, T.JUMP (T.NAME next_label, _) -> (
      match Symbol.look (label_to_block, next_label) with
      (*組み込まれていないブロックがあればそれをトレースに追加*)
      (*JUMPの次に遷移先のラベルがあるのでJUMPは省略*)
      | Some (_ :: _ as next_block) ->
          body @ trace (label_to_block, next_block, rest_blocks)
      (*どこにも遷移先がない時は新たにトレースを開始*)
      | _ -> current_block @ next_trace label_to_block rest_blocks)
  | body, T.CJUMP (op, left_exp, right_exp, true_label, false_label) -> (
      match
        ( Symbol.look (label_to_block, true_label),
          Symbol.look (label_to_block, false_label) )
      with
      | _, Some (_ :: _ as next_block) ->
          body @ trace (label_to_block, next_block, rest_blocks)
      | Some (_ :: _ as next_block), _ ->
          body
          @ [ T.CJUMP (op, left_exp, right_exp, false_label, true_label) ]
          @ trace (label_to_block, next_block, rest_blocks)
      | _ ->
          let new_false_label = Temp.new_label () in
          body
          @ [
              T.CJUMP (op, left_exp, right_exp, new_false_label, true_label);
              T.LABEL new_false_label;
              T.JUMP (T.NAME new_false_label, [ new_false_label ]);
            ]
          @ next_trace label_to_block rest_blocks)
  | _ -> current_block @ next_trace label_to_block rest_blocks

and next_trace (label_to_block : T.stm list Symbol.table)
    (blocks : T.stm list list) =
  match blocks with
  | (T.LABEL label :: _) :: rest -> (
      match Symbol.look (label_to_block, label) with
      (*組み込まれていないブロックがあればそこからトレースを開始*)
      | Some (_ :: _ as block) ->
          trace (label_to_block, block, rest) (*すでにトレースに組み込まれているブロックは無視*)
      | _ -> next_trace label_to_block rest)
  | [] -> []
  | _ -> next_trace label_to_block blocks

let trace_schedule ((blocks : T.stm list list), finish) =
  let label_to_block = List.fold_left enter_block Symbol.empty blocks in
  next_trace label_to_block blocks @ [ T.LABEL finish ]
