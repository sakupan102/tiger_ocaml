module Translate = Translate.Make

type venv = Env.env_entry Symbol.table
type tenv = Types.ty Symbol.table
type expty = { exp : Translate.exp; ty : Types.ty }

exception SemantError of (Tiger.pos option * string) list

let check_int ((ty : Types.ty), (pos : Tiger.pos)) : unit =
  match ty with
  | Types.Int -> ()
  | _ -> raise @@ SemantError [ (Some pos, "integer required") ]

let rec actual_type (typ, pos) : Types.ty =
  match typ with
  | Types.Name (_, ty_option) -> (
      match !ty_option with
      | Some ty -> actual_type (ty, pos)
      | None -> raise @@ SemantError [ (Some pos, "undefined type") ])
  | _ -> typ

let is_expected_type ((ty, pos), expected) : unit =
  if actual_type (ty, pos) = expected then ()
  else raise @@ SemantError [ (Some pos, "type does not match") ]

let trans_param ((tenv : tenv), (field : Absyn.field list)) :
    (Symbol.symbol * Types.ty) list =
  List.map
    (fun (name, typ, pos) ->
      match Symbol.look (tenv, typ) with
      | Some t -> (name, t)
      | None -> raise @@ SemantError [ (Some pos, "type does not defined") ])
    field

let rec trans_exp
    (((venv, tenv) : venv * tenv), (level : Translate.level), (exp : Absyn.exp))
    : expty =
  let rec trexp = function
    | Absyn.NilExp _ -> { exp = Translate.nil_exp (); ty = Types.Nil }
    | Absyn.IntExp (num, _) -> { exp = Translate.int_exp num; ty = Types.Int }
    | Absyn.OpExp (left, op, right, pos) -> (
        let { exp = left_exp; ty = left_type } = trexp left
        and { exp = right_exp; ty = right_type } = trexp right in
        match op with
        | Absyn.PlusOp | Absyn.MinusOp | Absyn.TimesOp | Absyn.DivideOp ->
            check_int (left_type, pos);
            check_int (right_type, pos);
            {
              exp = Translate.arith_exp (left_exp, op, right_exp);
              ty = Types.Int;
            }
        | Absyn.EqOp | Absyn.NeqOp | Absyn.LtOp | Absyn.LeOp | Absyn.GtOp | GeOp
          -> (
            match left_type with
            | Types.Int ->
                check_int (left_type, pos);
                check_int (right_type, pos);
                {
                  exp = Translate.compare_exp (left_exp, op, right_exp);
                  ty = Types.Nil;
                })
        (*TODO: 文字列の比較も実装する*))
    | Absyn.StringExp (str, _) ->
        { exp = Translate.string_exp str; ty = Types.String }
    | Absyn.CallExp (name, params, pos) -> (
        let check_param (formal : Types.ty) (param : Absyn.exp) =
          let { ty = param_type; exp = _ } = trexp param in
          is_expected_type ((param_type, pos), formal)
        in
        match Symbol.look (venv, name) with
        | Some (FunEntry { formals; result }) ->
            if List.length formals != List.length params then
              raise
              @@ SemantError [ (Some pos, "number of argument does not match") ]
            else
              for i = 0 to List.length formals do
                check_param (List.nth formals i) (List.nth params i)
              done;
            { exp = (); ty = result }
        | _ -> raise @@ SemantError [ (Some pos, "function does not defined") ])
    | Absyn.RecordExp (fields, typ, pos) -> (
        match Symbol.look (tenv, typ) with
        | Some (Types.Record (pairs, num)) ->
            let check_property ((key, value), (expected_key, expected_value)) :
                unit =
              let { ty = value_type; exp = _ } = trexp value in
              if key = expected_key then
                is_expected_type ((value_type, pos), expected_value)
              else
                raise @@ SemantError [ (Some pos, "property does not found") ]
            in
            if List.length fields != List.length pairs then
              raise
              @@ SemantError
                   [ (Some pos, "number of properties does not match") ]
            else
              for i = 0 to List.length fields do
                check_property (List.nth fields i, List.nth pairs i)
              done;
            { exp = (); ty = Types.Record (pairs, num) }
        | _ -> raise @@ SemantError [ (Some pos, "type does not defined") ])
    | Absyn.SeqExp exps ->
        let exp_results = List.map trexp exps in
        List.nth exp_results (List.length exp_results - 1)
    | Absyn.IfExp (cond, then_exp, Some else_exp, pos) ->
        let { exp = translated_cond; ty = result_ty } = trexp cond
        and { exp = translated_then; ty = true_ty } = trexp then_exp
        and { exp = translated_else; ty = false_ty } = trexp else_exp in
        check_int (result_ty, pos);
        is_expected_type ((true_ty, pos), false_ty);
        {
          exp =
            Translate.if_exp (translated_cond, translated_then, translated_else);
          ty = false_ty;
        }
    | Absyn.WhileExp (cond, body, pos) ->
        let { exp = cond_exp; ty = cond_ty } = trexp cond
        and { exp = body_exp; ty = _ } = trexp body in
        check_int (cond_ty, pos);
        { exp = Translate.while_exp (cond_exp, body_exp); ty = Types.Nil }
    | _ -> { exp = (); ty = Types.Nil }
  in
  trexp exp

and trans_dec
    (((venv, tenv) : venv * tenv), (level : Translate.level), (decs : Absyn.dec))
    : venv * tenv =
  match decs with
  | Absyn.VarDec (name, None, init, pos) ->
      let { ty; _ } = trans_exp ((venv, tenv), level, init) in
      let var_access = Translate.alloc_local level true in
      ( Symbol.enter (venv, name, Env.VarEntry { ty; pos; access = var_access }),
        tenv )
  | Absyn.VarDec (name, Some typ, init, pos) -> (
      let { ty; _ } = trans_exp ((venv, tenv), level, init) in
      match Symbol.look (tenv, typ) with
      | Some t ->
          is_expected_type ((ty, pos), t);
          let var_access = Translate.alloc_local level true in
          ( Symbol.enter
              (venv, name, Env.VarEntry { ty; pos; access = var_access }),
            tenv )
      | None -> raise @@ SemantError [ (Some pos, "integer required") ])
  | Absyn.TypeDec typedecs ->
      let tenv' = List.fold_left set_type_header tenv typedecs in
      let tenv'' = List.fold_left set_type_content tenv' typedecs in
      (venv, tenv'')
  | Absyn.FunctionDec function_decs ->
      let (venv', tenv), _ =
        List.fold_left set_function_header ((venv, tenv), level) function_decs
        (* TODO: 関数内の型検査を実装する *)
      in
      List.iter (trans_fun (venv, tenv)) function_decs;
      (venv', tenv)

and trans_ty ((tenv, ty) : tenv * Absyn.ty) : Types.ty =
  match ty with
  | Absyn.NameTy (typ, pos) -> (
      match Symbol.look (tenv, typ) with
      | Some t -> t
      | None -> raise @@ SemantError [ (Some pos, "undefined type") ])
  | Absyn.ArrayTy (typ, pos) -> (
      match Symbol.look (tenv, typ) with
      | Some t -> Types.Array (t, ref ())
      | None -> raise @@ SemantError [ (Some pos, "undefined type") ])
  | Absyn.RecordTy (fields, _) ->
      Types.Record (trans_param (tenv, fields), ref ())

and trans_var
    (((venv, tenv) : venv * tenv), (level : Translate.level), (var : Absyn.var))
    : expty =
  match var with
  | Absyn.SimpleVar (sym, pos) -> (
      match Symbol.look (venv, sym) with
      | None -> raise @@ SemantError [ (Some pos, "variable does not defined") ]
      | Some (Env.VarEntry { access; ty; pos }) ->
          { exp = Translate.simple_var (access, level); ty = Types.Nil }
      | _ -> raise @@ SemantError [ (Some pos, "variable does not defined") ])
  | Absyn.SubscriptVar (var, exp, pos) ->
      let { exp = index } = trans_exp ((venv, tenv), level, exp)
      and { exp = var } = trans_var ((venv, tenv), level, var) in
      { exp = Translate.subscript_var (var, index); ty = Types.Nil }

and set_type_content (tenv : tenv)
    ((symbol, ty, pos) : Symbol.symbol * Absyn.ty * Tiger.pos) =
  let typ = trans_ty (tenv, ty) in
  match Symbol.look (tenv, symbol) with
  | Some (Types.Name (_, type_ref)) ->
      type_ref := Some typ;
      tenv
  | _ -> raise @@ SemantError [ (Some pos, "types does not match") ]

and set_type_header (tenv : tenv)
    ((symbol, _, _) : Symbol.symbol * Absyn.ty * Tiger.pos) =
  Symbol.enter (tenv, symbol, Types.Name (symbol, ref None))

and trans_fun ((venv, tenv) : venv * tenv)
    ((sym, params, result, body, pos) : Absyn.fundec) : unit =
  let func = Symbol.look (venv, sym) in
  match func with
  | Some (Env.FunEntry { formals; result; label; level }) ->
      let var_access = Translate.formals level in
      let venv' =
        List.fold_left
          (fun tenv (((name, _, pos) : Absyn.field), access) ->
            match Symbol.look (venv, name) with
            | Some (Env.VarEntry { access; ty; pos }) ->
                Symbol.enter (venv, name, Env.VarEntry { access; ty; pos })
            | _ ->
                raise @@ SemantError [ (Some pos, "function does not found") ])
          venv
          (List.combine params var_access)
      in
      let exp = trans_exp ((venv', tenv), level, body) in
      is_expected_type ((result, pos), exp.ty)
  | _ -> raise @@ SemantError [ (Some pos, "function does not found") ]

and set_function_header
    (((venv, tenv) : venv * tenv), (level : Translate.level))
    ((sym, params, result, _, pos) : Absyn.fundec) :
    (venv * tenv) * Translate.level =
  let param_types =
    List.map
      (fun (_, type_symbol, pos) ->
        match Symbol.look (tenv, type_symbol) with
        | Some typ -> typ
        | None -> raise @@ SemantError [ (Some pos, "types does not match") ])
      params
  and result_ty =
    match result with
    | Some res -> ( match Symbol.look (tenv, res) with Some typ -> typ)
    | None -> Types.Name (sym, ref None)
  and fun_label = Temp.new_label () in
  let escapes =
    let rec build_escapes num =
      if num = 0 then [] else false :: build_escapes (num - 1)
    in
    build_escapes (List.length param_types)
  in
  let new_level = Translate.new_level level fun_label escapes in
  ( ( Symbol.enter
        ( venv,
          sym,
          Env.FunEntry
            {
              formals = param_types;
              result = result_ty;
              label = fun_label;
              level = new_level;
            } ),
      tenv ),
    level )
