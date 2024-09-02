module Translate = Translate.Make

type ty_entry = Types.ty
type access = Translate.access

let base_tenv : ty_entry Symbol.table =
  let tenv = Symbol.empty in
  let tenv' = Symbol.enter (tenv, Symbol.create "int", Types.Int) in
  let tenv'' = Symbol.enter (tenv', Symbol.create "string", Types.String) in
  tenv''

(* Base value environment: *)
type env_entry =
  | VarEntry of { access : access; ty : ty_entry; pos : Tiger.pos }
  | FunEntry of {
      formals : ty_entry list;
      result : ty_entry;
      label : Temp.label;
      level : Translate.level;
    }

let base_venv : env_entry Symbol.table = Symbol.empty
