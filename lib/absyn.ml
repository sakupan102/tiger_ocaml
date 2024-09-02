type pos = Tiger.pos
and symbol = Symbol.symbol

type var =
  | SimpleVar of symbol * pos
  | FieldVar of var * symbol * pos * pos
  | SubscriptVar of var * exp * pos

and exp =
  | VarExp of var
  | NilExp
  | IntExp of int
  | StringExp of string * pos (* func    params *)
  | CallExp of symbol * exp list * pos (* left    op     right *)
  | OpExp of exp * oper * exp * pos (* fields               type *)
  | RecordExp of ((symbol * exp) list * symbol * pos)
  | SeqExp of exp list
  | AssignExp of (var * exp)
(*
           | IfExp of ('a exp * 'a exp * 'a exp option) * 'a
           | WhileExp of ('a exp * 'a exp) * 'a
                       (* var    escape      lo       hi       body *)
           | ForExp of (symbol * bool ref * 'a exp * 'a exp * 'a exp) * 'a
           | BreakExp of unit * 'a
                       (* decs      body*)
           | LetExp of ('a dec list * 'a exp list) * 'a
                         (* type    size      init *)
           | ArrayExp of (symbol * 'a exp * 'a exp) * 'a
*)

and dec =
  | FunctionDec of fundec list (* name  type    init *)
  | VarDec of (symbol * symbol option * exp * pos)
  | TypeDec of (symbol * ty * pos) list

(* name   type *)
and field = symbol * symbol * pos

(* name     params         result                body *)
and fundec = symbol * field list * symbol option * exp * pos

and ty =
  | NameTy of symbol * pos
  | RecordTy of field list * pos
  | ArrayTy of symbol * pos

and oper =
  | PlusOp
  | MinusOp
  | TimesOp
  | DivideOp
  | EqOp
  | NeqOp
  | LtOp
  | LeOp
  | GtOp
  | GeOp
