type label = Temp.label
type size = int

(* Mem on the left side of a mode is a store
 * all other uses are fetch *)

type stm =
  | SEQ of stm * stm
  | LABEL of label
  | JUMP of exp * label list
  | CJUMP of relop * exp * exp * label * label
  | MOVE of exp * exp
  | EXP of exp

and exp =
  | BINOP of binop * exp * exp
  | MEM of exp
  | TEMP of Temp.temp
  | ESEQ of stm * exp
  | NAME of label
  | CONST of int
  | CALL of exp * exp list

and binop =
  | PLUS
  | MINUS
  | MUL
  | DIV
  | AND
  | OR
  | LSHIFT
  | RSHIFT
  | ARSHIFT
  | XOR

and relop = EQ | NE | LT | GT | LE | GE | ULT | ULE | UGT | UGE

let rec seq (stms : stm list) =
  match stms with
  | [] -> failwith "Can't build sequence from nothing"
  | stm :: [] -> stm
  | stm :: stms -> SEQ (stm, seq stms)
