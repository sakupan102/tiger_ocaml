open Tiger_ocaml
module A = Absyn
module Translate = Translate.Make

let a = Symbol.create "a"
let fake_symbol = Symbol.create "fake"

let fakepos : A.pos =
  { pos_fname = "fake"; pos_cnum = 0; pos_bol = 0; pos_lnum = 0 }

let test_exp =
  A.LetExp
    ( [ A.VarDec (a, Some Env.int, A.IntExp (1, fakepos), fakepos) ],
      A.SeqExp
        [
          A.AssignExp (A.SimpleVar (a, fakepos), A.IntExp (5, fakepos), fakepos);
          A.OpExp
            ( A.VarExp (A.SimpleVar (a, fakepos), fakepos),
              A.PlusOp,
              A.IntExp (1, fakepos),
              fakepos );
        ],
      fakepos )

let fake_frame = Frame.new_frame fake_symbol []

let base_level : Translate.level =
  { prev = None; frame = fake_frame; uniq = ref () }

let translated_exp =
  Semant.trans_exp ((Env.base_venv, Env.base_tenv), base_level, test_exp)

let () = Print_tree.print (Translate.unNx translated_exp.exp)
