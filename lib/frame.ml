type location = InReg of Temp.temp | InFrame of int
type formal = location
type access = location
type register = string

let wordsize = 4
let numRegisters = 8
let eax = Temp.newTemp ()
let ebx = Temp.newTemp ()
let ecx = Temp.newTemp ()
let edx = Temp.newTemp ()
let esp = Temp.newTemp ()
let esi = Temp.newTemp ()
let edi = Temp.newTemp ()
let ebp = Temp.newTemp ()
let fp = ebp
let rv = eax

type frame = {
  label : Temp.label;
  formals : formal list;
  locals : access list ref;
  local_offset : int ref;
}

let exp (access : access) (Tree.TEMP frame_pointer) =
  match access with
  | InFrame offset ->
      Tree.MEM
        (Tree.BINOP (Tree.PLUS, Tree.TEMP frame_pointer, Tree.CONST offset))
  | InReg tmp -> Tree.TEMP tmp

let new_frame label escapes =
  (* Make a list of all the formals *)
  let num_reg_params = 0 in
  (* Pass all parameters on the stack for now *)
  let rec build_formals spots offset acc = function
    | [] -> List.rev acc
    | esc :: tl ->
        if spots > 0 && not esc then
          let out = InReg (Temp.newTemp ()) in
          build_formals (spots - 1) offset (out :: acc) tl
        else
          let out = InFrame offset in
          build_formals spots (offset + 4) (out :: acc) tl
  in
  let formals = build_formals num_reg_params 8 [] escapes in
  { label; locals = ref []; local_offset = ref ~-4; formals }

let alloc_local frame escape =
  let loc =
    match escape with
    | false -> InReg (Temp.newTemp ())
    | true ->
        let off = !(frame.local_offset) in
        frame.local_offset := off - 4;
        InFrame off
  in
  frame.locals := loc :: !(frame.locals);
  loc
