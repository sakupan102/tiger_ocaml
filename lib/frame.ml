type location = InReg of Temp.temp | InFrame of int
type formal = location
type access = location
type register = string

let wordsize = 8
let registers = [ "rax"; "rbx"; "rcx"; "rdx" ]

(*TODO: 引数の数を無制限にする*)
let num_args = 6
let num_registers = 4
let rax = Temp.newTemp ()
let rbx = Temp.newTemp ()
let rcx = Temp.newTemp ()
let rdx = Temp.newTemp ()
let rsp = Temp.newTemp ()
let rsi = Temp.newTemp ()
let rdi = Temp.newTemp ()
let rbp = Temp.newTemp ()
let fp = rbp
let rv = rax
let caller_save_registers = [ rax; rcx; rdx ]
let callee_save_registers = [ rbx; rbp; rdi; rsi; rsp ]
let register_temps = [ rax; rbx; rcx; rdx; rsp; rsi; rdi; rbp ]

type program = { prolog : string; body : Assem.instr list; epilog : string }

type frame = {
  label : Temp.label;
  formals : formal list; (*args + static link*)
  locals : access list ref; (*local variables*)
  local_offset : int ref; (* offset from the base pointer *)
}

type frag =
  | PROC of { body : Tree.stm; frame : frame }
  | STRING of Temp.label * string

let exp (access : access) frame_pointer =
  match frame_pointer with
  | Tree.ESEQ _ | Tree.CONST _ | Tree.NAME _ | Tree.BINOP _ | Tree.CALL _ ->
      failwith "frame_pointer must be a register or a memory location"
  | _ -> (
      match access with
      | InFrame offset ->
          Tree.MEM (Tree.BINOP (Tree.PLUS, frame_pointer, Tree.CONST offset))
      | InReg tmp -> Tree.TEMP tmp)

let new_frame label escapes =
  let num_reg_params = 0 in
  let rec build_formals num_remained_registers offset accsesses = function
    | [] -> List.rev accsesses
    | escape :: tl ->
        if num_remained_registers > 0 && not escape then
          let out = InReg (Temp.newTemp ()) in
          build_formals
            (num_remained_registers - 1)
            offset (out :: accsesses) tl
        else
          let new_offset = offset + wordsize in
          let out = InFrame new_offset in
          build_formals num_remained_registers new_offset (out :: accsesses) tl
  in
  (*
    offset = 0: old rbp
    offset = wordsize: return address
    offset = 2 * wordsize: static link
  *)
  let formals = build_formals num_reg_params wordsize [] escapes in
  { label; locals = ref []; local_offset = ref ~-wordsize; formals }

let alloc_local frame escape =
  let loc =
    match escape with
    | false -> InReg (Temp.newTemp ())
    | true ->
        let off = !(frame.local_offset) in
        frame.local_offset := off - wordsize;
        InFrame off
  in
  frame.locals := loc :: !(frame.locals);
  loc

let proc_entry_exit1 (frame, body) =
  let memory_for_saved_registers =
    List.map (fun _ -> alloc_local frame true) callee_save_registers
  in
  let save_registers register memory_for_saved_register =
    Tree.MOVE (exp memory_for_saved_register (Tree.TEMP rbp), Tree.TEMP register)
  and restore_registers register memory_for_saved_register =
    Tree.MOVE (Tree.TEMP register, exp memory_for_saved_register (Tree.TEMP rbp))
  in
  let stm_list =
    List.map2 save_registers callee_save_registers memory_for_saved_registers
    @ [ body ]
    @ List.map2 restore_registers callee_save_registers
        memory_for_saved_registers
  in
  Tree.seq stm_list

let proc_entry_exit3 : frame * Assem.instr list -> program = function
  | frame, body ->
      let offset = (List.length !(frame.locals) + num_args) * wordsize in
      let prolog =
        Symbol.name frame.label ^ ":\n" ^ "\tpush rbp\n" ^ "\tmov [rbp], rsp"
        ^ "\tsub rsp, " ^ string_of_int offset ^ "\n"
      in
      let epilog = "\tmov rsp, rbp\n" ^ "\tpop rbp\n" ^ "\tret\n" in
      { prolog; body; epilog }

let default_temp_map =
  let temp_map : register Temp.Table.table = Temp.Table.empty () in
  Temp.Table.enter (temp_map, rax, "rax");
  Temp.Table.enter (temp_map, rbx, "rbx");
  Temp.Table.enter (temp_map, rcx, "rcx");
  Temp.Table.enter (temp_map, rdx, "rdx");
  Temp.Table.enter (temp_map, rsp, "rsp");
  Temp.Table.enter (temp_map, rsi, "rsi");
  Temp.Table.enter (temp_map, rdi, "rdi");
  Temp.Table.enter (temp_map, rbp, "rbp");
  temp_map
