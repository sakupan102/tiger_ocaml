type reg = string
type temp = Temp.temp
type label = Temp.label

type oper_record = {
  assem : string;
  dst : temp list;
  src : temp list;
  jump : label list option;
}

type label_record = { assem : string; lab : Temp.label }
type move_record = { assem : string; dst : temp; src : temp }
type instr = OPER of oper_record | LABEL of label_record | MOVE of move_record

let dst_matcher = Str.regexp {|d\([0-9]+\)|}
and src_matcher = Str.regexp {|s\([0-9]+\)|}

let rec replace (matcher : Str.regexp) (temps : reg list) (assem : string) =
  let i = try Str.search_forward matcher assem 0 with Not_found -> -1 in
  if i == -1 then assem
  else
    let matched_idx = int_of_string (Str.matched_group 1 assem) in
    let register = List.nth temps matched_idx in
    let replaced_assem = Str.replace_first matcher register assem in
    replace matcher temps replaced_assem

let format (instr : instr) (temp_to_register : reg Temp.Table.table) : instr =
  let temp_to_register_fn temp = Temp.Table.look_exn temp_to_register temp in
  match instr with
  | OPER instr ->
      let dst_temps = List.map temp_to_register_fn instr.dst
      and src_temps = List.map temp_to_register_fn instr.src in
      let replaced_assem =
        replace dst_matcher dst_temps
          (replace src_matcher src_temps instr.assem)
      in
      OPER
        {
          assem = replaced_assem;
          dst = instr.dst;
          src = instr.src;
          jump = instr.jump;
        }
  | LABEL label -> LABEL label
  | MOVE instr ->
      let dst_temp = temp_to_register_fn instr.dst
      and src_temp = temp_to_register_fn instr.src in
      let replaced_assem =
        replace dst_matcher [ dst_temp ]
          (replace src_matcher [ src_temp ] instr.assem)
      in
      MOVE { assem = replaced_assem; src = instr.src; dst = instr.dst }
