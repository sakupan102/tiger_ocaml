open Tiger_ocaml

let%test "assem_replace_test" =
  Assem.replace Assem.dst_matcher [ "eax"; "ebx"; "ecx" ] "mov d0 d1 d2"
  = "mov eax ebx ecx"

let test_temp_map =
  let temp_map : Frame.register Temp.Table.table = Temp.Table.empty () in
  Temp.Table.enter (temp_map, Frame.rax, "rax");
  Temp.Table.enter (temp_map, Frame.rbx, "rbx");
  Temp.Table.enter (temp_map, Frame.rcx, "rcx");
  Temp.Table.enter (temp_map, Frame.rdx, "rdx");
  Temp.Table.enter (temp_map, Frame.rsp, "rsp");
  Temp.Table.enter (temp_map, Frame.rsi, "rsi");
  Temp.Table.enter (temp_map, Frame.rdi, "rdi");
  Temp.Table.enter (temp_map, Frame.rbp, "rbp");
  temp_map

let test_instr : Assem.instr =
  Assem.OPER
    {
      assem = "mov s0 d0";
      dst = [ Frame.rax ];
      src = [ Frame.rbx ];
      jump = None;
    }

let%test "assem_format_test" =
  Assem.format test_instr test_temp_map
  = Assem.OPER
      {
        assem = "mov rbx rax";
        dst = [ Frame.rax ];
        src = [ Frame.rbx ];
        jump = None;
      }
