open Tiger_ocaml
module MakeGraph = MakeGraph.MakeGraph
module Graph = Graph.Graph

let main_label = Temp.new_label ()
let end_label = Temp.new_label ()

let test_instrs : Assem.instr list =
  [
    Assem.LABEL { assem = "main"; lab = main_label };
    Assem.OPER
      {
        assem = "add `d0, `s0";
        dst = [ Frame.rax ];
        src = [ Frame.rbx ];
        jump = None;
      };
    Assem.MOVE { assem = "move `d0, `s0"; src = Frame.rax; dst = Frame.rbx };
    Assem.OPER
      {
        assem = "cmp `s0, `s1";
        dst = [];
        src = [ Frame.rax; Frame.rbx ];
        jump = None;
      };
    Assem.OPER
      {
        assem = "je main";
        dst = [];
        src = [];
        jump = Some [ main_label; end_label ];
      };
    Assem.LABEL { assem = "end"; lab = end_label };
  ]

let flowgraph, _ = MakeGraph.instrs_to_graph test_instrs

let%test "make_graph_test" =
  let node = Graph.augment flowgraph.control 3 in
  let use_temps = Graph.ITable.look_exn flowgraph.use node in
  use_temps = [ Frame.rax; Frame.rbx ]

(*

let () =
  (*
  Graph.show flowgraph.control;
  printf "rax: %d " Frame.rax;
  printf "rbx: %d " Frame.rbx;
  *)
  let node = Graph.augment flowgraph.control 3 in
  let use_temps = Graph.ITable.look_exn flowgraph.use node in
  print_string (String.concat "; " (List.map string_of_int use_temps) ^ "\n");
  Graph.ITable.print
    (fun ((_, node_index) : Graph.node) -> string_of_int node_index)
    (fun temps -> String.concat "; " (List.map string_of_int temps))
    flowgraph.use
    *)
