module type MakeGraph = sig
  val instrs_to_graph :
    Assem.instr list -> FlowGraph.flowgraph * FlowGraph.FGraph.node list
end

module MakeGraph : MakeGraph = struct
  module Graph = Graph.Graph

  let instrs_to_graph (instrs : Assem.instr list) =
    let label_to_node : (Temp.label, Graph.node) Hashtbl.t = Hashtbl.create 10
    and new_graph = Graph.new_graph ()
    and node_to_use_registers : Temp.temp list Graph.ITable.table =
      Graph.ITable.empty ()
    and node_to_def_registers : Temp.temp list Graph.ITable.table =
      Graph.ITable.empty ()
    and node_to_is_move : bool Graph.ITable.table = Graph.ITable.empty () in

    let add_nodes_simple (instrs : Assem.instr) : Graph.node =
      let new_node = Graph.new_node new_graph in
      (match instrs with
      | Assem.LABEL label -> Hashtbl.add label_to_node label.lab new_node
      | _ -> ());
      new_node
    and add_label (node : Graph.node) (label : Temp.label) =
      let label_node = Hashtbl.find label_to_node label in
      Graph.make_edge { from = node; to_ = label_node }
    in

    let add_edges (nodes : Graph.node list) (instrs : Assem.instr list) : unit =
      for i = 0 to List.length nodes - 2 do
        match List.nth instrs i with
        | Assem.OPER { jump = Some labels; _ } ->
            List.iter (add_label (List.nth nodes i)) labels;
            Graph.make_edge
              { from = List.nth nodes i; to_ = List.nth nodes (i + 1) }
        | _ ->
            Graph.make_edge
              { from = List.nth nodes i; to_ = List.nth nodes (i + 1) }
      done
    and add_temp_to_table (instr : Assem.instr) (node : Graph.node) =
      match instr with
      | Assem.OPER { dst; src; _ } ->
          Graph.ITable.enter (node_to_use_registers, node, src);
          Graph.ITable.enter (node_to_def_registers, node, dst);
          Graph.ITable.enter (node_to_is_move, node, false)
      | Assem.MOVE { dst; src; _ } ->
          Graph.ITable.enter (node_to_use_registers, node, [ src ]);
          Graph.ITable.enter (node_to_def_registers, node, [ dst ]);
          Graph.ITable.enter (node_to_is_move, node, true)
      | Assem.LABEL _ ->
          Graph.ITable.enter (node_to_use_registers, node, []);
          Graph.ITable.enter (node_to_def_registers, node, []);
          Graph.ITable.enter (node_to_is_move, node, false)
    in

    let simple_nodes = List.map add_nodes_simple instrs in
    add_edges simple_nodes instrs;
    List.iter2 add_temp_to_table instrs simple_nodes;
    ( {
        FlowGraph.control = new_graph;
        def = node_to_def_registers;
        use = node_to_use_registers;
        ismove = node_to_is_move;
      },
      simple_nodes )
end
