module type Color = sig
  type temp_to_register = Frame.register Temp.Table.table

  type color = {
    interference : Liveness.Liveness.igraph;
    initial : temp_to_register; (*フレームポインター等の既彩色テンポラリー*)
    spill_cost : Graph.Graph.node -> int; (*スピルさせるテンポラリを決める際の基準*)
    registers : Frame.register list; (*使えるレジスター*)
  }

  val color : color -> temp_to_register * Temp.temp list
end

module Color : Color = struct
  module NodeTable = FlowGraph.FGraph.ITable
  module Graph = Graph.Graph

  type temp_to_register = Frame.register Temp.Table.table

  type color = {
    interference : Liveness.Liveness.igraph;
    initial : temp_to_register; (*フレームポインター等の既彩色テンポラリー*)
    spill_cost : Graph.node -> int; (*スピルさせるテンポラリを決める際の基準*)
    registers : Frame.register list; (*使えるレジスター*)
  }

  module Registers = Set.Make (struct
    type t = Frame.register

    let compare = compare
  end)

  module Temps = Set.Make (struct
    type t = Temp.temp

    let compare = compare
  end)

  module Nodes = Set.Make (struct
    type t = Graph.node

    let compare = compare
  end)

  module NodePairs = Set.Make (struct
    type t = Graph.node * Graph.node

    let compare = compare
  end)

  let node_stack_to_set (nodes : Graph.node Stack.t) : Nodes.t =
    let set = ref Nodes.empty in
    Stack.iter (fun x -> set := Nodes.add x !set) nodes;
    !set

  let simplify_worklist : Nodes.t ref = ref Nodes.empty
  let initial_worklist : Nodes.t ref = ref Nodes.empty

  (*
  let freeze_worklist : Nodes.t ref = ref Nodes.empty
  let spill_worklist : Nodes.t ref = ref Nodes.empty
  *)
  let colored_nodes : Nodes.t ref = ref Nodes.empty
  let select_stack : Graph.node Stack.t = Stack.create ()

  (*
  let active_moves : NodePairs.t ref = ref NodePairs.empty
  let worklist_moves : NodePairs.t ref = ref NodePairs.empty
  *)
  let temp_to_register : temp_to_register = Temp.Table.empty ()

  let color ({ interference; _ } : color) : temp_to_register * Temp.temp list =
    (*let node_to_move_nodes : NodePairs.t NodeTable.table = NodeTable.empty ()*)
    let temp_to_degree : int NodeTable.table = NodeTable.empty () in
    (*
    let set_move_table (move_nodes : (Graph.node * Graph.node) list) =
      List.map
        (fun (node1, node2) ->
          NodeTable.enter
            ( node_to_move_nodes,
              node1,
              NodePairs.add (node1, node2)
                (NodeTable.look_exn node_to_move_nodes node1) );
          NodeTable.enter
            ( node_to_move_nodes,
              node2,
              NodePairs.add (node1, node2)
                (NodeTable.look_exn node_to_move_nodes node2) ))
        move_nodes
    in
    let move_related_nodes (node : Graph.node) : NodePairs.t =
      NodePairs.inter
        (NodeTable.look_exn node_to_move_nodes node)
        (NodePairs.union !active_moves !worklist_moves)
    in
    let is_move_related_node (node : Graph.node) =
      NodePairs.cardinal (move_related_nodes node) == 1
    in
    *)
    let adjacent_nodes (node : Graph.node) : Nodes.t =
      Nodes.diff
        (Nodes.of_list (Graph.adj node))
        (node_stack_to_set select_stack)
    in
    let set_degree () =
      List.iter
        (fun (node : Graph.node) ->
          let num_adj_nodes = List.length (Graph.adj node) in
          NodeTable.enter (temp_to_degree, node, num_adj_nodes))
        (Graph.nodes interference.graph)
    in
    let build () = set_degree () in
    let make_work_list () =
      let initial_nodes = Graph.nodes interference.graph in
      List.iter
        (fun (node : Graph.node) ->
          if NodeTable.look_exn temp_to_degree node >= Frame.num_registers then
            failwith "spilling is not supported"
            (* spill_worklist := Nodes.add node !spill_worklist *)
            (*
          else if is_move_related_node node then
            freeze_worklist := Nodes.add node !freeze_worklist
          *)
          else simplify_worklist := Nodes.add node !simplify_worklist)
        initial_nodes
    in
    let decrement_degree (node : Graph.node) =
      let degree = NodeTable.look_exn temp_to_degree node in
      NodeTable.enter (temp_to_degree, node, degree - 1);
      if degree == Frame.num_registers then
        simplify_worklist := Nodes.add node !simplify_worklist
      else ()
    in
    let simplify () =
      let node = Nodes.choose !simplify_worklist in
      simplify_worklist := Nodes.remove node !simplify_worklist;
      Stack.push node select_stack;
      Nodes.iter decrement_degree (adjacent_nodes node)
    in
    let assign_color () =
      while Stack.is_empty select_stack do
        let n = Stack.pop select_stack
        and ok_colors = ref (Registers.of_list Frame.registers) in
        List.iter
          (fun (adj_node : Graph.node) ->
            if Nodes.exists (fun n -> Graph.eq n adj_node) !colored_nodes then
              ok_colors :=
                Registers.remove
                  (Temp.Table.look_exn temp_to_register
                     (interference.node_to_temp adj_node))
                  !ok_colors)
          (Graph.adj n);
        if Registers.is_empty !ok_colors then
          failwith "spilling is not supported"
        else colored_nodes := Nodes.add n !colored_nodes;
        let selected_color = Registers.choose !ok_colors in
        Temp.Table.enter
          (temp_to_register, interference.node_to_temp n, selected_color)
      done
    in
    build ();
    make_work_list ();
    while Nodes.is_empty !simplify_worklist do
      if not (Nodes.is_empty !simplify_worklist) then simplify ()
        (*
      else if not NodePairs.is_empty worklist_moves then coalesce() 
      else if not Nodes.is_empty freeze_worklist then freeze()
      else if not Nodes.is_empty spill_worklist then select_spill()
      *)
    done;
    assign_color ();
    (* if Nodes.is_empty spilled_nodes then rewrite_program spilled_nodes color () *)
    (temp_to_register, [])
end
