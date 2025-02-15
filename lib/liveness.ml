module type Liveness = sig
  type interference_graph
  type igraph

  val interference_graph :
    FlowGraph.flowgraph ->
    interference_graph * (FlowGraph.FGraph.node -> Temp.temp list)
end

module Liveness = struct
  module Graph = Graph.Graph

  type igraph = {
    graph : Graph.graph;
    temp_to_node : Temp.temp -> Graph.node;
    node_to_temp : Graph.node -> Temp.temp;
    moves : (Graph.node * Graph.node) list;
  }

  type interference_graph = IGRAPH of igraph

  module SN = Set.Make (struct
    type t = FlowGraph.FGraph.node

    let compare = compare
  end)

  module Temps = Set.Make (struct
    type t = Temp.temp

    let compare = compare
  end)

  type live_temps = Temps.t (*unit Temp.ITable.table * Temp.temp list*)
  type node_to_live_temps = live_temps FlowGraph.FGraph.ITable.table
  type node_to_temp = Temp.temp Graph.ITable.table
  type temp_to_node = Graph.node Temp.Table.table

  let interference_graph (flow_graph : FlowGraph.flowgraph) =
    let node_to_live_out_temps : node_to_live_temps =
      FlowGraph.FGraph.ITable.empty ()
    and node_to_live_in_temps : node_to_live_temps =
      FlowGraph.FGraph.ITable.empty ()
      (*
    and add_temps_to_set (temps : Temp.temp list) (temp_set : Temps.t) =
      List.fold_left (fun temps temp -> Temps.add temp temps) temp_set temps
    *)
    in
    let initialize_node_to_temps_table () =
      List.iter
        (fun (node : Graph.node) ->
          Graph.ITable.enter (node_to_live_in_temps, node, Temps.empty))
        (Graph.nodes flow_graph.control);
      List.iter
        (fun (node : Graph.node) ->
          Graph.ITable.enter (node_to_live_out_temps, node, Temps.empty))
        (Graph.nodes flow_graph.control)
    in

    let set_temps (graph : FlowGraph.flowgraph) (node : FlowGraph.FGraph.node) =
      let use_temps =
        Temps.of_list (FlowGraph.FGraph.ITable.look_exn graph.use node)
      and def_temps =
        Temps.of_list (FlowGraph.FGraph.ITable.look_exn graph.def node)
      and succ_nodes = FlowGraph.FGraph.succ node
      and live_out_temps =
        FlowGraph.FGraph.ITable.look_exn node_to_live_out_temps node
      in
      let new_live_in_temps =
        Temps.union use_temps (Temps.diff live_out_temps def_temps)
      and new_live_out_temps =
        List.fold_left
          (fun (temps : live_temps) (succ_node : FlowGraph.FGraph.node) ->
            let live_in_temps =
              FlowGraph.FGraph.ITable.look_exn node_to_live_in_temps succ_node
            in
            Temps.union temps live_in_temps)
          Temps.empty succ_nodes
      in
      FlowGraph.FGraph.ITable.enter
        (node_to_live_in_temps, node, new_live_in_temps);
      FlowGraph.FGraph.ITable.enter
        (node_to_live_out_temps, node, new_live_out_temps);
      (new_live_in_temps, new_live_out_temps)
    in
    let rec compute_live_map (graph : FlowGraph.flowgraph) =
      let nodes = Graph.nodes graph.control and all_equal = ref true in
      let compare_alive_temps (graph : FlowGraph.flowgraph)
          (node : FlowGraph.FGraph.node) =
        let old_live_in_temps =
          FlowGraph.FGraph.ITable.look_exn node_to_live_in_temps node
        and old_live_out_temps =
          FlowGraph.FGraph.ITable.look_exn node_to_live_out_temps node
        in
        let new_live_in_temps, new_live_out_temps = set_temps graph node in
        (*
        print_string ("node_index: " ^ string_of_int node_index ^ "\n");
        print_string
          ("["
          ^ String.concat "; "
              (List.map string_of_int (Temps.elements old_live_in_temps))
          ^ "]\n");
        print_string
          ("["
          ^ String.concat "; "
              (List.map string_of_int (Temps.elements new_live_in_temps))
          ^ "]\n");
          *)
        if
          (not (Temps.equal new_live_in_temps old_live_in_temps))
          || not (Temps.equal new_live_out_temps old_live_out_temps)
        then all_equal := false
        else ()
      in
      List.iter (compare_alive_temps graph) nodes;
      if not !all_equal then compute_live_map graph else ()
    in
    let create_interference_graph () : igraph =
      let graph = flow_graph.control
      and interference_graph = Graph.new_graph ()
      and node_to_temp : node_to_temp = Graph.ITable.empty ()
      and temp_to_node : temp_to_node = Temp.Table.empty () in
      let node_to_temp_fn node = Graph.ITable.look_exn node_to_temp node
      and temp_to_node_fn temp = Temp.Table.look_exn temp_to_node temp in
      let get_node temp =
        match Temp.Table.look (temp_to_node, temp) with
        | Some n -> n
        | None ->
            let new_node = Graph.new_node interference_graph in
            Temp.Table.enter (temp_to_node, temp, new_node);
            Graph.ITable.enter (node_to_temp, new_node, temp);
            new_node
      in
      let set_interference def_temps live_out_temps =
        List.iter
          (fun def_temp ->
            let def_temp_node = get_node def_temp in
            List.iter
              (fun live_out_temp ->
                let live_out_node = get_node live_out_temp in
                Graph.make_edge { from = def_temp_node; to_ = live_out_node };
                Graph.make_edge { from = live_out_node; to_ = def_temp_node })
              live_out_temps)
          def_temps
      in
      List.iter
        (fun (flow_graph_node : FlowGraph.FGraph.node) ->
          let def_temps =
            FlowGraph.FGraph.ITable.look_exn flow_graph.use flow_graph_node
          and live_out_temps =
            FlowGraph.FGraph.ITable.look_exn node_to_live_out_temps
              flow_graph_node
          in
          set_interference def_temps (Temps.elements live_out_temps))
        (FlowGraph.FGraph.nodes graph);
      {
        graph = interference_graph;
        temp_to_node = temp_to_node_fn;
        node_to_temp = node_to_temp_fn;
        moves = [];
      }
    in
    initialize_node_to_temps_table ();
    compute_live_map flow_graph;
    (*
    Graph.ITable.print
      (fun ((_, node_index) : Graph.node) -> string_of_int node_index)
      (fun (temps : Temps.t) ->
        "{"
        ^ String.concat "; " (List.map string_of_int (Temps.elements temps))
        ^ "}")
      node_to_live_in_temps;
    Graph.ITable.print
      (fun ((_, node_index) : Graph.node) -> string_of_int node_index)
      (fun (temps : Temps.t) ->
        "{"
        ^ String.concat "; " (List.map string_of_int (Temps.elements temps))
        ^ "}")
      node_to_live_out_temps;
      *)
    create_interference_graph ()
end
