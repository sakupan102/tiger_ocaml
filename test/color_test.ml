open Tiger_ocaml
module Color = Color.Color
module Graph = Graph.Graph
module Liveness = Liveness.Liveness

let augment = Graph.augment
let b = Temp.newTemp ()
let c = Temp.newTemp ()
let d = Temp.newTemp ()
let e = Temp.newTemp ()
let f = Temp.newTemp ()
let g = Temp.newTemp ()
let h = Temp.newTemp ()
let j = Temp.newTemp ()
let k = Temp.newTemp ()
let m = Temp.newTemp ()

let test_interference_graph : Graph.graph =
  let test_interference_graph = Graph.new_graph () in
  let _ = List.init 10 (fun _ -> Graph.new_node test_interference_graph) in
  Graph.make_edge
    {
      from = Graph.augment test_interference_graph 0;
      to_ = Graph.augment test_interference_graph 8;
    };
  Graph.make_edge
    {
      from = Graph.augment test_interference_graph 0;
      to_ = Graph.augment test_interference_graph 3;
    };
  Graph.make_edge
    {
      from = Graph.augment test_interference_graph 0;
      to_ = Graph.augment test_interference_graph 9;
    };
  Graph.make_edge
    {
      from = Graph.augment test_interference_graph 0;
      to_ = Graph.augment test_interference_graph 1;
    };
  Graph.make_edge
    {
      from = Graph.augment test_interference_graph 0;
      to_ = Graph.augment test_interference_graph 2;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 1;
      to_ = augment test_interference_graph 0;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 1;
      to_ = augment test_interference_graph 9;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 2;
      to_ = augment test_interference_graph 0;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 2;
      to_ = augment test_interference_graph 7;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 2;
      to_ = augment test_interference_graph 9;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 2;
      to_ = augment test_interference_graph 8;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 3;
      to_ = augment test_interference_graph 4;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 3;
      to_ = augment test_interference_graph 7;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 3;
      to_ = augment test_interference_graph 0;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 3;
      to_ = augment test_interference_graph 9;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 4;
      to_ = augment test_interference_graph 7;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 4;
      to_ = augment test_interference_graph 3;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 4;
      to_ = augment test_interference_graph 9;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 5;
      to_ = augment test_interference_graph 8;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 5;
      to_ = augment test_interference_graph 7;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 5;
      to_ = augment test_interference_graph 6;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 6;
      to_ = augment test_interference_graph 5;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 6;
      to_ = augment test_interference_graph 7;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 7;
      to_ = augment test_interference_graph 4;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 7;
      to_ = augment test_interference_graph 8;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 7;
      to_ = augment test_interference_graph 2;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 7;
      to_ = augment test_interference_graph 6;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 7;
      to_ = augment test_interference_graph 5;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 7;
      to_ = augment test_interference_graph 3;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 8;
      to_ = augment test_interference_graph 7;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 8;
      to_ = augment test_interference_graph 0;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 8;
      to_ = augment test_interference_graph 2;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 8;
      to_ = augment test_interference_graph 5;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 9;
      to_ = augment test_interference_graph 4;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 9;
      to_ = augment test_interference_graph 3;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 9;
      to_ = augment test_interference_graph 0;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 9;
      to_ = augment test_interference_graph 1;
    };
  Graph.make_edge
    {
      from = augment test_interference_graph 9;
      to_ = augment test_interference_graph 2;
    };
  test_interference_graph

let node_to_temp : Temp.temp Graph.ITable.table =
  let node_to_temp = Graph.ITable.empty () in
  Graph.ITable.enter (node_to_temp, augment test_interference_graph 0, b);
  Graph.ITable.enter (node_to_temp, augment test_interference_graph 1, c);
  Graph.ITable.enter (node_to_temp, augment test_interference_graph 2, d);
  Graph.ITable.enter (node_to_temp, augment test_interference_graph 3, e);
  Graph.ITable.enter (node_to_temp, augment test_interference_graph 4, f);
  Graph.ITable.enter (node_to_temp, augment test_interference_graph 5, g);
  Graph.ITable.enter (node_to_temp, augment test_interference_graph 6, h);
  Graph.ITable.enter (node_to_temp, augment test_interference_graph 7, j);
  Graph.ITable.enter (node_to_temp, augment test_interference_graph 8, k);
  Graph.ITable.enter (node_to_temp, augment test_interference_graph 9, m);
  node_to_temp

let temp_to_node : Graph.node Temp.Table.table =
  let temp_to_node = Temp.Table.empty () in
  Temp.Table.enter (temp_to_node, b, augment test_interference_graph 0);
  Temp.Table.enter (temp_to_node, c, augment test_interference_graph 1);
  Temp.Table.enter (temp_to_node, d, augment test_interference_graph 2);
  Temp.Table.enter (temp_to_node, e, augment test_interference_graph 3);
  Temp.Table.enter (temp_to_node, f, augment test_interference_graph 4);
  Temp.Table.enter (temp_to_node, g, augment test_interference_graph 5);
  Temp.Table.enter (temp_to_node, h, augment test_interference_graph 6);
  Temp.Table.enter (temp_to_node, j, augment test_interference_graph 7);
  Temp.Table.enter (temp_to_node, k, augment test_interference_graph 8);
  Temp.Table.enter (temp_to_node, m, augment test_interference_graph 9);
  temp_to_node

let interference_graph : Liveness.igraph =
  {
    graph = test_interference_graph;
    temp_to_node = (fun temp -> Temp.Table.look_exn temp_to_node temp);
    node_to_temp = (fun node -> Graph.ITable.look_exn node_to_temp node);
    moves = [];
  }

let default_temp_register_map = Frame.default_temp_map
let spill_cost (_ : Graph.node) = 0
let registers = Frame.registers

let temp_to_register, spilled_temps =
  Color.color
    {
      interference = interference_graph;
      initial = default_temp_register_map;
      spill_cost;
      registers;
    }

let get_node_reg (node : Graph.node) =
  let node_temp = Graph.ITable.look_exn node_to_temp node in
  Temp.Table.look_exn temp_to_register node_temp

let%test "color spill test" = spilled_temps = []

(* coloring conflict test *)
let () =
  List.iter
    (fun (node : Graph.node) ->
      let _, idx = node in
      let node_reg = get_node_reg node in
      let adj_nodes = Graph.adj node in
      List.iter
        (fun (adj_node : Graph.node) ->
          let adj_reg = get_node_reg adj_node in
          let _, adj_idx = adj_node in
          if node_reg == adj_reg then
            print_string
              (Printf.sprintf "confilct detected %d %d %s\n" idx adj_idx
                 node_reg))
        adj_nodes)
    (Graph.nodes test_interference_graph)
