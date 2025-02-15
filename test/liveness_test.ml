open Tiger_ocaml
module Liveness = Liveness.Liveness
module Graph = Graph.Graph

let test_flowgraph : Graph.graph =
  let test_flowgraph = Graph.new_graph () in
  let _ = List.init 7 (fun _ -> Graph.new_node test_flowgraph) in
  Graph.make_edge
    {
      from = Graph.augment test_flowgraph 0;
      to_ = Graph.augment test_flowgraph 1;
    };
  Graph.make_edge
    {
      from = Graph.augment test_flowgraph 1;
      to_ = Graph.augment test_flowgraph 2;
    };
  Graph.make_edge
    {
      from = Graph.augment test_flowgraph 2;
      to_ = Graph.augment test_flowgraph 3;
    };
  Graph.make_edge
    {
      from = Graph.augment test_flowgraph 3;
      to_ = Graph.augment test_flowgraph 4;
    };
  Graph.make_edge
    {
      from = Graph.augment test_flowgraph 4;
      to_ = Graph.augment test_flowgraph 5;
    };
  Graph.make_edge
    {
      from = Graph.augment test_flowgraph 5;
      to_ = Graph.augment test_flowgraph 6;
    };
  Graph.make_edge
    {
      from = Graph.augment test_flowgraph 5;
      to_ = Graph.augment test_flowgraph 2;
    };
  test_flowgraph

let a = Temp.newTemp ()
let b = Temp.newTemp ()
let c = Temp.newTemp ()

let temp_to_name : string Temp.Table.table =
  let temp_to_name = Temp.Table.empty () in
  Temp.Table.enter (temp_to_name, a, "a");
  Temp.Table.enter (temp_to_name, b, "b");
  Temp.Table.enter (temp_to_name, c, "c");
  temp_to_name

let node_to_def_temps : Temp.temp list Graph.ITable.table =
  let table = Graph.ITable.empty () in
  Graph.ITable.enter (table, Graph.augment test_flowgraph 0, [ c ]);
  Graph.ITable.enter (table, Graph.augment test_flowgraph 1, [ a ]);
  Graph.ITable.enter (table, Graph.augment test_flowgraph 2, [ b ]);
  Graph.ITable.enter (table, Graph.augment test_flowgraph 3, [ c ]);
  Graph.ITable.enter (table, Graph.augment test_flowgraph 4, [ a ]);
  Graph.ITable.enter (table, Graph.augment test_flowgraph 5, []);
  Graph.ITable.enter (table, Graph.augment test_flowgraph 6, []);
  table

let node_to_use_temps : Temp.temp list Graph.ITable.table =
  let table = Graph.ITable.empty () in
  Graph.ITable.enter (table, Graph.augment test_flowgraph 0, []);
  Graph.ITable.enter (table, Graph.augment test_flowgraph 1, []);
  Graph.ITable.enter (table, Graph.augment test_flowgraph 2, [ a ]);
  Graph.ITable.enter (table, Graph.augment test_flowgraph 3, [ b; c ]);
  Graph.ITable.enter (table, Graph.augment test_flowgraph 4, [ b ]);
  Graph.ITable.enter (table, Graph.augment test_flowgraph 5, [ a ]);
  Graph.ITable.enter (table, Graph.augment test_flowgraph 6, [ c ]);
  table

let node_to_is_move : bool Graph.ITable.table =
  let table = Graph.ITable.empty () in
  List.iter
    (fun node -> Graph.ITable.enter (table, node, false))
    (Graph.nodes test_flowgraph);
  table

let interference_graph =
  Liveness.interference_graph
    {
      control = test_flowgraph;
      def = node_to_def_temps;
      use = node_to_use_temps;
      ismove = node_to_is_move;
    }

let () = Graph.show interference_graph.graph
