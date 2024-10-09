module FGraph = Graph.Graph

type flowgraph = {
  control : FGraph.graph;
  def : Temp.temp list FGraph.ITable.table;
  use : Temp.temp list FGraph.ITable.table;
  ismove : bool FGraph.ITable.table;
}
