module type RegAlloc = sig
  type allocation = Frame.register Temp.Table.table

  val alloc : Assem.instr list * Frame.frame -> Assem.instr list * allocation
end

module RegAlloc : RegAlloc = struct
  module MakeGraph = MakeGraph.MakeGraph
  module Color = Color.Color
  module Liveness = Liveness.Liveness

  type allocation = Frame.register Temp.Table.table

  let alloc (instrs, _) =
    let flow_graph, _ = MakeGraph.instrs_to_graph instrs in
    let interference_graph = Liveness.interference_graph flow_graph in
    let temp_to_register, _ =
      Color.color
        {
          interference = interference_graph;
          registers = Frame.registers;
          initial = Frame.default_temp_map;
          spill_cost = (fun _ -> 0);
        }
    in
    ( List.map (fun instr -> Assem.format instr temp_to_register) instrs,
      temp_to_register )
end
