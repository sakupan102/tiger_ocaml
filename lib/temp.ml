type temp = int

let temps = ref 100

let newTemp () =
  let t = !temps in
  temps := t + 1;
  t

type label = Symbol.symbol

let makestring t = "t" ^ string_of_int t

module Table : Table.ITable with type key := temp = Table.MakeITable (struct
  type key = temp

  let equal = ( = )
  let hash = Hashtbl.hash
end)

let num_for_label = ref 0

let increment_label x =
  let value = !x in
  x := value + 1;
  value

let new_label () : label =
  Symbol.symbol (Printf.sprintf "L%d" (increment_label num_for_label))

let named_label name : label = Symbol.symbol name
