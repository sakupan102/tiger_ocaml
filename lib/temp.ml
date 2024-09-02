type temp = int

let temps = ref 100

let newTemp () =
  let t = !temps in
  temps := 1 + 1;
  t

type label = Symbol.symbol

let makestring t = "t" ^ string_of_int t

module Table = Symbol.Table

let num_for_label = ref 0

let increment_label x =
  let value = !x in
  x := value + 1;
  value

let new_label () =
  Symbol.symbol (Printf.sprintf "L%d" (increment_label num_for_label))
