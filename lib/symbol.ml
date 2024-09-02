type symbol = string * int

let nextsym = ref 0
let sizeHint = 128
let hashtable : (string, int) Hashtbl.t =
    Hashtbl.create sizeHint

let symbol name =
   try 
       let i = Hashtbl.find hashtable name in
       (name, i)
    with _ -> 
        let i = !nextsym in
        nextsym := i + 1;
        Hashtbl.add hashtable name i;
        (name, i)

let next = ref 0

(* Use a hash table with destructive update.
   We'll never need previous versions of [string -> symbol]. *)
let hash_table : (string, int) Hashtbl.t = Hashtbl.create 10 (* FIXME *)

let create (name : string) : symbol =
  let i =
    match Hashtbl.find_opt hash_table name with
    | Some i -> i
    | None ->
        let i = !next in
        next := i + 1;
        Hashtbl.add hash_table name i;
        i
  in
  (name, i)

let name ((s, _) : symbol) : string = s

module Table = Tablemap.Make (struct
  type t = symbol

  let compare (_, i0) (_, i1) = Int.compare i0 i1
end)

type 'a table = 'a Table.table

let empty = Table.empty
let enter = Table.enter
let look = Table.look