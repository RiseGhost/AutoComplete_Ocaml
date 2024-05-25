let create_node = Tree.create_node
let modulo = Tree.modulo
let add_tree = Tree.add_tree
let tree_count = Tree.tree_count
let init_tree = Tree.init_tree
let balance = Tree.balance
let print_tree_level = Tree.print_tree_level
let add_grama_tree = Tree.add_grama_tree
let is_balance = Tree.is_balance
let create_model = Tree.create_model
let get_predic = Tree.get_predic
let get_last_words = Tree.get_last_words

let craete_grama = Grama.craete_grama
let print_grama = Grama.print_grama
let find_on_n_grama = Grama.find_on_n_grama


let read_data filename =
  let data = ref [] in
  let ic = open_in filename in
  try
     let rec read_lines() =
        try
           let line = input_line ic in
           data := !data @ (String.split_on_char ' ' line);
           read_lines()
        with
           | End_of_file -> close_in_noerr ic; !data in
     read_lines()
  with e ->
     close_in_noerr ic;
     raise e

let rec print_data data =
  match data with
  | [] -> print_newline()
  | x::[] -> print_string(x); print_newline()
  | x::y -> print_string(x); print_string(","); print_data y;;


