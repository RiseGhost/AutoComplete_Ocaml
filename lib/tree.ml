open Grama
type tree =
   Node of tree * char * grama list * tree
   | Leaf

let create_node value = Node(Leaf,Char.uppercase_ascii value,[],Leaf);;

let modulo x =
   if x >= 0 then x
   else           -x

let rec add_tree tree value =
   match tree with
   | Leaf -> create_node value
   | Node (l,v,n_g,r) as t -> 
      if (value == v)            then t
      else if (value > v) then   Node(add_tree l value,v,n_g,r)
      else                       Node(l,v,n_g,r);;

let rec tree_count tree =
   match tree with
   | Leaf -> 0
   | Node(l,_,_,r) -> 1 + tree_count l + tree_count r

let rigth_rotation tree =
   match tree with
   | Leaf -> tree
   | Node (l,v,n_g,r) ->
      match r with
      | Leaf -> tree
      | Node (_,rv,rn_g,rr) -> Node(Node(l,v,n_g,Leaf),rv,rn_g,rr)

let left_rotation tree =
   match tree with
   | Leaf -> tree
   | Node(l,v,n_g,r) ->
      match l with
      | Leaf -> tree
      | Node (ll,lv,ln_g,_) -> Node(ll,lv,ln_g,Node(Leaf,v,n_g,r))

let is_balance tree =
   match tree with
   | Leaf -> true
   | Node(l,_,_,r) ->
      if (modulo ((tree_count l) - (tree_count r)) > 1) then false
      else true

let print_node node =
   match node with
   | Leaf -> print_string("")
   | Node (_,v,n_g,_) ->
      print_string("Node value: ");
      print_char(v);
      print_string("  N-Grama: [");
      print_n_grama n_g;;

let rec tree_height tree =
   match tree with
   | Leaf -> -1
   | Node (l,_,_,r) -> 1 + max (tree_height l) (tree_height r);;

let rec init_tree t d =
   match d with
   | [] -> t
   | x::y -> init_tree (add_tree t x) y;;

let rec print_tree_list list =
   match list with
   | [] -> print_string("")
   | x::y -> print_node x; print_string ("  "); print_tree_list y;;

let rec tree_list_height tree height =
   match tree with
   | Leaf -> []
   | Node(l,_,_,r) as n ->
      if height == 0 then [n] @ tree_list_height r (height - 1) @ tree_list_height l (height - 1)
      else tree_list_height r (height - 1) @ tree_list_height l (height - 1)

let rec balance tree =
   match tree with
   | Leaf -> tree
   | Node (l,v,n_g,r) as n ->
      let l = balance l in
      let r = balance r in
      if (is_balance tree) then Node(l,v,n_g,r)
      else if (((tree_count l) - (tree_count r)) > 1) then balance (left_rotation n)
      else balance (rigth_rotation n)

let rec add_grama_tree tree grama =
   match tree with
   | Leaf -> tree
   | Node (l,v,n_g,r) ->
      let prefix_char = Char.uppercase_ascii (String.get grama.prefix 0) in
      if prefix_char == v then Node(l,v,update_n_grama_score n_g grama,r)
      else if prefix_char < v then  Node(l,v,n_g,add_grama_tree r grama)
      else                          Node(add_grama_tree l grama,v,n_g,r)

let print_tree_level tree =
   for h = 0 to (tree_height tree) do
      print_tree_list (tree_list_height tree h);
      print_newline();
      print_string("--------------------------");
      print_newline();
   done;;

let rec build_prefix data_list n =
   match data_list with
   | [] -> ""
   | x::y when n > 0 -> x ^ build_prefix y (n-1)
   | _ -> ""

let rec get_last_words word_list n =
   match word_list with
   | [] -> ""
   | _::y ->
      if n == List.length word_list then build_prefix word_list n
      else get_last_words y n

let rec treiner tree data n =
   match data with
   | [] -> tree
   | x::y when (List.length ([x] @ y)) > n -> 
      treiner (add_grama_tree tree (craete_grama (build_prefix ([x]@y) n) (List.nth ([x] @ y) n))) y n
   | _ -> tree

let create_model dic data n =
   let t = init_tree Leaf dic in
   let t = balance t in
   let t = treiner t data n in
   t

let rec get_predic tree word n =
  let value = Char.uppercase_ascii (String.get word 0) in
  match tree with
  | Leaf -> ""
  | Node (_,v,n_g,_) when v == value -> (find_on_n_grama n_g word).suffix
  | Node (_,v,_,r) when v > value -> get_predic r word n
  | Node (l,v,_,_) when v < value -> get_predic l word n
  | _ -> ""