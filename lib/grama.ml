type grama = {
  prefix : string;
  suffix : string;
  score : int;
}

let craete_grama p s = {
  prefix = p;
  suffix = s;
  score = 1
}

let rec contain_grama n_g g =
  match n_g with
  | [] -> false
  | x::y ->
    if (x.prefix == g.prefix && x.suffix == g.suffix) then true
    else contain_grama y g

let update_grama_score g = {
  prefix = g.prefix;
  suffix = g.suffix;
  score = 1 + g.score
}

let update_n_grama_score n_grama g =
  if contain_grama n_grama g then
    let n_g = ref [] in
    for index = 0 to (List.length n_grama) - 1 do
      let element = List.nth n_grama index in
      if element.prefix == g.prefix && element.suffix == g.suffix then n_g := !n_g @ [update_grama_score element]
      else n_g := !n_g @ [element]
    done;
    !n_g
  else
    n_grama @ [g]

let print_grama grama = 
  print_string("Prefix: ");
  print_string(grama.prefix);
  print_string(", Suffix: ");
  print_string(grama.suffix);
  print_string(", Score: ");
  print_int(grama.score)

let rec print_n_grama n_grama =
  match n_grama with
  | [] -> print_char(']')
  | x::y -> print_grama x; print_n_grama y;;

let find_on_n_grama n_grama prefix =
  let max  = ref (craete_grama prefix "") in
  for index = 0 to (List.length n_grama) - 1 do
    let element = List.nth n_grama index in
    if element.prefix = prefix && element.score == !max.score then max := element
    else max := !max
  done;
  !max