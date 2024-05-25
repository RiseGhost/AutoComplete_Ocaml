open Predic
let dic = ['A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z']
let n = 2

let () =
   let data = read_data "data.txt" in
   let t = create_model dic data n in
   let i = String.split_on_char ' ' (read_line()) in
   print_endline (get_predic t ( get_last_words i n ) n);;

(*
Compile code:
   dune build
   dune exec bin/main.exe
*)