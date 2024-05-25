# AutoComplete em Ocaml:

Consiste na implementação de um **N-Grama** em Ocaml.

## Como utilizar:
A sua utilização é bastante simples, assim como ilustra o exemplo:
```Ocaml
open Predic
let dic = ['A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z']
let n = 2

let () =
   let data = read_data "data.txt" in
   let t = create_model dic data n in
   let i = String.split_on_char ' ' (read_line()) in
   print_endline (get_predic t ( get_last_words i n ) n);;

```
## Como configurar o modelo utilizado para treino:
O modelo utiliza o conteúdo que está presente no ficherio *"data.txt"*.

Se pretender basta alterar o conteúdo e a IA muda o seu comportamento.

## Project build and execute:
Para dar build o projeto baste executar:
```Shell
dune build
```

Para executar pode utilizar o comando:
```Shell
dune exec bin/main.exe
```

Após ser feito o **build** é gerado um executável na diretoria */_build/bin/main.exe*.