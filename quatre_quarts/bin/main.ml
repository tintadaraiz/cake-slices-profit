(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
 * Trabalho realizado por:                                                                                                           *
 * Rodrigo Silva (48069)                                                                                                             *
 * Leonardo Santos (48990)                                                                                                           *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

(* funções auxiliares *)
let nslice slices i =
  let (s, _) = slices.(i) in s (* selecionar apenas o número de fatias *)

let price slices i =
  let ( _, p) = slices.(i) in p (* selecionar apenas o preço do número de fatias *)

let max_profit f = match f with (* pattern matching para selecionar apenas o primeiro elemento do tuplo, ou seja, o preço máximo *)
    | (h,_) -> h

    
(* função principal *)
(* tipagem: int -> (int * int) array -> int * int array array *)
(* tuplo de int, int array array (matriz de inteiros) *)
(* criação do array dos preços máximos *)
(* inicialização de todos os elementos da matriz com 0 *)
let quatre_quarts_price n slices = 
  let num_slices = Array.length slices in 
    let m = Array.make (num_slices + 1) [||] in 
    for i = 0 to num_slices do
      m.(i) <- Array.make (n + 1) 0 
    done;

    (* algoritmo *)
    (* se o tamanho do bolo atual for maior ou igual ao tamanho da fatia, *) 
    (* o algoritmo compara o preço da fatia com o preço máximo possível *)
    (* para um bolo com o tamanho atual menos o tamanho da fatia *)
    (* e escolhe o máximo entre esses dois valores *)
    (* se o tamanho do bolo atual for menor que o tamanho da fatia, *)
    (* o preço máximo possível permanece o mesmo do bolo anterior. *)
   
    for i = 1 to num_slices do
        for j = 1 to n do
          if 
            nslice slices (i - 1) <= j
            then    
                  if nslice slices (i - 1) = 3
                  then  m.(i).(j) <- m.(i - 1).(j)
                else                         
            let p = price slices (i - 1) in    
            m.(i).(j) <- max                   
                (m.(i - 1).(j))            
                (m.(i).(j - nslice slices (i - 1)) + p) 
          else 
            m.(i).(j) <- m.(i - 1).(j)  
        done                                    
    done;
    (m.(num_slices).(n), m)


(* criação do array que contém o número de fatias e o preço + preenchimento do array com os valores *)
(* verifica se os valores de M e N estão dentro dos limites especificados *)
(* lê os tamanhos das fatias e os seus preços *)
(* verifica se o tamanho da fatia não é maior do que o tamanho do bolo *)
let () =
  let n, m = Scanf.scanf "%d %d\n" (fun a b -> a, b) in
  if m <= 0 || m > 10000 || n <= 0 || n > 10000 || m > n then
    failwith "Os valores de m e n devem estar no intervalo 0 < m, n <= 10000, e m <= n."
  else
    let slices = Array.init m (fun _ -> 
      let slice_size, slice_price = Scanf.scanf "%d %d\n" (fun a b -> a, b) in
      if slice_size > n then failwith "tamanho da fatia excedido"  
      else (slice_size, slice_price))
    in
    (quatre_quarts_price n slices |> max_profit |> print_int);
    print_endline("")


(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
 * INPUT                                                                                                                             *
 * uma linha com um inteiro n                                                                                                        *
 * uma linha com o valor m que é o numero de tamanho de fatias considerado na tabela de preço                                        *
 * as restantes m linhas contém dois inteiros i j(separados por um espaço). o inteiro i dá o tamanho da fatia e j é o seu preço      *
 *                                                                                                                                   *
 * OUTPUT                                                                                                                            *
 * uma primeira linha com o valor inteiro L, o lucro máximo que o pasteleiro consegue com a venda de um só bolo (inteiro)            *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
 * DEMONSTRAÇÃO                                                                                                                      *
 * (com o Dune)                                  (sem o Dune)                                                                        *
 * (INPUT)                                                                                                                           *
 * > dune build                                   > ocamlopt main.ml                                                                 *
 * > dune exec quatre_quarts                      > ./a.out                                                                          *
 *                                                                                                                                   *
 * > 10                                                                                                                              *
 * > 7                                                                                                                               *
 * > 2 3                                                                                                                             *
 * > 3 5                                                                                                                             *
 * > 4 3                                                                                                                             *
 * > 6 9                                                                                                                             *
 * > 7 4                                                                                                                             *
 * > 8 7                                                                                                                             *
 * > 9 8                                                                                                                             *
 * (OUTPUT)                                                                                                                          *
 * > 16                                                                                                                              *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
 * INSPIRAÇOES                                                                                                                       *
 * https://www.geeksforgeeks.org/0-1-knapsack-problem-dp-10/                                                                         *
 * https://v2.ocaml.org/api/Array.html                                                                                               *
 * https://cs3110.github.io/textbook/chapters/data/lists.html                                                                        *
 * https://rosettacode.org/wiki/Knapsack_problem/0-1                                                                                 *
 * https://medium.com/@fabianterh/how-to-solve-the-knapsack-problem-with-dynamic-programming-eb88c706d3cf                            *
 * https://ilyasergey.net/YSC2229/week-09-dynamic-programming.html                                                                   *
 * https://dev.realworldocaml.org/imperative-programming.html    tópico "Memoization and Dynamic Programming"                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)