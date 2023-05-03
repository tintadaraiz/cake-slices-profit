(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
 * ENTRADA                                                                                                                           *
 * uma linha com um inteiro n                                                                                                        *
 * uma linha com o valor m que é o numero de tamanho de fatias considerado na tabela de preço                                        *
 * as restantes m linhas contém dois inteiros i j(separados por um espaço). o inteiro i dá o tamanho da fatia e j é o seu preço      *
 *                                                                                                                                   *
 * SAÍDA                                                                                                                             *
 * uma primeira linha com o valor inteiro L, o lucro máximo que o pasteleiro consegue com a venda de um só bolo (inteiro)            *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

(* dynamic programming  *)
let validate_input n m =
  if m <= 0 || m > 10000 || n <= 0 || n > 10000 || m > n then
    failwith "Os valores de M(número de fatias) e N(número de fatias a serem vendidas) devem estar no intervalo 0 < M <= 10000 , 0 < N <= 10000, e M <= N."

let quatre_quarts_price n m slices =
  let memo = Array.make_matrix (m+1) (n+1) 0 in
  for i = 1 to m do
    let fatia, preco = slices.(i-1) in
    for j = 1 to n do
      if fatia > j then memo.(i).(j) <- memo.(i-1).(j)
      else memo.(i).(j) <- max (memo.(i-1).(j)) (preco + memo.(i-1).(j-fatia))
    done;
  done;
  memo.(m).(n)

let () =
  let n, m = Scanf.scanf "%d %d\n" (fun a b -> a, b) in
  validate_input n m;
  let slices = Array.init m (fun _ ->
    let fatia, preco = Scanf.scanf "%d %d\n" (fun a b -> a, b) in
    if fatia > n then failwith "Número de fatias excedido";
    (fatia, preco))
  in
  let max_price = quatre_quarts_price n m slices in
  print_string (string_of_int max_price ^ "\n");



(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
 * INSPIRAÇOES                                                                                                                       *
 * https://www.geeksforgeeks.org/0-1-knapsack-problem-dp-10/                                                                         *
 * https://v2.ocaml.org/api/Array.html                                                                                               *
 * https://cs3110.github.io/textbook/chapters/data/lists.html                                                                        *
 * https://rosettacode.org/wiki/Knapsack_problem/0-1                                                                                 *
 * https://medium.com/@fabianterh/how-to-solve-the-knapsack-problem-with-dynamic-programming-eb88c706d3cf                            *
 * https://ilyasergey.net/YSC2229/week-09-dynamic-programming.html                                                                   *
 * https://dev.realworldocaml.org/imperative-programming.html    Topic Memoization and Dynamic Programming                           *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)