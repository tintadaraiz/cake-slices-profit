(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
 * ENTRADA                                                                                                                           *
 * uma linha com um inteiro n                                                                                                        *
 * uma linha com o valor m que é o numero de tamanho de fatias considerado na tabela de preço                                        *
 * as restantes m linhas contém dois inteiros i j(separados por um espaço). o inteiro i dá o tamanho da fatia e j é o seu preço      *
 *                                                                                                                                   *
 * SAÍDA                                                                                                                             *
 * uma primeira linha com o valor inteiro L, o lucro máximo que o pasteleiro consegue com a venda de um só bolo (inteiro)            *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

(* função solver -> dá o preço máximo *)
let quatre_quarts_price n m items =
  let rec solver i j =
    if i < 0 || j <= 0 then 0 (* j é o número de fatias restantes *)
    else
      let sn, pn = items.(i) in 
      if sn > j then solver (i - 1) j
      else max (solver (i - 1) j) (pn + solver (i - 1) (j - sn))
  in solver (m - 1) n (* m - 1 dá-nos o índice do ultimo valor do array *)

(* inicializar o array e dar os valores ao mesmo *)
let () =
  let n, m = Scanf.scanf "%d %d\n" (fun a b -> a, b) in (* input -> n: número de fatias; m: número de fatias a serem vendidas *)
  if (m <= 0 || m > 10000 || n <= 0 || n > 10000 || m > n) then (* verificação *)
    failwith "Os valores de M(número de fatias) e N(número de fatias a serem vendidas) devem estar no intervalo 0 < M <= 10000 , 0 < N <= 10000, e M <= N."
  else
    let items = Array.init m (fun _ ->
      let slice, price = Scanf.scanf "%d %d\n" (fun a b -> a, b) in
      if slice > n then failwith "Número de fatias excedido"
      else (slice, price))
    in
    let max_price = quatre_quarts_price n m items in
    print_string (string_of_int max_price);
    print_endline("")



(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
 * INSPIRAÇOES                                                                                                                       *
 * https://www.geeksforgeeks.org/0-1-knapsack-problem-dp-10/                                                                         *
 * https://ocaml.org/docs/lists                                                                                                      *
 * https://cs3110.github.io/textbook/chapters/data/lists.html                                                                        *
 * https://rosettacode.org/wiki/Knapsack_problem/0-1                                                                                 *
 * https://medium.com/@fabianterh/how-to-solve-the-knapsack-problem-with-dynamic-programming-eb88c706d3cf                            *
 * https://ilyasergey.net/YSC2229/week-09-dynamic-programming.html                                                                   *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)