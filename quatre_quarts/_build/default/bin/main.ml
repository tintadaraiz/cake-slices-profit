(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
 * ENTRADA                                                                                                                           *
 * uma linha com um inteiro n                                                                                                        *
 * uma linha com o valor m que é o numero de tamanho de fatias considerado na tabela de preço                                        *
 * as restantes m linhas contém dois inteiros i j(separados por um espaço). o inteiro i dá o tamanho da fatia e j é o seu preço      *
 *                                                                                                                                   *
 * SAÍDA                                                                                                                             *
 * uma primeira linha com o valor inteiro L, o lucro máximo que o pasteleiro consegue com a venda de um só bolo (inteiro)            *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)
(* funções auxiliares *)
let nslice slices i =
  let (s, _) = slices.(i) in s

let price slices i =
  let ( _, p) = slices.(i) in p

let quatre_quarts_price n slices =
  let num_slices = Array.length slices in 
    let m = Array.make (num_slices + 1) [||] in (* criação do array *)
    for i = 0 to num_slices do
      m.(i) <- Array.make (n + 1) 0
    done;

    (* operação principal *)
    for i = 1 to num_slices do
        for j = 1 to n do
          if nslice slices (i - 1) <= j
          then
            let p = price slices (i - 1) in
            m.(i).(j) <- max
                (m.(i - 1).(j))
                (m.(i).(j - nslice slices (i - 1)) + p)
          else m.(i).(j) <- m.(i - 1).(j)
        done
    done;
    (m.(num_slices).(n), m)

let () =
  let n, m = Scanf.scanf "%d %d\n" (fun a b -> a, b) in
  if m <= 0 || m > 10000 || n <= 0 || n > 10000 || m > n then (* verifica se os valores de M e N estão dentro dos limites especificados *)
    failwith "Os valores de m e n devem estar no intervalo 0 < m, n <= 10000, e m <= n."
  else
    let slices = Array.init m (fun _ -> (* lê os tamanhos das fatias e os seus preços *)
      let slice_size, slice_price = Scanf.scanf "%d %d\n" (fun a b -> a, b) in
      if slice_size > n then failwith "tamanho da fatia excedido"  (* verifica se o tamanho da fatia não é maior do que o tamanho do bolo *)
      else (slice_size, slice_price))
    in
    let max_profit f = match f with (* pattern matching para selecionarmos apenas o primeiro elemento do tuplo *)
    | (h,_) -> h
  in
    (quatre_quarts_price n slices |> max_profit |> print_int);
    print_endline("")


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