(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
 * ENTRADA                                                                                                                           *
 * uma linha com um inteiro n                                                                                                        *
 * uma linha com o valor m que é o numero de tamanho de fatias considerado na tabela de preço                                        *
 * as restantes m linhas contém dois inteiros i j(separados por um espaço). o inteiro i dá o tamanho da fatia e j é o seu preço      *
 *                                                                                                                                   *
 * SAÍDA                                                                                                                             *
 * uma primeira linha com o valor inteiro L, o lucro máximo que o pasteleiro consegue com a venda de um só bolo (inteiro)            *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

(* linha com inteiro n -> número de fatias máximo *)
(*let n = read_int()

(* linha com o inteiro m -> número de diferentes números de fatias *)
let m = read_int() *)

(* lista *)
let rec combinations n xs =
  match n, xs with
  | 0, _ -> [[]]
  | _, [] -> []
  | k, x :: xs' ->
    let with_x = List.map (fun ys -> x :: ys) (combinations (k - 1) xs') in
    let without_x = combinations k xs' in
    with_x @ without_x

let () =
  let n = read_int () in
  let m = read_int () in
  let prices = Hashtbl.create m in
  for _ = 1 to m do
    let size, price = Scanf.scanf "%d %d\n" (fun x y -> x, y) in
    Hashtbl.add prices size price
  done;
  let max_profit =
    match Hashtbl.find_opt prices n with
    | Some price -> price - 20
    | None -> 0
  in
  let slices = ref [] in
  for i = 1 to n - 1 do
    slices := !slices @ combinations i (List.init (n - 1) (fun j -> j + 1)) @ [[]]
  done;
  let rec max_profit' = function
    | [] -> max_profit
    | slice :: slices' ->
      let profit = List.fold_left (fun acc size -> acc + Hashtbl.find prices size) 0 slice - 20 in
      max_profit' slices' |> max profit
  in
  max_profit' !slices |> Printf.printf "%d\n"


(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
* INSPIRAÇOES                                                                                                                       *
* https://www.geeksforgeeks.org/0-1-knapsack-problem-dp-10/                                                                         *
* https://ocaml.org/docs/lists                                                                                                      *
* https://cs3110.github.io/textbook/chapters/data/lists.html                                                                        *
* https://rosettacode.org/wiki/Knapsack_problem/0-1                                                                                 *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)