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
let lista =
  let n = read_int() in
  let m = read_int() in
  if m <= n then
    let fatias = Hashtbl.create m in
    let rec addlista lst act =
      if (Hashtbl.mem lst act) = true then failwith "valor repetido"
      else 
        let (i,j) = Scanf.scanf "%d %d" (fun a b -> (a,b)) in 
        Hashtbl.add lst i j in
        addlista fatias 0
  else 
    failwith "o valor de m tem de ser menor ou igual a n"



  


(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
 * INSPIRAÇOES                                                                                                                       *
 * https://www.geeksforgeeks.org/0-1-knapsack-problem-dp-10/                                                                         *
 * https://ocaml.org/docs/lists                                                                                                      *
 * https://cs3110.github.io/textbook/chapters/data/lists.html                                                                        *
 * https://rosettacode.org/wiki/Knapsack_problem/0-1                                                                                 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)