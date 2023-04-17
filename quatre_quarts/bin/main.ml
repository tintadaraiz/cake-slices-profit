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

(* lista recursiva *)
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

(* lista iterativa *)
let lista =
  let n = read_int() in
  let m = read_int() in
  if m<=n then
    let fatias = Hashtbl.create m in
    for i = 1 to m do
      let (i,j) = Scanf.scanf "%d %d\n" (fun a b -> (a,b)) in
      if i>n then failwith "o número de fatias tem de ser menor ou igual ao número de fatias do bolo" 
      else
        (if Hashtbl.mem fatias i = true then failwith "valor repetido"
         else 
          Hashtbl.add fatias i j)
    done
  else
        failwith "o valor de m tem de ser menor ou igual a n"
        



let fatias = 
  [1,2; 
   2,4;
   3,8;
   5,12;
   6,17;
   7,17;
   8,20;] 

let () = 
  let fatias = List.map (fun (nf,p) -> (nf,p, float p /.  float nf)) fatias in (* criar um ratio nfatias preço *)
  let fatias = List.sort (fun (_,_,val1) (_,_,val2) -> compare val1 val2) fatias in (* ordenar por ratio de nfatias *)
  let rec loop acc nfatias = function
  | ((_,nf,_) as fatia) :: tl -> 
    if float nf +. float nfatias != 8.0 (* depois alterar para o valor introduzido pelo utilizador *)
    then (nfatias, acc, fatia)
   else loop (fatia::acc) (nf + nfatias) tl
  | [] -> assert false 
  in
let nfatias, res, (nf,p,v) = loop [] 0 fatias in
print_endline "   Num_Fatias Preço";
let price =
  List.fold_left (fun price (nf,p,_) -> 
    Printf.printf " %7d: %6d\n" nf p;
    (p + price)
    ) 0 res
  in
  let rem_weight = 8 - nfatias in
  let last_price = v *. float rem_weight in 
  Printf.printf " %7d: %6f" rem_weight last_price;
  Printf.printf " Total Price: %.3f\n" (float price +. last_price);;


(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
 * INSPIRAÇOES                                                                                                                       *
 * https://www.geeksforgeeks.org/0-1-knapsack-problem-dp-10/                                                                         *
 * https://ocaml.org/docs/lists                                                                                                      *
 * https://cs3110.github.io/textbook/chapters/data/lists.html                                                                        *
 * https://rosettacode.org/wiki/Knapsack_problem/0-1                                                                                 *
 * https://medium.com/@fabianterh/how-to-solve-the-knapsack-problem-with-dynamic-programming-eb88c706d3cf                            *
 * https://ilyasergey.net/YSC2229/week-09-dynamic-programming.html                                                                   *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)