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
let n = read_int()

(* linha com o inteiro m -> número de diferentes números de fatias *)
let m = read_int()

(* lista *)
    let fatias = []

    (* numero maximo de diferentes fatias é o m, portanto é preciso uma funcao que faca apenas m vezes *)
    let rec lista lst max =
        if (List.length lst < max) then 
            let (i,j) = Scanf.scanf "%d %d" (fun a b -> (a,b)) in 
            match lst with
            | [] -> [[i,j]]@lst
            | [i,_]::_ -> failwith "erro, repeticao de valores"
            | _::_ -> [[i,j]]@lst           
        else    
        
    
    (* função de leitura *)
    (* recebe dois inteiros separados por espaços e armazena-os num tuplo *)
    (* recursividade para ler os valores *)
    (* fazer uma função para adicionar tuplos a uma lista *)
    
            
    

    
        
(* ciclo para adicionar o nº de fatias + preço *)
(*let rec adc m = 
    let (num1, num2) = Scanf.scanf "%d %d" (fun a b -> (a,b));
    Hashtbl.add htbl (num1, num2)*)

(* input das fatias + preço para uma lista 
let fatias = 
    [1, 3;
     2, 5;
     3, 7;
     4, 9;
     5, 11;]*)

(* output *)
(* read_int |> *)

(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
 * INSPIRAÇOES                                                                                                                       *
 * https://www.geeksforgeeks.org/0-1-knapsack-problem-dp-10/                                                                         *
 * https://ocaml.org/docs/lists                                                                                                      *
 * https://cs3110.github.io/textbook/chapters/data/lists.html                                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)
