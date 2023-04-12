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
    (* função de leitura *)
    let (val1, val2) = Scanf.scanf "%d %d" (fun a b -> (a,b)) (* recebe dois inteiros separados por espaços e armazena-os num tuplo *)
    (* recursividade para ler os valores *)
    (* fazer uma função para adicionar tuplos a uma lista *)
        
            
    let fatias = []
    let rec listafatia num = 
        match List.mem num fatias with
        | true -> if List.length fatias < num then  

    
        
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