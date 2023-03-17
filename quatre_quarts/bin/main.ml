(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
* ENTRADA *
* uma linha com um inteiro n *
* uma linha com o valor m que é o numero de tamanho de fatias considerado na tabela de preço *
* as restantes m linhas contém dois inteiros i j(separados por um espaço). o inteiro i dá o tamanho da fatia e j é o seu preço*

* SAÍDA *
* uma primeira linha com o valor inteiro L, o lucro máximo que o pasteleiro consegue com a venda de um só bolo (inteiro) 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

(* linha com inteiro n *)
let n = read_int()

(* linha com o inteiro m *)
let m = read_int()

 (* HashTable com o tamanho n*)
let htbl = Hashtbl.create m

(* ciclo para adicionar o nº de fatias + preço *)
let rec adc m = 
    let (num1, num2) = Scanf.scanf "%d %d" (fun a b -> (a,b));
    Hashtbl.add htbl (num1, num2)

