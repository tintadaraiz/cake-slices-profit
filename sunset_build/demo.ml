let n = read_int()
let m = read_int()

 let rec length acc l =
        match l with
        | [] -> acc
        | _ :: t -> length (acc + 1) t

    let (val1, val2) = Scanf.scanf "%d %d" (fun a b -> (a,b))

    let fatias = []
    let rec listafatia num = 
        match List.mem num fatias with
        | true -> if length fatias < num then
   	
