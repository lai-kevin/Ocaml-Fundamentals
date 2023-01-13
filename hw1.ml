let rec pow (x: int) (n:int) =
  if n = 0 then 1
  else x * pow x (n - 1);;

let rec float_pow (x:float) (n:int) =
  if n = 0 then 1.
  else x *. float_pow x (n - 1);;

let rec compress lst = match lst with
  | [] -> []
  | [x] -> [x]
  | h::(a::t) -> if h = a then compress (a::t) else h::compress(a::t);;
  
let rec remove_if (lst: 'a list) (predicate: ('a -> bool)) = match lst with
  | [] -> []
  | h::t -> if (predicate h) then remove_if t predicate else h :: remove_if t predicate;;


let rec slice_helper lst i j c =
   match lst with
    | [] -> []
    | h::t -> if (i <= c) && (c < j) then h::slice_helper t i j (c+1) else slice_helper t i j (c+1);;

let slice lst i j =
  if i > j then [] else slice_helper lst i j 0;;

let is_prime n = match n with
  | 0 -> false
  | 1 -> false
  | _ -> let rec aux n a =
        if a = 1 then true
        else if n mod a = 0 then false 
        else aux n (a-1)
      in 
      aux n (n-1);;

let goldbachpair x =
  let rec goldbachpair_helper x y = 
    if (is_prime y && is_prime (x-y)) then (y, x-y) else goldbachpair_helper x (y+1)
  in
    goldbachpair_helper x 2;;

let equiv_on f g lst =
  let rec map func ls = 
    match ls with
    | [] -> []
    | h::t -> func h::map func t
  in 
  if (map f lst) = (map g lst) then true else false;;

let pairwisefilter cmp lst =
  let rec aux cmp lst result = match lst with
  | [] -> []
  | h::t -> match t with
    | [] -> h::result
    | h2::t2 -> (cmp h h2):: aux cmp t2 result
in aux cmp lst [];;

let polynomial lst = 
  let rec aux lst = match lst with
  | [] -> fun x -> 0
  | (co,ex)::t -> fun x -> co*(pow x ex) + aux t x
in aux lst;;

let rec powerset lst = match lst with
  | [] -> [[]]
  | h::t -> let rec map func ls = 
    match ls with
    | [] -> []
    | h::t -> func h::map func t
  in
  (powerset t) @ (map (fun any_list -> h::any_list) (powerset t));;



  
  

  




 









  



  


  
  
  


