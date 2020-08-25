let print_list header l = 
    print_string header;
    List.iter (Printf.printf "%d ") l;
    print_endline ""


let print_list_of_list header l = 
    List.iter (print_list header) l


(* Partition a list into two parts, according to the pivot *)
let partition l p = 
    let rec aux acc list pivot = 
      match list with 
      | [] -> acc 
      | h::t -> let (left, right) = acc in 
                  if pivot >= h 
                  then aux (h::left, right) t pivot 
                  else aux (left, h::right) t pivot 
    in 
    aux ([], []) l p 


let rec quick_sort l =
    match l with
    | []  -> []
    (*this is the essential bottom case, otherwise dead-loop *)
    | [x] -> [x]  
    | pivot::tl -> 
      let (left, right) = partition l pivot in 
          (quick_sort left) @ (quick_sort right)


let rec quick_sort_2 l = 
    match l with 
    | [] -> []
    | pivot::t -> 
        let left  = List.filter (fun x -> x < pivot) t and 
            right = List.filter (fun x -> x >= pivot) t in 
        (quick_sort_2 left) @ [pivot] @ (quick_sort_2 right) 


let rec quick_sort_3 l =
    match l with
    | []  -> []
    (*this is the essential bottom case, otherwise dead-loop *)
    | [x] -> [x]  
    | pivot::tl -> 
      let (left, right) = List.partition (fun x -> x < pivot) tl in 
          (quick_sort_3 left) @ pivot::(quick_sort_3 right)

