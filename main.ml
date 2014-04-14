
(* Return the last element of a list *)
let rec last = function  
    | []  -> None 
    | [x] -> Some x
    | _::tl   -> last tl 


(* Return the last and the second to last elements *)
let rec last_two = function
    | []         -> None
    | [x]        -> None
    | x::[y]     -> Some (x, y)
    | _::tl      -> last_two tl 


let rec at i = function 
    | [] -> None 
    | hd::tl -> if i = 1 then Some hd else at (i-1) tl 

let length l = 
    let rec aux n = function 
    | [] -> n 
    | _::tl -> aux (n+1) tl 
    in aux 0 l 


let rev l = 
    let rec aux r = function 
    | [] -> r 
    | hd::tl -> aux (hd::r) tl  
    in aux [] l 


let is_palindrome l = 
    l = List.rev l 

(* Get a sublist from the list 'l', 
   starting from the index 's' to 'e'.
   whether to include 'e' is easy to adapt. *)
let rec sublist s e l =
    if s > e then [] 
    else
        match l with 
        | []     -> []
        | hd::tl ->
          let next = sublist (s-1) (e-1) tl in  
            (* move forwards till the case s=0 *)
            if s > 0 then next
            else
              (* move forwards till e=0, in the right zone *)
              if e > 0 then hd::next
              else []


let rec append_list a b = 
    match a with 
    | []     -> b 
    | hd::tl -> hd::(append_list tl b) 


type 'a node = 
    | One of 'a 
    | Many of 'a node list ;; 


let rec flatten node_list = 
    match node_list with 
    | []      -> []
    | (One a)::tl   -> a::(flatten tl)
    | (Many al)::tl -> (flatten al)@(flatten tl)


let compress l = 
    let rec aux acc = function 
    | [] -> List.rev acc 
    | hd::tl -> match acc with 
                | []   -> aux (hd::[]) tl 
                | h::t -> if h = hd then aux acc tl 
                          else aux (hd::acc) tl 
    in aux [] l 


(* sliding window of size 2, proceeding one by one! *)
let rec compress2 = function 
    | a::(b::tl as c) -> if a = b then compress2 c 
                         else a::(compress2 c)
    | smaller  -> smaller 


(* pack consecutive duplicate elements of list into sublist. *)
let pack l = 
    let rec aux cur acc l = 
    match l with 
    | [] -> [] 
    | [x] -> (x::cur)::acc 
    | a::(b::_ as tl) -> if a = b then aux (a::cur) acc tl
                         else aux [] ((a::cur)::acc) tl 
    in List.rev (aux [] [] l) 


let pack2 l = 
    let rec aux cur acc = function 
    | [] -> List.rev acc  
    | hd::tl -> if (List.length cur) = 0 then aux [hd] acc tl 
                else 
                  if hd = (List.nth cur 0) then aux (hd::cur) acc tl 
                  else aux [hd] (cur::acc) tl 
    in aux [] [] l 


(* run-length encoding of a list *)
let encode l = 
    let rec aux cur acc = function 
    | [] -> acc 
    | [x] -> ((List.length cur), x)::acc 
    | a::(b::_ as tl) -> if a = b then aux (a::cur) acc tl
                         else aux [] ((((List.length cur)+1), a)::acc) tl
    
    in List.rev (aux [] [] l) 


(* Duplicate elements of a list *)
let rec duplicate = function 
    | [] -> []
    | hd::tl -> hd::hd::(duplicate tl)


let duplicate2 l = 
    let rec aux acc = function 
    | [] -> List.rev acc 
    | hd::tl -> aux (hd::(hd::acc)) tl 
    in 
      aux [] l 


(* Replicate each element in the list 'l' for t times. *)
let replicate l t = 
    let rec aux acc n = function 
    | [] -> List.rev acc 
    | hd::tl as k -> if n = 1 then aux (hd::acc) t tl 
                     else aux (hd::acc) (n-1) k 
    in 
      aux [] t l


(* Drop every t'th element from the list *)
let drop_at l t = 
    let rec aux acc n = function 
    | [] -> List.rev acc 
    | hd::tl -> if n = 1 then aux acc t tl 
                else aux (hd::acc) (n-1) tl 
    in 
      aux [] t l 


(* Splite a list into two sublists. 
    The length of the first sublist is n. *)
let split l n = 
    if n >= (List.length l) then (l, []) 
    else 
        let rec aux c lacc racc = function 
        | [] -> (List.rev lacc, List.rev racc)
        | hd::tl -> if c > 0 then aux (c-1) (hd::lacc) racc tl 
                    else aux (-1) lacc (hd::racc) tl  
    in 
        aux n [] [] l 


let rotate l i = 
    if abs i >= List.length l then l 
    else 
        let rec aux c acc = function 
        | [] -> [] 
        | hd::tl as k -> if c > 0 then aux (c-1) (hd::acc) tl 
                         else k@(List.rev acc)
        in 
        if i > 0 then aux i [] l 
        else aux ((List.length l) - (abs i)) [] l 


let insert_at e i l = 
    if i >= List.length l then l@[e]
    else
        let rec aux e c acc = function 
        | [] -> List.rev acc 
        | hd::tl as k -> if c > 0 then aux e (c-1) (hd::acc) tl 
                         else (List.rev (e::acc))@k 
    in 
        aux e i [] l 


let permutation l = 
    let extract_rand a = 
      let _ = Random.self_init in 
        let i = Random.int (List.length a) in 
          ((List.nth a i), drop_at a i)
    in 
    let rec aux acc = function 
    | [] -> List.rev acc 
    | k  -> let (picked, rest) = extract_rand k in 
            aux (picked::acc) rest 
    in 
      aux [] l 


let reposition a i = 
    let _ = Random.self_init in 
    let j = i + Random.int ((Array.length a)-i) in 
      if j != i then let tmp = a.(j) in 
                     let _ = a.(j) <- a.(i) in 
                     let _ = a.(i) <- tmp in a 
      else a 


let shuffle l = 
    let a = Array.of_list l in 
      for i=0 to ((Array.length a)-1) 
      do 
            reposition a i
      done; 
      Array.to_list a


type 'a b_tree = 
  | Empty 
  | Node of 'a * 'a b_tree * 'a b_tree 
;; 


(* Construct a binary search tree from a list of integer numbers *)
let construct l = 
    let rec add_node tree list = 
    match list with 
    | [] -> tree 
    | hd::tl -> 
        match tree with 
        | Empty -> add_node (Node(hd, Empty, Empty)) tl
        | Node(a, l, r) -> if hd <= a then Node(a, (add_node l list), r)
                           else Node(a, l, (add_node r list))
    in
      add_node Empty l 


let rec print_b_tree tree = 
    match tree with 
    | Empty -> print_string "Empty"
    | Node(a, l, r) -> Printf.printf "Node(%d," a; 
                       print_b_tree l; print_string ",";  
                       print_b_tree r; print_endline ")"


let print_list l = 
    List.iter (Printf.printf "%d ") l;
    print_endline ""


(* The main function. The entrance of the program. *)

let _ =
    match Array.length Sys.argv with 
    | 1 | 2  -> 
      let a = [ 1; 2; 3; 4; 5] in 
      let b = [6; 7; 8] in
      let c = [7; 6; 8] in
      begin 
        match (last_two [1]) with 
        | None -> print_endline "None"
        | Some (x, y) -> Printf.printf "%d,%d\n" x y 
      end; 
      let perm = permutation a in 
      let b_tree = construct c in 
    (*
      let l = append_list a b in 
      let il = insert_at 8 5 a in 
      let rotate = rotate a (-3) in 
      let (ls, rs) = split a 3 in 
      let shuffle = shuffle l in 
      let cl = compress2 l in 
      let pl = pack2 l in 
      let rl = replicate a 2 in 
      let dl = drop rl 2 in 
      let sl = rev l in  
     *)
          print_list perm;
          print_b_tree b_tree

    | _      -> exit 1 





