
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
    let rec aux acc = function 
    | [] -> acc
    | hd::tl -> aux (hd::acc) tl  
    in aux [] l 


let rec count_digit i = 
    let remain = i / 10 in 
      if remain = 0 then 1 
      else 1+(count_digit remain)

(*
let rec reverseInt i = 
    let digit = i mod 10.0 in 
    let remain = i / 10.0 in 
    let rev_remain = reverseInt remain in 
    let num_digit = count_digit rev_remain in 
        digit * (10.0 ** (num_digit+1)) + rev_remain
*)


let is_palindrome l = 
    l = List.rev l 

(* Get a sublist from the list 'l', 
   starting from the index 's' to 'e'.
   whether to include 'e' is easy to adapt. *)
let sublist si ei list =
    if si > ei then []
    else if si >= List.length list then [] 
    else
        let rec aux s e = function 
        | []     -> []
        | hd::tl ->
          let next = aux (s-1) (e-1) tl in  
            (* move forwards till the case s=0 *)
            if s > 0 then next
            else
              (* move forwards till e=0, in the right zone *)
              if e > 0 then hd::next
              else []
        in 
          aux si ei list 


(* a tail-recursive version of the sublist function. *)
let sublist_2 si ei list = 
    if si > ei then []
    else if si >= List.length list then []
    else 
        let rec aux acc s e = function 
            | [] -> List.rev acc 
            | hd::tl -> 
                if s > 0 then aux acc (s-1) (e-1) tl 
                else if e > 0 then aux (hd::acc) (s-1) (e-1) tl 
                else List.rev acc 
        in 
          aux [] si ei list 


let rec append_list a b = 
    match a with 
    | []     -> b 
    | hd::tl -> hd::(append_list tl b) 


(* Calculate Fibonacci number. Complexity O(N^2) size of complete b-tree. *)
let rec fib n = 
    if n <= 0 then 0 
    else if n = 1 || n = 2 then 1 
    else (fib (n-1)) + (fib (n-2))


(* Complexity O(N) *)
let fib_2 n = 
    if n <= 0 then 0 
    else 
    let rec aux f1 f2 c = 
      if c <= 0 then f2 
      else aux f2 (f1+f2) (c-1) 
    in 
      aux 0 1 (n-1) 


(* Get all the possible combination of K elements from a list of N elements. 
 *  
 * Complexity: N^k ? 
 *)
let rec comb l k = 
    if k <= 0 then []
    else if k > List.length l then [] 
    else if k = List.length l then [l] 
    else 
    (* Subfunction: add the element a to each list element in l *)
    let rec add_to_each l a = 
        match l with
        | [] -> [[a]]
        | [x]-> [a::x]   (* avoid an additional add by recursion. *)
        | hd::tl -> (a::hd)::(add_to_each tl a)
    in 
    match l with
    | hd::tl -> let r = comb tl (k-1) in 
                (add_to_each r hd)@(comb tl k)


(* vectorize the list into a list of sublist with the size of K. 
 * Complexity: N
 *)
let vectorize k l = 
    if k <= 0 then []
    else if k >= List.length l then [l]
    else
      let rec aux acc vec c = function
        | [] -> List.rev (vec::acc)
        | (hd::tl as w) -> if c > 0 then 
                      aux acc (hd::vec) (c-1) tl
                    else 
                      aux ((List.rev vec)::acc) [] k w
      in
        aux [] [] k l


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
                  if hd = (List.hd cur) then aux (hd::cur) acc tl 
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


(* Drop every t'th element from the list.
   BUGGY!  the index of the first element is 1. 
*)
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


(* Rotate the sublist starting from i to the beginning. *)
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
    (*It doesnot seem to be necessary to reverse the list*)
    | [] -> List.rev acc  
    | k  -> let (picked, rest) = extract_rand k in 
            aux (picked::acc) rest 
    in 
      aux [] l 


let reposition a i = 
    let j = i + Random.int ((Array.length a)-i) in 
      if j != i then let tmp = a.(j) in 
                     let _ = a.(j) <- a.(i) in 
                     let _ = a.(i) <- tmp in a 
      else a 


let shuffle l = 
    let _ = Random.self_init in 
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


(* Something wrong with the following function! *)
let construct l = 
    let rec aux root list = 
    match list with 
    | [] -> root
    | hd::tl -> 
        match root with 
        | Empty -> aux (Node(hd, Empty, Empty)) tl
        | Node(a, l, r) -> if hd <= a then Node(a, (aux l list), r)
                           else Node(a, l, (aux r list))
    in
      aux Empty l 


(* Construct a binary search tree from a list of integer numbers *)
let construct2 l = 
    let rec insert tree value = match tree with 
    | Empty -> Node(value, Empty, Empty) 
    | Node(a, l, r) -> if value = a then tree 
                       else if value < a then Node(a, (insert l value), r)
                       else Node(a, l, (insert r value))
    in 
      List.fold_left insert Empty l


let rec count_leaves tree = match tree with  
    | Empty -> 0 
    | Node(_, Empty, Empty) -> 1
    | Node(_, l, r) -> (count_leaves l) + (count_leaves r)


let rec print_b_tree tree = 
    match tree with 
    | Empty -> print_string "Empty"
    | Node(a, l, r) -> Printf.printf "Node(%d," a; 
                       print_b_tree l; print_string ",";  
                       print_b_tree r; print_endline ")"


(* The greatest common dividor *)
let rec gcd a b = 
    if b = 0 then a 
    (* if b > a then gcd b a   No need to do so! *) 
    else gcd b (a mod b) 


let is_coprime a b = 
    if gcd a b = 1 then true else false 


let is_prime n = 
    let a = abs n in 
    let rec is_not_divisor m = 
      m*m > a || ((a mod m) <>0 && is_not_divisor (m+1)) 
    in 
      n <> 1 && is_not_divisor 2 


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

(* The main function. The entrance of the program. *)


(* Given a list, return a list of tuple (i, v, max_i, max_v):
   i: the index of each element.
   v: the value of the element at i. 
   max_i: the index of the max element following the element 'i'.
   mav_v: the value of the max element. *)
let best_sell_list l = 
    let size = List.length l in 
    let (hd, res) =  
        List.fold_right 
        (fun a ((c, i, mi, mv), acc) -> 
            let item = 
                if a > mv then (c-1, a, c-1, a) 
                else (c-1, a, mi, mv) in 
            (item, item::acc)
        )  
        l ((size, 0, 0, min_int), []) 
    in 
      res

(* Find the best buy and sell point to maximize the profit. *)
let best_buy_and_sell l = 
    let bslist = best_sell_list l in 
    List.fold_left 
        (fun (ai, av, ami, amv) (bi, bv, bmi, bmv) -> 
            let adiff = amv - av and 
                bdiff = bmv - bv in 
            if bdiff > adiff 
            then (bi, bv, bmi, bmv)
            else (ai, av, ami, amv)
        )
        (0, 0, 0, min_int) bslist


let print_best_sell_list l = 
    List.iter 
    (fun (i, v, mi, mv) -> 
        Printf.printf "%d, %d: %d, %d\n" i v mi mv) l 


let print_distance_max_result r = 
    match r with
    | (minIn, maxIn, maxDiff)
      -> Printf.printf "minIndex: %d, maxIndex: %d, maxDiff: %d\n"
          minIn maxIn maxDiff 


(* Distance maximizing problem, i.e. best buy and sell points *)
let distance_max_index = function 
    | []  -> (-1, -1, -1)
    | [x] -> (-1, -1, -1)
    | a::b::tl -> 
        let rec aux minIn maxIn curMin maxDiff c l = 
          match l with 
          | [] -> (minIn, maxIn, maxDiff)
          | hd::tl -> 
              let dis = hd - curMin in 
              let newMaxDiff = 
                  if dis > maxDiff then dis else maxDiff in 
              let newCurMin = 
                  if hd < curMin then hd else curMin in
              let newMinIn = 
                  if hd = newCurMin then c else minIn in
              let newMaxIn = 
                  if dis = newMaxDiff then c else maxIn 
              in
                aux newMinIn newMaxIn newCurMin newMaxDiff (c+1) tl 
         in
           if a < b then aux 1 2     a (b-a) 3 tl
           else          aux 2 (-1)  b    0  3 tl


let trim_str str = 
    let rec aux s i acc = 
        if i = (String.length s) - 1  then
          List.rev (s.[i]::acc)
        else 
          if ' ' = s.[i] && ' ' = s.[i+1] then
            aux s (i+1) acc
          else
            aux s (i+1) (s.[i]::acc) 
    in 
      aux str 0 []




let _ =
    match Array.length Sys.argv with 
    | 1 | 2  -> 
      let a = [ 1; 2; 3; 4; 5] in 
      (* let cb = vectorize 2 a in *)
      let cb = comb a 2 in
      let s = [ 3; 2; 1; 4; 5] in 
      let b = [6; 7; 8] in
      let c = [7; 6; 8] in
      let d = [ 3; 2; 5; 4; 1] in 
      let m = 9 and n = 3 in
      begin 
        match (last_two [1]) with 
        | None -> print_endline "None"
        | Some (x, y) -> Printf.printf "%d,%d\n" x y 
      end; 
      let perm = permutation a in 
      let b_tree = construct2 a in 
      let sl = sublist_2 3 7 a in 
      let ql = quick_sort_3 s in 
      let shuffle = shuffle a in 
    (*
      let l = append_list a b in 
      let il = insert_at 8 5 a in 
      let rotate = rotate a (-3) in 
      let (ls, rs) = split a 3 in 

      let cl = compress2 l in 
      let pl = pack2 l in 
      let rl = replicate a 2 in 
      let dl = drop rl 2 in 
      let sl = rev l in  
     *)

      let str_test = "  a  b   c" in 
      let str_array = trim_str str_test in 
      List.iter (Printf.printf "%c") str_array;
      print_endline "@string_end";
      
      let rev_i = 21 in 
      Printf.printf "%d:  num of digits:%d\n" rev_i (count_digit rev_i);

      print_distance_max_result (distance_max_index a);
      print_list_of_list "#" cb;
      print_list "input:" d;
      print_best_sell_list (best_sell_list d);    
      print_endline "best_buy_and_sell_point:";
      print_best_sell_list [best_buy_and_sell d];
      Printf.printf "fib %d: %d\n" 10 (fib_2 10); 
      print_list "sublist:" sl;
      print_list "shuffle:" shuffle;
      print_b_tree b_tree;
      Printf.printf "%s\n" (string_of_bool (is_coprime m n))

    | _      -> exit 1 





