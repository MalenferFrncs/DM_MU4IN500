
type 'a heapArray = int ref * int ref * ('a option) Array.t;;

let pere (i : int) = (i-1) /2;;

let fg (i : int) = 2*i +1;;

let fd (i : int) = 2*i +2;;

let makeEmptyHeapArray (sz: int) : ('a heapArray) = (ref 0, ref sz, (Array.make sz None));;


let copie (hp : 'a heapArray) (i : int) : 'a option = 
  let (_,sz, tab) = hp in
  if i >= !sz then None else tab.(i);;


(*Rajoute un rang dans le tas*)
let re_size (hp : 'a heapArray) : 'a heapArray = 
  let (ind, sz, tab) = hp in 
  let ntab = Array.init (Int.shift_left  !sz 1 ) (copie hp)
  in 
  sz := (Int.shift_left  !sz 1 ); 
  (ind, sz, ntab );;

let rec ajout (hp : 'a heapArray) (elt : 'a) : 'a heapArray = 
  let (ind, sz, tab) = hp in 
  let i = ref !ind and ind_pere = ref (pere !ind) in 
  try (
    tab.(!ind) <- Some(elt);
    ind := !ind +1;
    while (!i > 0 && (tab.(!i) < tab.(!ind_pere))) do   (*Pas de problème avec les None car on remonte*)
      let tmp = tab.(!i) in
      tab.(!i) <- tab.(!ind_pere);
      tab.(!ind_pere) <- tmp ; 
      i := !ind_pere;
      ind_pere := pere !i;
    done;
    hp
  ) with Invalid_argument s -> Printf.printf "coucou" ; (ajout (re_size hp) elt);;



let est_non_valide (i : int) (hp : 'a heapArray) : bool = 
  let (ind, _, tab) = hp in 
  if (fg i) > (!ind -1) then false    (*On est au bout du tas*) 
  else 
    match tab.(i) with 
    |Some(racine) ->
        (try (
           match (tab.(fg i), tab.(fd i)) with 
           | (None, None) -> false
           | (Some(eltg), None) -> racine > eltg
           | (Some(eltg), Some(eltd)) -> racine > eltg || racine > eltd
           | (None, Some(_)) -> failwith "Tas mal construit !"
         ) with Invalid_argument s -> 
           (
             match tab.(fg i) with
             | Some(eltg) -> racine > eltg
             | None -> false 
           )
        )
    |None -> false;; 
    

let supprMin (hp : 'a heapArray) : ('a option * 'a heapArray) = 
  let (ind, sz, tab) = hp in 
  try (
    let min = tab.(0) and i = ref 0 in 
    tab.(0) <- tab.(!ind -1);
    tab.(!ind -1) <- None; 
    ind := !ind -1;
    while (est_non_valide !i hp) do
      (try (
         match (tab.(fg !i), tab.(fd !i)) with 
         | (Some(eltg), None) -> let tmp = tab.(!i) in tab.(!i) <- tab.(fg !i) ; tab.(fg !i) <- tmp ; i := (fg !i)
         | (Some(eltg), Some(eltd)) -> 
             if eltg > eltd then let tmp = tab.(!i) in tab.(!i) <- tab.(fd !i) ; tab.(fd !i) <- tmp ; i := (fd !i)
             else let tmp = tab.(!i) in tab.(!i) <- tab.(fg !i) ; tab.(fg !i) <- tmp ; i := (fg !i)
         | (_,_) -> failwith "Cas impossible"
       ) with Invalid_argument s -> let tmp = tab.(!i) in tab.(!i) <- tab.(fg !i) ; tab.(fg !i) <- tmp ; i := (fg !i);
      )
    done;
    (min,hp)
  ) with Invalid_argument s -> (None, (makeEmptyHeapArray 0));; (*Cas où on retire le minimum d'un tas vide*)
      
    
    

let ajout_iteratif (l : 'a list) : 'a heapArray = 
  let rec aux (hp : 'a heapArray) (l : 'a list) : 'a heapArray =
    match l with
    | [] -> hp
    | h::tl -> let nhp = ajout hp h in (aux nhp tl)
  in (aux (makeEmptyHeapArray (List.length l)) l);;






type int128 = (Int32.t * Int32.t * Int32.t * Int32.t);;

let cmp_128 (cle1 : int128 ) (cle2 : int128) : int =
  let (a1, b1, c1, d1) = cle1 and (a2, b2, c2, d2) = cle2 in 
  if a1=a2 then 
    if b1=b2 then 
      if c1 = c2 then
        (Int32.unsigned_compare d1 d2)
      else
        (Int32.unsigned_compare c1 c2)
    else
      (Int32.unsigned_compare b1 b2) 
  else 
    (Int32.unsigned_compare a1 a2);;


let  (<) (c1:int128) (c2:int128) = (cmp_128 c1 c2 < 0) ;;
let (>) (c1:int128) (c2:int128) =  (cmp_128 c1 c2 > 0) ;;
let (=) (c1:int128) (c2:int128) = (cmp_128 c1 c2 = 0);;
                                  
       

let inf = (<) ;;
let eg = (=);;
         
         

let int_128_of_str(str:string):int128 =
  let x1 : Int32.t = Int32.of_string(String.sub str 0 10) in
  let x2 : Int32.t = Int32.of_string(String.cat "0x" (String.sub str 10 8)) in
  let x3 : Int32.t = Int32.of_string(String.cat "0x"(String.sub str 18 8)) in
  let x4 : Int32.t = Int32.of_string(String.cat "0x"(String.sub str 26 8)) in 
  (x1,x2,x3,x4)
;;
    

let l = ["0xdf6943ba6d51464f6b02157933bdd9ad";
         "0xd192acf4c06fe7c7df042f07d290bdd4";
         "0x2c15aed1a9eab93338d0348f12ef9a3b";
         "0x5f003a2587337655af8a166be8439a49"] in ajout_iteratif (List.map int_128_of_str l);; 
