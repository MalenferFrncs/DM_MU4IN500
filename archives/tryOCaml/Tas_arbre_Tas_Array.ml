

(*Ce code est inspiré par la structure proposée par Chris Okasaki dans Purely Functionnal Data Structures
  le rang est défini comme étant la distance vers l'emplacement vide le plus proche*)
(* Noeud ( rang,ndescendances, elt, fg, fd*)
type 'a heapTree = E | L of 'a | N of int * int *  'a * 'a heapTree * 'a heapTree;;

let rank (t: 'a heapTree) : int=
  match t with
  | E -> 0
  | L (_) -> 1
  | N (r,_,_,_,_) -> r ;;

let elt (t : 'a heapTree ) : 'a = 
  match t with 
  | E -> failwith "empty heap"
  | L(e) -> e
  | N (_,_,e,_,_) -> e
    
let nbdesc (t : 'a heapTree) : int = 
  match t with 
  |  E -> 0
  | L(_) -> 0
  | N(_,nd,_,_,_) -> nd;;
          



let elemSurDernierRang (h :'a  heapTree) = nbdesc h > (Int.shift_left 1 ((rank h)))-2
                                           
                     
(*Fonctions auxiliaires servant à faire des modifications locales pour garder la propriété du tas
  énonçant que la racine est inférieure à la racine de ses fils *) 
let reeq_tas_droite (t : 'a heapTree) (nfd : 'a heapTree) (res : 'a) : 'a heapTree * 'a = 
  match t with 
  | N(r,d, e, fg, fd) -> 
            (*Garder la propriété du tas*)
      if e < res then 
        ( (*On a besoin de rééquilibrer avec l'arbre droite dans le cas où on change de racine du nœud*)
          match fg with 
          | E -> (N (( min (rank fg) (rank nfd))+1, d-1, res, fg, nfd),e) 
          | L(efg) -> 
              if efg < res then 
                (N (( min (rank fg) (rank nfd))+1, d-1, efg, L(res), nfd),e)
              else 
                (N (( min (rank fg) (rank nfd))+1, d-1, res, fg, nfd),e)
          |N(rfg,dfg,efg,fgfg,fgfd) -> 
              if efg < res then 
                (N (( min (rank fg) (rank nfd))+1, d-1, efg, N(rfg,dfg,res,fgfg,fgfd), nfd),e)
              else 
                (N (( min (rank fg) (rank nfd))+1, d-1, res, fg, nfd),e)
        )
      else (*res est déjà le minimum dans ce tas, on le renvoie sans modifier le reste*)
        (N (( min (rank fg) (rank nfd))+1, d-1, e, fg, nfd),res) 
  | _ -> failwith "Invalid Argument"
           
let reeq_tas_gauche (t : 'a heapTree) (nfg : 'a heapTree) (res : 'a) : 'a heapTree * 'a = 
  match t with 
  | N(r,d, e, fg, fd) -> 
  (*Garder la propriété du tas*)
      if e < res then 
        ( (*On a besoin de rééquilibrer avec l'arbre droite dans le cas où on change de racine du nœud*)
          match fd with 
          | E -> (N (( min (rank nfg) (rank fd))+1, d-1, res, nfg, fd),e) 
          | L(efd) -> 
              if efd < res then 
                (N (( min (rank nfg) (rank fd))+1, d-1, efd, nfg, L(res)),e)
              else 
                (N (( min (rank nfg) (rank fd))+1, d-1, res, nfg, fd),e)
          |N(rfd,dfd,efd,fdfg,fdfd) -> 
              if efd < res then 
                (N (( min (rank nfg) (rank fd))+1, d-1, efd, nfg, N(rfd,dfd,res,fdfg,fdfd)),e)
              else 
                (N (( min (rank nfg) (rank fd))+1, d-1, res, nfg, fd),e)
        )
      else (*res est déjà le minimum dans ce tas, on le renvoie sans modifier le reste*)
        (N (( min (rank nfg) (rank fd))+1, d-1, e, nfg, fd),res) 
 (*Fin de garder la propriété du tas*)
  | _ -> failwith "invalid argument"
  
        

let min a b = if a > b  then b else a;;

let rec ajout_tasse (h : 'a heapTree) (x : int) :  'a heapTree = 
  match h with 
  | E -> L(x)
  | L(e) -> if e < x then  N(1, 1,e, L(x),E) else N(1, 1,x, L(e),E) 
  | N(r,d, e, fg, E) -> 
      if e < x then 
        N(r+1,d+1, e, fg, L(x))
      else 
        N(r+1,d+1, x, fg, L(e)) 
  | N(r, d,e, fg, fd) -> 
      let rfg = (rank fg) and rfd = rank fd in 
      if rfg > rfd then (*On ne peut plus ajouter à gauche sans deséquilibrer*)
        if e < x then 
          let nfd = ajout_tasse fd x in N(( min rfg (rank nfd) )+1,d+1,e,fg,nfd)
        else 
          let nfd = ajout_tasse fd e in N(( min rfg (rank nfd) )+1,d+1,x,fg,nfd) (*x < e, puisque e < elt fg, x < elt fg, la propriété du tas est conservée*)
      else 
      if e < x then 
        let nfg = ajout_tasse fg x in N (( min rfd (rank nfg) )+1,d+1, e, nfg, fd)
      else 
        let nfg = ajout_tasse fg e in N (( min rfd (rank nfg) )+1,d+1, x, nfg, fd)



let rec retrait_tasse (h : 'a heapTree) : 'a heapTree * 'a = 
  match h with 
  | E -> failwith "Empty heap"
  | L(e) -> (E, e)
  | N(r, d, e, L(efg), E) -> 
      if e < efg then 
        (L(efg), e)
      else 
        (L( e), efg)
  | N(r,d, e, fg, fd) -> 
      if rank fg = rank fd  then 
        if (nbdesc fg > nbdesc fd) then 
          let (nfg, res) = retrait_tasse fg in (reeq_tas_gauche h nfg res)
        else
          let (nfd, res) = retrait_tasse fd in (reeq_tas_droite h nfd res)
                                               
      else if elemSurDernierRang fd then (*On vérifie s'il y a des choses à retirer à droite avant d'en retirer à gauche !*)
        let (nfd, res) = retrait_tasse fd in (reeq_tas_droite h nfd res) 
      else 
        let (nfg, res) = retrait_tasse fg in (reeq_tas_gauche h nfg res);; 


           
let rec ajout_feuille_iter (l : int list) (h : 'a heapTree) : 'a heapTree = 
  match l with 
  | [] -> h
  | hd::tl -> ajout_feuille_iter tl (ajout_tasse h hd);;
          
let rec retrait_feuille_iter (n : int) (h : 'a heapTree) : 'a heapTree = 
  if n = 0 then h else let ( h2, _) = retrait_tasse h in retrait_feuille_iter (n-1) h2;;
          
let t= ajout_feuille_iter [1;2;3;4;5;6;7;8] E in t;;



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


  let bubble_down (hp : 'a heapArray) (p : int) : unit = 
    let (ind,sz, tab) = hp and i = ref p in
    while (est_non_valide !i hp) do
      try(
        match (tab.(fg !i), tab.(fd !i)) with 
        | (Some(eltg), None) -> 
            let tmp = tab.(!i) in 
            tab.(!i) <- tab.(fg !i) ; 
            tab.(fg !i) <- tmp ; 
            i := (fg !i)
        | (Some(eltg), Some(eltd)) -> 
            if eltg > eltd then 
              let tmp = tab.(!i) in 
              tab.(!i) <- tab.(fd !i) ; 
              tab.(fd !i) <- tmp ; 
              i := (fd !i)
            else 
              let tmp = tab.(!i) in 
              tab.(!i) <- tab.(fg !i) ; 
              tab.(fg !i) <- tmp ; 
              i := (fg !i)
        | (_,_) -> failwith "Cas impossible"
      ) with Invalid_argument s -> 
        (match tab.(fg !i) with
         | None -> ()
         | Some(elt) ->
             let tmp = tab.(!i) in 
             tab.(!i) <- tab.(fg !i) ; 
             tab.(fg !i) <- tmp ; 
             i := (fg !i)
        )
    done; 
  ;;
  
  let construction (l : 'a list) : 'a heapArray = 
    let lgth = List.length l in 
    let hp = makeHeapArray lgth lgth (Array.of_list (List.map (fun x -> Some(x)) l))
    and i = ref ((lgth-1) /2)
    in 
    while (!i >= 0) do 
      Printf.printf " On regarde le nœud %d\n" !i;
      (bubble_down hp !i);
      i := !i -1
    done;
    hp;;
  
  
  let t= construction [8;5;6;2;3;4;7;1] in t;;
  

