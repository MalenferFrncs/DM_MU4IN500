

type 'a heapArray = int * int * ('a option) Array.t;;

let pere (i : int) = (i-1) /2;;

let fg (i : int) = 2*i +1;;

let fd (i : int) = 2*i +2;;

let makeEmptyHeapArray (sz: int) : ('a heapArray) = (0, sz, (Array.make sz None));;


let copie (hp : 'a heapArray) (i : int) : 'a option = 
  let (_,sz, tab) = hp in
  if i >= sz then None else tab.(i);;


let double_taille (hp : 'a heapArray) : 'a heapArray = 
  let (ind, sz, tab) = hp in (ind, 2*sz, Array.init (2*sz) (copie hp));;

let rec ajout (hp : 'a heapArray) (elt : 'a) : 'a heapArray = 
  let (ind, sz, tab) = hp in 
  let i = ref ind and ind_pere = ref (pere ind) in 
  try (
    tab.(ind) <- Some(elt);
    while (!i > 0 && (tab.(!i) < tab.(!ind_pere))) do
      let tmp = tab.(!i) in
      tab.(!i) <- tab.(!ind_pere);
      tab.(!ind_pere) <- tmp ; 
      i := !ind_pere;
      ind_pere := pere !i;
    done;
    (ind+1, sz, tab) 
  ) with Invalid_argument s -> (ajout (double_taille hp) elt);;



let est_non_valide (i : int) (hp : 'a heapArray) : bool = 
  let (ind, sz, tab) = hp in 
  if (fg i) > (ind-1) then false    (*On est au bout du tas*) 
  else 
    let Some(racine) = tab.(i) in
    try (
      match (tab.(fg i), tab.(fd i)) with 
      | (None, None) -> false
      | (Some(eltg), None) -> racine > eltg
      | (Some(eltg), Some(eltd)) -> racine > eltg || racine > eltd
      | (None, Some(_)) -> failwith "Tas mal construit !"
    ) with Invalid_argument s -> let Some(eltg) = tab.(fg i) in racine > eltg;;
    

let supprMin (hp : 'a heapArray) : ('a option * 'a heapArray) = 
  let (ind, sz, tab) = hp in 
  let min = tab.(0) and i = ref 0 in
  tab.(0) <- tab.(ind-1);
  tab.(ind-1) <- None;
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
  (min,(ind -1, sz, tab));;
      
    
    

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
  
  
  
  
  let t= construction [8;25;-3;13;24;16;145;6;19;2;3;4;7;1] in t;;
  
  let ta = ajout_iteratif [8;25;-3;13;24;16;145;6;19;2;3;4;7;1] in ta;;
