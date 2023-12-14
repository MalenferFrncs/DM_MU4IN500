open Int128


(*indice dernier élément * taille du tableau * tableau*)
type heapArray = int ref * int ref * (Int128.t option) Array.t;;

let pere (i : int) = (i-1) /2;;

let fg (i : int) = 2*i +1;;

let fd (i : int) = 2*i +2;;

(*Fonction créant un tas vide de taille sz*)
let makeEmptyHeapArray (sz: int) : (heapArray) = (ref 0, ref sz, (Array.make sz None));;

(*Fonction créant un tas à partir d'un tableau de clés (sous forme de type option)*)
let makeHeapArray (ind : int) (sz : int) (tab : (Int128.t option) Array.t) : heapArray = (ref ind,ref sz,tab);;

(*Retourne la valeur de size*)
let size (h : heapArray) : int = 
  let (_,sz,_) = h in !sz

(*Copie l'élément d'un tas minimum à l'indice i dans un autre tas. Si on dépasse la taille du tableau, on 
  met none, la case est vide dans l'autre tas*)
let copie (hp : heapArray) (i : int) : Int128.t option = 
  let (_,sz, tab) = hp in
  if i >= !sz then None else tab.(i);;


(*Rajoute un rang dans le tas*)
let re_size (hp : heapArray) : heapArray = 
  let (ind, sz, tab) = hp in 
  let ntab = Array.init (Int.shift_left  !sz 1 ) (copie hp)
  in 
  sz := (Int.shift_left  !sz 1 ); 
  (ind, sz, ntab );;

(*Fonction permettant d'ajouter un élément à un tas*)
let rec ajout (hp : heapArray) (elt : Int128.t) : heapArray = 
  let (ind, sz, tab) = hp in 
  let i = ref !ind and ind_pere = ref (pere !ind) in 
  try (
    tab.(!ind) <- Some(elt);
    ind := !ind +1;
    while (!i > 0 && (  Int128.inf2 tab.(!i) tab.(!ind_pere) )) do   (*Pas de problème avec les None car on remonte*)
      let tmp = tab.(!i) in
      tab.(!i) <- tab.(!ind_pere);
      tab.(!ind_pere) <- tmp ; 
      i := !ind_pere;
      ind_pere := pere !i;
    done;
    hp
  ) with Invalid_argument s ->(ajout (re_size hp) elt);;

(*Fonction renvoyant *)
let est_non_valide (i : int) (hp : heapArray) : bool = 
  let (ind, _, tab) = hp in 
  if (fg i) > (!ind -1) then false    (*On est au bout du tas*) 
  else 
    match tab.(i) with 
    |Some(racine) ->
      (try (
         match (tab.(fg i), tab.(fd i)) with 
         | (None, None) -> false
         | (Some(eltg), None) -> Int128.inf eltg racine
         | (Some(eltg), Some(eltd)) -> (Int128.inf eltg racine) || (Int128.inf eltd racine)
         | (None, Some(_)) -> failwith "Tas mal construit !"
       ) with Invalid_argument s -> 
         (
           match tab.(fg i) with
           | Some(eltg) -> Int128.inf eltg racine
           | None -> false 
         )
      )
    |None -> false;; 


let supprMin (hp : heapArray) : (Int128.t option * heapArray) = 
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
           if (Int128.inf eltd eltg) then let tmp = tab.(!i) in tab.(!i) <- tab.(fd !i) ; tab.(fd !i) <- tmp ; i := (fd !i)
           else let tmp = tab.(!i) in tab.(!i) <- tab.(fg !i) ; tab.(fg !i) <- tmp ; i := (fg !i)
         | (_,_) -> failwith "Cas impossible"
       ) with Invalid_argument s -> let tmp = tab.(!i) in tab.(!i) <- tab.(fg !i) ; tab.(fg !i) <- tmp ; i := (fg !i);
      )
    done;
    (min,hp)
  ) with Invalid_argument s -> (None, (makeEmptyHeapArray 0));; (*Cas où on retire le minimum d'un tas vide*)




let ajout_iteratif (l : Int128.t list) : heapArray = 
  let rec aux (hp : heapArray) (l : Int128.t list) : heapArray =
    match l with
    | [] -> hp
    | h::tl -> let nhp = ajout hp h in (aux nhp tl)
  in (aux (makeEmptyHeapArray (List.length l)) l);;





let heapify (hp : heapArray) (p : int) : unit = 
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
        if (Int128.inf eltd eltg )then 
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

let construction (l : Int128.t list) : heapArray = 
  let lgth = List.length l in 
  let hp = makeHeapArray lgth lgth (Array.of_list (List.map (fun x -> Some(x)) l))
  and i = ref ((lgth -1)/2)
  in 
  while (!i >= 0) do 
    (heapify hp !i);
    i := !i -1
  done;
  hp;;


let union (h1 : heapArray) (h2 : heapArray) : heapArray = 
  let (ind1,sz1, tab1) = h1 and (ind2,sz2, tab2) = h2 in
  let newtab = (ref (!ind1+ !ind2), ref (!sz1 + !sz2), Array.append tab1 tab2) in
  let i = ref ( ((size newtab) -1)/2 ) in
  while (!i >= 0) do
    heapify newtab !i ;
    i := !i -1
  done ;
  newtab;;




(*Fonction crééant, ouvrant, et remplissant un fichier .dot dont le nom est passé en paramètres*)
let to_dot (nom : string) (hp : heapArray) : unit =
  let f = open_out nom in (*Ouverture du fichier où on met le graphe*)
  (* *)
  Printf.fprintf f "digraph g {\n";   (*Préambule*)
  let (ind,sz,tab) = hp in
  (*Fonction locale permettant de mettre tous les noeuds de l'arbre au bon format en faisant un parcourt du graphe*)
  for i = 0 to !sz -1 do
    if (2*i +2 < !sz) then
      match ( tab.(i),tab.(fg i), tab.(fd i)) with 
      | (Some(elt),None,None) -> 
        Printf.fprintf f "\n%d [shape = box, style = \"rounded,bold\", label = \"%s\",color =seagreen];" i (Int128.to_str elt);
      | (Some(elt), Some(fg),None) ->
        Printf.fprintf f "\n%d [shape = box, style = bold, label = \"%s\", color =sienna];\n %d -> %d[style=dotted];" i (Int128.to_str elt) i (2*i+1);
      | (Some(elt),Some(fg), Some(fd)) ->
        Printf.fprintf f "\n%d [shape = box, style = bold, label = \"%s\",color =sienna];\n %d-> %d[style=dotted];" i (Int128.to_str elt) i (2*i +1);
        Printf.fprintf f "\n %d-> %d;" i (2*i +2);
      | (_,_,_) -> failwith "Invalid Argument"
    else if (2*i +1 < !sz) then 
      match ( tab.(i),tab.(fg i)) with 
      | (Some(elt),None) -> 
        Printf.fprintf f "\n%d [shape = box,style = \"rounded,bold\",label = \"%s\",color =seagreen];" i (Int128.to_str elt);
      | (Some(elt), Some(fg)) ->
        Printf.fprintf f "\n%d [shape = box, style = bold,label = \"%s\",color =sienna];\n %d-> %d[style=dotted];" i (Int128.to_str elt) i (2*i +1);
      | (_,_) -> failwith "Invalid Argument"
      else 
      match tab.(i) with Some(elt) -> Printf.fprintf f  "\n%d [shape = box, style = \"rounded,bold\",label = \"%s\",color =seagreen];" i (Int128.to_str elt) | None -> ()
  done; 
  Printf.fprintf f "}\n";
  close_out f;
;;