open Int128




(*Ce code est inspiré par la structure proposée par Chris Okasaki dans Purely Functionnal Data Structures
  le rang est défini comme étant la distance vers l'emplacement vide le plus proche*)
(* Noeud ( rang,ndescendances, elt, fg, fd*)
type  heapTree = E | L of Int128.t | N of int * int *  Int128.t *  heapTree *  heapTree;;

let rank (t:  heapTree) : int=
  match t with
  | E -> 0
  | L (_) -> 1
  | N (r,_,_,_,_) -> r ;;

let elt (t :  heapTree ) : Int128.t = 
  match t with 
  | E -> failwith "empty heap"
  | L(e) -> e
  | N (_,_,e,_,_) -> e

let nbdesc (t :  heapTree) : int = 
  match t with 
  |  E -> 0
  | L(_) -> 0
  | N(_,nd,_,_,_) -> nd;;


let min (a : int) (b : int) : int = 
  if b< a then b else a


let elemSurDernierRang (h :  heapTree) = nbdesc h > (Int.shift_left 1 ((rank h)))-2


(*Fonctions auxiliaires servant à faire des modifications locales pour garder la propriété du tas
  énonçant que la racine est inférieure à la racine de ses fils *) 
let reeq_tas_droite (t :  heapTree) (nfd :  heapTree) (res : Int128.t) :  heapTree * Int128.t = 
  match t with 
  | N(r,d, e, fg, fd) -> 
    (*Garder la propriété du tas*)
    if (Int128.inf e res) then 
      ( (*On a besoin de rééquilibrer avec l'arbre droite dans le cas où on change de racine du nœud*)
        match fg with 
        | E -> (N (( min (rank fg) (rank nfd))+1, d-1, res, fg, nfd),e) 
        | L(efg) -> 
          if (Int128.inf  efg res) then 
            (N (( min (rank fg) (rank nfd))+1, d-1, efg, L(res), nfd),e)
          else 
            (N (( min (rank fg) (rank nfd))+1, d-1, res, fg, nfd),e)
        |N(rfg,dfg,efg,fgfg,fgfd) -> 
          if (Int128.inf  efg res) then 
            (N (( min (rank fg) (rank nfd))+1, d-1, efg, N(rfg,dfg,res,fgfg,fgfd), nfd),e)
          else 
            (N (( min (rank fg) (rank nfd))+1, d-1, res, fg, nfd),e)
      )
    else (*res est déjà le minimum dans ce tas, on le renvoie sans modifier le reste*)
      (N (( min (rank fg) (rank nfd))+1, d-1, e, fg, nfd),res) 
  | _ -> failwith "Invalid Argument"

let reeq_tas_gauche (t : heapTree) (nfg : heapTree) (res : Int128.t) : heapTree * Int128.t= 
  match t with 
  | N(r,d, e, fg, fd) -> 
    (*Garder la propriété du tas*)
    if (Int128.inf e res) then 
      ( (*On a besoin de rééquilibrer avec l'arbre droite dans le cas où on change de racine du nœud*)
        match fd with 
        | E -> (N (( min (rank nfg) (rank fd))+1, d-1, res, nfg, fd),e) 
        | L(efd) -> 
          if (Int128.inf  efd res) then 
            (N (( min (rank nfg) (rank fd))+1, d-1, efd, nfg, L(res)),e)
          else 
            (N (( min (rank nfg) (rank fd))+1, d-1, res, nfg, fd),e)
        |N(rfd,dfd,efd,fdfg,fdfd) -> 
          if (Int128.inf  efd res) then 
            (N (( min (rank nfg) (rank fd))+1, d-1, efd, nfg, N(rfd,dfd,res,fdfg,fdfd)),e)
          else 
            (N (( min (rank nfg) (rank fd))+1, d-1, res, nfg, fd),e)
      )
    else (*res est déjà le minimum dans ce tas, on le renvoie sans modifier le reste*)
      (N (( min (rank nfg) (rank fd))+1, d-1, e, nfg, fd),res) 
  (*Fin de garder la propriété du tas*)
  | _ -> failwith "invalid argument"




let rec ajout_tasse (h :  heapTree) (x : Int128.t) :   heapTree = 
  match h with 
  | E -> L(x)
  | L(e) -> if (Int128.inf e x) then  N(1, 1,e, L(x),E) else N(1, 1,x, L(e),E) 
  | N(r,d, e, fg, E) -> 
    if (Int128.inf e x) then 
      N(r+1,d+1, e, fg, L(x))
    else 
      N(r+1,d+1, x, fg, L(e)) 
  | N(r, d,e, fg, fd) -> 
    let rfg = (rank fg) and rfd = rank fd in 
    if rfg > rfd then (*On ne peut plus ajouter à gauche sans deséquilibrer*)
      if (Int128.inf e x) then 
        let nfd = ajout_tasse fd x in N(( min rfg (rank nfd) )+1,d+1,e,fg,nfd)
      else 
        let nfd = ajout_tasse fd e in N(( min rfg (rank nfd) )+1,d+1,x,fg,nfd) (*x < e, puisque e < elt fg, x < elt fg, la propriété du tas est conservée*)
    else 
    if (Int128.inf e x) then 
      let nfg = ajout_tasse fg x in N (( min rfd (rank nfg) )+1,d+1, e, nfg, fd)
    else 
      let nfg = ajout_tasse fg e in N (( min rfd (rank nfg) )+1,d+1, x, nfg, fd)



let rec retrait_tasse (h :  heapTree) :  heapTree * Int128.t = 
  match h with 
  | E -> failwith "Empty heap"
  | L(e) -> (E, e)
  | N(r, d, e, L(efg), E) -> 
    if (Int128.inf e efg) then 
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



let rec ajout_feuille_iter (l : Int128.t list) (h :  heapTree) :  heapTree = 
  match l with 
  | [] -> h
  | hd::tl -> ajout_feuille_iter tl (ajout_tasse h hd);;

let rec retrait_feuille_iter (n : int) (h :  heapTree) :  heapTree = 
  if n = 0 then h else let ( h2, _) = retrait_tasse h in retrait_feuille_iter (n-1) h2;;


(*Idée : construire par le bas (en faisant les buble down) en utilisant la fin de la récursion pour récupérer les arbres
  construits précédemment pour réassembler avec l'élément de la liste actuelle*)

let rec log2 x =
  match x with
  | 1 -> 0
  | _ -> 1 + log2 ((x) / 2)

let two_pow (n : int) = Int.shift_left 1 n

let empty_dernier_rang (n : int ) : int =
  (two_pow (log2 n+1))-1 - n







let rec bubble_down (hp : heapTree) :  heapTree = 
  match hp with 
  | E -> hp
  | L(_) -> hp
  | N(rk, sz, er, fg, fd) ->
    match (fg,fd) with 
    | (E,E) -> hp
    |(L(eltg), E) -> if (Int128.inf eltg er) then N(rk, sz,eltg,L(er),E)
      else hp
    | (L(eltg), L(eltd)) -> 
      if (Int128.inf eltg er) then 
        if (Int128.inf eltd eltg ) then (*On remonte le min des deux fils, qui est à droite *)
          let nfd = bubble_down (L(er)) in N(rk,sz,eltd,fg,nfd)
        else (*Le min des deux fils est à gauche*)
          let nfg = bubble_down (L(er)) in N(rk, sz, eltg, nfg, fd)
      else
        hp

    |(N(rkg,szg,eltg,fgfg,fgfd), E) -> if (Int128.inf eltg er) then N(rk, sz,eltg,N(rkg,szg,er,fgfg,fgfd),E)
      else hp 
    | (N(rkg,szg,eltg,fgfg,fgfd), L(eltd)) -> 
      if (Int128.inf eltg er) then 
        if (Int128.inf eltd eltg ) then (*On remonte le min des deux fils qui est à droite *)
          let nfd = bubble_down (L(er)) in N(rk,sz,eltd,fg,nfd)
        else (*Le min des deux fils est à gauche*)
          let nfg = bubble_down (N(rkg,szg, er, fgfg,fgfd)) in N(rk, sz, eltg, nfg, fd)
      else
        hp
    |(N(rkg,szg,eltg,fgfg,fgfd), N(rkd,szd,eltd,fdfg,fdfd)) -> 
      if (Int128.inf eltg er) then 
        if (Int128.inf eltd eltg ) then (*On remonte le min des deux fils qui est à droite *)
          let nfd = bubble_down (N(rkd,szd, er, fdfg, fdfd)) in N(rk,sz,eltd,fg,nfd)
        else (*Le min des deux fils est à gauche*)
          let nfg = bubble_down (N(rkg,szg, er, fgfg,fgfd)) in N(rk, sz, eltg, nfg, fd)
      else
        hp
    | (_,_) -> failwith "Invalid argument"






(* Calcule le nombre de fils qu'il veut à gauche : récupère la liste, la balance à droite (il sait sait aussi combien il en faut à droite )*) 
let rec make_tas (li : Int128.t list) (taille : int) :  ( heapTree * Int128.t list)= 
  if taille = 0 || taille < 0 then (E,li)
  else if taille = 1 then
    match li with 
    | [] -> failwith "invalid argument"
    | h::tl -> (L(h), tl)
  else
    let hauteur = log2 taille in
    let hauteur_prec = hauteur -1 in
    let reste = taille - ((two_pow hauteur)-1) in 
    if reste < ((two_pow hauteur)/2) then
      let nb_elem_gauche = reste+ (((two_pow (hauteur_prec+1)) -1)/2) in
      let nb_elem_droite = (((two_pow (hauteur_prec+1)) -1)/2) in
      let (fg,lr) = make_tas li nb_elem_gauche in
      let (fd,lr2) = make_tas lr nb_elem_droite in
      match lr2 with 
      | [] -> failwith "invalid argument"
      | h::tl ->  ((bubble_down ( N( (min (rank fg) (rank fd)) +1, taille -1, h, fg, fd))), tl) 
    else
      let nb_elem_gauche = ((two_pow hauteur)/2) + (((two_pow (hauteur_prec+1)) -1)/2) in
      let nb_elem_droite = (((two_pow (hauteur_prec+1)) -1)/2) + (reste - ((two_pow hauteur)/2)) in
      let (fg,lr) = make_tas li nb_elem_gauche in
      let (fd,lr2) = make_tas lr nb_elem_droite in
      match lr2 with 
      | [] -> failwith "invalid argument"
      | h::tl ->( (bubble_down ( N( (min (rank fg) (rank fd)) +1, taille -1, h, fg, fd) ) ), tl)


let construction (li : Int128.t list) : heapTree = 
  let (hp,_) = make_tas li (List.length li) in hp;;

let rec heap_to_list (hp :  heapTree) (acc : Int128.t list) : Int128.t list = 
  match hp with 
  | E -> acc
  | L(elt) -> elt::acc
  | N(_,_,r,fg,fd) -> let lfg = (heap_to_list fg acc) in let lfd = heap_to_list fd lfg in r::lfd;;



let union (hp1 :  heapTree) (hp2 :  heapTree) :  heapTree = 
  match (heap_to_list (N(0,0,(0l,0l,0l,0l),hp1, hp2)) []) with 
  | [] -> E
  | _::tl -> let (res,_) = (make_tas tl (List.length tl)) in res




let to_dot (nom : string) (hp : heapTree) : unit =
  let f = open_out nom in (*Ouverture du fichier où on met le graphe*)
  (* *)
  let rec print_noeud (hp : heapTree) (i : int) : int = 
    match hp with 
    | E -> ()
    | L(elt) -> Printf.fprintf f "\n%d [shape = box, style = \"rounded,bold\", label = \"%s\",color =seagreen];" i (Int128.to_str elt); (i+1);
    | N(_,_,elt,fg, E) -> 
      Printf.fprintf f "\n%d [shape = box, style = bold, label = \"%s\", color =sienna];\n %d -> %d;" i (Int128.to_str elt) i (i+1);
      let i = print_noeud fg (i+1) in i
    | N(_,_,elt,fg,fd) -> 
      Printf.fprintf f "\n%d [shape = box, style = bold, label = \"%s\", color =sienna];\n %d -> %d;" i (Int128.to_str elt) i (i+1);
      let ig = print_noeud fg (i+1) in 
      Printf.fprintf f "\n %d -> %d" i ig ; 
      let id = print_noeud fd ig in (id +1)
  in 
  Printf.fprintf f "digraph g {\n";   (*Préambule*) 
  print_noeud hp 0;
  Printf.fprintf f "}\n";
  close_out f;
;;

let l = ["0xF0000000000000000000000000000001";
         "0x00000000000000000000000000000001";
         "0x00000000000000000000000000000011";
         "0x00000000000000000000000000010001";
         "0x00000000000000000000000000000101";
         "0x00000000000000000000000000001001"] in (construction (List.map Int128.of_str l));; 


         (*PROBLÈMES
            
         * trucs bizarres sur le tas fait avec l...
         * compteur pour dot pas bon
         *)


