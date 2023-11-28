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


(*Idée : construire par le bas (en faisant les buble down) en utilisant la fin de la récursion pour récupérer les arbres
  construits précédemment pour réassembler avec l'élément de la liste actuelle*)

let rec log2 x =
  match x with
  | 1 -> 0
  | _ -> 1 + log2 ((x) / 2)
  
let two_pow (n : int) = Int.shift_left 1 n
  
let empty_dernier_rang (n : int ) : int =
  (two_pow (log2 n+1))-1 - n

  
let rec faire_n_feuilles (n : int) (l : 'a list) (cpt : int): ('a heapTree list * 'a list) = 
  match l with 
  |[] -> ([],[])
  | h:: tl -> 
      if n > 0 then 
        if cpt > 0 then 
          let (lf, reste) = faire_n_feuilles (n-1) l (cpt-1) in (E::lf,reste)
        else
          let (lf, reste) = faire_n_feuilles (n-1) tl 0 in (L(h)::lf,reste) 
      else ([],l) 
           
          (* Calcule le nombre de fils qu'il veut à gauche : récupère la liste, la balance à droite (il sait sait aussi combien il en faut droite *)

let rec make_tas (li : 'a list) (taille : int) :  ('a heapTree * 'a list)= 
  if taille = 0 or taille < 0 then (E,li)
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
      | h::tl ->( N( (min (rank fg) (rank fd)) +1, taille -1, h, fg, fd), tl) 
    else
      let nb_elem_gauche = ((two_pow hauteur)/2) + (((two_pow (hauteur_prec+1)) -1)/2) in
      let nb_elem_droite = (((two_pow (hauteur_prec+1)) -1)/2) + (reste - ((two_pow hauteur)/2)) in
      let (fg,lr) = make_tas li nb_elem_gauche in
      let (fd,lr2) = make_tas lr nb_elem_droite in
      match lr2 with 
      | [] -> failwith "invalid argument"
      | h::tl ->( N( (min (rank fg) (rank fd)) +1, taille -1, h, fg, fd), tl) 
    
  
  


let rec make_liste_test (n : int) : int list = 
  if n = 0 then []
  else n::(make_liste_test (n-1));;
(*Faire un accumulateur pour l'étage en construction*)
make_tas (make_liste_test 32) 32 ;;


N (5, 31, 1,
   N (4, 15, 17,
      N (3, 7, 25, 
         N (2, 3, 29, 
            N (1, 1, 31, 
               L 32, 
               E), 
            L 30),
         N (2, 2, 26, 
            L 28, 
            L 27)
        ),
      N (3, 6, 18, 
         N (2, 2, 22, 
            L 24, 
            L 23), 
         N (2, 2, 19, 
            L 21, 
            L 20))),
   N (4, 14, 2,
      N (3, 6, 10, 
         N (2, 2, 14, 
            L 16, 
            L 15), 
         N (2, 2, 11, 
            L 13, 
            L 12)),
      N (3, 6, 3, 
         N (2, 2, 7, 
            L 9, 
            L 8), 
         N (2, 2, 4, 
            L 6, 
            L 5))))
  
