

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
          let (nfg, res) = retrait_tasse fg in (
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
          ) (*Fin de garder la propriété du tas*)
        else
          let (nfd, res) = retrait_tasse fd in 
          
          (
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
          ) (*Fin de garder la propriété du tas*)
                                                
      else if elemSurDernierRang fd then (*On vérifie s'il y a des choses à retirer à droite avant d'en retirer à gauche !*)
        let (nfd, res) = retrait_tasse fd in 
        (
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
        ) (*Fin de garder la propriété du tas*)
        
      else 
        let (nfg, res) = retrait_tasse fg in
        (
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
        ) (*Fin de garder la propriété du tas*);; 


           
(*TODO*)
          (*Still issues after removing 20*)

let rec ajout_feuille_iter (l : int list) (h : 'a heapTree) : 'a heapTree = 
  match l with 
  | [] -> h
  | hd::tl -> ajout_feuille_iter tl (ajout_tasse h hd);;
          
let rec retrait_feuille_iter (n : int) (h : 'a heapTree) : 'a heapTree = 
  if n = 0 then h else let ( h2, _) = retrait_tasse h in retrait_feuille_iter (n-1) h2;;
          
let t= ajout_feuille_iter [16;2;3;11;14;15;10;13;4;5;6;7;8;9;12;1] E in (t,retrait_tasse t);;


N (4, 15, 1,
   N (3, 7, 2, 
      N (2, 3, 4, 
         N (1, 1, 11, 
            L 16, E), 
         L 13),
      N (2, 2, 5, 
         L 14,
         L 6)),
   N (3, 6, 3, 
      N (2, 2, 7, 
         L 15, 
         L 8), 
      N (2, 2, 9, 
         L 10, 
         L 12)))



