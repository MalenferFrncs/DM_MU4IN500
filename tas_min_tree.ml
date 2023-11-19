

(*Code caduque à retravailler pour le rééquilibrage du tas tel que défini après*)

let rec descenteReeqRacine (t : 'a tm_tree)  : 'a tm_tree = 
  match t with 
  | Empty -> Empty
  | Leaf (e)-> Leaf(e)
  | Node (r, fg, fd)-> 
      (
        match (fg, fd) with 
        | (Empty, Empty) -> failwith "Invalid Argument"
        | (Leaf(e), Empty) -> if (e < r) then Node (e, Leaf(r), Empty) else t
              
        | (Leaf(eg), Leaf(ed)) -> 
            if (eg < r) then 
              if (eg < ed) then 
                Node (eg, Leaf(r), Leaf(ed))
              else
                Node (ed, Leaf(eg), Leaf(r))
            else if (ed < r) then 
              Node (ed, Leaf(eg), Leaf(r))
            else t
              
        | (Node (eg,ffg,ffd), Leaf (ed)) -> 
            if (eg < r) then 
              if (eg < ed) then 
                Node (eg, (descenteReeqRacine (Node(r, ffg,ffd))), fd) 
              else 
                Node (ed, fg, Leaf(r))
            else if (ed < r) then 
              Node (ed, fg, Leaf(r))
            else t
              
        | (Node (eg,fgfg,fgfd), Node (ed,fdfg,fdfd)) -> 
            if (eg < r) then 
              if (eg < ed) then 
                Node (eg, (descenteReeqRacine (Node(r, fgfg,fgfd))), fd )
              else 
                Node (ed, fg, (descenteReeqRacine (Node(r, fdfg,fdfd))))
            else if (ed < r) then 
              Node (ed, fg, (descenteReeqRacine (Node(r, fdfg,fdfd))))
            else t
        | (_,_) -> failwith "Invalid argument" 
      )

let supprMin (t : 'a tm_tree) : ('a tm_tree * 'a) = 
  let (t_sans_dernier, dernier) = remonteValDerniereFeuille t in
  match t_sans_dernier with 
  | Empty -> (Empty, dernier)
  | Leaf (e) -> (Leaf (dernier), e)
  | Node (r, fg, fd) -> 
      (*On met la dernière feuille à la place de la racine et on la fait descendre*)
      let tas_eq = descenteReeqRacine (Node (dernier, fg,fd)) (*Rééquilibrage à faire sur cet arbre*)
      in (tas_eq, r)
    
                                              


(*Cette structure est inspirée par celle proposée par Chris Okasaki dans Purely Functionnal Data Structures
  le rang est défini comme étant la distance vers l'emplacement vide le plus proche,*)
(* Noeud ( rang,ndescendances, elt, fg, fd*)
type 'a heapTree = E | L of 'a | N of int * int *  'a * 'a heapTree * 'a heapTree;;

let rank (t: 'a heapTree) : int=
  match t with
  | E -> 0
  | L (_) -> 1
  | N (r,_,_,_,_) -> r ;;
    
let nbdesc (t : 'a heapTree) : int = 
  match t with 
  |  E -> 0
  | L(_) -> 0
  | N(_,nd,_,_,_) -> nd;;
          


(*Calcule si le tas a des éléments sur son dernier rang (encore incomplet)*)
let elemSurDernierRang (h :'a  heapTree) = nbdesc h > (Int.shift_left 1 ((rank h)))-2
                                           


let min a b = if a > b  then b else a;;

let rec ajout_tasse (h : 'a heapTree) (x : int) :  'a heapTree = 
  match h with 
  | E -> L(x)
  | L(e) -> N(1, 1,e, L(x),E) 
  | N(r,d, e, fg, E) -> N(r+1,d+1, e, fg, L(x))
  | N(r, d,e, fg, fd) -> 
      let rfg = (rank fg) and rfd = rank fd in 
      if rfg > rfd then (*On ne peut plus ajouter à gauche sans deséquilibrer*)
        let nfd = ajout_tasse fd x in N(( min rfg (rank nfd) )+1,d+1,e,fg,nfd)
      else
        let nfg = ajout_tasse fg x in N (( min rfd (rank nfg) )+1,d+1, e, nfg, fd) ;;

let rec retrait_tasse (h : 'a heapTree) : 'a heapTree * 'a = 
  match h with 
  | E -> failwith "Empty heap"
  | L(e) -> (E, e)
  | N(r, d, e, L(efg), E) -> (L( e), efg)
  | N(r,d, e, fg, fd) -> 
      if rank fg = rank fd  then 
        (if (nbdesc fg > nbdesc fd) then 
           let (nfg, res) = retrait_tasse fg in (N (( min (rank nfg) (rank fd))+1, d-1, e, nfg, fd),res)
         else
           let (nfd, res) = retrait_tasse fd in (N (( min (rank nfd) (rank fg))+1, d-1, e, fg, nfd),res) 
        )
      else if elemSurDernierRang fd then (*On vérifie s'il y a des choses à retirer à droite avant d'en retirer à gauche !*)
        let (nfd, res) = retrait_tasse fd in (N (( min (rank nfd) (rank fg))+1, d-1, e, fg, nfd),res) 
      else 
        let (nfg, res) = retrait_tasse fg in (N (( min (rank nfg) (rank fd))+1, d-1, e, nfg, fd),res)


           
(*TODO*)
          (*Still issues after removing 20*)
;;
          
let rec ajout_feuille_iter (l : int list) (h : 'a heapTree) : 'a heapTree = 
  match l with 
  | [] -> h
  | hd::tl -> ajout_feuille_iter tl (ajout_tasse h hd);;
          
let rec retrait_feuille_iter (n : int) (h : 'a heapTree) : 'a heapTree = 
  if n = 0 then h else let ( h2, _) = retrait_tasse h in retrait_feuille_iter (n-1) h2;;
          
let t= ajout_feuille_iter [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25;26;27;28;29;30;31;32] E in retrait_feuille_iter 16 t;;

