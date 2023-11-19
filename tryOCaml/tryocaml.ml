

(*Ce code est inspiré par la structure proposée par Chris Okasaki dans Purely Functionnal Data Structures
  le rang est défini comme étant la distance vers l'emplacement vide le plus proche*)

type 'a heapTree = E | N of int * 'a * 'a heapTree * 'a heapTree;;

let rank (t: 'a heapTree) : int=
  match t with
  | E -> 0
  | N (r,_,_,_) -> r
          
      (*Construit un tas en gardant la propriété sur le rang et le tassage à gauche*)


let min a b = if a > b  then b else a

let rec ajout_tasse (h : 'a heapTree) (x : int) :  'a heapTree = 
  match h with 
  | E -> N (1, x, E,E)
  | N(r, e, E, E) -> N(r, e, N(1,x,E,E),E) 
  | N(r, e, fg, E) -> N(r+1, e, fg, N(1,x,E,E))
  | N(r, e, fg, fd) -> 
      let rfg = (rank fg) and rfd = rank fd in 
      if rfg > rfd then (*On ne peut plus ajouter à gauche sans deséquilibrer*)
        let nfd = ajout_tasse fd x in N(( min rfg (rank nfd) )+1,e,fg,nfd)
      else
        let nfg = ajout_tasse fg x in N (( min rfd (rank nfg) )+1, e, nfg, fd) ;;

let rec retrait_tasse (h : 'a heapTree) : 'a heapTree * 'a = 
  match h with 
  | E -> failwith "Empty heap"
  | N(r, e, E, E) -> (E, e)
  | N(r, e, N(_,efg,E,E), E) -> (N (r-1, e, E,E), efg)
  | N(r, e, N(_,efg,E,E), N(_,efd,E,E)) -> (N (r-1, e, fg,E), efd)                             
  | N(r, e, fg, fd) -> _ (*TODO*)
;;
          
let rec ajout_feuille_iter (l : int list) (h : 'a heapTree) : 'a heapTree = 
  match l with 
  | [] -> h
  | hd::tl -> ajout_feuille_iter tl (ajout_tasse h hd);;
          
let rec retrait_feuille_iter (n : int) (h : 'a heapTree) : 'a heapTree = 
  if n = 0 then h else let ( h2, _) = retrait_tasse h in retrait_feuille_iter (n-1) h2;;
          
ajout_feuille_iter [1;2;3;4;5;6;7;9;10;11;12;13;14;15;16;17;18;19;20] E
    
  
  (N (-3, 1,
      N (-2, 2,
         N (3, 4, 
            N (2, 9, 
               N (1, 17, E, E), 
               N (1, 18, E, E)),
            N (2, 10, 
               N (1, 19, E, E), 
               N (1, 20, E, E))),
         N (-1, 5, 
            E, 
            N (1, 12, E, E))),
      N (3, 3, 
         N (2, 6, 
            N (1, 13, E, E), 
            N (1, 14, E, E)),
         N (2, 7, 
            N (1, 15, E, E), 
            N (1, 16, E, E)))),
   11)



