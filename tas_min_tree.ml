

(*Structure arborescente pour un tas minimum*)

type 'a tm_tree = Empty | Node of 'a* tm_tree * tm_tree;;

(*Fonction auxiliaire permettant de remonter la valeur de la dernière feuille du tas ainsi que le tas privé de cette dernière feuille*)
let rec remonteValDerniereFeuille (t : 'a tm_tree) : ('a tm_tree * 'a) = 
  match t with 
  | Empty -> failwith "structure normalement impossible"
  | Leaf (r) -> (Empty, r)
  | Node (r, fg, fd) ->
      (
        match (fg, fd) with 
        | (Empty, Empty) -> failwith "structure normalement impossible"
        | (Leaf(e), Empty) -> (Leaf(r),e)
        | (Leaf(eg), Leaf(ed)) -> (Node (r, fg, Empty), ed)
        | (Node (eg,_,_), Leaf (ed)) -> let (nfg, res) = remonteValDerniereFeuille fg in (Node(r, nfg, fd),res)  (*On va chercher à gauche pour garder le tassage*)
        | (Node (eg,_,_), Node (ed,_,_)) -> let (nfd, res) = remonteValDerniereFeuille fd in (Node(r, fg, nfd),res) (*On va chercher à droite pour garder le tassage*)
        | (_,_) -> failwith "tas invalide"
      );;


(*Fonction auxiliaire permettant de rééquilibrer le tas en redescendant la racine pour garder l'ordre*)
let rec descenteReeqRacine (t : 'a tm_tree)  : 'a tm_tree = 
  match t with 
  | Empty -> Empty
  | Leaf (e)-> Leaf(e)
  | Node (r, fg, fd)-> 
      (
        match (fg, fd) with 
        | (Empty, Empty) -> failwith "Tas invalide"
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
              
        | (Node (eg,fgfg,fgfd), Leaf (ed)) -> 
            if (eg < r) then 
              if (eg < ed) then 
                Node (eg, (descenteReeqRacine (Node(r, fgfg,fgfd))), fd) 
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
        | (_,_) -> failwith "tas invalide" 
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
    
                                              
         