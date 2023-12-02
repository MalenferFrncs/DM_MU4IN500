open Int128

type arbre234 = 
  | Feuille1 of Int128.t
  | Feuille2 of Int128.t * Int128.t
  | Noeud2 of Int128.t * arbre234 * arbre234 
  | Noeud3 of Int128.t * Int128.t * arbre234 * arbre234 * arbre234 
  | Noeud4 of Int128.t * Int128.t * Int128.t * arbre234 * arbre234 * arbre234 * arbre234 
                
let est_dans_noeud (cle : Int128.t) (t : arbre234) : bool = 
  match t with 
  | Feuille1(elt) -> Int128.eg elt cle 
  | Feuille2 (elt1,elt2) -> (Int128.eg elt1  cle) || (Int128.eg elt2  cle) 
  | Noeud2 (elt, _,_) -> Int128.eg elt cle 
  | Noeud3 (elt1,elt2,_,_,_) -> (Int128.eg elt1 cle) || (Int128.eg elt2 cle) 
  | Noeud4 (elt1,elt2,elt3,_,_,_,_) -> (Int128.eg elt1 cle) || (Int128.eg elt2 cle) || (Int128.eg elt3 cle) (*À CHANGER*)
                                                                   
let rec recherche (cle : Int128.t) (t : arbre234) : bool = 
  (est_dans_noeud cle t) ||(
    match t with 
    | Feuille1(_) -> false
    | Feuille2(_,_) -> false
    | Noeud2 (elt, g,d) ->  if (Int128.eg cle elt) then recherche cle g else recherche cle d
    | Noeud3 (elt1, elt2, g, m, d) -> 
        if (Int128.eg cle elt1) then (*À CHANGER*)
          recherche cle g 
        else if (Int128.eg cle elt2) then (*À CHANGER*)
          recherche cle m
        else 
          recherche cle d
    | Noeud4 (elt1, elt2, elt3, g, m, n, d) ->
        if (Int128.eg cle elt1) then (*À CHANGER*)
          recherche cle g 
        else if (Int128.eg cle elt2) then (*À CHANGER*)
          recherche cle m 
        else if (Int128.eg cle elt3) then (*À CHANGER*)
          recherche cle n 
        else 
          recherche cle d
  )
            
exception Eclatement of (Int128.t * (arbre234) * (arbre234)) ;;

let ajout (cle : Int128.t) (t : arbre234) : arbre234 = 
  let rec insertion234 (cle : Int128.t) (t : arbre234) : arbre234 = 
    match t with 
    | Feuille1(elt) -> 
        if (Int128.eg cle elt) then (*À CHANGER*)
          Feuille2(cle,elt) 
        else 
          Feuille2(elt,cle)
    | Feuille2(elt1,elt2) -> 
        if (Int128.eg cle elt1) then (*À CHANGER*)
          raise (Eclatement (elt1,Feuille1 (cle),Feuille1(elt2)))
        else if (Int128.eg cle elt2) then (*À CHANGER*)
          raise (Eclatement (cle,Feuille1 (elt1),Feuille1(elt2))) 
        else 
          raise (Eclatement (elt2,Feuille1 (elt1),Feuille1(cle))) 
    | Noeud2 (elt, g,d) ->  
        if (Int128.eg cle elt) then (*À CHANGER*)
          (try Noeud2 (elt,insertion234 cle g, d)
           with Eclatement (r,ng,nd) -> Noeud3(r,elt,ng, nd, d))
        else 
          (try Noeud2 (elt,g, insertion234 cle d)
           with Eclatement (r,ng,nd) -> Noeud3(elt, r,g, ng, nd)) 
    | Noeud3 (elt1, elt2, g,m,d) -> 
        if (Int128.eg cle elt1) then (*À CHANGER*)
          (try (Noeud3 (elt1,elt2, (insertion234 cle g), m, d))
           with Eclatement(r,ng,nd) -> Noeud4(r,elt1,elt2, ng,nd, m, d))
        else if (Int128.eg cle elt2) then 
          (try (Noeud3 (elt1,elt2, g, insertion234 cle m, d))
           with Eclatement(r,ng,nd) -> Noeud4(elt1, r,elt2, g, ng,nd, d))
        else
          (try (Noeud3 (elt1,elt2, g,  m, insertion234 cle d))
           with Eclatement(r,ng,nd) -> Noeud4(elt1,elt2, r, g, m, ng,nd) )
                                      
    | Noeud4 (elt1, elt2, elt3, g, m, n, d) ->
        if (Int128.eg cle elt1) then (*À CHANGER*)
        (*Insérer à g et récupérer un possible éclatement*)
          ( try (Noeud4 (elt1, elt2, elt3, insertion234 cle g, m, n ,d))
            with Eclatement (r,ng, nd) -> 
              raise (Eclatement (elt2,Noeud3 (r, elt1,ng,nd,m), Noeud2 (elt3, n, d)))
          )
        else if (Int128.eg cle elt2) then (*À CHANGER*)
        (*Insérer à m et récupérer un possible éclatement*)
          ( try (Noeud4 (elt1, elt2, elt3, g,insertion234 cle m, n ,d))
            with Eclatement (r,ng, nd) -> 
              raise (Eclatement (elt2,Noeud3 ( elt1,r,g,ng,nd), Noeud2 (elt3, n, d)))
          )
        else if (Int128.eg cle elt3) then 
        (*Insérer à n et récupérer un possible éclatement*)
          ( try (Noeud4 (elt1, elt2, elt3, g,m, insertion234 cle n ,d))
            with Eclatement (r,ng, nd) -> 
              raise (Eclatement (elt2,Noeud2 ( elt1,g,m), Noeud3 (r,elt3, ng,nd, d)))
          )
        else 
        (*Insérer à d et récupérer un possible éclatement*)
          ( try (Noeud4 (elt1, elt2, elt3, g,m, n , insertion234 cle d))
            with Eclatement (r,ng, nd) -> 
              raise (Eclatement (elt2,Noeud2 ( elt1,g,m), Noeud3 (elt3,r, n, ng,nd)))
          )
  in 
  match t with 
  | Feuille2(elt1,elt2) -> 
      if cle = elt1 || cle = elt2 then 
        t 
      else if (Int128.eg cle elt1) then 
        Noeud2(elt1, Feuille1(cle),Feuille1(elt2))
      else if (Int128.eg cle elt2) then
        Noeud2(cle, Feuille1(elt1),Feuille1(elt2)) 
      else
        Noeud2(elt2, Feuille1(elt1),Feuille1(cle)) 
  | _ -> try insertion234 cle t with Eclatement(r,g,d) -> Noeud2(r,g,d)
                        
                                   
  


let rec ajout_successif (l : Int128.t list) (acc : arbre234) : arbre234 = 
  match l with
  | [] -> acc
  | hd :: tl -> ajout_successif tl (ajout hd acc);;


(*À FINIR !!!*)
exception Unexpected;; (*Comportement indéfini, ne devrait pas arriver *)
exception Fils_Unique of arbre234 ;;
    
          
let suppression (cle : int) (t : arbre234) = 
  let rec suppr (t : arbre234) : arbre234 = 
    match t with 
    | Empty -> raise Unexpected
    | Feuille1(elt) -> if x = cle then Empty else t 
    | Feuille2 (elt1,elt2) -> if elt1 = cle then Feuille1(elt2) else if elt2 = cle then Feuille1(elt1) else t
    | Noeud2 (_, Feuille(eltg),d) -> if eltg = cle then raise (FilsUnique d)
    | Noeud2 (_,g, Feuille(eltd)) -> if eltd = cle then raise (FilsUnique g)
          


                  
                  
                  
