

type arbre234 = 
  | Feuille1 of int
  | Feuille2 of int * int
  | Noeud2 of int * arbre234 * arbre234 
  | Noeud3 of int * int * arbre234 * arbre234 * arbre234 
  | Noeud4 of int * int * int * arbre234 * arbre234 * arbre234 * arbre234 
                
let est_dans_noeud (cle : int) (t : arbre234) : bool = 
  match t with 
  | Feuille1(elt) -> elt = cle (*À CHANGER*)
  | Feuille2 (elt1,elt2) -> elt1 = cle || elt2 = cle (*À CHANGER*)
  | Noeud2 (elt, _,_) -> elt = cle (*À CHANGER*)
  | Noeud3 (elt1,elt2,_,_,_) -> elt1 = cle || elt2 = cle (*À CHANGER*)
  | Noeud4 (elt1,elt2,elt3,_,_,_,_) -> elt1 = cle || elt2 = cle || elt3 = cle (*À CHANGER*)
                                                                   
let rec recherche (cle : int) (t : arbre234) : bool = 
  (est_dans_noeud cle t) ||(
    match t with 
    | Feuille1(_) -> false
    | Feuille2(_,_) -> false
    | Noeud2 (elt, g,d) ->  if cle < elt then recherche cle g else recherche cle d
    | Noeud3 (elt1, elt2, g, m, d) -> 
        if cle < elt1 then (*À CHANGER*)
          recherche cle g 
        else if cle < elt2 then (*À CHANGER*)
          recherche cle m
        else 
          recherche cle d
    | Noeud4 (elt1, elt2, elt3, g, m, n, d) ->
        if cle < elt1 then (*À CHANGER*)
          recherche cle g 
        else if cle < elt2 then (*À CHANGER*)
          recherche cle m 
        else if cle < elt3 then (*À CHANGER*)
          recherche cle n 
        else 
          recherche cle d
  )
            
exception Eclatement of (int * (arbre234) * (arbre234)) ;;

let ajout (cle : int) (t : arbre234) : arbre234 = 
  let rec insertion234 (cle : int) (t : arbre234) : arbre234 = 
    match t with 
    | Feuille1(elt) -> 
        if (cle < elt) then (*À CHANGER*)
          Feuille2(cle,elt) else Feuille2(elt,cle)
    | Feuille2(elt1,elt2) -> 
        if (cle < elt1) then (*À CHANGER*)
          raise (Eclatement (elt1,Feuille1 (cle),Feuille1(elt2)))
        else if (cle < elt2) then (*À CHANGER*)
          raise (Eclatement (cle,Feuille1 (elt1),Feuille1(elt2))) 
        else 
          raise (Eclatement (elt2,Feuille1 (elt1),Feuille1(cle))) 
    | Noeud2 (elt, g,d) ->  
        if cle < elt then (*À CHANGER*)
          (try Noeud2 (elt,insertion234 cle g, d)
           with Eclatement (r,ng,nd) -> Noeud3(r,elt,ng, nd, d))
        else 
          (try Noeud2 (elt,g, insertion234 cle d)
           with Eclatement (r,ng,nd) -> Noeud3(elt, r,g, d, nd)) 
    | Noeud3 (elt1, elt2, g,m,d) -> 
        if cle < elt1 then (*À CHANGER*)
          (try (Noeud3 (elt1,elt2, (insertion234 cle g), m, d))
           with Eclatement(r,ng,nd) -> Noeud4(r,elt1,elt2, ng,nd, m, d))
        else if cle < elt2 then 
          (try (Noeud3 (elt1,elt2, g, insertion234 cle m, d))
           with Eclatement(r,ng,nd) -> Noeud4(elt1, r,elt2, g, ng,nd, d))
        else
          (try (Noeud3 (elt1,elt2, g,  m, insertion234 cle d))
           with Eclatement(r,ng,nd) -> Noeud4(elt1,elt2, r, g, m, ng,nd) )
                                      
    | Noeud4 (elt1, elt2, elt3, g, m, n, d) ->
        if cle < elt1 then (*À CHANGER*)
        (*Insérer à g et récupérer un possible éclatement*)
          ( try (Noeud4 (elt1, elt2, elt3, insertion234 cle g, m, n ,d))
            with Eclatement (r,ng, nd) -> 
              raise (Eclatement (elt2,Noeud3 (r, elt1,ng,nd,m), Noeud2 (elt3, n, d)))
          )
        else if cle < elt2 then (*À CHANGER*)
        (*Insérer à m et récupérer un possible éclatement*)
          ( try (Noeud4 (elt1, elt2, elt3, g,insertion234 cle m, n ,d))
            with Eclatement (r,ng, nd) -> 
              raise (Eclatement (elt2,Noeud3 ( elt1,r,g,ng,nd), Noeud2 (elt3, n, d)))
          )
        else if cle < elt3 then (*À CHANGER*)
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
      else if cle < elt1 then 
        Noeud2(elt1, Feuille1(cle),Feuille1(elt2))
      else if cle < elt2 then
        Noeud2(cle, Feuille1(elt1),Feuille1(elt2)) 
      else
        Noeud2(elt2, Feuille1(elt1),Feuille1(cle)) 
  | _ -> try insertion234 cle t with Eclatement(r,g,d) -> Noeud2(r,g,d)
                        
                                                            

let rec ajout_successif (l : int list) (acc : arbre234) : arbre234 = 
  match l with
  | [] -> acc
  | hd :: tl -> ajout_successif tl (ajout hd acc)
                  
                  
                  (*ATTENTION AUX DÉDOUBLEMENTS !!!*)
                  
let l = [1;2;3;4;5;6;7;8;9;10;11;12] in ajout_successif l (Feuille1(0));;


