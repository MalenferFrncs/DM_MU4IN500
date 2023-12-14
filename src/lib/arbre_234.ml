open Int128

type arbre234 = 
  | Empty
  | Feuille1 of Int128.t
  | Feuille2 of Int128.t * Int128.t
  | Noeud2 of Int128.t * arbre234 * arbre234 
  | Noeud3 of Int128.t * Int128.t * arbre234 * arbre234 * arbre234 
  | Noeud4 of Int128.t * Int128.t * Int128.t * arbre234 * arbre234 * arbre234 * arbre234 


(*Fonction testant si une clé est dans le nœud courant de l'arbre t*)
let est_dans_noeud (cle : Int128.t) (t : arbre234) : bool = 
  match t with 
  | Empty -> false
  | Feuille1(elt) -> Int128.eg elt cle 
  | Feuille2 (elt1,elt2) -> (Int128.eg elt1  cle) || (Int128.eg elt2  cle) 
  | Noeud2 (elt, _,_) -> Int128.eg elt cle 
  | Noeud3 (elt1,elt2,_,_,_) -> (Int128.eg elt1 cle) || (Int128.eg elt2 cle) 
  | Noeud4 (elt1,elt2,elt3,_,_,_,_) -> (Int128.eg elt1 cle) || (Int128.eg elt2 cle) || (Int128.eg elt3 cle) 

(*Fonction permettant de rechercher une clé dans l'arbre*)
let rec recherche (cle : Int128.t) (t : arbre234) : bool = 
  (est_dans_noeud cle t) ||(
    match t with 
    | Empty -> false
    | Feuille1(_) -> false
    | Feuille2(_,_) -> false
    | Noeud2 (elt, g,d) ->  if (Int128.inf cle elt) then recherche cle g else recherche cle d
    | Noeud3 (elt1, elt2, g, m, d) -> 
      if (Int128.inf cle elt1) then 
        recherche cle g 
      else if (Int128.inf cle elt2) then 
        recherche cle m
      else 
        recherche cle d
    | Noeud4 (elt1, elt2, elt3, g, m, n, d) ->
      if (Int128.inf cle elt1) then 
        recherche cle g 
      else if (Int128.inf cle elt2) then 
        recherche cle m 
      else if (Int128.inf cle elt3) then 
        recherche cle n 
      else 
        recherche cle d
  )

(*Exception utilisée pour l'ajout*)
exception Eclatement of (Int128.t * (arbre234) * (arbre234)) ;;

(*On suppose que cle n'est pas dans t
  Fonction permettant d'ajouter une clé à t*)
let ajout (cle : Int128.t) (t : arbre234) : arbre234 = 
  let rec insertion234 (cle : Int128.t) (t : arbre234) : arbre234 = 
    if (est_dans_noeud cle t) then t 
    else
      match t with 
      | Empty -> Feuille1(cle)
      | Feuille1(elt) -> 
        if (Int128.inf cle elt) then 
          Feuille2(cle,elt) 
        else 
          Feuille2(elt,cle)
      | Feuille2(elt1,elt2) -> 
        if (Int128.inf cle elt1) then 
          raise (Eclatement (elt1,Feuille1 (cle),Feuille1(elt2)))
        else if (Int128.inf cle elt2) then 
          raise (Eclatement (cle,Feuille1 (elt1),Feuille1(elt2))) 
        else 
          raise (Eclatement (elt2,Feuille1 (elt1),Feuille1(cle))) 
      | Noeud2 (elt, g,d) ->  
        if (Int128.inf cle elt) then 
          (try Noeud2 (elt,insertion234 cle g, d)
           with Eclatement (r,ng,nd) -> Noeud3(r,elt,ng, nd, d))
        else 
          (try Noeud2 (elt,g, insertion234 cle d)
           with Eclatement (r,ng,nd) -> Noeud3(elt, r,g, ng, nd)) 
      | Noeud3 (elt1, elt2, g,m,d) -> 
        if (Int128.inf cle elt1) then 
          (try (Noeud3 (elt1,elt2, (insertion234 cle g), m, d))
           with Eclatement(r,ng,nd) -> Noeud4(r,elt1,elt2, ng,nd, m, d))
        else if (Int128.inf cle elt2) then 
          (try (Noeud3 (elt1,elt2, g, insertion234 cle m, d))
           with Eclatement(r,ng,nd) -> Noeud4(elt1, r,elt2, g, ng,nd, d))
        else
          (try (Noeud3 (elt1,elt2, g,  m, insertion234 cle d))
           with Eclatement(r,ng,nd) -> Noeud4(elt1,elt2, r, g, m, ng,nd) )

      | Noeud4 (elt1, elt2, elt3, g, m, n, d) ->
        if (Int128.inf cle elt1) then 
          (*Insérer à g et récupérer un possible éclatement*)
          ( try (Noeud4 (elt1, elt2, elt3, insertion234 cle g, m, n ,d))
            with Eclatement (r,ng, nd) -> 
              raise (Eclatement (elt2,Noeud3 (r, elt1,ng,nd,m), Noeud2 (elt3, n, d)))
          )
        else if (Int128.inf cle elt2) then 
          (*Insérer à m et récupérer un possible éclatement*)
          ( try (Noeud4 (elt1, elt2, elt3, g,insertion234 cle m, n ,d))
            with Eclatement (r,ng, nd) -> 
              raise (Eclatement (elt2,Noeud3 ( elt1,r,g,ng,nd), Noeud2 (elt3, n, d)))
          )
        else if (Int128.inf cle elt3) then 
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
    else if (Int128.inf cle elt1) then 
      Noeud2(elt1, Feuille1(cle),Feuille1(elt2))
    else if (Int128.inf cle elt2) then
      Noeud2(cle, Feuille1(elt1),Feuille1(elt2)) 
    else
      Noeud2(elt2, Feuille1(elt1),Feuille1(cle)) 
  | _ -> try insertion234 cle t with Eclatement(r,g,d) -> Noeud2(r,g,d)





(*Fonction ajoutant itérativement toute une liste dans un arbre 234 (on suppose toutes les clés de l différentes)*)
let rec ajout_iteratif (l : Int128.t list) (acc : arbre234) : arbre234 = 
  match l with
  | [] -> acc
  | hd :: tl -> ajout_iteratif tl (ajout hd acc);;

(*Fonction permettant d'obtenir un fichier en langage dot pour avoir une représentation graphique de l'arbre 234*)
let to_dot (nom : string) (t : arbre234) : unit = 
  let f = open_out nom in 
  let rec aux  (t : arbre234) : unit = 
    match t with 
    | Empty -> ()
    | Feuille1 (elt) -> 
      Printf.fprintf f "\n%d [label = \"%s\",color =seagreen];" (Obj.magic t) (Int128.to_str elt)
    | Feuille2 (elt1, elt2) ->
      Printf.fprintf f "\n%d [fontsize = \"10pt\", label = \"%s | %s\", color = seagreen];" (Obj.magic t) (Int128.to_str elt1) (Int128.to_str elt2)
    | Noeud2(elt,g,d) ->       
      Printf.fprintf f "\n%d [ label = \"%s\"];" (Obj.magic t) (Int128.to_str elt);
      Printf.fprintf f "\n%d -> {%d,%d};" (Obj.magic t) (Obj.magic g) (Obj.magic d) ;
      aux g;
      aux d
    | Noeud3(elt1,elt2, g, m,d) ->
      Printf.fprintf f "\n%d [ fontsize = \"10pt\",label = \"<f0>%s | <f1>%s\"];" (Obj.magic t) (Int128.to_str elt1) (Int128.to_str elt2);
      Printf.fprintf f "\n%d:f0 -> %d" (Obj.magic t) (Obj.magic g);
      Printf.fprintf f "\n%d:f1 -> %d" (Obj.magic t) (Obj.magic m);
      Printf.fprintf f "\n%d:f1 -> %d" (Obj.magic t) (Obj.magic d);
      aux g; 
      aux m;
      aux d;
    |Noeud4(elt1,elt2,elt3, g, m,n,d) ->
      Printf.fprintf f "\n%d [fontsize = \"8pt\", label = \"<f0>%s | <f1>%s\ | <f2>%s\"];" (Obj.magic t) (Int128.to_str elt1) (Int128.to_str elt2)(Int128.to_str elt3);
      Printf.fprintf f "\n%d:f0 -> %d" (Obj.magic t) (Obj.magic g);
      Printf.fprintf f "\n%d:f1 -> %d" (Obj.magic t) (Obj.magic m);
      Printf.fprintf f "\n%d:f2 -> %d" (Obj.magic t) (Obj.magic n);
      Printf.fprintf f "\n%d:f2 -> %d" (Obj.magic t) (Obj.magic d); 
      aux g; 
      aux m;
      aux n; 
      aux d
  in 
  Printf.fprintf f "digraph g {\nnode [shape=record];\n"; 
  aux t ;
  Printf.fprintf f "\n}";
    close_out f
;;


