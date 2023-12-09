open Int128


type tournois_b = Racine of int * Int128.t * (tournois_b list)  | Empty (*Racine(degree,clé,fils)*)
type file_b = File of int * (tournois_b list) | Empty (*File(indice,tournois) tournois le plus petit a droite de la liste *) 


(* ajouts des primitives sur les files binomiales et les tournois binomiaux *)
let est_vide_t (t : tournois_b) : bool =
  match t with
    Empty -> true
  | _ -> false
;;

let degree (t:tournois_b) : int =
  match t with
    Racine (deg,cle,tl) -> deg
  | Empty -> 0 
;;

let union2Tid (t1:tournois_b) (t2:tournois_b) : tournois_b =
  match t1,t2 with
    Racine(deg1,cle1,tl1),Racine(deg2,cle2,tl2) -> 
    if (Int128.inf cle2 cle1) then Racine ((deg2+1), cle2 , (t1::tl2))
    else Racine ((deg1+1),cle1,(t2::tl1))

  |_ -> Empty
;;

let rec  pow_2 (n:int) : int =
  if n = 0 then 1 
  else
    Int.shift_left 1 n
;;

let rec tournois_reverse (ol : tournois_b list ) (nl : tournois_b list): tournois_b list = 
  match ol with
  | Empty::_ | [] -> nl 
  | e::Empty::_ | e::[] -> e::nl
  | e::tl -> tournois_reverse tl (e::nl)

let decapiter (t: tournois_b) : file_b =
  match t with
  |Empty -> Empty
  |Racine(deg,cle,tl) -> File(((pow_2  deg )-1),(tournois_reverse tl []))
;;

let file(t:tournois_b) : file_b =
  match t with
  | Empty -> Empty
  | Racine(deg,cle,tl) -> File((pow_2 deg ),t::[])
;;

let est_vide_f (f: file_b ) : bool =
  match f with
  | Empty -> true
  | _-> false
;;

let rec last_tournois  (li : tournois_b list) : tournois_b =
  match li with
  |e::[] -> e
  |e::tl -> last_tournois tl
  |[] -> Empty
;;


let mindeg (f:file_b) : tournois_b =
  match f with
  |Empty |File(_,[]) -> Empty 
  |File(indice,e::tl) -> e
;;

let reste (f:file_b) : file_b =
  match f with
  |Empty -> Empty  (* si on donne une file vide en entré ou renvois une file vide*)
  |File(indice,tl)->
    match tl with  (* on match le couple (tournois restant, ancien dernier tournois)*)
    |[]|Empty::_ |    (* deux cas ou on a des listes de tournois vides*)
     _::Empty::_ | _::[] -> Empty  (* desux cas ou on a des listes de tournois de 1 element (file vide après retrait)*)
    |Racine(deg,cle,fils)::li-> File((indice-(pow_2 deg)),li)  (* on renvois la nouvelle file d'indice (n-nombre de noeud de l'ancien dernier tournois) *)
;;

let ajout_min (t:tournois_b) (f:file_b) : file_b = 
  match t,f with
  | Empty,Empty -> Empty  (* on donne un tournois et une file vide on renvois une file vide*)
  | Empty,fi -> fi (* on donne un tournois vide et une file  on renvoit la file*)
  | tr,Empty -> file t (* on donne un tournois et une file vide on renvoie la file composé du tournois *)
  | Racine(deg,cle,fils),File(indice,lt)-> File((indice+ (pow_2  deg )),(Racine(deg,cle,fils)::lt) ) (*tournois et file generals, on ajoute le nombre de noeuds du tournois au degrés de la file et on ajoute le tournois au debut de la liste de tournois de file*)
;;


(* fin de l'ajout des primitives sur les tournois binomiaux et les fils binomiales *)

(* ajouts des 4 fonctions fondamentales *)

(*ajout de l'Union *)

let rec unionFile (f1:file_b) (f2:file_b) : file_b =

  let rec uFret (f1 : file_b) (f2:file_b) (t:tournois_b) :file_b = 
    if est_vide_t t then  (* pas de tournois en retenu*)


      if est_vide_f f1 then f2
      else if est_vide_f f2 then f1 
      else
        let t1 = mindeg f1 in 
        let t2 = mindeg f2 in

        if (degree t1) < (degree t2) then ajout_min t1 (unionFile (reste f1) f2)
        else if (degree t2) < (degree t1) then ajout_min t2 (unionFile (reste f2)f1)
        else uFret (reste f1) (reste f2) (union2Tid t1 t2)


    else (*  t tournois en retenue *)
    if est_vide_f f1 then unionFile (file t) f2
    else if est_vide_f f2 then unionFile (file t) f1
    else
      let t1 = mindeg f1 in 
      let t2 = mindeg f2 in 

      if ((degree t) < (degree t1)) && ((degree t) < (degree t2)) then
        ajout_min t (unionFile f1 f2)
      else if ( degree t = degree t1) && (degree t = degree t2) then 
        ajout_min t (uFret (reste f1) (reste f2) (union2Tid t1 t2))
      else if (degree t = degree t1) && (degree t < degree t2) then
        uFret (reste f1) f2 (union2Tid t1 t) 
      else 
        uFret (reste f2) f1 (union2Tid t2 t)
  in

  uFret f1 f2 Empty
;;


(* fin de l'union *)


(* debut de Suppr min *) 
let rec get_min (tl: tournois_b list) (i : Int128.t): tournois_b list * tournois_b =
  match tl with
    []-> [],Empty
  |Empty::tl -> [],Empty
  |Racine(deg,cle,fils)::tl -> if (Int128.inf cle i) then (    (* si on a une racine plus petite que tout les tournois precedant*)
      match (get_min tl cle) with
      |li,Empty-> li,Racine(deg,cle,fils)   (* si on  a pas de tournois plus a droite *)
      |li,Racine(deg2,cle2,fils2)-> Racine(deg,cle,fils)::li,Racine(deg2,cle2,fils2)  (* si on a un tournois plus petit a droite*)
    )
    else  (* si on a un racine plus petite dans un tournois a gauche *)
      let tl,min = get_min tl i in 
      Racine(deg,cle,fils)::tl,min   (*on renvoit la liste privié du plus petit tournois + le tournois actuel et le plus petit tournois*)
;;

let suppr_min (f:file_b) : file_b =
  match f with
  |Empty -> Empty
  |File(indice,tournois) ->   (*si la file est normal*)
    match tournois with
    |[]->Empty
    |Empty::tl -> Empty 
    |Racine(deg,cle,fils)::tl-> 
      let list_sans_min,tournois_min = get_min tournois cle in 
      (unionFile (File((indice - (pow_2  (degree tournois_min) )),list_sans_min)) (decapiter tournois_min))
      (* union entre la file privé de son tournois min et de la file produite par le tournois min decapité*)
;;

(* fin supression pin*)


(* debut ajout *)

let ajout (x:Int128.t) (f:file_b) : file_b =
  let tx : tournois_b = Racine(0,x,Empty::[]) in
  let fx : file_b = file tx in
  unionFile f fx 
;;

(* fin de l'ajout *)

(* debut de la construction *)

let rec construction (liste_cles : Int128.t list): file_b =

  let rec loop (liste_cles : Int128.t list)  (file_bino : file_b) : file_b = 
    match liste_cles with 
    | [] -> file_bino
    | hd::tl -> (loop tl (ajout hd file_bino))
  in
  loop liste_cles Empty;;




let rec to_dot (nom : string) (file_bino : file_b) : unit = 
  let f = open_out nom in 
  let rec fils_to_dot (pere : tournois_b)(tb : tournois_b) : unit = 
    match tb with 
    | Empty -> ()
    | _ -> Printf.fprintf f "\n %d -> %d;" (Obj.magic pere) (Obj.magic tb) ;
  and tournois_to_dot (tb : tournois_b) : tournois_b list = 
    match tb with 
    | Empty -> []
    | Racine (_,elt, fils)->
      match fils with 
      | [] -> Printf.fprintf f "\n%d [shape = box, style = \"rounded,bold\", label = \"%s\",color =seagreen];" (Obj.magic tb) (Int128.to_str elt) ;[]
      | [Empty] -> Printf.fprintf f "\n%d [shape = box, style = \"rounded,bold\", label = \"%s\",color =seagreen];" (Obj.magic tb) (Int128.to_str elt) ;[]
      | hd::tl -> Printf.fprintf f "\n%d [shape = box, style = \"rounded,bold\", label = \"%s\",color =sienna];" (Obj.magic tb) (Int128.to_str elt);
        List.map (fils_to_dot tb) fils ; 
        (tournois_to_dot hd)@tl
  and liste_tournois_to_dot (l : tournois_b list) : unit = 
    match l with 
    | [] -> ()
    | hd::tl -> liste_tournois_to_dot ((tournois_to_dot hd)@ tl)
  in
  Printf.fprintf f "digraph g {\n";   (*Préambule*) 
  (match file_bino with 
   |Empty -> ()
   |File (_, liste) -> liste_tournois_to_dot liste);
  Printf.fprintf f "}\n";
  close_out f;

