open Int32;;
(*open UnixLabels ;;
module Unix = UnixLabels ;; *)

(*Representation des entiers codes sur 128 bits*)

type entier128 = (Int32.t * Int32.t * Int32.t * Int32.t);;

let inf (cle1 : entier128) (cle2 : entier128) : bool = 
  let (a1, b1, c1, d1) = cle1 and (a2, b2, c2, d2) = cle2 in 
      if a1 = a2 then 
        if b1 = b2 then 
          if c1 = c2 then
            (Int32.unsigned_compare d1 d2) < 0
          else
            (Int32.unsigned_compare c1 c2) < 0
        else
          (Int32.unsigned_compare b1 b2) < 0
      else 
        (Int32.unsigned_compare a1 a2) < 0;;
      ;;

let inf (cle1 : entier128) (cle2 : entier128) : bool = 
  let (a1, b1, c1, d1) = cle1 and (a2, b2, c2, d2) = cle2 in 
  if a1 = a2 then 
    if b1 = b2 then 
      if c1 = c2 then
        if d1 = d2 then
          true
        else
          false
      else
        false
    else
      false
  else 
    false
  ;;

  let int_128_of_str(str:string):entier128 =
  let i = String.length str in 
  let x2 : int32 = Int32.of_string(String.cat "0x" (String.sub str (i-24) 8)) in
  let x3 : int32 = Int32.of_string(String.cat "0x"(String.sub str (i-16) 8)) in
  let x4 : int32 = Int32.of_string(String.cat "0x"(String.sub str (i-8) 8)) in 
  if i = 34 then 
    let x1 : int32 = Int32.of_string(String.sub str 0 10 ) in
    (x1,x2,x3,x4)
  else 
    let x1 : int32 = Int32.of_string(String.sub str 0 9 ) in 
    (x1,x2,x3,x4)
;;


(* representation des files binomials et des tournois binomiaux *)

type tournois_b = Racine of int * entier128 * (tournois_b list)  | Empty (*Racine(degree,clé,fils)*)
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
      if cle1 > cle2 then Racine ((deg2+1), cle2 , (t1::tl2))
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

(* ajouts des 4 fonctiosn fondamentals *)

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
let rec get_min (tl: tournois_b list) (i : entier128): tournois_b list * tournois_b =
  match tl with
  []-> [],Empty
  |Empty::tl -> [],Empty
  |Racine(deg,cle,fils)::tl -> if (inf cle i) then (    (* si on a une racine plus petite que tout les tournois precedant*)
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

let ajout_file (x:entier128) (f:file_b) : file_b =
  let tx : tournois_b = Racine(0,x,Empty::[]) in
  let fx : file_b = file tx in
  unionFile f fx 
;;

(* fin de l'ajout *)

(* debut de la construction *)

let construction_file (file_name : string) (nb_entier : int): file_b =
  let fileIN = open_in file_name in
  let rec loop file_bino nb_entier : file_b = 
    if nb_entier = 0 then file_bino
    else(
      let str : string = input_line fileIN in  
      loop (ajout_file (int_128_of_str str) file_bino) (nb_entier -1)  )
  in
  loop Empty nb_entier
;;

let list_of_file (file_name : string) (nb_entier : int ): entier128 list =
  let fileIN = open_in file_name in 
  let rec loop li_128 nb_entier : entier128 list =
    if nb_entier = 0 then li_128 
    else 
      let str : string = input_line fileIN in 
      loop ((int_128_of_str str)::li_128) (nb_entier -1 )
  in
  loop [] nb_entier
;; 


let rec construction_list (li : entier128 list) (acc : file_b): file_b =
  match  li  with
  | [] -> acc
  | hd::tl -> construction_list tl (ajout_file hd acc)
;;

(* fin de la construction *)

(* debut des test *)
let file_test = "cles_alea/jeu_3_nb_cles_200000.txt"
let file_test_2 = "cles_alea/jeu_1_nb_cles_200000.txt"

let () = 
  let li_int128_2 = list_of_file file_test_2 200000 in
  let t0 = Sys.time() in

  let f = construction_file file_test 200000 in

  let t1 = Sys.time() in

  let f2 = construction_list li_int128_2 Empty in

  let t2 = Sys.time() in

  let u = unionFile f f2 in

  let t3 = Sys.time() in

  let a = ajout_file (0l,120l,32l,3l) u in 

  let t4 = Sys.time() in

  let s = suppr_min a in

  let t5 = Sys.time() in

  let tmp_cons_1 = t1 -. t0  in
  let tmp_cons_2 = t2 -. t1 in 
  let tmp_union = t3 -. t2 in 
  let tmp_ajout = t4 -. t3 in 
  let tmp_suppr = t5 -. t4 in 

  print_string "temps de la 1er construction de 200 000 : "; print_float tmp_cons_1 ; print_endline "" ;
  print_string "temps de la 2nd construction de 200 000 : "; print_float tmp_cons_2 ; print_endline "" ;
  print_string "temps de l'union de 2 files de 200 000 : "; print_float tmp_union ; print_endline "" ;
  print_string "temps de l'ajout d'un element dans une file de 400 000 : "; print_float tmp_ajout ; print_endline "" ;
  print_string "temps de la supression d'un element dans une file de 400 001 : "; print_float tmp_suppr ; print_endline "" ;


  print_endline ""



(*
  print_int 0 ;
  print_endline "";
  let file : file_b = (construction_list ([(0l,0l,1l,0l);(1l,0l,1l,0l)]) Empty) in
  print_int 1 ;
  print_endline "";
  let fileIN = open_in file_test in
  print_int 2 ;
  print_endline "";
  let i = (int_128_of_str "0xdf6943ba6d51464f6b02157933bdd9ad") in
  print_int 3 ;
  print_endline "";
  print_string (input_line fileIN);
  print_endline "" *)
