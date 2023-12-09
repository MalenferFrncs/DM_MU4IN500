open Int128
open Tas_min_tab
open Tas_min_arbre
open File_binomiale
open Arbre_234
open Manipulation_fichiers
open Md5;;


let filtregauche (ligne : string ) : string = 
  let l = String.split_on_char '[' ligne in 
  match l with 
  | [] -> failwith "ligne vide !"
  | [s1] -> s1
  | [s1;s2] -> s2
  | _ -> failwith "forme invalide !"

let filtre_droite (ligne : string ) : string = 
  let l = String.split_on_char ']' ligne in 
  match l with 
  | [] -> failwith "ligne vide !"
  | [s1] -> s1 
  | [s1;s2] -> s1
  | _ -> failwith "forme invalide !"

let extraire_mot (ligne : string) : string = 
  let filtre1 = filtregauche ligne in filtre_droite filtre1



let extraire_liste (nom : string) (liste : Int128.t list) (arbre : Arbre_234.arbre234): Int128.t list* Arbre_234.arbre234 = 
  let f = open_in nom in
  let rec aux (liste : Int128.t list) (arbre : Arbre_234.arbre234) : Int128.t list * Arbre_234.arbre234 = 
    try (
      let mot : string = (extraire_mot (input_line f ))in
      let hash_mot : Int128.t = Md5.digest mot in 
      if Arbre_234.recherche hash_mot arbre then 
        aux liste arbre
      else 
        aux (hash_mot::liste) (Arbre_234.ajout hash_mot arbre)

    )
    with End_of_file -> close_in f  ; (List.rev liste , arbre) 
  in
  (aux liste arbre) 


let extraire_liste_rep (path_rep : string) : Int128.t list = 
  let rec aux (liste_nom_fichier : string list) (liste_mot : Int128.t list ) (arbre : Arbre_234.arbre234): Int128.t list =
    match liste_nom_fichier with 
    | [] -> liste_mot
    | hd :: tl -> let (nliste_mot, narbre) = extraire_liste (String.cat path_rep hd) liste_mot arbre in  (aux tl nliste_mot narbre)
  in aux  (Array.to_list (Sys.readdir path_rep)) [] Empty;;





