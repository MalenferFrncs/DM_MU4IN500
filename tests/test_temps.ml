open Int128
open Tas_min_arbre
open Tas_min_tab
open Manipulation_fichiers
open File_binomiale


let time_of (f : 'a -> 'b) (arg : 'a): float =
  let debut = Sys.time() in
  let _ = f arg in
  let fin = Sys.time() in
  fin -. debut
;;

let temps_moy (nb_cles : int) (nb_jeu : int) (f : 'a ->'b) : float = 
  let sum = ref 0.0 in
  for i = 1 to nb_jeu do
    let l = Int128.list_of_file (Printf.sprintf "../src/jeux_de_données/cles_alea/jeu_%d_nb_cles_%d.txt" i nb_cles) nb_cles
    in sum := !sum +. (time_of f l)
  done;
  !sum /. Int.to_float nb_jeu;;



let complexite_temporelle (f1 : 'a -> 'b) : (int*float) list = 
  let rec aux (liste_echantillons : int list) (acc : (int*float) list): (int*float) list = 
    match liste_echantillons with 
    | [] -> acc
    | hd :: tl -> 
      let tm_f1 = temps_moy hd 5 f1
      in
      (aux tl ((hd,tm_f1)::acc) )
  in 
  (aux [1000;5000;10000;20000;50000;80000;120000;200000] []);;

let temps_moy_union (nb_cles : int) (nb_jeu : int) (construction : Int128.t list -> 'a) (f : 'a -> 'a -> 'a) : float = 
  let sum = ref 0.0 in
  for i = 1 to nb_jeu do 
    for j = 1 to nb_jeu do 
      if i != j then 
        let tas1 = (construction (Int128.list_of_file (Printf.sprintf "../src/jeux_de_données/cles_alea/jeu_%d_nb_cles_%d.txt" i nb_cles) nb_cles))
        and tas2 = (construction (Int128.list_of_file (Printf.sprintf "../src/jeux_de_données/cles_alea/jeu_%d_nb_cles_%d.txt" j nb_cles) nb_cles))
        in 
        sum := !sum +. (time_of (f tas1) tas2);
    done;
  done ; 
  !sum /. Int.to_float (nb_jeu * (nb_jeu -1))
;;

let complexite_temporelle_union (construction : Int128.t list -> 'a) (f1 : 'a -> 'a -> 'a) : (int*float) list = 
  
  let rec aux (liste_echantillons : int list) (acc : (int*float) list): (int*float) list = 
    match liste_echantillons with 
    | [] -> acc
    | hd :: tl -> 
      let tm_f1 = temps_moy_union hd 5 construction f1
      in
      (aux tl ((hd,tm_f1)::acc) )
  in 
  (aux [1000;5000;10000;20000;50000;80000;120000;200000] []);;


Manipulation_fichiers.write_complexite "complexite_cons_file.txt"  (complexite_temporelle File_binomiale.construction);;
Manipulation_fichiers.write_complexite "complexite_union_file.txt"  (complexite_temporelle_union  File_binomiale.construction File_binomiale.unionFile);;

Manipulation_fichiers.write_complexite "complexite_ajout_arbre.txt"  (complexite_temporelle Tas_min_arbre.ajout_iteratif);;
Manipulation_fichiers.write_complexite "complexite_ajout_tab.txt"  (complexite_temporelle Tas_min_tab.ajout_iteratif);;
Manipulation_fichiers.write_complexite "complexite_cons_arbre.txt"  (complexite_temporelle Tas_min_arbre.construction);;
Manipulation_fichiers.write_complexite "complexite_cons_tab.txt"  (complexite_temporelle Tas_min_tab.construction);;
Manipulation_fichiers.write_complexite "complexite_union_arbre.txt"  (complexite_temporelle_union Tas_min_arbre.construction Tas_min_arbre.union);;
Manipulation_fichiers.write_complexite "complexite_union_tab.txt"  (complexite_temporelle_union Tas_min_tab.construction Tas_min_tab.union);;





