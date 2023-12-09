open Int128
open Tas_min_tab
open Tas_min_arbre
open File_binomiale
open Arbre_234
open Manipulation_fichiers
open Md5
open Liste_mots;;


let time_of (f : 'a -> 'b) (arg : 'a): float =
  let debut = Sys.time() in
  let _ = f arg in
  let fin = Sys.time() in
  fin -. debut
;;



let experimentation_cons (liste_mots : Int128.t list) : unit = 
  let cons_a = time_of Tas_min_arbre.construction liste_mots and
  cons_t = time_of Tas_min_tab.construction liste_mots and
  cons_f = time_of File_binomiale.construction liste_mots in

  Manipulation_fichiers.write_comparaison_3_struct "construction_shakespeare.txt" (cons_a, cons_t, cons_f) ;;


let experimentation_supprMin (liste_mots : Int128.t list) : unit = 
  let tas_min_a = Tas_min_arbre.construction liste_mots
  and tas_min_t = Tas_min_tab.construction liste_mots 
  and file = File_binomiale.construction liste_mots
  in 
  let suppr_a = time_of Tas_min_arbre.supprMin tas_min_a
  and suppr_t = time_of Tas_min_tab.supprMin tas_min_t
  and suppr_f = time_of File_binomiale.suppr_min file 

  in Manipulation_fichiers.write_comparaison_3_struct "suppr_Min_shakespeare.txt" (suppr_a,suppr_t,suppr_f);;

let experimentation_ajout (liste_mots : Int128.t list) : unit = 
  let tas_min_a = Tas_min_arbre.construction liste_mots
  and tas_min_t = Tas_min_tab.construction liste_mots 
  and file = File_binomiale.construction liste_mots in

  let (nta, suppr_a) = Tas_min_arbre.supprMin tas_min_a
  and (Some(suppr_t), ntt) = Tas_min_tab.supprMin tas_min_t
  and nf= File_binomiale.suppr_min file  in

  let ajout_a = time_of (Tas_min_arbre.ajout nta) suppr_a
  and ajout_t = time_of (Tas_min_tab.ajout ntt ) suppr_t
  and ajout_f = time_of (File_binomiale.ajout  suppr_t) nf 

  in Manipulation_fichiers.write_comparaison_3_struct "ajout_shakespeare.txt" (ajout_a,ajout_t,ajout_f);;


let experimentation_union (liste_mots : Int128.t list) (n : int) : unit = 

  let union_a = ref 0.0 and union_t = ref 0.0 and union_f = ref 0.0 in 

  for i = 0 to n do 
    let (l1,l2) = List.partition (fun x -> Random.bool())  liste_mots in 
    let (t1a,t2a) = ((Tas_min_arbre.construction l1), (Tas_min_arbre.construction l2))
    and (t1t,t2t) = ((Tas_min_tab.construction l1), (Tas_min_tab.construction l2)) 
    and (f1,f2) = ((File_binomiale.construction l1), (File_binomiale.construction l2)) in

    union_a := !union_a +. time_of (Tas_min_arbre.union t1a) t2a ;
    union_t := !union_t +. time_of (Tas_min_tab.union t1t) t2t;
    union_f := !union_f +. time_of (File_binomiale.unionFile f1) f2;
  done ; 

  let nf = Int.to_float n in 
  Manipulation_fichiers.write_comparaison_3_struct "union_shakespeare.txt" (!union_a/.nf, !union_t/.nf, !union_f/.nf);;


Random.self_init();;

let l = (Liste_mots.extraire_liste_rep "../src/jeux_de_données/Shakespeare/") in Printf.printf "Nombre de mots différents dans l'œuvre de Shakespeare : %d\n" (List.length l) ;
experimentation_ajout l ; experimentation_cons l ; experimentation_supprMin l ; experimentation_union l 20;;
