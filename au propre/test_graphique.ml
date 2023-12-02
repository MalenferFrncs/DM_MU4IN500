open Int128
open Tas_min_tab
open Tas_min_arbre
open Manipulation_fichiers;;

let l1 = Int128.list_of_file "../cles_alea/jeu_1_nb_cles_1000.txt" 31 in 
(Tas_min_tab.to_dot "tas_tab_cons.dot" (Tas_min_tab.construction l1));
let ajout = Tas_min_tab.ajout_iteratif l1 in
Tas_min_tab.to_dot "tas_tab_ajouts.dot" (ajout);
let (min,tas) = (Tas_min_tab.supprMin ajout) in Tas_min_tab.to_dot "tas_tab_ajouts_supprMin.dot" tas;
;;


let l2 = Int128.list_of_file "../cles_alea/jeu_1_nb_cles_1000.txt" 31 in 
(Tas_min_arbre.to_dot "tas_arbre_cons.dot" (Tas_min_arbre.construction l2));
let ajout = Tas_min_arbre.ajout_iteratif l2 in
Tas_min_arbre.to_dot "tas_arbre_ajouts.dot" (ajout);
let (tas, min) = (Tas_min_arbre.supprMin ajout) in Tas_min_arbre.to_dot "tas_arbre_ajouts_supprMin.dot" tas;
;;

let l3 = Int128.list_of_file "../cles_alea/jeu_1_nb_cles_1000.txt" 15 in 
(File_binomiale.to_dot "file_binomiale_cons.dot" (File_binomiale.construction l3));
;;



