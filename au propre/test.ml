open Int128
open Tas_min_tab
open Tas_min_arbre

let list_of_file (file_name : string) (nb_entier : int ): Int128.t list =
  let fileIN = open_in file_name in 
  let rec loop li_128 nb_entier : Int128.t list =
    if nb_entier = 0 then li_128 
    else 
      let str : string = input_line fileIN in 
      loop ((Int128.of_str str)::li_128) (nb_entier -1 )
  in
  loop [] nb_entier
;; 

Printf.printf "coucou";;
let l1 = list_of_file "../cles_alea/jeu_1_nb_cles_1000.txt" 31 in 
(Tas_min_tab.to_dot "tas_tab.dot" (Tas_min_tab.construction l1));;


let l2 = list_of_file "../cles_alea/jeu_1_nb_cles_1000.txt" 31 in 
(Tas_min_arbre.to_dot "tas_arbre.dot" (Tas_min_arbre.construction l2));;


