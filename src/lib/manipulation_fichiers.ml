open Int128;;

let write_complexite (nom_fichier : string) (data : (int*float) list) : unit = 
  let file = open_out nom_fichier in 
  let rec write_data_to_file (data : (int*float) list) : unit=
    match data with
    | [] -> ()
    | (abs,x1) :: tl -> 
      Printf.fprintf file "%d\t %f\n" abs x1 ;
      write_data_to_file tl 
  in write_data_to_file data ;
  close_out file;;

let write_comparaison_3_struct (nom_fichier : string) (data : float * float * float) : unit = 
  let f = open_out nom_fichier 
  and (arbre,tab, file) = data in 
  Printf.fprintf f "\"Tas min Arbre\"\t%.10f\n" arbre ;
  Printf.fprintf f "\"Tas min Tableau\"\t%.10f\n" tab ;
  Printf.fprintf f "\"File binomiale\"\t%.10f\n" file ;
  close_out f;;

