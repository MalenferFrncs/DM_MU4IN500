
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
