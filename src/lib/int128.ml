open Int32;;

(*Représentation des entiers codés sur 128 bits*)

type t = (Int32.t * Int32.t * Int32.t * Int32.t)
let cmp (cle1 : t) (cle2 : t) : int = 
  let (a1, b1, c1, d1) = cle1 and (a2, b2, c2, d2) = cle2 in 
  if a1=a2 then 
    if b1=b2 then 
      if c1 = c2 then
        (Int32.unsigned_compare d1 d2)
      else
        (Int32.unsigned_compare c1 c2)
    else
      (Int32.unsigned_compare b1 b2) 
  else 
    (Int32.unsigned_compare a1 a2)

(*Prédicat inf sur deux clés*)
let inf (cle1 : t) (cle2 : t) : bool = (cmp cle1 cle2) < 0

(*Prédicat inf sur deux clés dans le cas où il s'agit de type option, on prend que None < Some (e) pour tout e.*)
let inf2 (cle1 : t option) (cle2 : t option) : bool = 
  match (cle1,cle2) with 
  | (None,None) -> false
  | (Some(e), None) -> false
  | (None, Some(e) ) -> true
  | (Some (e1), Some(e2)) -> (inf e1 e2)

(*Prédicat d'égalité sur deux clés*)
let eg (cle1 : t) (cle2 : t) : bool = (cmp cle1 cle2) = 0


(*Fonction permettant de récupérer une chaîne de caractères au bon format et de produire un entier 128 bits*)
let of_str(str:string): t =
  let sz = (String.length str) in 
  let x1 : Int32.t = Int32.of_string(String.sub str 0 10) in
  let x2 : Int32.t = Int32.of_string(String.cat "0x" (String.sub str 10 8)) in
  let x3 : Int32.t = Int32.of_string(String.cat "0x"(String.sub str 18 8)) in
  let x4 : Int32.t = Int32.of_string(String.cat "0x"(String.sub str 26 (sz - 26)))    (*Traite les cas où il y a moins de 32 nombres hexadécimaux*)
  in 
  (x1,x2,x3,x4)

(*Fonctions d'affichage pour les to dot : la première permet d'avoir la représentation complète, la seconde de ne garder que le
premier entier 128 bits pour des raisons de lisibilité pour les transparents de la soutenance*)
let to_str (cle : t) : string = 
  let (x1,x2,x3,x4) = cle in 
  let (sx1,sx2, sx3, sx4) = (
    (Printf.sprintf "%lx ." x1),
    (Printf.sprintf " %lx ." x2),
    (Printf.sprintf " %lx ." x3),
    (Printf.sprintf " %lx" x4)
  ) in String.cat sx1 (String.cat sx2 (String.cat sx3 sx4))

(*Pour un affichage plus petit pour les diapos*)
(*
let to_str (cle : t) : string = 
  let (x,_,_,_) = cle in
  Printf.sprintf "%lx" x;;
  *)

(*Extrait une liste de nb_entiers 128 bits à partir d'un fichier, 
  si le fichier n'a plus d'éléments, on retourne la liste obtenue jusqu'ici*)
let list_of_file (file_name : string) (nb_entier : int ): t list =
  let fileIN = open_in file_name in 
  let rec loop (li_128 : t list) (nb_entier : int) : t list =
    if nb_entier = 0 then li_128 
    else 
      try (let str : string = input_line fileIN in 
      loop ((of_str str)::li_128) (nb_entier -1 ))
    with End_of_file -> close_in fileIN  ; li_128
  in
  let res = loop [] nb_entier in close_in fileIN ; res
;; 



