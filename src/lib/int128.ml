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


let inf (cle1 : t) (cle2 : t) : bool = (cmp cle1 cle2) < 0

let inf2 (cle1 : t option) (cle2 : t option) : bool = 
  match (cle1,cle2) with 
  | (None,None) -> false
  | (Some(e), None) -> false
  | (None, Some(e) ) -> true
  | (Some (e1), Some(e2)) -> (inf e1 e2)


let eg (cle1 : t) (cle2 : t) : bool = (cmp cle1 cle2) = 0

let of_str(str:string): t =
  let sz = (String.length str) in 
  let x1 : Int32.t = Int32.of_string(String.sub str 0 10) in
  let x2 : Int32.t = Int32.of_string(String.cat "0x" (String.sub str 10 8)) in
  let x3 : Int32.t = Int32.of_string(String.cat "0x"(String.sub str 18 8)) in
  let x4 : Int32.t = Int32.of_string(String.cat "0x"(String.sub str 26 (sz - 26)))    (*Traite les cas où il y a moins de 32 nombres hexadécimaux*)
  in 
  (x1,x2,x3,x4)



let to_str (cle : t) : string = 
  let (x1,x2,x3,x4) = cle in 
  let (sx1,sx2, sx3, sx4) = (
    (Printf.sprintf "0x%08lX" x1),
    (Printf.sprintf "%08lX" x2),
    (Printf.sprintf "%08lX" x3),
    (Printf.sprintf "%08lX" x4)
  ) in String.cat sx1 (String.cat sx2 (String.cat sx3 sx4))

let list_of_file (file_name : string) (nb_entier : int ): t list =
  let fileIN = open_in file_name in 
  let rec loop (li_128 : t list) (nb_entier : int) : t list =
    if nb_entier = 0 then li_128 
    else 
      let str : string = input_line fileIN in 
      loop ((of_str str)::li_128) (nb_entier -1 )
  in
  let res = loop [] nb_entier in close_in fileIN ; res
;; 



