open Int32;;

module type Int128 = sig 
  type t = (Int32.t * Int32.t * Int32.t * Int32.t)
  val cmp : t -> t -> int
  val inf : t -> t -> bool
  val inf : t option -> t option -> bool
  val eg : t -> t -> int
  val of_str : string -> t
  val to_str : t -> string
  val list_of_file : string -> int -> t list
end


(*Représentation des entiers codés sur 128 bits*)
module Int128 = struct 
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
    loop [] nb_entier
        

end




let bit_n_is_1 (entier : int32) (n:int): bool =
  (Int32.logand (Int32.shift_right entier n) 1l) = 1l
;;

let bit_faible (t : int32) : string =
  if (bit_n_is_1 t 3)
  then if (bit_n_is_1 t 2)
    then if (bit_n_is_1 t 1)
      then if (bit_n_is_1 t 0)
        then "f"
        else "e"
      else if (bit_n_is_1 t 0)
      then "d"
      else "c"
    else if (bit_n_is_1 t 1)
    then if (bit_n_is_1 t 0)
      then "b"
      else "a"
    else if (bit_n_is_1 t 0)
    then "9"
    else "8"
  else if (bit_n_is_1 t 2)
  then if (bit_n_is_1 t 1)
    then if (bit_n_is_1 t 0)
      then "7"
      else "6"
    else if (bit_n_is_1 t 0)
    then "5"
    else "4"
  else if (bit_n_is_1 t 1)
  then if (bit_n_is_1 t 0)
    then "3"
    else "2"
  else if (bit_n_is_1 t 0)
  then "1"
  else "0"
    
let hex_to_chr (hex : string ) : char = Char.chr (Int32.to_int (Int32.of_string hex))
    (*bon*)

let str_of_int32 (taille : int32 ) : string = 
  (*bon*)
  let rec str_byte_of_int32 (taille : int32) (res : string) (i:int): string =
    if i = 4 then res
    else 
      let p = (bit_faible taille) in
      let g = (bit_faible (Int32.shift_right taille 4)) in
      let str = String.make 1 (hex_to_chr ("0x"^g^p)) in
      str_byte_of_int32 (Int32.shift_right_logical taille 8) (str^res) (i+1)
  in
  str_byte_of_int32 taille "" 0
;;
  


  

let padding (seq : bytes) (len : int) (len_total : int32 ) : bytes =
  (* bon *)
  let res = Bytes.extend seq 0 ((56-len)+4) in
  Bytes.set res len '\080';
  Bytes.cat res (Bytes.of_string(str_of_int32 len_total))
                 
  
;;  






let byte_array_of_str (str :string) : int32 array list = 
  let byte_str = Bytes.of_string str in
  let len = Bytes.length byte_str in
  if len < 57 then  
    let pad_str = padding (Bytes.of_string str) len (Int32.of_int len) in 
    let res = Array.make 16 0l in
    for i = 0 to 15 do 
      res.(i)<-Bytes.get_int32_be pad_str (i*4) ;
    done ;
    
    res::[]
  else
    (Array.make 16 0l)::[]
;;

  
let rol32 a n = Int32.logor (Int32.shift_left a n)  (Int32.shift_right_logical a (32 - n))
                                           
                              
                              
let f1 (b : int32) (c : int32) (d : int32) : int32 = Int32.logor (Int32.logand b c) (Int32.logand (Int32.lognot b) d)
let f2 (b : int32) (c : int32) (d : int32) : int32 = Int32.logor (Int32.logand b d) (Int32.logand c (Int32.lognot d) )
let f3 (b : int32) (c : int32) (d : int32) : int32  = Int32.logxor b  (Int32.logxor c d)
let f4 (b : int32) (c : int32) (d : int32) : int32 = Int32.logxor c ( Int32.logor b (Int32.lognot d))
    


let s = [| 7; 12; 17; 22;  7; 12; 17; 22;  7; 12; 17; 22;  7; 12; 17; 22; 
           5;  9; 14; 20;  5;  9; 14; 20;  5;  9; 14; 20;  5;  9; 14; 20; 
           4; 11; 16; 23;  4; 11; 16; 23;  4; 11; 16; 23;  4; 11; 16; 23; 
           6; 10; 15; 21;  6; 10; 15; 21;  6; 10; 15; 21;  6; 10; 15; 21 |]
        
let k = [| 0xd76aa478l; 0xe8c7b756l; 0x242070dbl; 0xc1bdceeel ;
           0xf57c0fafl; 0x4787c62al; 0xa8304613l; 0xfd469501l ;
           0x698098d8l; 0x8b44f7afl; 0xffff5bb1l; 0x895cd7bel ;
           0x6b901122l; 0xfd987193l; 0xa679438el; 0x49b40821l ;
           0xf61e2562l; 0xc040b340l; 0x265e5a51l; 0xe9b6c7aal ;
           0xd62f105dl; 0x02441453l; 0xd8a1e681l; 0xe7d3fbc8l ;
           0x21e1cde6l; 0xc33707d6l; 0xf4d50d87l; 0x455a14edl ;
           0xa9e3e905l; 0xfcefa3f8l; 0x676f02d9l; 0x8d2a4c8al ;
           0xfffa3942l; 0x8771f681l; 0x6d9d6122l; 0xfde5380cl ;
           0xa4beea44l; 0x4bdecfa9l; 0xf6bb4b60l; 0xbebfbc70l ;
           0x289b7ec6l; 0xeaa127fal; 0xd4ef3085l; 0x04881d05l ;
           0xd9d4d039l; 0xe6db99e5l; 0x1fa27cf8l; 0xc4ac5665l ;
           0xf4292244l; 0x432aff97l; 0xab9423a7l; 0xfc93a039l ;
           0x655b59c3l; 0x8f0ccc92l; 0xffeff47dl; 0x85845dd1l ;
           0x6fa87e4fl; 0xfe2ce6e0l; 0xa3014314l; 0x4e0811a1l ;
           0xf7537e82l; 0xbd3af235l; 0x2ad7d2bbl; 0xeb86d391l |]
        
let h = [| 0x67452301l; 0xefcdab89l; 0x98badcfel; 0x10325476l |] 
        
let digest (m_li : int32 array list) : Int128.t =        

  let h = [| 0x67452301l; 0xefcdab89l; 0x98badcfel; 0x10325476l |] in



  let md5 (m : int32 array) : unit =
  
    let a, b, c, d = ref h.(0), ref h.(1), ref h.(2), ref h.(3) in
    let r :int32 ref = ref 0l in
    let g :int ref = ref 0 in

    for i = 0 to 63 do 
      
      print_string (str_of_int32 !a); print_endline " a";
      print_string (str_of_int32 !b); print_endline " b";
      print_string (str_of_int32 !c); print_endline " c";
      print_string (str_of_int32 !d); print_endline " d"; print_endline "";
    
    
      if   i  < 16 then (
        r := f1 !b !c !d ;
        g := i ; )
  
      else if i < 32 then (
        r := f2 !b !c !d ;
        g := (5*i + 1) mod 16 ; )
      
      else if i < 48 then (
        r := f3 !b !c !d ;
        g := (3*i + 5) mod 16 ; )
      
      else (
        r := f4 !b !c !d ;
        g := (7*i) mod 16 ; ) ;
  
      r := Int32.add ( Int32.add (Int32.add !r  !a)  k.(i) ) m.(!g) ;
      a := !d ;
      d := !c ;
      c := !b ;
      b := Int32.add !b  (rol32 !r s.(i)) ;
  
    
    done ;
  
    h.(0) <- Int32.add h.(0) !a ;
    h.(1) <- Int32.add h.(1) !b ;
    h.(2) <- Int32.add h.(2) !c ;
    h.(3) <- Int32.add h.(3) !d ;
  in
  
  match m_li with
  |hd::tl -> 
      md5 hd;
      (h.(0),h.(1),h.(2),h.(3))
  |_-> 
      (h.(0),h.(1),h.(2),h.(3))
  
      
;;

let test = [|0l;1l;2l;3l;4l;5l;6l;7l;8l;9l;10l;11l;12l;13l;14l;15l|] ;;




digest (byte_array_of_str "jesus")
    
    

















  
  