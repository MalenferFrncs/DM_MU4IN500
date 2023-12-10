open Int32;;
open Int64;;
open Int128 ;;
let int32_of_int64 (n : int64) : int32*int32 =
  let faible = Int64.to_int32 n in
  let fort = Int64.to_int32 (Int64.shift_right_logical n 32) in
  fort,faible

;;
  

let padding (str : string) (len : int) (fin :bool ) : string = 
  let remplire (str : string) (i : int) :char =
    if i < (String.length str) then str.[i]
    else if (i=(String.length str) && fin) then (Char.chr 128) 
    else '\000'
  in 
  String.init 64 (remplire str) 
;;  





let rec int32_array_of_str (str :string) (len_total : int): int32 array list = 
  
  let len = String.length str in
  if len < 56 then  
    let pad_str = padding  str len true in 
    
    let res = Array.make 16 0l in
    for i = 0 to 13 do 
      let res_i32 = String.get_int32_le pad_str (i*4) in
      
      res.(i)<- res_i32
    done ; 
    let grand,petit = int32_of_int64 (Int64.mul (Int64.of_int (len_total+len)) 8L)in
    res.(14)<- petit;
    res.(15)<- grand;
      
    res::[] 
         
         
  else if len >= 64 then
    let res = Array.make 16 0l in
    for i = 0 to 15 do 
      let res_i32 = String.get_int32_le str (i*4) in
      
      res.(i)<- res_i32
    done ;
    res::(int32_array_of_str (String.sub str 64 (len-64)) (len_total + 64))
         
         
  else 
    let debut_res = str^(String.make 1 (Char.chr 128))^(String.make (64-len-1) '\000') in
    let fin_res = (padding "" 0  ) false in
    let res_d = Array.make 16 0l in
    let res_f = Array.make 16 0l in
    for i = 0 to 15 do 
      let res_i32_d = String.get_int32_le debut_res (i*4) in
      let res_i32_f = String.get_int32_le fin_res (i*4) in
      res_d.(i)<- res_i32_d;
      res_f.(i)<- res_i32_f;
    done ;
    let grand,petit = int32_of_int64 (Int64.mul (Int64.of_int (len_total+len)) 8L)in
    res_f.(14)<- petit;
    res_f.(15)<- grand;
    res_d::res_f::[]
    
    
;;

  
let rol32 a n = Int32.logor (Int32.shift_left a n)  (Int32.shift_right_logical a (32 - n))
                                           
                              
                              
let f1 (b : int32) (c : int32) (d : int32) : int32 = Int32.logor (Int32.logand b c) (Int32.logand (Int32.lognot b) d)
let f2 (b : int32) (c : int32) (d : int32) : int32 = Int32.logor (Int32.logand b d) (Int32.logand c (Int32.lognot d) )
let f3 (b : int32) (c : int32) (d : int32) : int32  = Int32.logxor d  (Int32.logxor c b)
let f4 (b : int32) (c : int32) (d : int32) : int32 = Int32.logxor c ( Int32.logor b (Int32.lognot d))
    
let finish (entre : int32) : int32 =
  let res = 0l in
  let res = Int32.logor res (Int32.shift_left (Int32.logand entre 0x000000FFl) 24) in 
  let res = Int32.logor res (Int32.shift_left (Int32.logand entre 0x0000FF00l) 8) in 
  let res = Int32.logor res (Int32.shift_right_logical (Int32.logand entre 0x00FF0000l) 8) in 
  let res = Int32.logor res (Int32.shift_right_logical (Int32.logand entre 0xFF000000l) 24) in 
  res
;;


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
  
        
let digest (str:string) : t =        
  
  let m_li : int32 array list = int32_array_of_str str 0 in

  let h = [| 0x67452301l; 0xefcdab89l; 0x98badcfel; 0x10325476l |] in


  let md5 (m : int32 array) : unit =
  
    let a, b, c, d = ref h.(0), ref h.(1), ref h.(2), ref h.(3) in
    let r :int32 ref = ref 0l in
    let g :int ref = ref 0 in

    for i = 0 to 63 do 
  
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
      
      h.(0) <- finish h.(0)  ;
      h.(1) <- finish h.(1)  ;
      h.(2) <- finish h.(2)  ;
      h.(3) <- finish h.(3)  ; 
      
      (h.(0),h.(1),h.(2),h.(3))
  |_-> 
      (h.(0),h.(1),h.(2),h.(3))
  
      
;;