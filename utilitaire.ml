open Int32;;
type entier128 = (Int32.t * Int32.t * Int32.t * Int32.t);;


let int_128_of_str(str:string):entier128 =
  let x1 : int32 = Int32.of_string(String.sub str 0 10) in
  let x2 : int32 = Int32.of_string(String.cat "0x" (String.sub str 10 8)) in
  let x3 : int32 = Int32.of_string(String.cat "0x"(String.sub str 18 8)) in
  let x4 : int32 = Int32.of_string(String.cat "0x"(String.sub str 26 8)) in 
  (x1,x2,x3,x4)
;;
    