open Int32;;

(*Représentation des entiers codés sur 128 bits*)

type entier128 = (Int32.t * Int32.t * Int32.t * Int32.t);;

let cmp (cle1 : entier128) (cle2 : entier128) : int = 
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
    (Int32.unsigned_compare a1 a2);;


let inf (cle1 : entier128) (cle2 : entier128) : bool = (cmp cle1 cle2) < 0
let eg (cle1 : entier128) (cle2 : entier128) : bool = (cmp cle1 cle2) = 0


module type Comparable = sig
  type t 
  val compare : t -> t -> int
end

module Int128Comp : Comparable with type t = entier128 = struct
  type t = entier128
  let compare = cmp
end