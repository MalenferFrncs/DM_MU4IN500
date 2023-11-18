open Int32;;

(*Représentation des entiers codés sur 128 bits*)

type entier128 = (Int32.t * Int32.t * Int32.t * Int32.t);;

let inf (cle1 : entier128) (cle2 : entier128) : bool = 
  let (a1, b1, c1, d1) = cle1 and (a2, b2, c2, d2) = cle2 in 
  if a1=a2 then 
    if b1=b2 then 
      if c1 = c2 then
        (Int32.unsigned_compare d1 d2) < 0
      else
        (Int32.unsigned_compare c1 c2) < 0
    else
      (Int32.unsigned_compare b1 b2) < 0
  else 
    (Int32.unsigned_compare a1 a2) < 0;;


let eg (cle1 : entier128) (cle2 : entier128) : bool = 
  let (a1, b1, c1, d1) = cle1 and (a2, b2, c2, d2) = cle2 in 
  if a1=a2 then 
    if b1=b2 then 
      if c1 = c2 then
        d1 = d2
      else
        false
    else
      false
  else 
    false;;
