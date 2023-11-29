open Int32;;

(*Représentation des entiers codés sur 128 bits*)

module type Int128 = sig 
  type t
  val cmp : t -> t -> int
  val inf : t -> t -> bool
  val inf : t option -> t option -> bool
  val eg : t -> t -> int
  val of_str : string -> t
end

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
    let x1 : Int32.t = Int32.of_string(String.sub str 0 10) in
    let x2 : Int32.t = Int32.of_string(String.cat "0x" (String.sub str 10 8)) in
    let x3 : Int32.t = Int32.of_string(String.cat "0x"(String.sub str 18 8)) in
    let x4 : Int32.t = Int32.of_string(String.cat "0x"(String.sub str 26 8)) in 
    (x1,x2,x3,x4)

end

