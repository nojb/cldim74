#open "globals";;

let simprat (x,y)=
   let rec pgcd x y =
     if x=0 then y else pgcd (y mod x) x
   in let p=pgcd x y in
     (x/p,y/p);;

(* (expo n m dim) raises dim to exponent n/m (if n and m are not zero!) *)

let expo n m dim =
  let rec exp= 
   fun [] -> []
    |  ((ty,n1,m1)::l) -> let (x,y)=simprat (n1*n,m1*m) in ((ty,x,y)::exp l)
  in exp dim
;;

(* (mult n m dim dim') multiplies dimension dim' by dim^(n/m)
   (if n and m are not zero!) *)

let rec searchvar x =
   fun ((a,n,m)as d::l) -> 
     if (a==x) or (match (a.dim_desc, x.dim_desc) with 
                     (DConst ac, DConst xc) -> ac == xc
                   | _ -> false)
     then
        (n,m,l)
     else
        let (n',m',l')=searchvar x l
	in (n',m',d::l')
    | [] -> (0,1,[])
;;

let mult n m dim dim'=
  let rec mul=
   fun [] dim' -> dim'
    |  dim []  -> expo n m dim
    |  (({dim_desc=DVar _} as a,x,y)::s) l -> let (x',y',l')=searchvar a l 
      	       	       in let (x'', y'') = simprat (x'*y*m+y'*x*n, y'*y*m)
			  in if x''<>0 then
                               (a,x'',y'')::mul s l'
			     else 
                               mul s l'
    | l ((a,x,y)::s) -> let (x',y',l')=searchvar a l
                        in let (x'', y'') = simprat (x*y'*m+y*x'*n, y*y'*m)
			   in if x''<>0 then
			        (a,x'',y'')::mul l' s
			      else
			         mul l' s
(* To keep the order variables and then constants *)
  in mul dim dim'
;;

