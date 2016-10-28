#open "dimen0";;
#open "globals";;
#open "misc";;

exception Unify;;

(* Creates a new dimension variable *)
let current_level = ref 0;;

let new_dim_var () =
  {dim_desc = DVar Dnolink; dim_level = !current_level}
;;

let anydimdef = ref false;;

let opt_dim_var () =
   if !anydimdef then
      [(new_dim_var (),1,1)]
   else
      []
;;

let rec opt_dim_var_list = 
   fun 0 -> []
    | n -> opt_dim_var ()::(opt_dim_var_list (n-1))
;; 

(* get the canonical representative of a dimension *)

let rec dim_type_repr dim=
    try
      dim_type_repr(dim_repr_one (search_linked dim))
    with
      Not_found -> dim

and search_linked =
   fun ((ty,n,m) as t1::l) ->
     begin
       match ty.dim_desc with
         DVar(Dlinkto t2 as r) -> (t1,l)
       | _ -> let (t3,l2)=search_linked l in
                 (t3,t1::l2)
     end
   | [] -> raise Not_found

and dim_repr_one =
   fun (( {dim_desc = DVar(Dlinkto t1 as r) },n,m),l) ->
         let t2 = dim_type_repr t1 in
           r <- Dlinkto t2;
           mult n m t2 l
    | _ -> fatal_error "dim_repr_one" (* This should never happen *) 
;;

(* Unifies dimensions dim and dim' *)

let dim_unify dim dim'=
   let dim2=dim_type_repr(mult (-1) 1 dim dim') in
      match dim2 with
        [] -> ()
      | (({dim_desc = DVar (Dnolink as v)},n,m) ::dim3) ->
         v <- Dlinkto (expo (-m) n dim3)
      | _ ->
         raise Unify
;;

let rec dimlist_unify =
   fun ([], []) -> ()
    |  (a1::l1, a2::l2) -> dim_unify a1 a2;
                            dimlist_unify (l1, l2)
    |  _ -> raise Unify
;;

let rec occur v=
   fun (( { dim_desc = DVar(Dnolink) as v2 },n,m)::l) ->
       if v2==v then true else occur v l
    |  _ -> false
;;

(* Finds a variable v which is in dim' and not in dim
   Raises Unify if there is no such variable *)

let rec dim_occur_check dim =
   fun (({ dim_desc = DVar(Dnolink) as v} ,n,m) as t1 :: l) ->
          if occur v dim then
              let (t2,l2)=dim_occur_check dim l
              in (t2,t1::l)
          else
              (t1,l)
    | _ -> raise Unify
      (* When we encounter a constant, there is no more variable *)
;;

(* Instantiates dim so that it is equal to dim' *)

let dim_filter dim dim'=
   let dim2=dim_type_repr dim
   and dim2'=dim_type_repr dim' in
   if mult (-1) 1 dim2 dim2'=[] then 
      () 
   else
      let ((ty,n,m),dim3)=dim_occur_check dim2' dim2 in
      match ty.dim_desc with
        DVar(v) -> v <- Dlinkto (expo m n (mult (-1) 1 dim3 dim2'))
       | _ -> raise Unify
;;

let rec filter_dim_list =
   fun ([], []) -> ()
    |  (a1::l1, a2::l2) -> dim_filter a1 a2;
                           filter_dim_list (l1, l2)
    |  _ -> raise Unify
;;

(* Computes free variables in a dimension *)

let free_dim_vars level dim=
   let rec free_vars=
      fun (({dim_desc=DVar _} as ty,_,_)::l) ->
        if ty.dim_level >= level then
          ty::free_vars l
        else
          free_vars l
      | _ -> []
   in free_vars(dim_type_repr dim)
;;

(* Generalizes a dimension *)

let gen_dim dim=
   let rec gen_vars=
      fun (({dim_desc=DVar _} as ty,_,_)::l) ->
           if ty.dim_level > !current_level then
              ty.dim_level <- generic;
           let lvl2=gen_vars l in
             if lvl2 <= ty.dim_level then lvl2 else ty.dim_level
       | _ -> notgeneric
   in gen_vars(dim_type_repr dim)
;;

let generalize_dim dim=
  let _ = gen_dim dim in ()
;;

let rec gen_dim_list=
   fun (a::l) -> let lvl1=gen_dim a
                 and lvl2=gen_dim_list l 
		 in if lvl1 <= lvl2 then lvl1 else lvl2
    | [] -> notgeneric
;;

let nongen_dim dim =
   let nongenvar = fun ({dim_desc = DVar _} as d,_,_) -> 
                    if d.dim_level > !current_level then 
                        d.dim_level <- !current_level
                 | _ -> ()
   in do_list nongenvar (dim_type_repr dim)
;;

(* Copies a dimension *)

let rec copy_dim =
  fun (({dim_desc = DVar(Dnolink as link); dim_level=level} as dvar,n,m)::l) ->
        let firstvar=(if level==generic
            then begin let v=new_dim_var() in 
                           link <- Dlinkto [(v,1,1)];
                           v 
                 end
            else dvar) in
        let (x',y',l')=searchvar firstvar (copy_dim l) in
        let (x'',y'')= simprat (x'*m+y'*n, y'*m) in
        if x''<>0 then
             (firstvar,x'',y'')::l'
        else
             l'
   |  (({dim_desc = DVar(Dlinkto d as link); dim_level=level },n,m)::l) ->
        mult n m (if level==generic
        then d
        else copy_dim d) (copy_dim l)
   |  l -> l
      (* When we encounter a constant, there is no more variable *)
;;

(* Clean up a dimension *)

let rec cleanup_dim =
  fun (({dim_desc = DVar(Dnolink)},_,_)::l) -> cleanup_dim l
   |  (({dim_desc = DVar(Dlinkto d as link); dim_level=level },_,_)::l) ->
        if level==generic
        then begin link <- Dnolink end
        else cleanup_dim d;
        cleanup_dim l
   | _ -> ()
      (* When we encounter a constant, there is no more variable *)
;;

let bind_dimvar dim1 dim2 =
  match dim1 with
    [({dim_desc=DVar(Dnolink as link) },1,1)] -> link <- Dlinkto dim2
   | _ -> fatal_error "bind_dimvar"
;;

