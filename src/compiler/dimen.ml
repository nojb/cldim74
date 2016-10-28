
#open "globals";;
#open "misc";;
#open "interntl";;
#open "location";;
#open "syntax";;
#open "dimen0";;
#open "dimen1";;
#open "pr_type";;
#open "error";;
#open "modules";;

(* Error messages *)

let bad_op_err name loc =
  eprintf "%aUnexpected operator %s in dimension expression.\n"
   output_location loc name;
  raise Toplevel
;;

let without_dim_err name loc =
  eprintf "%aUndimensioned data can only be represented by 1 and not by %s.\n"
   output_location loc name;
  raise Toplevel
;; 

let unbound_dim_var_err v dim =
  eprintf "%aThe dimension variable %s is unbound.\n"
    output_location dim.de_loc v;
  raise Toplevel
;;

let unbound_dim_err v loc =
  eprintf "%aThe dimension identifier %a is not defined.\n"
    output_location loc output_globalref v;
  raise Toplevel
;;

let div_by_0_err loc =
  eprintf "%aDivision by zero.\n"
    output_location loc;
  raise Toplevel
;;

let implicit_dim_arg_warning cstr args loc =
  eprintf "%aWarning: The type constructor %a expects %d dimension \
     argument(s),\nbut is here given %d argument(s). \n"
   output_location loc
   output_type_constr cstr
   cstr.info.ty_dimarity
   (list_length args);
  if !anydimdef then
  (eprintf "I have added unused variables to replace the missing arguments.\n")
  else
  (eprintf "The missing arguments are considered being [1].\n");
  flush stderr
;;

let dim_arity_err cstr args loc =
  eprintf "%a The type constructor %a expects %d dimension \
     argument(s),\n but is here given %d argument(s). \n"
   output_location loc
   output_type_constr cstr
   cstr.info.ty_dimarity
   (list_length args);
  raise Toplevel
;;

let dim_decl_arity_err loc ty_desc1 ty_desc2 =
  eprintf "%aThe type %a has been declared with %d dimension parameter(s)\n\
           but is here defined with %d dimension parameter(s).\n"
    output_location loc
    output_type_constr ty_desc1
    ty_desc1.info.ty_dimarity
    ty_desc2.info.ty_dimarity;
  raise Toplevel
;;


(* Operations on rationals *)

let mulrat (x,y) (x',y') = (x*x',y*y');;

let divrat (x,y) (x',y') = (x*y',y*x');;

let addrat (x,y) (x',y') = (x*y'+x'*y,y*y');;

let subrat (x,y) (x',y') = (x*y'-x'*y,y*y');;

let negrat (x,y) = (-x,y);;

let rec calcrat rexpr=
   match rexpr.re_desc with 
   (Oper(s,e1,e2)) -> 
         let r1=calcrat e1 
         and r2=calcrat e2
         in
         begin
            match s with
                 "*" -> mulrat r1 r2
                |"/" -> if fst(r2)=0 then
                          div_by_0_err rexpr.re_loc
                        else
                          divrat r1 r2
                |"+" -> addrat r1 r2
                |"-" -> subrat r1 r2
                | _ ->  bad_op_err s rexpr.re_loc 
         end
   | (Neg e1) -> negrat (calcrat e1)
   | (Int n) -> (n,1);;

(* Calculates a dimension from its syntactical expression *)

let dim_expr_vars =
  ref ([] : (string * dimtyp) list);;

let reset_dim_expression_vars () =
  dim_expr_vars := []
;;

let bind_dim_expression_vars var_list =
  dim_expr_vars := [];
  map
    (fun v ->
      if mem_assoc v !dim_expr_vars then
        failwith "bind_dim_expression_vars"
      else begin
        let t = new_dim_var() in
          dim_expr_vars := (v, t) :: !dim_expr_vars; t
      end)
    var_list
;;

let calcdim strict_flag dexpr=
 let rec dimof dexpr=
   match dexpr.de_desc with
   (DMul(s,d1,d2)) -> 
      let r1=dimof d1
      and r2=dimof d2 
      in 
      begin
         match s with
	  "*" -> mult 1 1 r1 r2
	 |"/" -> mult (-1) 1 r2 r1
         | _ -> bad_op_err s dexpr.de_loc
      end
   | (DExpo(s,d1,e2)) -> 
      let r1=dimof d1
      and (n,m)=simprat(calcrat e2)
      in
      begin
         match s with
	  "**" -> if n<>0 then expo n m r1 else []
         | _ -> bad_op_err s dexpr.de_loc
      end
   | (DVari v) -> 
      [(begin try
        assoc v !dim_expr_vars 
      with Not_found ->
        if strict_flag then
          unbound_dim_var_err v dexpr
        else begin
          let t = new_dim_var() in
          dim_expr_vars := (v,t) :: !dim_expr_vars; t
        end
      end,1,1)]
   | (DIdent i) -> [( {dim_desc = DConst (
      try
        find_dim i
      with Desc_not_found ->
        unbound_dim_err i dexpr.de_loc
     ); dim_level = notgeneric},1,1)]
   | (DWithout i) -> if i=1 then [] else 
               without_dim_err (string_of_int i) dexpr.de_loc
  in dimof dexpr
;;

