#open "globals";;
#open "const";;
#open "modules";;
#open "dimen1";;

(* Printing dimensions *)

let output_global sel_fct oc gl =
  if not (can_omit_qualifier sel_fct gl) then begin
    output_string oc gl.qualid.qual;
    output_string oc "__"
  end;
  output_string oc gl.qualid.id
;;

let int_to_alpha i =
  if i < 26
  then make_string 1 (char_of_int (i+97))
  else make_string 1 (char_of_int ((i mod 26) + 97)) ^ string_of_int (i/26)
;;

let type_vars_counter = ref 0
and type_vars_names = ref ([] : (typ * string) list)
and dim_vars_names = ref ([] : (dimtyp * string) list);;


let name_of_dim_var sch var =
  try
    assq var !dim_vars_names
  with Not_found ->
    let name = int_to_alpha !type_vars_counter in
    let var_name =
      if (not sch) or var.dim_level == generic then name else "_" ^ name in
    incr type_vars_counter;
    dim_vars_names := (var, var_name) :: !dim_vars_names;
    var_name
;;

let output_dim_ident oc q = 
   output_global dim_of_module oc q;;

let output_dim oc sch dim=
  let rec out_dim=
  fun [] -> output_char oc `1`
   |  [ty] -> out_elem ty
   |  (ty::l) ->
        out_elem ty;
        output_string oc " * ";
        out_dim l 
  and out_elem (a,n,m) =
       begin
         match a.dim_desc with
          DConst x -> output_dim_ident oc x 
          | DVar v -> output_char oc `'`;
	              output_string oc (name_of_dim_var sch a)
               (* To give a name to dimension variables *)
       end;
       if n<>m then
       begin
         output_string oc " ** ";
         if n mod m<>0 then
         begin
           output_char oc `(`;
           output_string oc (string_of_int n);
           output_string oc " / ";
           output_string oc (string_of_int m);
           output_char oc `)`
	 end
	 else
         begin
           if n/m < 0 then output_string oc "(";
	   output_string oc (string_of_int (n/m));
           if n/m < 0 then output_string oc ")"
         end
       end
  in out_dim (dim_type_repr dim)
;;

let output_dim_list oc sch =
  fun [] -> ()
   |  dimlist ->  output_char oc `[`;
                  out_dim_list dimlist;
                  output_string oc "] "
  where rec out_dim_list=fun
    [] -> ()
  | [a] -> output_dim oc sch a
  | (a::l) -> output_dim oc sch a;
              output_string oc ", ";
              out_dim_list l
;;
