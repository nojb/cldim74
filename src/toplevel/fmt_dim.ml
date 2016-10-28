#open "globals";;
#open "const";;
#open "modules";;
#open "dimen1";;
#open "format";;

(* Printing dimensions *)

let print_global sel_fct gl =
  if not (can_omit_qualifier sel_fct gl) then begin
    print_string gl.qualid.qual;
    print_string "__"
  end;
  print_string gl.qualid.id
;;

let int_to_alpha i =
  if i < 26
  then make_string 1 (char_of_int (i+97))
  else make_string 1 (char_of_int ((i mod 26) + 97)) ^ string_of_int (i/26)
;;

let type_vars_counter = ref 0
and type_vars_names = ref ([] : (typ * string) list)
and dim_vars_names = ref ([] : (dimtyp * string) list);;


let name_of_dim_var var =
  try
    assq var !dim_vars_names
  with Not_found ->
    let var_name = int_to_alpha !type_vars_counter in
    incr type_vars_counter;
    dim_vars_names := (var, var_name) :: !dim_vars_names;
    var_name
;;

let print_dim_ident q = 
   print_global dim_of_module q;;

let print_dim dim=
  let rec out_dim=
  fun [] -> print_string "1"
   |  [ty] -> out_elem ty
   |  (ty::l) ->
        out_elem ty;
        print_string " * ";
        out_dim l 
  and out_elem (a,n,m) =
       begin
         match a.dim_desc with
          DConst x -> print_dim_ident x 
          | DVar v -> print_string "'";
	              print_string (name_of_dim_var a)
               (* To give a name to dimension variables *)
       end;
       if n<>m then
       begin
         print_string " ** ";
         if n mod m<>0 then
         begin
           print_char `(`;
           print_int n;
           print_string " / ";
           print_int m;
           print_char `)`
	 end
	 else
         begin
           if n/m < 0 then print_string "(";
	   print_int (n/m);
           if n/m < 0 then print_string ")"
         end
       end
  in out_dim (dim_type_repr dim)
;;

let print_dim_list =
  fun [] -> ()
   |  dimlist ->  print_string "[";
                  out_dim_list dimlist;
                  print_string "] "
  where rec out_dim_list=fun
    [] -> ()
  | [a] -> print_dim a
  | (a::l) -> print_dim a;
              print_string ", ";
              out_dim_list l
;;
