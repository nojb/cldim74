(* Printing a type expression *)

#open "globals";;
#open "types";;
#open "modules";;
#open "format";;
#open "fmt_dim";;

let output_type_constr = 
  (print_global types_of_module: type_desc global -> unit)
and output_value =
  (print_global values_of_module: value_desc global -> unit)
and output_constr =
  (print_global constrs_of_module: constr_desc global -> unit)
and output_label =
  (print_global labels_of_module: label_desc global -> unit)
;;

let reset_type_var_name () =
  type_vars_counter := 0;
  type_vars_names := [];
  dim_vars_names := []
;;

let name_of_type_var var =
  try
    assq var !type_vars_names
  with Not_found ->
    let name = int_to_alpha !type_vars_counter in
    let var_name = if var.typ_level == generic then name else "_" ^ name in
    incr type_vars_counter;
    type_vars_names := (var, var_name) :: !type_vars_names;
    var_name
;;

let rec print_typ priority ty =
  let ty = type_repr ty in
  match ty.typ_desc with
    Tvar _ ->
      print_string "'";
      print_string (name_of_type_var ty)
  | Tarrow(ty1, ty2) ->
      if priority >= 1 then begin open_box 1; print_string "(" end
       else open_box 0;
      print_typ 1 ty1;
      print_string " ->"; print_space();
      print_typ 0 ty2;
      if priority >= 1 then print_string ")";
      close_box()
  | Tproduct(ty_list) ->
      if priority >= 2 then begin open_box 1; print_string "(" end
       else open_box 0;
      print_typ_list 2 " *" ty_list;
      if priority >= 2 then print_string ")";
      close_box()
  | Tconstr(cstr, args, dimargs) ->
      open_box 0;
      begin match args with
        []    -> ()
      | [ty1] ->
          print_typ 2 ty1; print_space ()
      | tyl ->
          open_box 1;
          print_string "(";
          print_typ_list 0 "," tyl;
          print_string ")";
          close_box();
          print_space()
      end;
      print_dim_list dimargs;
      print_global types_of_module cstr;
      close_box()

and print_typ_list priority sep = function
    [] ->
      ()
  | [ty] ->
      print_typ priority ty
  | ty::rest ->
      print_typ priority ty;
      print_string sep; print_space();
      print_typ_list priority sep rest
;;

let print_one_type ty = reset_type_var_name(); print_typ 0 ty;;
