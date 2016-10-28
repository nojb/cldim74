#open "const";;
#open "globals";;
#open "misc";;
#open "builtins";;
#open "modules";;

let define_new_dim d_desc = 
  add_dim d_desc;
  let dim_res =
    { dim_desc = DConst(d_desc);
      dim_level = notgeneric} in
  if d_desc.info<>"" then
  add_value (defined_global d_desc.info {val_typ = {typ_desc = Tconstr
    (constr_type_float, [], [[( dim_res,1,1)]]); typ_level = notgeneric};
    val_prim = ValueNotPrim});
  d_desc
;;

let enter_new_dim (dim_name, unit_name) =
  define_new_dim (defined_global dim_name unit_name)
;;

let type_dimdecl loc decl =
  map enter_new_dim decl
;;

let print_one_dimdecl {qualid = qual_id; info = unit_name} =
      print_string qual_id.id;
      if unit_name<>"" then
      begin
        print_string " (";
        print_string unit_name;
        print_string ")"
      end
;;

let print_dimdecl = function
     [] -> fatal_error "print_dimdecl"
   | dcl1::dcll ->      
      print_string "dimension "; print_one_dimdecl dcl1;
      do_list (fun dcl -> print_string " and "; print_one_dimdecl dcl) dcll;
      print_string ";;\n"; flush std_out
;;
