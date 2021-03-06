(* Operations on floating-point numbers *) 

#open "exc";;
#open "fstring";;
#open "int";;

let string_of_float f =
  let s = format_float "%.12g" f in
  try
    for i = 0 to pred(string_length s) do
      match nth_char s i with `.` | `e` | `E` -> raise Exit | _ -> ()
    done;
    s ^ ".0"
  with Exit ->
    s
;;

#dim "defanydim";;

let abs_float f =
  if f <. 0.0 then minus_float f else f
;;

let zero = 0.0;;

let one = 1.0;;

#dim "defnodim";;

let nodim x = x;;

let anydim x = x *. one;;

let sqr x = x *. x;;

let fzero =zero;;

let fone = one;;

let fnodim = nodim;;

let fanydim = anydim;;
