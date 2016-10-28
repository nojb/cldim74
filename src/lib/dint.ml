(* Operations on integers *)

#open "eq";;

#dim "defanydim";;

let abs n =
  if n < 0 then -n else n
;;

let zero = 0;;

let one = 1;;

#dim "defnodim";;

let lnot n =
  n lxor (-1)
;;

let nodim x = x;;

let anydim x = x * one;;

let string_of_int i = format_int "%ld" i;;
