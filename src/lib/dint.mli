(* Operations on integers *)

(* Integers are 31 bits wide (or 63 bits on 64-bit processors).
   All operations are taken modulo $2^{31}$ (or $2^{63}$).
   They do not fail on overflow. *)

#dim "warning";;

exception Division_by_zero;;

value minus : ['d] int -> ['d] int = 1 "~int"
  and minus_int : ['d] int -> ['d] int = 1 "~int"
        (* Unary negation. You can write [-e] instead of [minus e]. *)
  and succ : ['d] int -> ['d] int = 1 "succ"
        (* [succ x] is [x+1]. *)
  and pred : ['d] int -> ['d] int = 1 "pred"
        (* [pred x] is [x-1]. *)

  and prefix + : ['d] int -> ['d] int -> ['d] int = 2 "+int"
  and add_int : ['d] int -> ['d] int -> ['d] int = 2 "+int" 

        (* Addition. *)
  and prefix - : ['d] int -> ['d] int -> ['d] int = 2 "-int"
  and sub_int : ['d] int -> ['d] int -> ['d] int = 2 "-int"
        (* Subtraction. *)
  and prefix * : ['d1] int -> ['d2] int -> ['d1 * 'd2] int = 2 "*int"
  and mult_int : ['d1] int -> ['d2] int -> ['d1 * 'd2] int = 2 "*int"
        (* Multiplication. *)
  and prefix / : ['d1] int -> ['d2] int -> ['d1 / 'd2] int = 2 "div"
  and div_int : ['d1] int -> ['d2] int -> ['d1 / 'd2] int = 2 "div"
  and prefix quo : ['d1] int -> ['d2] int -> ['d1 / 'd2] int = 2 "div"
        (* Integer division. Raise [Division_by_zero] if the second argument
           is 0. Give unpredictable results if either argument is negative. *)
  and prefix mod : ['d1] int -> ['d2] int -> ['d1] int = 2 "mod"
        (* Remainder. Raise [Division_by_zero] if the second argument is 0.
           Give unpredictable results if either argument is negative. *)
  and eq_int : ['d] int -> ['d] int -> bool = 2 "=int"
        (* Integer equality. Equivalent to generic equality, just faster. *)
  and neq_int : ['d] int -> ['d] int -> bool = 2 "<>int"
        (* Negation of [eq_int]. *)
  and lt_int : ['d] int -> ['d] int -> bool = 2 "<int"
  and gt_int : ['d] int -> ['d] int -> bool = 2 ">int"
  and le_int : ['d] int -> ['d] int -> bool = 2 "<=int"
  and ge_int : ['d] int -> ['d] int -> bool = 2 ">=int"
        (* Usual comparisons between integers. *)
;;

value abs : ['d] int -> ['d] int
        (* Return the absolute value of the argument. *)
  and zero : ['d] int
        (* 0 of any dimension *)
  and one : ['d] int
        (* 1 of any dimension. Use with care, because it can
           override any dimension check *)
  and nodim : [1] int -> [1] int
        (* Be sure that a value has no dimension *)
  and anydim : ['d1] int -> ['d2] int
        (* Converts an integer value to another dimension 
           Use with care because it can override any dimension check *)
;;

(*** Bitwise operations *)

value prefix land : ['d1] int -> ['d2] int -> ['d1 * 'd2] int = 2 "and"
        (* Bitwise logical and. *)
  and prefix lor : ['d] int -> ['d] int -> ['d] int = 2 "or"
        (* Bitwise logical or. *)
  and prefix lxor : ['d1] int -> ['d2] int -> ['d1 * 'd2] int = 2 "xor"
        (* Bitwise logical exclusive or. *)
  and lnot : ['d] int -> ['d] int
        (* Bitwise complement *)
  and prefix lsl : ['d] int -> [1] int -> ['d] int = 2 "shift_left"
  and lshift_left : ['d] int -> [1] int -> ['d] int = 2 "shift_left"
        (* [n lsl m], or equivalently [lshift_left n m], shifts [n] to the
           left by [m] bits. *)
  and prefix lsr : ['d] int -> [1] int -> ['d] int = 2 "shift_right_unsigned"
        (* [n lsr m] shifts [n] to the right by [m] bits.
            This is a logical shift: zeroes are inserted regardless of sign.*)
  and prefix asr : ['d] int -> [1] int -> ['d] int = 2 "shift_right_signed"
  and lshift_right : ['d] int -> [1] int -> ['d] int = 2 "shift_right_signed"
        (* [n asr m], or equivalently [lshift_right n m], shifts [n] to the
           right by [m] bits.
           This is an arithmetic shift: the sign bit is replicated. *)
;;

(*** Conversion functions *)

value string_of_int : [1] int -> string
        (* Convert the given integer to its decimal representation. *)
  and int_of_string : string -> [1] int = 1 "int_of_string"
        (* Convert the given string to an integer, in decimal (by default)
           or in hexadecimal, octal or binary if the string begins with
           [0x], [0o] or [0b].
           Raise [Failure "int_of_string"] if the given string is not
           a valid representation of an integer. *)
(*--*)
  and format_int : string -> [1] int -> string = 2 "format_int"
;;
