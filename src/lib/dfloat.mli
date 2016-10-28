(* Operations on floating-point numbers *)

#dim "warning";;

value int_of_float : ['d] float -> ['d] int = 1 "int_of_float"
        (* Truncate the given float to an integer value.
           The result is unspecified if it falls outside the
           range of representable integers. *)
  and float_of_int : ['d] int -> ['d] float = 1 "float_of_int";;
        (* Convert an integer to floating-point. *)

value minus : ['d] float -> ['d] float = 1 "~float"
  and minus_float : ['d] float -> ['d] float = 1 "~float"
        (* Unary negation. *)
  and prefix + : ['d] float -> ['d] float -> ['d] float = 2 "+float"
  and prefix +. : ['d] float -> ['d] float -> ['d] float = 2 "+float"
  and add_float : ['d] float -> ['d] float -> ['d] float = 2 "+float"
        (* Addition. *)
  and prefix - : ['d] float -> ['d] float -> ['d] float = 2 "-float"
  and prefix -. : ['d] float -> ['d] float -> ['d] float = 2 "-float"
  and sub_float : ['d] float -> ['d] float -> ['d] float = 2 "-float"
        (* Subtraction. *)
  and prefix * : ['d1] float -> ['d2] float -> ['d1 * 'd2] float = 2 "*float"
  and prefix *. : ['d1] float -> ['d2] float -> ['d1 * 'd2] float = 2 "*float"
  and mult_float : ['d1] float -> ['d2] float -> ['d1 * 'd2] float = 2 "*float"
        (* Product. *)
  and prefix / : ['d1] float -> ['d2] float -> ['d1 / 'd2] float = 2 "/"
  and prefix /. : ['d1] float -> ['d2] float -> ['d1 / 'd2] float = 2 "/"
  and div_float : ['d1] float -> ['d2] float -> ['d1 / 'd2] float = 2 "/"
        (* Division. Raise [Division_by_zero] if the dividend is 0.0. *)
  and prefix ** : [1] float -> [1] float -> [1] float = 2 "power_float"
  and prefix **. : [1] float -> [1] float -> [1] float = 2 "power_float"
  and power : [1] float -> [1] float -> [1] float = 2 "power_float"
        (* Exponentiation. *)
  and eq_float : ['d] float -> ['d] float -> bool = 2 "=float"
  and prefix =. : ['d] float -> ['d] float -> bool = 2 "=float"
        (* Floating-point equality.
           Equivalent to generic equality, just faster. *)
  and neq_float : ['d] float -> ['d] float -> bool = 2 "<>float"
  and prefix <>. : ['d] float -> ['d] float -> bool = 2 "<>float"
        (* Negation of [eq_float]. *)
  and prefix <. : ['d] float -> ['d] float -> bool = 2 "<float"
  and lt_float : ['d] float -> ['d] float -> bool = 2 "<float"
  and prefix >. : ['d] float -> ['d] float -> bool = 2 ">float"
  and gt_float : ['d] float ->['d]  float -> bool = 2 ">float"
  and prefix <=. : ['d] float -> ['d] float -> bool = 2 "<=float"
  and le_float : ['d] float -> ['d] float -> bool = 2 "<=float"
  and prefix >=. : ['d] float -> ['d] float -> bool = 2 ">=float"
  and ge_float : ['d] float -> ['d] float -> bool = 2 ">=float"
        (* Usual comparisons between floating-point numbers. *)
;;

value exp : [1] float -> [1] float = 1 "exp_float"
  and log : [1] float -> [1] float = 1 "log_float"
  and sqr : ['d] float -> ['d ** 2] float
  and sqrt : ['d ** 2] float -> ['d] float = 1 "sqrt_float"
  and sin : [1] float -> [1] float = 1 "sin_float"
  and cos : [1] float -> [1] float = 1 "cos_float"
  and tan : [1] float -> [1] float = 1 "tan_float"
  and asin : [1] float -> [1] float = 1 "asin_float"
  and acos : [1] float -> [1] float = 1 "acos_float"
  and atan : [1] float -> [1] float = 1 "atan_float"
  and atan2 : [1] float -> [1] float -> [1] float = 2 "atan2_float"
          (* Usual transcendental functions on floating-point numbers. *)
;;

value abs_float : ['d] float -> ['d] float
          (* Return the absolute value of the argument. *)
  and zero : ['d] float
  and fzero : ['d] float
          (* 0 of any dimension *)
  and one : ['d] float
  and fone : ['d] float
          (* 1 of any dimension. Use with care, because it can
           override any dimension check *)
  and nodim : [1] float -> [1] float
  and fnodim : [1] float -> [1] float
          (* Be sure that a value has no dimension *)
  and anydim : ['d1] float -> ['d2] float
  and fanydim : ['d1] float -> ['d2] float
          (* Converts a float value to any dimension  
             Use with care because it can override any dimension check *)
;;

value string_of_float : [1] float -> string
        (* Convert the given float to its decimal representation. *)
  and float_of_string : string -> [1] float = 1 "float_of_string"
        (* Convert the given string to a float, in decimal.
           The result is unspecified if the given string is not
           a valid representation of a float. *)
(*--*)
  and format_float : string -> [1] float -> string = 2 "format_float"
;;

