#dim "defnodim";;
#dim "warning";;
#open "dfloat";;

(* time units *)

let min = 60. * s;;
let hour = 60. * min;;

let hertz = 1. / s;;

(* mechanical units *)

let km = 1000. * m;;
let angstrom = 1e-10 * m;;

let newton = m * kg / (s * s);;

let pascal = kg / m / s / s;;
let bar = 1e5 * pascal;;

let joule = kg * m * m / (s * s);;

let watt = joule / s;;

(* electrical units *)

let coulomb = A * s;;

let volt = watt / A;;

let siemens = A / volt;;

let ohm = 1. / siemens;; 

let farad = s * A / volt;;

let henry = volt * s / A;;

let tesla = kg / (s * s * A);;

let weber = kg * m * m / (s * s * A);;

(* angles *)
let pi = 3.1415926535;;

let radian = 1.;;
let degre = pi / 180.;;
let grade = pi / 200.;;

(* constants *)

let c = 299792458. * m / s;;

let N = 6.02217e23 / mol;;

let e = 1.602192e-19 * coulomb;;

(* Masses of elementary particles *)
let me = 9.10956e-31 * kg;;
let mn = 1.67492e-27 * kg;;
let mp = 1.67261e-27 * kg;;

(* Planck constant *)
let h = 6.62619e-34 * joule * s;;

(* Boltzmann constant *)
let k = 1.38054e-23 * joule / K;;

let eV = e * volt;;

let MeV = 1e6 * eV;;

let G = 6.67e-11 * m * m / (s * s * kg);; 

let mu0 = pi * 4e-7 * henry / m;;

let epsilon0 = 1. / (mu0 * c * c);;
 