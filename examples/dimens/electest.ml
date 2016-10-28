#open "dfloat";;
#open "dimens";;

let R = 1e3 * ohm;;
let C = 1e-10 * farad;;

type ['d] complex = {re : ['d] float; im : ['d] float};;

let (prefix *) = fun a b -> 
   {re = a.re *. b.re -. a.im *. b.im;
    im = a.re *. b.im +. b.re *. a.im};;

let (prefix /) = fun a b -> 
   let carremod = sqr(b.re)+. sqr(b.im) 
   in {re = (a.re *. b.re +. a.im *. b.im) /. carremod;
       im = (a.im *. b.re -. a.re *. b.im) /. carremod};;
   
let calcu w ( i : [intensity] complex ) =
   i / {re = 2. /. R; im = C *. w};;

