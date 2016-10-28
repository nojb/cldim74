#open "dimens";;
#open "dfloat";;
#open "printf";;

let calcenerg v m =
   printf "L'energie de masse de la particule est %.3g MeV\n" (m * sqr(c) / MeV );
   let mu = m / sqrt(1.-sqr(v/c)) in
   printf "Son energie cinetique est %.3g MeV\n" ((mu - m) * sqr(c) / MeV);
   printf "Son energie totale est %.3g MeV\n" (mu * sqr(c) / MeV);
;;

let exemples = 
   print_string "\nElectron a la vitesse c/2 :\n";
   calcenerg (c/2.) me;
   print_string "\nProton a la vitesse c/2 :\n";
   calcenerg (c/2.) mp;
   print_string "\nNeutron a la vitesse c/2 :\n";
   calcenerg (c/2.) mn
;;

let saisie =
   print_string "\nEntrez la masse de la particule (en kg) : ";
   let m=read_float() * kg in
      print_string "Entrez sa vitesse (en fraction de c) : ";
      let v=read_float() * c in
         calcenerg v m
;;

exemples;;
flush std_out;;
saisie;;
flush std_out;;
