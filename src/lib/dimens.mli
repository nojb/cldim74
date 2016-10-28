#dim "warning";;

dimension length(m);;
dimension mass(kg);;
dimension time(s);;
dimension intensity(A);;
dimension temperature(K);;
dimension quantity(mol);;

value min : [time] float;;
value hour : [time] float;;
value hertz : [time ** (-1)] float;;
value km : [length] float;;
value angstrom : [length] float;;
value newton : [time ** (-2) * length * mass] float;;
value pascal : [time ** (-2) * length ** (-1) * mass] float;;
value bar : [time ** (-2) * length ** (-1) * mass] float;;
value joule : [time ** (-2) * mass * length ** 2] float;;
value watt : [time ** (-3) * mass * length ** 2] float;;
value coulomb : [intensity * time] float;;
value volt : [intensity ** (-1) * time ** (-3) * mass * length ** 2]
float;;
value siemens : [intensity ** 2 * time ** 3 * mass ** (-1) * length **
(-2)] float;;
value ohm : [intensity ** (-2) * time ** (-3) * mass * length ** 2]
float;;
value farad : [intensity ** 2 * time ** 4 * mass ** (-1) * length ** (-2)]
float;;
value henry : [intensity ** (-2) * time ** (-2) * mass * length ** 2]
float;;
value tesla : [time ** (-2) * intensity ** (-1) * mass] float;;
value weber : [time ** (-2) * intensity ** (-1) * mass * length ** 2]
float;;
value pi : [1] float;;
value radian : [1] float;;
value degre : [1] float;;
value grade : [1] float;;
value c : [time ** (-1) * length] float;;
value N : [quantity ** (-1)] float;;
value e : [intensity * time] float;;
value me : [mass] float;;
value mn : [mass] float;;
value mp : [mass] float;;
value h : [time ** (-1) * mass * length ** 2] float;;
value k : [temperature ** (-1) * time ** (-2) * mass * length ** 2]
float;;
value eV : [time ** (-2) * mass * length ** 2] float;;
value MeV : [time ** (-2) * mass * length ** 2] float;;
value G : [time ** (-2) * mass ** (-1) * length ** 2] float;;
value mu0 : [length * intensity ** (-2) * time ** (-2) * mass] float;;
value epsilon0 : [length ** (-3) * intensity ** 2 * time ** 4 * mass **
(-1)]
float;;
