10 5 6 10 /#steps,#steps to write_data2, thermostat KFLAG(-1=isothermal; 0=adiabatic; 1=Berendsen; 6=minimize energy),#steps t xmol
0.6955 0.0 0.0/Random # seed, allowed neighbor motion (before rebuild the neighborhood lists of all atoms) , desired temperature(K)
1                      / =1 REBO (C,H,Si,Ge), =2 tight-binding for C 
 6     12.0    43.125  3.392    / carbon Lennard-Jones parameters (negative AN is none)(epsilon(kelvin); sigma(angstrons))
-1     1.0    8.6    2.81    / hydrogen 
-14    28.0   51.2   2.28   / silicon = carbon 
-10    20.0   47.0   2.72   / Neon 
-18    40.0  119.8   3.41   / Argon 
-36   131.0  164.0   3.83   / Krypton 
