c     atom masses (average)
      real am(109)
      data am/
c     Hydrogen H 1
     & 1.00794,
c     Helium He 2
     & 4.002602,
c     Lithium Li 3
     & 6.941,
c     Beryllium Be 4
     & 9.012182,
c     Boron B 5
     & 10.811,
c     Carbon C 6
     & 12.011,
c     Nitrogen N 7
     & 14.00674,
c     Oxygen O 8
     & 15.9994,
c     Fluorine F 9
     & 18.9984,
c     Neon Ne 10
     & 20.1797,
c     Sodium Na 11
     & 22.98977,
c     Magnesium Mg 12
     & 24.305,
c     Aluminum Al 13
     & 26.98154,
c     Silicon Si 14
     & 28.0855,
c     Phosphorus P 15
     & 30.97376,
c     Sulfur S 16
     & 32.066,
c     Chlorine Cl 17
     & 35.4527,
c     Argon Ar 18
     & 39.948,
c     Potassium K 19
     & 39.0983,
c     Calcium Ca 20
     & 40.078,
c     Scandium Sc 21
     & 44.95591,
c     Titanium Ti 22
     & 47.88,
c     Vanadium V 23
     & 50.9415,
c     Chromium Cr 24
     & 51.9961,
c     Manganese Mn 25
     & 54.93805,
c     Iron Fe 26
     & 55.847,
c     Cobalt Co 27
     & 58.9332,
c     Nickel Ni 28
     & 58.6934,
c     Copper Cu 29
     & 63.546,
c     Zinc Zn 30
     & 65.39,
c     Gallium Ga 31
     & 69.723,
c     Germanium Ge 32
     & 72.61,
c     Arsenic As 33
     & 74.92159,
c     Selenium Se 34
     & 78.96,
c     Bromine Br 35
     & 79.904,
c     Krypton Kr 36
     & 83.8,
c     Rubidium Rb 37
     & 85.4678,
c     Strontium Sr 38
     & 87.62,
c     Yttrium Y 39
     & 88.90585,
c     Zirconium Zr 40
     & 91.224,
c     Niobium Nb 41
     & 92.90638,
c     Molybdenum Mo 42
     & 95.94,
c     Technetium Tc 43
     & 98,
c     Ruthenium Ru 44
     & 101.07,
c     Rhodium Rh 45
     & 102.9055,
c     Palladium Pd 46
     & 106.42,
c     Silver Ag 47
     & 107.8682,
c     Cadmium Cd 48
     & 112.411,
c     Indium In 49
     & 114.82,
c     Tin Sn 50
     & 118.71,
c     Antimony Sb 51
     & 121.757,
c     Tellurium Te 52
     & 127.6,
c     Iodine I 53
     & 126.9045,
c     Xenon Xe 54
     & 131.29,
c     Cesium Cs 55
     & 132.9054,
c     Barium Ba 56
     & 137.327,
c     Lanthanum La 57
     & 138.9055,
c     Cerium Ce 58
     & 140.115,
c     Praseodymium Pr 59
     & 140.9077,
c     Neodymium Nd 60
     & 144.24,
c     Promethium Pm 61
     & 145,
c     Samarium Sm 62
     & 150.36,
c     Europium Eu 63
     & 151.965,
c     Gadolinium Gd 64
     & 157.25,
c     Terbium Tb 65
     & 158.9253,
c     Dysprosium Dy 66
     & 162.5,
c     Holmium Ho 67
     & 164.9303,
c     Erbium Er 68
     & 167.26,
c     Thulium Tm 69
     & 168.9342,
c     Ytterbium Yb 70
     & 173.04,
c     Lutetium Lu 71
     & 174.967,
c     Hafnium Hf 72
     & 178.49,
c     Tantalum Ta 73
     & 180.9479,
c     Tungsten W 74
     & 183.85,
c     Rhenium Re 75
     & 186.207,
c     Osmium Os 76
     & 190.2,
c     Iridium Ir 77
     & 192.22,
c     Platinum Pt 78
     & 195.08,
c     Gold Au 79
     & 196.9665,
c     Mercury Hg 80
     & 200.59,
c     Thallium Tl 81
     & 204.3833,
c     Lead Pb 82
     & 207.2,
c     Bismuth Bi 83
     & 208.9804,
c     Polonium Po 84
     & 209,
c     Astatine At 85
     & 210,
c     Radon Rn 86
     & 222,
c     Francium Fr 87
     & 223,
c     Radium Ra 88
     & 226.0254,
c     Actinium Ac 89
     & 227,
c     Thorium Th 90
     & 232.0381,
c     Protactinium Pa 91
     & 213.0359,
c     Uranium U 92
     & 238.0289,
c     Neptunium Np 93
     & 237.0482,
c     Plutonium Pu 94
     & 244,
c     Americium Am 95
     & 243,
c     Curium Cm 96
     & 247,
c     Berkelium Bk 97
     & 247,
c     Californium Cf 98
     & 251,
c     Einsteinium Es 99
     & 252,
c     Fermium Fm 100
     & 257,
c     Mendelevium Md 101
     & 258,
c     Nobelium No 102
     & 259,
c     Lawrencium Lr 103
     & 260,
c     Rutherfordium Rf 104
     & 261,
c     Dubnium Db 105
     & 262,
c     Seaborgium Sg 106
     & 263,
c     Bohrium Bh 107
     & 262,
c     Hassium Hs 108
     & 265,
c     Meitnerium Mt 109
     & 266/
