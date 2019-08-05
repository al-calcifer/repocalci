#!/bin/bash -f                                                                  

gfortran -o file2xyz.x file2xyz.f
gfortran -o Man_dados2.x Man_dados2.f
gfortran -o grad_min.x grad_min.f
#gfortran -o just_carbon.x just_carbon.f
##################################################
# Apagando arquivos temporarios e desnecessarios #
##################################################
rm out out2 Emin-Vfix Emin-Vfix2 hat1 hat2 pressure plotV.dat 
rm erro plotV-able.dat info.dat info2.dat D.dat D-un.dat
rm -r xyz-files indice
rm -r p-files
rm Emin-az dia_az.d coord-2.d coord-1.d input-1.dat teste-Emin
##################################################
##################################################
##################################################
##################################################
##################################################
##################################################
mkdir xyz-files
mkdir p-files
cp coord.d coord-0.d
##################################################

#### Para cada volume procuramos o minimo de #####
#### energia antes de fazer uma variacao para ####
#### acrescimo e descrescimo de az ###############

set rs=1
while ($rs < 401)

 set case=1
 while ($case < 76 )
    echo $case > indice
    echo '       delta=5.d-3' > D.dat
    ./main coord.d >> hat0
    head -4 coord.d > hat1
    tail -1 hat1 >> teste-Emin
    tail -1 hat0 >> teste-Emin
    more fort.801 >> teste-Emin

    gfortran -o alt_latt.x alt_latt.f
    ./alt_latt.x
    cp coord2.d coord.d
    rm coord2.d
    ./main coord.d >> hat0
    head -4 coord.d > hat1
    tail -1 hat1 >> teste-Emin
    tail -1 hat0 >> teste-Emin
    more fort.801 >> teste-Emin

    gfortran -o grad_min.x grad_min.f
    ./grad_min.x >> pressure
    rm teste-Emin hat1 hat0

    echo '       delta=-5.d-3' > D.dat
    ./main coord.d >> hat0
    head -4 coord.d > hat1
    tail -1 hat1 >> teste-Emin
    tail -1 hat0 >> teste-Emin
    more fort.801 >> teste-Emin    

    gfortran -o alt_latt.x alt_latt.f
    ./alt_latt.x
    cp coord2.d coord.d
    rm coord2.d

    ./main coord.d >> hat0
    head -4 coord.d > hat1
    tail -1 hat1 >> teste-Emin
    tail -1 hat0 >> teste-Emin
    more fort.801 >> teste-Emin

    gfortran -o grad_min.x grad_min.f
    ./grad_min.x >> pressure
    rm teste-Emin hat1 hat0

    ./main coord.d >> hat0
    head -4 coord.d > hat1
    tail -1 hat1 >> teste-Emin
    tail -1 hat0 >> teste-Emin
    more fort.801 >> teste-Emin

    gfortran -o alt_latt.x alt_latt.f
    ./alt_latt.x
    cp coord2.d coord.d
    rm coord2.d

    ./main coord.d >> hat0
    head -4 coord.d > hat1
    tail -1 hat1 >> teste-Emin
    tail -1 hat0 >> teste-Emin
    more fort.801 >> teste-Emin

    gfortran -o grad_min.x grad_min.f
    ./grad_min.x >> pressure
    rm teste-Emin hat1 hat0

    echo '       delta=5.d-3' > D.dat
    ./main coord.d >> hat0
    head -4 coord.d > hat1
    tail -1 hat1 >> teste-Emin
    tail -1 hat0 >> teste-Emin
    more fort.801 >> teste-Emin    

    gfortran -o alt_latt.x alt_latt.f
    ./alt_latt.x
    cp coord2.d coord.d
    rm coord2.d

    ./main coord.d >> hat0
    head -4 coord.d > hat1
    tail -1 hat1 >> teste-Emin
    tail -1 hat0 >> teste-Emin
    more fort.801 >> teste-Emin

    gfortran -o grad_min.x grad_min.f
    ./grad_min.x >> pressure
    rm teste-Emin hat1 hat0

    echo $case
 @ case = $case + 1
 end
cp coord.d coord-2.d
cp coord.d coord-1.d
#####./just_carbon.x <coord-2.d> coord.d
#####./main coord.d > hat1
#####tail -1 hat1 > hat0

gfortran -o Man_dados2.x Man_dados2.f
./Man_dados2.x >> info2.dat
#####rm hat1

cp coord.d xyz-files/coord$rs.d
mv pressure p-files/pressure$rs
./file2xyz.x <coord-1.d> coord.xyz
mv coord.xyz xyz-files/coord$rs.xyz

echo '       delta=-8.d-2' > D.dat
gfortran -o alt_latt.x alt_latt.f
./alt_latt.x
cp coord2.d coord.d
rm coord2.d


@ rs = $rs + 1
end
