PROGRAM chain
IMPLICIT NONE

REAL(KIND=8) :: latt1(3), latt2(3)
INTEGER :: i, n
REAL(KIND=8) :: x, y, z, dhc, dcc

PRINT*, 'Digite numeros de atomos de carbono:'
READ*, n

dhc=1.31
dcc=1.42

OPEN(UNIT=7, FILE="animacao.dat")
OPEN(UNIT=7, FILE="animacao.xyz")

DATA latt1/1.42, 0.000, 0.000/
!DATA latt2/0.000, 1.000, 0.00/


WRITE(*,*) 'H', -dhc, y, z
WRITE(10,*) 'H', -dhc, y, z
WRITE(7,*) 'H', -dhc, y, z


DO i=0, n
	x=latt1(1)*i
	y=latt1(2)*i
	z=latt1(3)*i	

WRITE(7,*) 'C', x, y, z
WRITE(10,*) 'C', x, y, z
WRITE(*,*) 'C', x, y, z

END DO

WRITE(*,*) 'H', (n*dcc)+dhc, y, z
WRITE(10,*) 'H', (n*dcc)+dhc, y, z
WRITE(7,*) 'H', (n*dcc)+dhc, y, z



END PROGRAM
