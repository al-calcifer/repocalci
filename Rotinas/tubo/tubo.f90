PROGRAM THEWEB
IMPLICIT NONE

REAL(KIND=8) :: latt1(3), latt2(3), Ch(2), T(2)
!INTEGER, PARAMETER :: maxim=30
REAL(KIND=8), PARAMETER :: PI = 3.14159265358979323846
REAL(KIND=8), PARAMETER :: c=2.459512146747806
REAL(KIND=8) :: x, y, z, u, v, w, t1, t2, dt, pinChT, a, b, theta, k
REAL(KIND=8) :: R1t, R2t, R1ch, R2ch, xcil1, xcil2, ycil1, ycil2, zcil1, zcil2
REAL(KIND=8) :: con11, con12, con21, con22, modCh, L
INTEGER :: i1, i2, n, m, nmax, D, Nat, i, dr, r1, r2, rmax

!PRINT*, '(nmax,nmax):'
!READ*, nmax,nmax


OPEN(UNIT=9, FILE='saida0.dat')
OPEN(UNIT=10, FILE='saida0.xyz')
OPEN(UNIT=11, FILE='saida1.dat')
OPEN(UNIT=12, FILE='saida2.dat')
OPEN(UNIT=13, FILE='cnt.xyz')

DATA latt1/2.13, 1.22, 0.00/
DATA latt2/2.13, -1.22, 0.00/


DO i1=0, 200
DO i2=0, 200

x=i1*latt1(1)+i2*latt2(1)
y=i1*latt1(2)+i2*latt2(2)
z=i1*latt1(3)+i2*latt2(3)


WRITE(9,*) 'C', x, y, z
WRITE(10,*) 'C', x, y, z
WRITE(9,*) 'C', x+2.84, y, z
WRITE(10,*) 'C', x+2.84, y, z
WRITE(11,"(A,3F16.4)") 'C', x, y, z
WRITE(12,"(A,3E16.4)") 'C', x, y, z

END DO
END DO


!!!!! informacoes do nanotubo

PRINT*, '(n,m):'
READ*, n,m

k=n**2+m**2+n*m
w=t1**2+t2**2+t1*t2

!!!!! calcular diametro e |Ch|

dt=2.45*(sqrt(k)/PI)

Ch(1)=n*latt1(1)+m*latt2(1)
Ch(2)=n*latt1(2)+m*latt2(2)

modCh=sqrt(Ch(1)**2+Ch(2)**2)

!!!!! calcular angulo quira
i1=(2*m+n)
i2=(2*n+m)

PRINT*, i1
PRINT*, i2

theta=acos(i2/2*sqrt(k))

!!!!! calcular dr=mdc(2n+m,2m+n)

! Algoritmo de Euclides

DO i=1, 20
	IF (i1 .GE. i2) THEN
	D=i1-i2
		IF (D .EQ. 0) THEN
		dr=i1
		ELSE
		i1=i1-i2
		END IF
	ELSE
	D=i2-i1
		IF (D .EQ. 0) THEN
		dr=i2
		ELSE
		i2=i2-i1
		END IF
	END IF
END DO

PRINT*, 'dr:',dr

!!!!! calcular t1 e t2

t1=i1/dr
t2=-i2/dr


!!!!! calcular comprimento do tubo L=|T|
T(1)=t1*latt1(1)+t2*latt2(1)
T(2)=t1*latt1(2)+t2*latt2(2)

L=sqrt(T(1)**2+T(2)**2)

!!!!! calcular numero de atomos

Nat=2*(n**2+m**2+n*m)/dr

!!!!! Produto interno Ch-T
con11=n*latt1(1)+m*latt2(1)
con12=n*latt1(2)+m*latt2(2)
con21=t1*latt1(1)+t2*latt2(1)
con22=t1*latt1(2)+t2*latt2(2)

pinChT=(con11*con21)+(con12*con22)
!pinChT=((Ch(1)*T(1))+(Ch(2)*T(2)))

!PRINT*, 'Ch(1):',Ch(1), 'Ch(2):',Ch(2), 'T(1):',T(1), 'T(2):',T(2) 
!PRINT*, 'produto interno Ch.T:',pinChT, 'theta:',theta 
!PRINT*, 'numero de particulas:',Nat, 'comprimento do tubo:',L


!!!! calculo para o corte na folha 

do r1=0, 100
  do r2=0, 100
R1t=(2*t1+t2)*r1+(2*t2+t1)*r2
R2t=(2*t1+t2)*r1+(2*t2+t1)*r2+(t1+t2)
R1ch=(2*n+m)*r1+(2*m+n)*r2
R2ch=(2*n+m)*r1+(2*m+n)*r2+(n+m)
	if(R1t .lt. 2*w .and. R1t .gt. 0 .and. R1ch .lt. 2*k .and. R1ch .gt. 0) then

	xcil1=R1t/l
	ycil1=(dt/2)*cos(2*PI*R1ch/k)
	zcil1=(dt/2)*sin(2*PI*R1ch/k)
	write(13,*) 'C', xcil1, ycil1, 3*zcil1

	   if(R2t .lt. 2*w .and. R2t .gt. 0 .and. R2ch .lt. 2*k .and. R2ch .gt. 0) then

	   xcil2=R2t/l
	   ycil2=(dt/2)*cos(2*PI*R2ch/k)
	   zcil2=(dt/2)*sin(2*PI*R2ch/k)
	

	   write(13,*) 'C', xcil2, ycil2, 3*zcil2
!	   print*, 'C', xcil2, ycil2, zcil2
	   end if
	end if
  end do
end do


CLOSE(UNIT=10)	
CLOSE(UNIT=11)
CLOSE(UNIT=12)
CLOSE(UNIT=13)


END PROGRAM
