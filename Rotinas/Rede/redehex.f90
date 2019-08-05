PROGRAM THEWEB
IMPLICIT NONE

REAL(KIND=8) :: latt1(3), latt2(3), latt3(3), latt4(3), Ch(3), T(3)
INTEGER, PARAMETER :: maxim=30
REAL(KIND=8), PARAMETER :: PI = 3.14159265358979323846
REAL(KIND=8), PARAMETER :: a=2.459512146747806
REAL(KIND=8) ::	x, y, z, u, v, w, t1, t2, dt, pinChT, b
INTEGER :: i1, i2

PRINT*, '(n,m):'
READ*, a,b


OPEN(UNIT=9, FILE='saida0.dat')
OPEN(UNIT=10, FILE='saida0.xyz')
OPEN(UNIT=11, FILE='saida1.dat')
OPEN(UNIT=12, FILE='saida2.dat')

DATA latt1/2.13, 1.22, 0.00/
DATA latt2/2.13, -1.22, 0.00/

CALL diametro(a, b, dt)

t1=(2*b+a)/dt
t1=-(2*a+b)/dt

DO i1=0, maxim
DO i2=0, maxim

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

Ch(1)=a*latt1(1)+b*latt2(1)
Ch(2)=a*latt1(2)+b*latt2(2)
Ch(3)=a*latt1(3)+b*latt2(3)
T(1)=t1*latt1(1)+t2*latt2(1)
T(2)=t1*latt1(2)+t2*latt2(2)
T(3)=t1*latt1(3)+t2*latt2(3)

!pinChT=(n*latt1*t1*latt1)+(m*latt2*t2*latt2)
pinChT=(Ch(1)*T(1))+(Ch(2)*T(2))+(Ch(3)*T(3))

PRINT*, pinChT 

CLOSE(UNIT=10)	
CLOSE(UNIT=11)
CLOSE(UNIT=12)

SUBROUTINE diametro(n, m, d)
REAL(KIND=8) :: d
INTEGER :: n, m, n1, m1, n2, m2

!OPEN(UNIT=10, FILE='zigzag.dat')
!OPEN(UNIT=11, FILE='armchair.dat')

!PRINT*, '(n,m)inicial:'
!READ*, n1,m1
!PRINT*, '(n,m)final:'
!READ*, n2,m2

!d=(a*sqrt(n**2.0+m**2.0+n*m))/PI
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

IF (m1.EQ.0 .AND. m2.EQ.0) THEN
DO n=n1,n2
	DO m=m1, m2

		d=(a*n)/PI

		PRINT*, d
		WRITE(10,*) d

	END DO
END DO
END IF

IF (m1.EQ.n1 .AND. m2.EQ.n2) THEN
DO n=n1,n2
!	DO m=m1, m2

		d=(a*n*1.73205)/PI

		PRINT*, d
		WRITE(11,*) d

!	END DO
END DO
END IF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

RETURN

END SUBROUTINE

END PROGRAM
