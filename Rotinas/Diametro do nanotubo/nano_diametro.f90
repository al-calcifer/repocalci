PROGRAM DIAMETRO
IMPLICIT NONE

REAL(KIND=8), PARAMETER :: PI = 3.14159265358979323846
REAL(KIND=8), PARAMETER :: a=2.459512146747806
REAL(KIND=8) :: d
INTEGER :: n, m, n1, m1, n2, m2

OPEN(UNIT=10, FILE='zigzag.dat')
OPEN(UNIT=11, FILE='armchair.dat')

PRINT*, '(n,m)inicial:'
READ*, n1,m1
PRINT*, '(n,m)final:'
READ*, n2,m2

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

END PROGRAM
