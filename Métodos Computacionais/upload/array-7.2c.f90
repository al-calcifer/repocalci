

PROGRAM ellis_7_2b
IMPLICIT NONE
INTEGER:: i,init,n
DOUBLE PRECISION:: pi
DOUBLE PRECISION,ALLOCATABLE:: x(:),phi(:)

PRINT*,'Entre com o número de pontos.'
READ*,n
ALLOCATE(x(n),phi(n))

OPEN(UNIT=10,FILE='gassiana.dat')

pi=DACOS(-1.0D0)
DO i=1,n
  x(i)=-3.0D0+6.0D0*(i-1)/(n-1)
  phi(i)=(1.0D0/DSQRT(2.0D0*pi))*DEXP(-0.5D0*x(i)*x(i))
END DO

DO i=1,n
  WRITE(10,*) x(i),phi(i)
END DO


END PROGRAM ellis_7_2b
