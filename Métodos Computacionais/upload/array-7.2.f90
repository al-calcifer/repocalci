

PROGRAM ellis_7_2
IMPLICIT NONE
INTEGER:: i,init
DOUBLE PRECISION:: x(31),phi(31),pi
pi=DACOS(-1.0D0)
DO i=1,31
  x(i)=i*0.2-3.2
  phi(i)=(1.0D0/DSQRT(2.0D0*pi))*DEXP(-0.5D0*x(i)*x(i))
END DO

init=1
DO i=1,6
  PRINT*,phi(init),phi(init+1),phi(init+2),phi(init+3),phi(init+4)
  init=init+5
END DO
PRINT*,phi(31)

END PROGRAM ellis_7_2
