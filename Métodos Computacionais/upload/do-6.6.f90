

PROGRAM ellis_6_6
IMPLICIT NONE
DOUBLE PRECISION:: d1,d2,effort,load,dmin,dmax
INTEGER:: n,i

PRINT*,'Entre com o comprimento mínimo em metros'
READ*,dmin
PRINT*,'Entre com o comprimento máximo em metros'
READ*,dmax
d2=2.0
load=2000.0
n=(dmax-dmin)/2
DO i=0,n
  d1=dmin+i*2
  effort=load*d2/d1
  PRINT*,'d1=',d1,'-> Effort=',effort
END DO




END PROGRAM ellis_6_6
