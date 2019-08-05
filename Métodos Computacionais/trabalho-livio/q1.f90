PROGRAM q1
IMPLICIT NONE
REAL(KIND=8):: x, fmed, e 
INTEGER:: i,n

n=10000000
fmed=0.0D0
DO i=1,n
  CALL RANDOM_NUMBER(x)
  fmed=fmed+(1/(x+1))
END DO

fmed=fmed/DFLOAT(n)

e=2.0d0**(1.0d0/fmed)

WRITE(*,*) 'O valor aproximado de neperiano(e) é ', e

END PROGRAM 






