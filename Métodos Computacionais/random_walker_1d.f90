PROGRAM random_walker_1d
IMPLICIT NONE
REAL(KIND=8):: x,passo,alpha,xmed,x2med
INTEGER:: n,m,i,j

m=1000

OPEN(UNIT=1,FILE='xmedio.dat')
OPEN(UNIT=2,FILE='x2medio_sqrt.dat')
OPEN(UNIT=3,FILE='x2medio_sqrt_expected.dat')
DO n=1,100
  xmed=0.0D0
  x2med=0.0D0
  DO j=1,m
    x=0.0D0
    DO i=1,n
      CALL RANDOM_NUMBER(passo)
      IF(passo.le.0.5D0)THEN
        alpha=-1.0D0
      ELSE
        alpha=1.0D0
      END IF
      x=x+alpha
    END DO
    xmed=xmed+x
    x2med=x2med+x*x
  END DO
  xmed=xmed/DFLOAT(m)
  x2med=x2med/DFLOAT(m)
  WRITE(1,*) n,xmed
  WRITE(2,*) n,DSQRT(x2med)
  WRITE(3,*) n,DSQRT(DFLOAT(n))
END DO
WRITE(1,*) 
WRITE(2,*) 
WRITE(3,*) 
CLOSE(UNIT=1)
CLOSE(UNIT=2)
CLOSE(UNIT=3)


END PROGRAM random_walker_1d






