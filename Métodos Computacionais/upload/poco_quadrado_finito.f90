


PROGRAM square_well
IMPLICIT NONE
REAL(KIND=8):: x0,xtrial,f,x2,m,b,pi,h,x
INTEGER:: n,i,j

pi=DACOS(-1.0D0)
h=1.0D-8

WRITE(*,*) 'Entre com x0.'
READ(*,*) x0
n=x0/pi

OPEN(UNIT=1,FILE='function.dat')
DO i=1,10000
  WRITE(1,*) x0*(DFLOAT(i)/1.0D4)/pi,f(x0*(DFLOAT(i)/1.0D4),x0)
END DO
WRITE(1,*)
WRITE(1,*) 0.0D0,0.0D0
WRITE(1,*) x0,0.0D0
WRITE(1,*)
DO j=1,n
  m=(f(j*pi+0.5D0*h,x0)-f(j*pi-0.5D0*h,x0))/h
  b=f(j*pi,x0)-j*pi*m
  DO i=1,10001,10000
    x=i*x0/10000.0
    WRITE(1,*) x/pi,m*x+b
  END DO
  WRITE(1,*)
END DO

CLOSE(UNIT=1)



DO i=1,n
  xtrial=(i-1)*pi+0.5D0*pi-1.0D-2
  CALL newton_raphson(xtrial,x0)
  print*,i,'->',xtrial,f(xtrial,x0)
END DO

xtrial=n*pi+1.0D-2
x2=MIN(x0,n*pi+0.5D0*pi)
IF(f(xtrial,x0)*f(x2,x0).lt.0.0D0)THEN
  CALL newton_raphson(xtrial,x0)
  print*,i,'->',xtrial,f(xtrial,x0)
END IF

END PROGRAM square_well

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE newton_raphson(xtrial,x0)
IMPLICIT NONE
REAL(KIND=8):: f,xtrial,x0,ftrial,dftrial,h,tol
INTEGER:: iteration


tol=1.0D-10
h=1.0D-8
iteration=0
DO
  iteration=iteration+1
  ftrial=f(xtrial,x0)
  IF(ABS(ftrial).le.tol)THEN
    EXIT
  END IF
  dftrial=(f(xtrial+h*0.5D0,x0)-f(xtrial-h*0.5D0,x0))/h
  xtrial=xtrial-ftrial/dftrial
END DO

END SUBROUTINE newton_raphson

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION f(x,x0)
IMPLICIT NONE
REAL(KIND=8):: x,f,x0

f=((DSQRT(x0**2-x**2))/x)-DTAN(x)

END FUNCTION f



